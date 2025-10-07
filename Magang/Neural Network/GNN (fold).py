# Library Imports
import sys
import pandas as pd
import numpy as np
import tensorflow as tf
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras import Model, layers
from tensorflow.keras.layers import Dense, Dropout
from sklearn.model_selection import TimeSeriesSplit
from sklearn.metrics import mean_squared_error

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# Dataset has columns: 'Waktu', 'Pengangkutan', 'Libur'
data['Waktu'] = pd.to_datetime(data['Waktu'])
data.set_index('Waktu', inplace=True)

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Feature scaling (include 'Libur' as an additional feature)
scaler = MinMaxScaler(feature_range=(0, 1))
train_scaled = scaler.fit_transform(train)
test_scaled = scaler.transform(test)

# Prepare data for GNN
def create_dataset(dataset):
    X, y = [], []
    for i in range(len(dataset)):
        X.append(dataset[i, :])  # Include 'Libur' as an additional feature
        y.append(dataset[i, 0])  # 'Pengangkutan' as the target
    return np.array(X), np.array(y)

X_train, y_train = create_dataset(train_scaled)
X_test, y_test = create_dataset(test_scaled)

# Ensure the dataset has enough data points
if len(test_scaled) <= 0:
    raise ValueError("Test set does not have enough data points. Please increase the test set size.")

# Create the graph structure: nodes connected based on time (edges between consecutive nodes)
window_size = 5  # Number of time steps to consider as neighbors
adj_matrix = np.zeros((X_train.shape[0], X_train.shape[0]))
for i in range(X_train.shape[0]):
    # Connect with previous and next window_size steps
    start = max(0, i - window_size)
    end = min(X_train.shape[0], i + window_size + 1)
    adj_matrix[i, start:end] = 1
    adj_matrix[i, i] = 1  # Self-connection

# Normalize adjacency matrix
adj_matrix = adj_matrix / np.maximum(adj_matrix.sum(axis=1, keepdims=True), 1)
adj_matrix = tf.convert_to_tensor(adj_matrix, dtype=tf.float32)
X_train = tf.convert_to_tensor(X_train, dtype=tf.float32)
y_train = tf.convert_to_tensor(y_train, dtype=tf.float32)

# Custom Graph Convolutional Layer
class GraphConvLayer(layers.Layer):
    def __init__(self, units, activation='relu'):
        super().__init__()
        self.units = units
        self.activation = tf.keras.activations.get(activation)
        
    def build(self, input_shape):
        features_shape = input_shape[0][-1]  # Get the last dimension of the features
        self.weight = self.add_weight(
            shape=(features_shape, self.units),
            initializer='glorot_uniform',
            trainable=True,
            name='weight'
        )
        
    def call(self, inputs):
        features, adj = inputs
        batch_size = tf.shape(features)[0]
        
        # Ensure features are 3D (batch_size, nodes, features)
        if len(features.shape) == 2:
            features = tf.expand_dims(features, 1)  # Add node dimension
            features = tf.tile(features, [1, tf.shape(adj)[0], 1])  # Replicate for each node
        
        # First compute XW
        x = tf.matmul(features, self.weight)  # Shape: (batch_size, nodes, units)
        
        # Prepare adjacency matrix for batch processing
        if len(adj.shape) == 2:
            # If adj is 2D, expand for batch processing
            adj = tf.expand_dims(adj, 0)  # Shape: (1, nodes, nodes)
            adj = tf.tile(adj, [batch_size, 1, 1])  # Shape: (batch_size, nodes, nodes)
        
        # Perform graph convolution: AXW
        output = tf.matmul(adj, x)  # Shape: (batch_size, nodes, units)
        return self.activation(output)

# Build the GNN model
class GNNModel(Model):
    def __init__(self):
        super().__init__()
        self.gc1 = GraphConvLayer(64)
        self.gc2 = GraphConvLayer(32)
        self.dropout = Dropout(0.2)
        self.dense = Dense(1)
        
    def call(self, inputs):
        features, adj = inputs
        
        # First graph conv layer
        x = self.gc1([features, adj])
        x = self.dropout(x)
        
        # Second graph conv layer
        x = self.gc2([x, adj])
        x = self.dropout(x)
        
        # Global average pooling over nodes
        x = tf.reduce_mean(x, axis=1)  # Average over nodes dimension
        
        # Final prediction
        return self.dense(x)  # Output shape: (batch_size, 1)

# Initialize and compile the model
model = GNNModel()
optimizer = tf.keras.optimizers.Adam(learning_rate=0.001)
model.compile(optimizer=optimizer, loss='mean_squared_error')

# Training parameters
BATCH_SIZE = 32
num_epochs = 120

# Create and normalize test adjacency matrix before training
test_adj_matrix = np.zeros((X_test.shape[0], X_test.shape[0]))
for i in range(X_test.shape[0]):
    # Connect with previous and next window_size steps
    start = max(0, i - window_size)
    end = min(X_test.shape[0], i + window_size + 1)
    test_adj_matrix[i, start:end] = 1
    test_adj_matrix[i, i] = 1  # Self-connection

# Normalize test adjacency matrix
test_adj_matrix = test_adj_matrix / np.maximum(test_adj_matrix.sum(axis=1, keepdims=True), 1)
test_adj_matrix = tf.convert_to_tensor(test_adj_matrix, dtype=tf.float32)
X_test = tf.convert_to_tensor(X_test, dtype=tf.float32)
y_test = tf.convert_to_tensor(y_test, dtype=tf.float32)  # Convert y_test to float32

# Training loop

# Training loop
history = {'train_loss': [], 'val_loss': []}
for epoch in range(num_epochs):
    epoch_loss = []
    
    # Process data in batches
    for i in range(0, len(X_train), BATCH_SIZE):
        end_idx = min(i + BATCH_SIZE, len(X_train))
        batch_features = X_train[i:end_idx]
        batch_targets = y_train[i:end_idx]
        
        with tf.GradientTape() as tape:
            # Get predictions
            predictions = model([batch_features, adj_matrix])
            # Reshape targets to match predictions
            batch_targets = tf.reshape(batch_targets, [-1, 1])
            # Compute loss
            loss = tf.reduce_mean(tf.square(predictions - batch_targets))
        
        # Compute and apply gradients
        gradients = tape.gradient(loss, model.trainable_variables)
        optimizer.apply_gradients(zip(gradients, model.trainable_variables))
        epoch_loss.append(float(loss))
    
    # Calculate validation loss
    test_predictions = model([X_test, test_adj_matrix])
    val_loss = tf.reduce_mean(tf.square(test_predictions - tf.reshape(y_test, [-1, 1])))
    
    # Record losses
    avg_loss = np.mean(epoch_loss)
    history['train_loss'].append(avg_loss)
    history['val_loss'].append(float(val_loss))
    
    if (epoch + 1) % 10 == 0:
        print(f'Epoch {epoch + 1}/{num_epochs}, Train Loss: {avg_loss:.4f}, Val Loss: {float(val_loss):.4f}')

# Plot training and validation loss
plt.figure(figsize=(10, 6))
plt.plot(history['train_loss'], label='Training Loss', color='blue')
plt.plot(history['val_loss'], label='Validation Loss', color='orange')
plt.title('Training and Validation Loss')
plt.xlabel('Epochs')
plt.ylabel('Loss')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()

# Predict on train and test data
train_predict = model([X_train, adj_matrix])
train_predict = train_predict.numpy()

# X_test and test_adj_matrix are already created and normalized earlier

# Predict on test data
test_predict = model([X_test, test_adj_matrix])
test_predict = test_predict.numpy()

# Convert targets to numpy if they're tensors
if isinstance(y_train, tf.Tensor):
    y_train = y_train.numpy()
if isinstance(y_test, tf.Tensor):
    y_test = y_test.numpy()

# Reshape predictions for inverse transform
train_predict = train_predict.reshape(-1, 1)
test_predict = test_predict.reshape(-1, 1)

# Inverse transform predictions
train_predict_full = np.concatenate((train_predict, np.zeros((train_predict.shape[0], train_scaled.shape[1] - 1))), axis=1)
test_predict_full = np.concatenate((test_predict, np.zeros((test_predict.shape[0], test_scaled.shape[1] - 1))), axis=1)

train_predict = scaler.inverse_transform(train_predict_full)[:, 0]
test_predict = scaler.inverse_transform(test_predict_full)[:, 0]

# Inverse transform actual values
y_train_2d = y_train.reshape(-1, 1)
y_test_2d = y_test.reshape(-1, 1)

y_train_full = np.concatenate((y_train_2d, np.zeros((y_train_2d.shape[0], train_scaled.shape[1] - 1))), axis=1)
y_test_full = np.concatenate((y_test_2d, np.zeros((y_test_2d.shape[0], test_scaled.shape[1] - 1))), axis=1)

y_train_actual = scaler.inverse_transform(y_train_full)[:, 0]
y_test_actual = scaler.inverse_transform(y_test_full)[:, 0]

# Calculate MAPE (Mean Absolute Percentage Error)
train_mape = np.mean(np.abs((y_train_actual - train_predict) / y_train_actual)) * 100
test_mape = np.mean(np.abs((y_test_actual - test_predict) / y_test_actual)) * 100

# Calculate MAE (Mean Absolute Error)
train_mae = np.mean(np.abs(y_train_actual - train_predict))
test_mae = np.mean(np.abs(y_test_actual - test_predict))

# Evaluate model
train_rmse = np.sqrt(mean_squared_error(y_train_actual, train_predict))
test_rmse = np.sqrt(mean_squared_error(y_test_actual, test_predict))

# Display metrics
print(f'Train RMSE: {train_rmse}')
print(f'Test RMSE: {test_rmse}')
print(f'Train MAPE: {train_mape:.2f}%')
print(f'Test MAPE: {test_mape:.2f}%')
print(f'Train MAE: {train_mae}')
print(f'Test MAE: {test_mae}')

# Plot results
plt.figure(figsize=(12, 6))

# Plot Train Actual and Train Predicted (2023-01-01 to 2025-01-01)
train_dates = data['2023-01-01':'2025-01-01'].index[0:len(y_train_actual)]
plt.plot(train_dates, y_train_actual, label='Train Actual')
plt.plot(train_dates, train_predict[:len(train_dates)], label='Train Predicted')

# Plot Test Actual and Test Predicted (2025-01-01 to 2025-02-28)
test_dates = data['2025-01-01':'2025-02-28'].index[:len(test_predict)]
plt.plot(test_dates, y_test_actual, label='Test Actual')
plt.plot(test_dates, test_predict, label='Test Predicted')

# Add details to the plot
plt.title('GNN Model Predictions vs Actual Data')
plt.xlabel('Date')
plt.ylabel('Pengangkutan')
plt.xticks(rotation=45)
plt.grid()
plt.legend()
plt.tight_layout()
plt.show()

# Convert tensors to numpy for cross-validation split
if isinstance(train_scaled, tf.Tensor):
    train_scaled = train_scaled.numpy()

# Cross-validation setup for time-series
ts_splits = 5
tscv = TimeSeriesSplit(n_splits=ts_splits)

# Perform cross-validation
cv_results = []
fold_histories = []  # Store training history for each fold
for fold, (train_index, val_index) in enumerate(tscv.split(train_scaled)):
    print(f"\nTraining fold {fold + 1}/{ts_splits}")
    
    # Split data
    X_train_cv, X_val_cv = train_scaled[train_index], train_scaled[val_index]
    y_train_cv, y_val_cv = X_train_cv[:, 0], X_val_cv[:, 0]  # First column is target
    
    # Create adjacency matrix for training fold
    adj_cv = np.zeros((len(train_index), len(train_index)))
    for i in range(len(train_index)):
        start = max(0, i - window_size)
        end = min(len(train_index), i + window_size + 1)
        adj_cv[i, start:end] = 1
        adj_cv[i, i] = 1
    
    # Normalize adjacency matrix
    adj_cv = adj_cv / np.maximum(adj_cv.sum(axis=1, keepdims=True), 1)
    adj_cv = tf.convert_to_tensor(adj_cv, dtype=tf.float32)
    
    # Convert features to tensors
    X_train_cv = tf.convert_to_tensor(X_train_cv, dtype=tf.float32)
    y_train_cv = tf.convert_to_tensor(y_train_cv, dtype=tf.float32)
    X_val_cv = tf.convert_to_tensor(X_val_cv, dtype=tf.float32)
    y_val_cv = tf.convert_to_tensor(y_val_cv, dtype=tf.float32)
    
    # Create and compile fresh model for each fold
    fold_model = GNNModel()
    fold_optimizer = tf.keras.optimizers.Adam(learning_rate=0.001)
    fold_model.compile(optimizer=fold_optimizer, loss='mean_squared_error')
    
    # Train the model
    history = []
    for epoch in range(num_epochs):
        epoch_loss = []
        for i in range(0, len(X_train_cv), BATCH_SIZE):
            end_idx = min(i + BATCH_SIZE, len(X_train_cv))
            batch_features = X_train_cv[i:end_idx]
            batch_targets = y_train_cv[i:end_idx]
            
            with tf.GradientTape() as tape:
                predictions = fold_model([batch_features, adj_cv])
                batch_targets = tf.reshape(batch_targets, [-1, 1])
                loss = tf.reduce_mean(tf.square(predictions - batch_targets))
            
            gradients = tape.gradient(loss, fold_model.trainable_variables)
            fold_optimizer.apply_gradients(zip(gradients, fold_model.trainable_variables))
            epoch_loss.append(float(loss))
        
        # Calculate validation loss
        val_predictions = fold_model([X_val_cv, adj_cv])
        val_loss = tf.reduce_mean(tf.square(val_predictions - tf.reshape(y_val_cv, [-1, 1])))
        
        history.append({
            'train_loss': avg_loss,
            'val_loss': float(val_loss)
        })
        
        if (epoch + 1) % 10 == 0:
            print(f'Fold {fold + 1}, Epoch {epoch + 1}: Train Loss = {avg_loss:.4f}, Val Loss = {val_loss:.4f}')
    
    fold_histories.append(history)
    
    # Plot losses for this fold
    plt.figure(figsize=(10, 6))
    train_losses = [h['train_loss'] for h in history]
    val_losses = [h['val_loss'] for h in history]
    plt.plot(train_losses, label=f'Fold {fold + 1} Training Loss')
    plt.plot(val_losses, label=f'Fold {fold + 1} Validation Loss')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.title(f'Training and Validation Loss for Fold {fold + 1}')
    plt.legend()
    plt.show()

# Plot average losses across all folds
plt.figure(figsize=(10, 6))
for fold, history in enumerate(fold_histories):
    train_losses = [h['train_loss'] for h in history]
    val_losses = [h['val_loss'] for h in history]
    plt.plot(train_losses, alpha=0.3, label=f'Fold {fold + 1} Train')
    plt.plot(val_losses, alpha=0.3, label=f'Fold {fold + 1} Val')

# Calculate and plot average losses
avg_train = np.mean([[h['train_loss'] for h in history] for history in fold_histories], axis=0)
avg_val = np.mean([[h['val_loss'] for h in history] for history in fold_histories], axis=0)
plt.plot(avg_train, linewidth=2, label='Average Train Loss', color='blue')
plt.plot(avg_val, linewidth=2, label='Average Val Loss', color='orange')

plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.title('Average Training and Validation Loss Across All Folds')
plt.legend()
plt.show()