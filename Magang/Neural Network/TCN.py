# Library Imports
import sys
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
import tensorflow as tf
from tensorflow import keras
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt
from sklearn.model_selection import TimeSeriesSplit

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# Dataset has columns: 'Date', 'Transportation', 'Holiday'
data['Date'] = pd.to_datetime(data['Date'])
data.set_index('Date', inplace=True)

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Feature scaling (include 'Holiday' as an additional feature)
scaler = MinMaxScaler(feature_range=(0, 1))
train_scaled = scaler.fit_transform(train)
test_scaled = scaler.transform(test)

# Prepare data for TCN
def create_dataset(dataset):
    X, y = [], []
    for i in range(len(dataset)):
        X.append(dataset[i, :])  # Include 'Holiday' as an additional feature
        y.append(dataset[i, 0])  # 'Transportation' as the target
    return np.array(X), np.array(y)

X_train, y_train = create_dataset(train_scaled)
X_test, y_test = create_dataset(test_scaled)

# Ensure the dataset has enough data points
if len(test_scaled) <= 0:
    raise ValueError("Test set does not have enough data points. Please increase the test set size.")

# Reshape input to be [samples, timesteps, features] where timesteps=1 for this case
X_train = X_train.reshape((X_train.shape[0], 1, X_train.shape[1]))  # Shape: (samples, 1, features)
X_test = X_test.reshape((X_test.shape[0], 1, X_test.shape[1]))      # Shape: (samples, 1, features)

# Build TCN model
from tensorflow.keras.layers import Conv1D, Dense, Dropout, Input

def build_tcn_model(input_shape):
    model = keras.Sequential([
        Input(shape=input_shape),  # Input shape: (timesteps, features)
        
        # First convolutional layer (with dilation)
        Conv1D(filters=64, kernel_size=3, dilation_rate=1, activation='relu', padding='causal'),
        Dropout(0.2),
        
        # Second convolutional layer (with dilation)
        Conv1D(filters=128, kernel_size=3, dilation_rate=2, activation='relu', padding='causal'),
        Dropout(0.2),
        
        # Third convolutional layer (with dilation)
        Conv1D(filters=64, kernel_size=3, dilation_rate=4, activation='relu', padding='causal'),
        Dropout(0.2),
        
        # Dense layer for output
        Dense(1)
    ])
    
    # Compile the model
    model.compile(optimizer='adam', loss='mean_squared_error')
    return model

# Build model
model = build_tcn_model(X_train.shape[1:])

# Print the model summary
model.summary()

# Train the model
history = model.fit(X_train, y_train, epochs=120, batch_size=32, validation_data=(X_test, y_test), verbose=1)

# Plot training and validation loss
plt.figure(figsize=(10, 6))
plt.plot(history.history['loss'], label='Training Loss', color='blue')
plt.plot(history.history['val_loss'], label='Validation Loss', color='orange')
plt.title('Training and Validation Loss')
plt.xlabel('Epochs')
plt.ylabel('Loss')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()

# Predict
train_predict = model.predict(X_train)
test_predict = model.predict(X_test)

# Print shapes for debugging
print("\nOriginal prediction shapes:")
print(f"train_predict shape: {train_predict.shape}")
print(f"test_predict shape: {test_predict.shape}")

# Reshape predictions to 2D before inverse transform
train_predict = train_predict.reshape((train_predict.shape[0], 1))
test_predict = test_predict.reshape((test_predict.shape[0], 1))

# Create arrays with zeros for the 'Holiday' column
train_with_zeros = np.concatenate((train_predict, np.zeros((train_predict.shape[0], 1))), axis=1)
test_with_zeros = np.concatenate((test_predict, np.zeros((test_predict.shape[0], 1))), axis=1)

print("\nShapes before inverse transform:")
print(f"train_with_zeros shape: {train_with_zeros.shape}")
print(f"test_with_zeros shape: {test_with_zeros.shape}")

# Inverse transform predictions
train_predict = scaler.inverse_transform(train_with_zeros)[:, 0]
test_predict = scaler.inverse_transform(test_with_zeros)[:, 0]

# Prepare actual values for inverse transform
y_train_2d = y_train.reshape(-1, 1)
y_test_2d = y_test.reshape(-1, 1)

# Create arrays with zeros for the 'Holiday' column for actual values
y_train_with_zeros = np.concatenate((y_train_2d, np.zeros((y_train_2d.shape[0], 1))), axis=1)
y_test_with_zeros = np.concatenate((y_test_2d, np.zeros((y_test_2d.shape[0], 1))), axis=1)

# Inverse transform actual values
y_train_actual = scaler.inverse_transform(y_train_with_zeros)[:, 0]
y_test_actual = scaler.inverse_transform(y_test_with_zeros)[:, 0]

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
plt.title('TCN Model Predictions vs Actual Data')
plt.xlabel('Date')
plt.ylabel('Transportation')
plt.xticks(rotation=45)
plt.grid()
plt.legend()
plt.tight_layout()
plt.show()

# Cross-validation setup for time-series
from sklearn.model_selection import TimeSeriesSplit
ts_splits = 5
tscv = TimeSeriesSplit(n_splits=ts_splits)

# Perform cross-validation
cv_results = []
for train_index, test_index in tscv.split(train_scaled):
    X_train_cv, X_test_cv = train_scaled[train_index], train_scaled[test_index]
    y_train_cv, y_test_cv = train_scaled[train_index, 0], train_scaled[test_index, 0]

    # Reshape input to be [samples, time steps, features]
    X_train_cv = X_train_cv.reshape((X_train_cv.shape[0], 1, X_train_cv.shape[1]))
    X_test_cv = X_test_cv.reshape((X_test_cv.shape[0], 1, X_test_cv.shape[1]))

    # Train the model
    model.fit(X_train_cv, y_train_cv, epochs=120, batch_size=32, verbose=0)

    # Evaluate the model
    loss = model.evaluate(X_test_cv, y_test_cv, verbose=0)
    cv_results.append(loss)

# Calculate average cross-validation loss
average_loss = np.mean(cv_results)
print(f'Average Cross-Validation Loss: {average_loss}')
