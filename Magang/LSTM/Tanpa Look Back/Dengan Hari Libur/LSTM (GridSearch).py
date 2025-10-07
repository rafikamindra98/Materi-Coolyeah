# Required Libraries
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
import tensorflow as tf
from tensorflow import keras
from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt
from itertools import product

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

# Prepare data for LSTM
def create_dataset(dataset):
    # For a model without lookback, we just need to reshape the data
    # Each sample will have shape (1, num_features)
    X = dataset  # Shape: (samples, features)
    y = dataset[:, 0]  # Target is the first column (Pengangkutan)
    return X, y

X_train, y_train = create_dataset(train_scaled)
X_test, y_test = create_dataset(test_scaled)

# Ensure the dataset has enough data points
if len(test_scaled) <= 0:
    raise ValueError("Test set does not have enough data points. Please increase the test set size.")

# Reshape input to be [samples, time steps, features]
# For a model without lookback, time steps is 1
X_train = X_train.reshape((X_train.shape[0], 1, X_train.shape[1]))
X_test = X_test.reshape((X_test.shape[0], 1, X_test.shape[1]))

# Function to create and compile the LSTM model
def create_model(units=100, dropout_rate=0.2, optimizer='adam'):
    model = keras.Sequential([
        keras.layers.LSTM(units, return_sequences=True, input_shape=(1, train_scaled.shape[1])),
        keras.layers.Dropout(dropout_rate),
        keras.layers.LSTM(units // 2),
        keras.layers.Dropout(dropout_rate),
        keras.layers.Dense(1)
    ])
    
    if optimizer == 'adam':
        opt = keras.optimizers.Adam(learning_rate=0.001)
    else:  # rmsprop
        opt = keras.optimizers.RMSprop(learning_rate=0.001)
        
    model.compile(optimizer=opt, loss='mean_squared_error')
    return model

# Define the hyperparameters to test
hyperparameters = {
    'units': [50, 100],
    'dropout_rate': [0.2, 0.3],
    'optimizer': ['adam', 'rmsprop'],
    'epochs': [100, 120],
    'batch_size': [32, 64]
}

# Manual implementation of grid search with cross-validation
best_score = float('inf')
best_params = None
best_model = None
kf = KFold(n_splits=3, shuffle=True, random_state=42)

# Store all combinations of hyperparameters
param_combinations = [dict(zip(hyperparameters.keys(), v)) for v in product(*hyperparameters.values())]

# Perform grid search
print("Starting Grid Search...")
for params in param_combinations:
    cv_scores = []
    
    for train_idx, val_idx in kf.split(X_train):
        # Split data
        X_train_cv, X_val_cv = X_train[train_idx], X_train[val_idx]
        y_train_cv, y_val_cv = y_train[train_idx], y_train[val_idx]
        
        # Create and train model
        model = create_model(
            units=params['units'],
            dropout_rate=params['dropout_rate'],
            optimizer=params['optimizer']
        )
        
        # Train the model
        model.fit(
            X_train_cv, y_train_cv,
            epochs=params['epochs'],
            batch_size=params['batch_size'],
            verbose=0
        )
        
        # Evaluate the model
        val_pred = model.predict(X_val_cv, verbose=0)
        mse = mean_squared_error(y_val_cv, val_pred)
        cv_scores.append(mse)
    
    # Calculate mean score for this parameter combination
    mean_score = np.mean(cv_scores)
    
    # Update best parameters if this combination is better
    if mean_score < best_score:
        best_score = mean_score
        best_params = params
        # Retrain model on full training data with best parameters
        best_model = create_model(
            units=params['units'],
            dropout_rate=params['dropout_rate'],
            optimizer=params['optimizer']
        )
        best_model.fit(
            X_train, y_train,
            epochs=params['epochs'],
            batch_size=params['batch_size'],
            verbose=0
        )

print(f"Best Hyperparameters: {best_params}")
print(f"Best Score (MSE): {best_score}")

# Make predictions with the best model
train_predict = best_model.predict(X_train, verbose=0)
test_predict = best_model.predict(X_test, verbose=0)

# Inverse transform predictions
train_predict = scaler.inverse_transform(np.concatenate((train_predict, np.zeros((train_predict.shape[0], train_scaled.shape[1] - 1))), axis=1))[:, 0]
test_predict = scaler.inverse_transform(np.concatenate((test_predict, np.zeros((test_predict.shape[0], test_scaled.shape[1] - 1))), axis=1))[:, 0]

# Inverse transform actual values
y_train_actual = scaler.inverse_transform(np.concatenate((y_train.reshape(-1, 1), np.zeros((y_train.shape[0], train_scaled.shape[1] - 1))), axis=1))[:, 0]
y_test_actual = scaler.inverse_transform(np.concatenate((y_test.reshape(-1, 1), np.zeros((y_test.shape[0], test_scaled.shape[1] - 1))), axis=1))[:, 0]

# Calculate and display metrics (MAPE, MAE, RMSE)
train_mape = np.mean(np.abs((y_train_actual - train_predict) / y_train_actual)) * 100
test_mape = np.mean(np.abs((y_test_actual - test_predict) / y_test_actual)) * 100
train_mae = np.mean(np.abs(y_train_actual - train_predict))
test_mae = np.mean(np.abs(y_test_actual - test_predict))

train_rmse = np.sqrt(mean_squared_error(y_train_actual, train_predict))
test_rmse = np.sqrt(mean_squared_error(y_test_actual, test_predict))

print(f'Train RMSE: {train_rmse}')
print(f'Test RMSE: {test_rmse}')
print(f'Train MAPE: {train_mape:.2f}%')
print(f'Test MAPE: {test_mape:.2f}%')
print(f'Train MAE: {train_mae}')
print(f'Test MAE: {test_mae}')

# Plot results
plt.figure(figsize=(12, 6))
plt.plot(data.index[:len(y_train_actual)], y_train_actual, label='Train Actual')
plt.plot(data.index[:len(train_predict)], train_predict, label='Train Predicted')
plt.plot(data.index[len(y_train_actual):len(y_train_actual) + len(y_test_actual)], y_test_actual, label='Test Actual')
plt.plot(data.index[len(train_predict):], test_predict, label='Test Predicted')
plt.title('LSTM Model Predictions vs Actual Data')
plt.xlabel('Date')
plt.ylabel('Pengangkutan')
plt.xticks(rotation=45)
plt.grid()
plt.legend()
plt.tight_layout()
plt.show()
