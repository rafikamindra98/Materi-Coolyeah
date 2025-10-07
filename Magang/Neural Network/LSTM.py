# Librarys
import sys
print("Python executable being used:", sys.executable)
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

# Prepare data for LSTM
def create_dataset(dataset):
    # Each sample is already a feature vector, no need for additional processing
    X = dataset  # Shape will be (samples, features)
    y = dataset[:, 0]  # First column (Transportation) is our target
    return X, y

X_train, y_train = create_dataset(train_scaled)
X_test, y_test = create_dataset(test_scaled)

# Ensure the dataset has enough data points
if len(test_scaled) <= 0:
    raise ValueError("Test set does not have enough data points. Please increase the test set size.")

# Reshape input to be [samples, timesteps, features] where timesteps=1 for this case
X_train = X_train.reshape((X_train.shape[0], 1, X_train.shape[1]))  # Shape: (samples, 1, features)
X_test = X_test.reshape((X_test.shape[0], 1, X_test.shape[1]))      # Shape: (samples, 1, features)

# Build LSTM model
model = keras.Sequential([
    keras.layers.Input(shape=(1, train_scaled.shape[1])),  # Shape: (timesteps, features)
    keras.layers.LSTM(100, return_sequences=True),
    keras.layers.Dropout(0.2),
    keras.layers.LSTM(50),
    keras.layers.Dropout(0.2),
    keras.layers.Dense(1)
])

# Compile the model
optimizer = keras.optimizers.Adam(learning_rate=0.001)
model.compile(optimizer=optimizer, loss='mean_squared_error')
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

# Inverse transform predictions
train_predict = scaler.inverse_transform(np.concatenate((train_predict, np.zeros((train_predict.shape[0], train_scaled.shape[1] - 1))), axis=1))[:, 0]
test_predict = scaler.inverse_transform(np.concatenate((test_predict, np.zeros((test_predict.shape[0], test_scaled.shape[1] - 1))), axis=1))[:, 0]

# Inverse transform actual values
y_train_actual = scaler.inverse_transform(np.concatenate((y_train.reshape(-1, 1), np.zeros((y_train.shape[0], train_scaled.shape[1] - 1))), axis=1))[:, 0]
y_test_actual = scaler.inverse_transform(np.concatenate((y_test.reshape(-1, 1), np.zeros((y_test.shape[0], test_scaled.shape[1] - 1))), axis=1))[:, 0]

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
plt.title('LSTM Model Predictions vs Actual Data')
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
    X_train_cv = X_train_cv.reshape((X_train_cv.shape[0], 1, train_scaled.shape[1]))
    X_test_cv = X_test_cv.reshape((X_test_cv.shape[0], 1, train_scaled.shape[1]))

    # Train the model
    model.fit(X_train_cv, y_train_cv, epochs=120, batch_size=32, verbose=0)

    # Evaluate the model
    loss = model.evaluate(X_test_cv, y_test_cv, verbose=0)
    cv_results.append(loss)

# Calculate average cross-validation loss
average_loss = np.mean(cv_results)
print(f'Average Cross-Validation Loss: {average_loss}')