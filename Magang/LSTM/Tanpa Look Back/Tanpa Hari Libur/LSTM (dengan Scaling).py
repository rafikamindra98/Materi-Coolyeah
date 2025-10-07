# Librarys
import sys
print("Python executable being used:", sys.executable)
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
from sklearn.metrics import mean_squared_error

import matplotlib.pyplot as plt
from sklearn.model_selection import TimeSeriesSplit

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset 2023-2024.xlsx')

# Pastikan dataset memiliki kolom: 'Waktu' dan 'Pengangkutan'
data['Waktu'] = pd.to_datetime(data['Waktu'])
data.set_index('Waktu', inplace=True)

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Feature scaling
scaler = MinMaxScaler(feature_range=(0, 1))
train_scaled = scaler.fit_transform(train)
test_scaled = scaler.transform(test)

# Prepare data for LSTM
def create_dataset(dataset):
    X, y = [], []
    for i in range(len(dataset)):
        X.append(dataset[i, :])
        y.append(dataset[i, 0])  # Kolom 0 adalah 'Pengangkutan'
    return np.array(X), np.array(y)

X_train, y_train = create_dataset(train_scaled)
X_test, y_test = create_dataset(test_scaled)

# Ensure the dataset has enough data points
if len(test_scaled) <= 0:
    raise ValueError("Test set does not have enough data points. Please increase the test set size.")

# Reshape input to be [samples, time steps, features]
X_train = X_train.reshape((X_train.shape[0], X_train.shape[1], train_scaled.shape[1]))
X_test = X_test.reshape((X_test.shape[0], X_test.shape[1], test_scaled.shape[1]))

# Build LSTM model
from tensorflow.keras.layers import Dropout
model = Sequential([
    LSTM(100, return_sequences=True, input_shape=(X_train.shape[1], X_train.shape[2])),
    Dropout(0.2),
    LSTM(50),
    Dropout(0.2),
    Dense(1)
])

# Compile the model
from tensorflow.keras.optimizers import Adam
model.compile(optimizer=Adam(learning_rate=0.001), loss='mean_squared_error')
model.summary()

# Train the model
history = model.fit(X_train, y_train, epochs=150, batch_size=32, validation_data=(X_test, y_test), verbose=1)

# Plot training and validation loss
plt.figure(figsize=(10, 6))
plt.plot(history.history['loss'], label='Training Loss')
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

from tensorflow.keras.callbacks import EarlyStopping

# Tambahkan EarlyStopping dengan kriteria val_loss mendekati data aktual
early_stopping = EarlyStopping(monitor='val_loss', patience=10, restore_best_weights=True, min_delta=0.01)  # min_delta menentukan seberapa kecil perubahan yang dianggap signifikan

# Latih model dengan EarlyStopping
model.fit(X_train, y_train, epochs=150, batch_size=32, validation_data=(X_test, y_test),
          callbacks=[early_stopping], verbose=1)

history = model.fit(X_train, y_train, epochs=150, batch_size=32, validation_data=(X_test, y_test), verbose=1)

# Callback untuk mengurangi learning rate
from tensorflow.keras.callbacks import ReduceLROnPlateau
reduce_lr = ReduceLROnPlateau(monitor='val_loss', factor=0.5, patience=5, min_lr=1e-5)
model.fit(X_train, y_train, epochs=150, batch_size=32, validation_data=(X_test, y_test),
          callbacks=[early_stopping, reduce_lr], verbose=1)

# Evaluate model
train_rmse = np.sqrt(mean_squared_error(y_train_actual, train_predict))
test_rmse = np.sqrt(mean_squared_error(y_test_actual, test_predict))
print(f'Train RMSE: {train_rmse}')
print(f'Test RMSE: {test_rmse}')

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
plt.ylabel('Pengangkutan')
plt.xticks(rotation=45)
plt.grid()
plt.legend()
plt.tight_layout()
plt.show()

# Define the number of splits for cross-validation
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
    model.fit(X_train_cv, y_train_cv, epochs=150, batch_size=32, verbose=0)

    # Evaluate the model
    loss = model.evaluate(X_test_cv, y_test_cv, verbose=0)
    cv_results.append(loss)

# Calculate average cross-validation loss
average_loss = np.mean(cv_results)
print(f'Average Cross-Validation Loss: {average_loss}')

# Log train-test indices and visualize splits
for fold, (train_index, test_index) in enumerate(tscv.split(train_scaled)):
    print(f"Fold {fold + 1}:")
    print(f"Train indices: {train_index[:5]}...{train_index[-5:]}")
    print(f"Test indices: {test_index[:5]}...{test_index[-5:]}")

    # Visualize train-test split
    plt.figure(figsize=(10, 2))
    plt.plot(train_index, np.zeros_like(train_index), 'b.', label='Train')
    plt.plot(test_index, np.zeros_like(test_index), 'r.', label='Test')
    plt.title(f"Train-Test Split for Fold {fold + 1}")
    plt.legend()
    plt.show()

    # Ensure scaling is done only on train data
    train_fold = train_scaled[train_index]
    test_fold = train_scaled[test_index]

    scaler = MinMaxScaler()
    train_fold_scaled = scaler.fit_transform(train_fold)
    test_fold_scaled = scaler.transform(test_fold)  # Test data scaled using train scaler