import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error, mean_absolute_error
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, LSTM

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# Preprocessing
data['Time'] = pd.to_datetime(data['Time'])
data.set_index('Time', inplace=True)

# Add day of week feature
data['DayOfWeek'] = data.index.dayofweek

# Add month feature
data['Month'] = data.index.month

# Use additional features in model
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']
train_exog = train[['Holiday', 'DayOfWeek', 'Month']]
test_exog = test[['Holiday', 'DayOfWeek', 'Month']]

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Feature engineering
train['Holiday'] = train.index.isin(pd.to_datetime(['2023-01-01', '2023-12-25'])) * 1
test['Holiday'] = test.index.isin(pd.to_datetime(['2025-01-01', '2025-12-25'])) * 1
train['Transportation'] = train['Transportation'].astype(float)
test['Transportation'] = test['Transportation'].astype(float)

# Normalization using MinMaxScaler
scaler = MinMaxScaler()
train_scaled = scaler.fit_transform(train[['Transportation']])
test_scaled = scaler.transform(test[['Transportation']])

# Prepare data for RNN without time steps
X_train = train_scaled
y_train = train_scaled
X_test = test_scaled
y_test = test_scaled

# Reshape data for LSTM input
X_train = X_train.reshape((X_train.shape[0], 1, 1))
X_test = X_test.reshape((X_test.shape[0], 1, 1))

# Build RNN model
model = Sequential([
    LSTM(50, activation='relu', input_shape=(1, 1)),
    Dense(1)
])
model.compile(optimizer='adam', loss='mse')

# Train the model
model.fit(X_train, y_train, epochs=10, batch_size=8, verbose=1)

# Forecast
y_pred = model.predict(X_test)
y_pred_rescaled = scaler.inverse_transform(y_pred)
y_test_rescaled = scaler.inverse_transform(y_test.reshape(-1, 1))

# Evaluate model
mse = mean_squared_error(y_test_rescaled, y_pred_rescaled)
rmse = np.sqrt(mse)
mae = mean_absolute_error(y_test_rescaled, y_pred_rescaled)
mape = np.mean(np.abs((y_test_rescaled - y_pred_rescaled) / y_test_rescaled)) * 100
accuracy = 100 - mape

print(f"Forecasting Results:")
print(f"Root Mean Squared Error (RMSE): {rmse}")
print(f"Mean Absolute Error (MAE): {mae}")
print(f"Mean Absolute Percentage Error (MAPE): {mape}%")
print(f"Accuracy: {accuracy}%")
print(f"Mean Squared Error (MSE): {mse}") 

# Plot results
plt.figure(figsize=(16, 8))
plt.plot(test.index, y_test_rescaled, label='Test Data', color='orange', linewidth=2)
plt.plot(test.index, y_pred_rescaled, label='Forecast', color='green', linestyle='--', linewidth=2)
plt.axvline(x=pd.to_datetime('2025-01-01'), color='red', linestyle='--', label='Start of Test Data', linewidth=1.5)
plt.plot(train.index, train['Transportation'], label='Train Data', color='blue', linewidth=2)  
plt.legend(fontsize=12)
plt.title('Passenger Transportation Forecasting with RNN', fontsize=16)
plt.xlabel('Time', fontsize=14)
plt.ylabel('Transportation', fontsize=14)
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)
plt.show()