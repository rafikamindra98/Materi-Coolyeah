import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error, mean_absolute_error
import matplotlib.pyplot as plt

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset.xlsx')

# Preprocessing
data['Waktu'] = pd.to_datetime(data['Waktu'])
data.set_index('Waktu', inplace=True)

# Tambahkan fitur hari dalam seminggu
data['DayOfWeek'] = data.index.dayofweek

# Tambahkan fitur bulan
data['Month'] = data.index.month

# Gunakan fitur tambahan dalam model
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Feature engineering
train['Pengangkutan'] = train['Pengangkutan'].astype(float)
test['Pengangkutan'] = test['Pengangkutan'].astype(float)

# Define Exponential Smoothing function
def exponential_smoothing(series, alpha):
    result = [series[0]]  # First value is same as series
    for n in range(1, len(series)):
        result.append(alpha * series[n] + (1 - alpha) * result[n-1])
    return np.array(result)

# Apply Exponential Smoothing
alpha = 0.5  # Smoothing factor
train_smoothed = exponential_smoothing(train['Pengangkutan'].values, alpha)

# Forecast using Exponential Smoothing
forecast = []
last_value = train_smoothed[-1]
for _ in range(len(test)):
    forecast.append(last_value)  # Use the last smoothed value for forecasting

test['Forecast'] = forecast

# Evaluate model
mse = mean_squared_error(test['Pengangkutan'], test['Forecast'])
rmse = np.sqrt(mse)
mae = mean_absolute_error(test['Pengangkutan'], test['Forecast'])
mape = np.mean(np.abs((test['Pengangkutan'] - test['Forecast']) / test['Pengangkutan'])) * 100
accuracy = 100 - mape

print(f"Forecasting Results:")
print(f"Root Mean Squared Error (RMSE): {rmse}")
print(f"Mean Absolute Error (MAE): {mae}")
print(f"Mean Absolute Percentage Error (MAPE): {mape}%")
print(f"Accuracy: {accuracy}%")
print(f"Mean Squared Error (MSE): {mse}")  # Tambahkan ini untuk menampilkan MSE

# Plot results
plt.figure(figsize=(16, 8))
plt.plot(train['Pengangkutan'], label='Train Data', linewidth=2)
plt.plot(test['Pengangkutan'], label='Test Data', color='orange', linewidth=2)
plt.plot(test['Forecast'], label='Forecast', color='green', linestyle='--', linewidth=2)
plt.axvline(x=pd.to_datetime('2025-01-01'), color='red', linestyle='--', label='Start of Test Data', linewidth=1.5)
plt.legend(fontsize=12)
plt.title('Pengangkutan Penumpang Forecasting with Exponential Smoothing', fontsize=16)
plt.xlabel('Waktu', fontsize=14)
plt.ylabel('Pengangkutan', fontsize=14)
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)
plt.show()