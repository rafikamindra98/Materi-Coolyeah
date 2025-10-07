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

# Define Fuzzy Time Series functions
def fuzzify(data, intervals):
    fuzzy_sets = []
    for value in data:
        membership = [max(0, 1 - abs(value - center) / (intervals[1] - intervals[0])) for center in intervals]
        fuzzy_sets.append(membership)
    return np.array(fuzzy_sets)

def defuzzify(fuzzy_forecast, intervals):
    return np.dot(fuzzy_forecast, intervals) / np.sum(fuzzy_forecast, axis=1)

# Define intervals for fuzzification
min_val, max_val = train['Pengangkutan'].min(), train['Pengangkutan'].max()
num_intervals = 20  # Increased from 10 to 20
intervals = np.linspace(min_val, max_val, num_intervals)

# Fuzzify training data
train_fuzzy = fuzzify(train['Pengangkutan'].values, intervals)

# Forecast using Fuzzy Time Series
forecast_fuzzy = []
for i in range(len(test)):
    if i == 0:
        forecast_fuzzy.append(train_fuzzy[-1])  # Use the last fuzzy set from training data
    else:
        forecast_fuzzy.append(forecast_fuzzy[-1])  # Repeat the last forecasted fuzzy set

forecast_fuzzy = np.array(forecast_fuzzy)
test['Forecast'] = defuzzify(forecast_fuzzy, intervals)

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
plt.title('Pengangkutan Penumpang Forecasting with Fuzzy Time Series', fontsize=16)
plt.xlabel('Waktu', fontsize=14)
plt.ylabel('Pengangkutan', fontsize=14)
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)
plt.show()