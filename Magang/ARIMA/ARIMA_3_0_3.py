import pandas as pd
from statsmodels.tsa.arima.model import ARIMA
from sklearn.metrics import mean_squared_error

import matplotlib.pyplot as plt

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset 2024.xlsx')

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
train_exog = train[['Libur', 'DayOfWeek', 'Month']]
test_exog = test[['Libur', 'DayOfWeek', 'Month']]

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Feature engineering
train['Libur'] = train.index.isin(pd.to_datetime(['2023-01-01', '2023-12-25'])) * 1
test['Libur'] = test.index.isin(pd.to_datetime(['2025-01-01', '2025-12-25'])) * 1
train['Pengangkutan'] = train['Pengangkutan'].astype(float)
test['Pengangkutan'] = test['Pengangkutan'].astype(float)

# Normalization
from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()
train[['Pengangkutan', 'Libur']] = scaler.fit_transform(train[['Pengangkutan', 'Libur']])
test[['Pengangkutan', 'Libur']] = scaler.transform(test[['Pengangkutan', 'Libur']])

# Normalization using MinMaxScaler
from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()
train_scaled = scaler.fit_transform(train[['Pengangkutan', 'Libur']])
test_scaled = scaler.transform(test[['Pengangkutan', 'Libur']])

# Pisahkan kembali data setelah normalisasi
train['Pengangkutan'], train['Libur'] = train_scaled[:, 0], train_scaled[:, 1]
test['Pengangkutan'], test['Libur'] = test_scaled[:, 0], test_scaled[:, 1]

# Build ARIMA model
from statsmodels.tsa.arima.model import ARIMA
from sklearn.metrics import mean_squared_error
import itertools

# Pencarian grid untuk parameter ARIMA
p = range(0, 4)  # Nilai p (autoregressive)
d = range(0, 2)  # Nilai d (differencing)
q = range(0, 4)  # Nilai q (moving average)

best_score, best_cfg = float("inf"), None
for param in itertools.product(p, d, q):
    try:
        model = ARIMA(train['Pengangkutan'], order=param, exog=train[['Libur']])
        model_fit = model.fit()
        forecast = model_fit.forecast(steps=len(test), exog=test[['Libur']])
        mse = mean_squared_error(test['Pengangkutan'], forecast)
        if mse < best_score:
            best_score, best_cfg = mse, param
    except:
        continue

print(f"Best ARIMA parameters: {best_cfg} with MSE: {best_score}")

# Train ARIMA model with best parameters
model = ARIMA(train['Pengangkutan'], order=best_cfg, exog=train[['Libur']])
model_fit = model.fit()
print(model_fit.summary())

# Forecast
forecast = model_fit.forecast(steps=len(test), exog=test[['Libur']])
test['Forecast'] = forecast

# Evaluate model
from sklearn.metrics import mean_absolute_error
import numpy as np

mse = mean_squared_error(test['Pengangkutan'], test['Forecast'])
rmse = np.sqrt(mse)
mae = mean_absolute_error(test['Pengangkutan'], test['Forecast'])
mape = np.mean(np.abs((test['Pengangkutan'] - test['Forecast']) / test['Pengangkutan'])) * 100  # MAPE calculation
accuracy = 100 - mape  # Accuracy percentage calculation

print(f"Forecasting Results:")
print(f"Forecasted Values: {test['Forecast'].values}")
print(f"Actual Values: {test['Pengangkutan'].values}")
print(f"Forecasting Accuracy:")
print(f"Root Mean Squared Error (RMSE): {rmse}")
print(f"Mean Absolute Error (MAE): {mae}")
print(f"Mean Absolute Percentage Error (MAPE): {mape}%")
print(f"Accuracy: {accuracy}%")  # Display accuracy percentage
print(f'Mean Squared Error: {mse}')

# Plot results
plt.figure(figsize=(12, 6))
plt.plot(train['Pengangkutan'], label='Train Data')
plt.plot(test['Pengangkutan'], label='Test Data', color='orange')
plt.plot(test['Forecast'], label='Forecast', color='green')
plt.axvline(x=pd.to_datetime('2025-01-01'), color='red', linestyle='--', label='Start of Test Data')  # Fix here
plt.legend()
plt.title('Pengangkutan Penumpang Forecasting with ARIMA')
plt.xlabel('Waktu')
plt.ylabel('Pengangkutan')
plt.grid()
plt.show()