import pandas as pd
from prophet import Prophet  # Updated import statement
from sklearn.metrics import mean_squared_error
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

# Preprocessing for Prophet
train_prophet = train.reset_index()[['Waktu', 'Pengangkutan']].rename(columns={'Waktu': 'ds', 'Pengangkutan': 'y'})
test_prophet = test.reset_index()[['Waktu', 'Pengangkutan']].rename(columns={'Waktu': 'ds', 'Pengangkutan': 'y'})

# Build and train Prophet model
model = Prophet()
model.add_regressor('Libur')  # Add external regressor
train_prophet['Libur'] = train['Libur'].values  # Add regressor to training data
model.fit(train_prophet)

# Forecast
future = test.reset_index()[['Waktu', 'Libur']].rename(columns={'Waktu': 'ds'})
forecast = model.predict(future)

# Merge forecast with test data
test['Forecast'] = forecast['yhat'].values

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

# Plot results with larger and clearer visualization
plt.figure(figsize=(16, 8))  # Adjusted figure size for better clarity
plt.plot(train['Pengangkutan'], label='Train Data', linewidth=2)
plt.plot(test['Pengangkutan'], label='Test Data', color='orange', linewidth=2)
plt.plot(test['Forecast'], label='Forecast', color='green', linestyle='--', linewidth=2)
plt.axvline(x=pd.to_datetime('2025-01-01'), color='red', linestyle='--', label='Start of Test Data', linewidth=1.5)
plt.legend(fontsize=12)  # Increased font size for legend
plt.title('Pengangkutan Penumpang Forecasting with Prophet', fontsize=16)  # Updated title
plt.xlabel('Waktu', fontsize=14)  # Larger x-axis label font
plt.ylabel('Pengangkutan', fontsize=14)  # Larger y-axis label font
plt.grid(True, linestyle='--', alpha=0.7)  # Improved grid visibility
plt.xticks(fontsize=12)  # Larger x-axis ticks
plt.yticks(fontsize=12)  # Larger y-axis ticks
plt.show()