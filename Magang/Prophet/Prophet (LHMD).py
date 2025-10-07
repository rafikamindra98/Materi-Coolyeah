# 1. Import Library
import pandas as pd
import numpy as np
from prophet import Prophet
import matplotlib.pyplot as plt
from sklearn.metrics import mean_absolute_error, mean_squared_error
import holidays

# 2. Load Dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset.xlsx')

# 3. Ganti nama kolom
data = data.rename(columns={
    'tanggal': 'Waktu',
    'jumlah penumpang': 'Pengangkutan',
    'weekend': 'Libur'
})

# 4. Pastikan format datetime
data['Waktu'] = pd.to_datetime(data['Waktu'])
data = data.sort_values('Waktu')

# 5. Tambahkan kolom fitur tambahan
data['Libur'] = (data['Waktu'].dt.weekday >= 5).astype(int)
indonesia_holidays = holidays.Indonesia(years=[2023, 2024, 2025])
data['Is_Holiday'] = data['Waktu'].apply(lambda x: 1 if x in indonesia_holidays else 0)
data['Month'] = data['Waktu'].dt.month
data['DayOfWeek'] = data['Waktu'].dt.weekday

# 6. Format untuk Prophet
df_prophet = data[['Waktu', 'Pengangkutan', 'Libur', 'Is_Holiday', 'Month', 'DayOfWeek']]
df_prophet = df_prophet.rename(columns={'Waktu': 'ds', 'Pengangkutan': 'y'})

# 7. Split data training dan testing
train = df_prophet[(df_prophet['ds'] >= '2023-01-01') & (df_prophet['ds'] <= '2024-12-31')]
test = df_prophet[(df_prophet['ds'] >= '2025-01-01') & (df_prophet['ds'] <= '2025-02-28')]

# 8. Inisialisasi model Prophet dengan semua regressor
model = Prophet()
model.add_regressor('Libur')
model.add_regressor('Is_Holiday')
model.add_regressor('Month')
model.add_regressor('DayOfWeek')

# 9. Training model
model.fit(train)

# 10. Buat future dataframe untuk prediksi
future = test[['ds', 'Libur', 'Is_Holiday', 'Month', 'DayOfWeek']]

# 11. Prediksi
forecast = model.predict(future)

# 12. Evaluasi
y_true = test['y'].values
y_pred = forecast['yhat'].values

mae = mean_absolute_error(y_true, y_pred)
rmse = np.sqrt(mean_squared_error(y_true, y_pred))
mape = np.mean(np.abs((y_true - y_pred) / y_true)) * 100

print('\nEvaluasi Model Prophet (dengan Month & DayOfWeek):')
print(f'MAE  : {mae:.2f}')
print(f'RMSE : {rmse:.2f}')
print(f'MAPE : {mape:.2f}%')

# 13. Visualisasi hasil
plt.figure(figsize=(14,6))
plt.plot(train['ds'], train['y'], label='Training Data')
plt.plot(test['ds'], test['y'], label='Actual Test Data', color='black')
plt.plot(forecast['ds'], forecast['yhat'], label='Forecast Prophet', color='green')
plt.title('Forecasting Pengangkutan Penumpang menggunakan Prophet (dengan fitur tambahan)')
plt.xlabel('Tanggal')
plt.ylabel('Jumlah Pengangkutan')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
