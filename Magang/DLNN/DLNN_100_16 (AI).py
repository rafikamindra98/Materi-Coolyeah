# 1. Import library
import pandas as pd
import numpy as np
import holidays
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout

# 2. Load data
data = pd.read_excel("Dataset/Dataset.xlsx")

# 3. Rename kolom
data = data.rename(columns={
    'tanggal': 'Waktu',
    'jumlah penumpang': 'Pengangkutan',
    'weekend': 'Libur'
})

# 4. Konversi waktu
data['Waktu'] = pd.to_datetime(data['Waktu'])
data = data.sort_values('Waktu')

# 5. Tambahkan fitur tambahan
data['Libur'] = (data['Waktu'].dt.weekday >= 5).astype(int)
indonesia_holidays = holidays.Indonesia(years=[2023, 2024, 2025])
data['Is_Holiday'] = data['Waktu'].apply(lambda x: 1 if x in indonesia_holidays else 0)
data['Month'] = data['Waktu'].dt.month
data['DayOfWeek'] = data['Waktu'].dt.weekday

# 6. Tentukan fitur dan target
features = ['Libur', 'Is_Holiday', 'Month', 'DayOfWeek']
target = 'Pengangkutan'

# 7. Split data
train_df = data[(data['Waktu'] >= '2023-01-01') & (data['Waktu'] <= '2024-12-31')]
test_df = data[(data['Waktu'] >= '2025-01-01') & (data['Waktu'] <= '2025-02-28')]

# 8. Normalisasi
scaler_X = MinMaxScaler()
scaler_y = MinMaxScaler()

X_train = scaler_X.fit_transform(train_df[features])
y_train = scaler_y.fit_transform(train_df[[target]])

X_test = scaler_X.transform(test_df[features])
y_test_actual = test_df[[target]].values  # untuk evaluasi

# 9. Bangun model DLNN
model = Sequential([
    Dense(128, activation='relu', input_shape=(len(features),)),
    Dropout(0.2),
    Dense(64, activation='relu'),
    Dropout(0.2),
    Dense(32, activation='relu'),
    Dense(1)
])

model.compile(optimizer='adam', loss='mse', metrics=['mae'])

# 10. Training
history = model.fit(X_train, y_train, epochs=100, batch_size=8, verbose=1)

# 11. Prediksi
y_pred_scaled = model.predict(X_test)
y_pred = scaler_y.inverse_transform(y_pred_scaled)

# 12. Evaluasi
mae = mean_absolute_error(y_test_actual, y_pred)
rmse = np.sqrt(mean_squared_error(y_test_actual, y_pred))
mape = np.mean(np.abs((y_test_actual - y_pred) / y_test_actual)) * 100

print("\nEvaluasi Model DLNN:")
print(f"MAE  : {mae:.2f}")
print(f"RMSE : {rmse:.2f}")
print(f"MAPE : {mape:.2f}%")

# 13. Visualisasi
plt.figure(figsize=(14,6))
plt.plot(test_df['Waktu'], y_test_actual, label='Actual')
plt.plot(test_df['Waktu'], y_pred, label='Predicted (DLNN)', color='green')
plt.title('Forecasting Pengangkutan Penumpang dengan DLNN')
plt.xlabel('Tanggal')
plt.ylabel('Jumlah Pengangkutan')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
