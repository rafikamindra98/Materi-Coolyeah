# 1. Import library
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import holidays
from sklearn.metrics import mean_absolute_error, mean_squared_error
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense

# 2. Load Dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code (0,1,2,3)/Dataset/Dataset.xlsx')

# 3. Rename kolom
data = data.rename(columns={
    'tanggal': 'Waktu',
    'jumlah penumpang': 'Pengangkutan',
    'weekend': 'Libur'
})

# 4. Format datetime & sorting
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

# 7. Bagi data: train (2023-2024), test (2025 Jan-Feb)
train_df = data[(data['Waktu'] >= '2023-01-01') & (data['Waktu'] <= '2024-12-31')]
test_df = data[(data['Waktu'] >= '2025-01-01') & (data['Waktu'] <= '2025-02-28')]

# 8. Scaling fitur dan target
scaler_X = MinMaxScaler()
scaler_y = MinMaxScaler()

X_train = scaler_X.fit_transform(train_df[features])
y_train = scaler_y.fit_transform(train_df[[target]])

X_test = scaler_X.transform(test_df[features])
y_test = test_df[[target]].values  # untuk evaluasi

# 9. Bangun model FFNN
model = Sequential([
    Dense(64, activation='relu', input_shape=(len(features),)),
    Dense(32, activation='relu'),
    Dense(1)  # output layer
])

model.compile(optimizer='adam', loss='mse', metrics=['mae'])

# 10. Training model
history = model.fit(X_train, y_train, epochs=10, batch_size=16, verbose=1)

# 11. Prediksi dan evaluasi
y_pred_scaled = model.predict(X_test)
y_pred = scaler_y.inverse_transform(y_pred_scaled)

mae = mean_absolute_error(y_test, y_pred)
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100

print("\nEvaluasi Model FFNN:")
print(f"MAE  : {mae:.2f}")
print(f"RMSE : {rmse:.2f}")
print(f"MAPE : {mape:.2f}%")

# 12. Visualisasi hasil
plt.figure(figsize=(14,6))
plt.plot(test_df['Waktu'], y_test, label='Actual')
plt.plot(test_df['Waktu'], y_pred, label='Predicted (FFNN)', color='orange')
plt.title('Forecasting Pengangkutan Penumpang dengan FFNN')
plt.xlabel('Tanggal')
plt.ylabel('Jumlah Pengangkutan')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
