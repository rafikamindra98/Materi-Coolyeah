# 1. Import Library
import pandas as pd
import numpy as np
import holidays
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout
from pmdarima import auto_arima
from statsmodels.tsa.statespace.sarimax import SARIMAX

# 2. Load dan Rename Dataset
data = pd.read_excel("Dataset/Dataset.xlsx")
data = data.rename(columns={
    'tanggal': 'Waktu',
    'jumlah penumpang': 'Pengangkutan',
    'weekend': 'Libur'
})
data['Waktu'] = pd.to_datetime(data['Waktu'])
data = data.sort_values('Waktu')

# 3. Feature Engineering
data['Libur'] = (data['Waktu'].dt.weekday >= 5).astype(int)
indonesia_holidays = holidays.Indonesia(years=[2023, 2024, 2025])
data['Is_Holiday'] = data['Waktu'].apply(lambda x: 1 if x in indonesia_holidays else 0)
data['Month'] = data['Waktu'].dt.month
data['DayOfWeek'] = data['Waktu'].dt.weekday

# 4. Split Data
train = data[(data['Waktu'] >= '2023-01-01') & (data['Waktu'] <= '2024-12-31')]
test = data[(data['Waktu'] >= '2025-01-01') & (data['Waktu'] <= '2025-02-28')]

y_train = train['Pengangkutan']
y_test = test['Pengangkutan']

exog_cols = ['Libur', 'Is_Holiday', 'Month', 'DayOfWeek']
X_train_exog = train[exog_cols]
X_test_exog = test[exog_cols]

# 5. Fit ARIMAX
print("Training ARIMAX...")
auto_model = auto_arima(
    y_train, exogenous=X_train_exog, seasonal=True, m=7,
    stepwise=True, trace=False, error_action='ignore', suppress_warnings=True
)

arimax_model = SARIMAX(
    endog=y_train, exog=X_train_exog,
    order=auto_model.order, seasonal_order=auto_model.seasonal_order,
    enforce_stationarity=False, enforce_invertibility=False
)
arimax_fit = arimax_model.fit(disp=False)

# 6. Predict ARIMAX & Residuals
arimax_pred = arimax_fit.predict(start=test.index[0], end=test.index[-1], exog=X_test_exog)
residuals_train = y_train - arimax_fit.fittedvalues
residuals_test = y_test - arimax_pred

# 7. Normalisasi untuk DLNN
scaler_X = MinMaxScaler()
scaler_y = MinMaxScaler()

X_train_scaled = scaler_X.fit_transform(X_train_exog)
X_test_scaled = scaler_X.transform(X_test_exog)

residuals_train_scaled = scaler_y.fit_transform(residuals_train.values.reshape(-1,1))

# 8. Build DLNN untuk mempelajari pola residual ARIMAX
model = Sequential([
    Dense(128, activation='relu', input_shape=(len(exog_cols),)),
    Dropout(0.3),
    Dense(64, activation='relu'),
    Dropout(0.3),
    Dense(32, activation='relu'),
    Dense(1)
])

model.compile(optimizer='adam', loss='mse')
print("Training DLNN (residual learner)...")
model.fit(X_train_scaled, residuals_train_scaled, epochs=100, batch_size=8, verbose=1)

# 9. Prediksi residual oleh DLNN
residuals_pred_scaled = model.predict(X_test_scaled)
residuals_pred = scaler_y.inverse_transform(residuals_pred_scaled).flatten()

# 10. Final Forecast = ARIMAX prediction + DLNN predicted residuals
hybrid_forecast = arimax_pred + residuals_pred

# 11. Evaluasi Model
mae = mean_absolute_error(y_test, hybrid_forecast)
rmse = np.sqrt(mean_squared_error(y_test, hybrid_forecast))
mape = np.mean(np.abs((y_test - hybrid_forecast) / y_test)) * 100

print("\nEvaluasi Hybrid Model ARIMAX + DLNN:")
print(f"MAE  : {mae:.2f}")
print(f"RMSE : {rmse:.2f}")
print(f"MAPE : {mape:.2f}%")

# 12. Visualisasi
plt.figure(figsize=(14,6))
plt.plot(test['Waktu'], y_test, label='Actual')
plt.plot(test['Waktu'], arimax_pred, label='ARIMAX Only', linestyle='--')
plt.plot(test['Waktu'], hybrid_forecast, label='Hybrid ARIMAX + DLNN', color='red')
plt.title('Forecasting Pengangkutan Penumpang (Hybrid ARIMAX + DLNN)')
plt.xlabel('Tanggal')
plt.ylabel('Jumlah Penumpang')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
