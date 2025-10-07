# 1. Import libraries
import pandas as pd
import numpy as np
import holidays
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from pmdarima import auto_arima
from statsmodels.tsa.statespace.sarimax import SARIMAX

# 2. Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code (0,1,2,3)/Dataset/Dataset.xlsx')

# 3. Rename kolom
data = data.rename(columns={
    'tanggal': 'Waktu',
    'jumlah penumpang': 'Pengangkutan',
    'weekend': 'Libur'
})

# 4. Preprocessing
data['Waktu'] = pd.to_datetime(data['Waktu'])
data = data.sort_values('Waktu')

# Tambahkan fitur exogenous tambahan
data['Libur'] = (data['Waktu'].dt.weekday >= 5).astype(int)
indonesia_holidays = holidays.Indonesia(years=[2023, 2024, 2025])
data['Is_Holiday'] = data['Waktu'].apply(lambda x: 1 if x in indonesia_holidays else 0)
data['Month'] = data['Waktu'].dt.month
data['DayOfWeek'] = data['Waktu'].dt.weekday

# 5. Split data train/test
train = data[(data['Waktu'] >= '2023-01-01') & (data['Waktu'] <= '2024-12-31')]
test = data[(data['Waktu'] >= '2025-01-01') & (data['Waktu'] <= '2025-02-28')]

y_train = train['Pengangkutan']
y_test = test['Pengangkutan']

exog_cols = ['Libur', 'Is_Holiday', 'Month', 'DayOfWeek']
X_train_exog = train[exog_cols]
X_test_exog = test[exog_cols]

# 6. Model ARIMAX
print("Training ARIMAX...")
auto_model = auto_arima(
    y_train, 
    exogenous=X_train_exog, 
    seasonal=True, 
    m=7, 
    stepwise=True, 
    suppress_warnings=True, 
    trace=False, 
    error_action='ignore'
)

print(f"ARIMAX order: {auto_model.order}, seasonal_order: {auto_model.seasonal_order}")

arimax_model = SARIMAX(
    endog=y_train, 
    exog=X_train_exog, 
    order=auto_model.order, 
    seasonal_order=auto_model.seasonal_order, 
    enforce_stationarity=False, 
    enforce_invertibility=False
)
arimax_fit = arimax_model.fit(disp=False)

# 7. ARIMAX Forecast & Residual Calculation
arimax_pred = arimax_fit.predict(start=test.index[0], end=test.index[-1], exog=X_test_exog)
residuals_train = y_train - arimax_fit.fittedvalues
residuals_test = y_test - arimax_pred

# 8. Feature Scaling untuk FFNN
scaler_X = MinMaxScaler()
scaler_y = MinMaxScaler()

X_train_ffnn = scaler_X.fit_transform(train[exog_cols])
X_test_ffnn = scaler_X.transform(test[exog_cols])

# Gunakan residual sebagai target FFNN
y_train_resid = scaler_y.fit_transform(residuals_train.values.reshape(-1, 1))

# 9. Bangun FFNN Model
ffnn_model = Sequential([
    Dense(64, activation='relu', input_shape=(len(exog_cols), )),
    Dense(32, activation='relu'),
    Dense(1)
])

ffnn_model.compile(optimizer='adam', loss='mse')
print("Training FFNN...")
ffnn_model.fit(X_train_ffnn, y_train_resid, epochs=100, batch_size=16, verbose=1)

# 10. Prediksi residual dengan FFNN
resid_pred_scaled = ffnn_model.predict(X_test_ffnn)
resid_pred = scaler_y.inverse_transform(resid_pred_scaled).flatten()

# 11. Final Prediction (ARIMAX prediction + predicted residual)
hybrid_pred = arimax_pred + resid_pred

# 12. Evaluasi Model Hybrid
mae = mean_absolute_error(y_test, hybrid_pred)
rmse = np.sqrt(mean_squared_error(y_test, hybrid_pred))
mape = np.mean(np.abs((y_test - hybrid_pred) / y_test)) * 100

print("\nEvaluasi Model Hybrid ARIMAX-FFNN:")
print(f"MAE  : {mae:.2f}")
print(f"RMSE : {rmse:.2f}")
print(f"MAPE : {mape:.2f}%")

# 13. Visualisasi perbandingan
plt.figure(figsize=(14,6))
plt.plot(test['Waktu'], y_test, label='Actual', color='black')
plt.plot(test['Waktu'], arimax_pred, label='Prediksi ARIMAX', color='blue', linestyle='--')
plt.plot(test['Waktu'], hybrid_pred, label='Prediksi Hybrid ARIMAX-FFNN', color='red')
plt.title('Forecasting Pengangkutan Penumpang dengan Hybrid ARIMAX-FFNN')
plt.xlabel('Tanggal')
plt.ylabel('Jumlah Pengangkutan')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
