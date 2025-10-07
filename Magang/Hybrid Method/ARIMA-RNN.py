# 1. Import library
import pandas as pd
import numpy as np
import holidays
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error
from pmdarima import auto_arima
from statsmodels.tsa.statespace.sarimax import SARIMAX
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import SimpleRNN, Dense, Dropout

# 2. Load & Rename Data
data = pd.read_excel("/Users/user/Downloads/Magang/VS Code (0,1,2,3)/Dataset/Dataset.xlsx")
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

# 4. Lag Features
data['Lag1'] = data['Pengangkutan'].shift(1)
data['Lag2'] = data['Pengangkutan'].shift(2)
data['Lag3'] = data['Pengangkutan'].shift(3)
data = data.dropna().reset_index(drop=True)

# 5. Split Data
train = data[(data['Waktu'] >= '2023-01-01') & (data['Waktu'] <= '2024-12-31')]
test = data[(data['Waktu'] >= '2025-01-01') & (data['Waktu'] <= '2025-02-28')]

y_train = train['Pengangkutan']
y_test = test['Pengangkutan']

# 6. ARIMA Training
exog_cols = ['Libur', 'Is_Holiday', 'Month', 'DayOfWeek']
X_train_arima = train[exog_cols]
X_test_arima = test[exog_cols]

print("Training ARIMA model...")
auto_model = auto_arima(
    y_train, exogenous=X_train_arima,
    seasonal=True, m=7, stepwise=True, suppress_warnings=True
)

model_arima = SARIMAX(
    y_train, exog=X_train_arima,
    order=auto_model.order,
    seasonal_order=auto_model.seasonal_order,
    enforce_stationarity=False, enforce_invertibility=False
)
result_arima = model_arima.fit(disp=False)

# 7. Prediksi ARIMA + Residuals
arima_pred = result_arima.predict(start=test.index[0], end=test.index[-1], exog=X_test_arima)
residuals_train = y_train - result_arima.fittedvalues

# 8. Prepare Data for RNN
features_rnn = ['Libur', 'Is_Holiday', 'Month', 'DayOfWeek', 'Lag1', 'Lag2', 'Lag3']
scaler_X = MinMaxScaler()
scaler_y = MinMaxScaler()

train_rnn = train.copy()
train_rnn['Residuals'] = residuals_train.values

X_train_rnn_scaled = scaler_X.fit_transform(train_rnn[features_rnn])
y_train_rnn_scaled = scaler_y.fit_transform(train_rnn[['Residuals']])

test_rnn = test.copy()
X_test_rnn_scaled = scaler_X.transform(test_rnn[features_rnn])

# 9. Buat sequence untuk RNN
def create_rnn_sequence(X, y=None, window_size=7):
    X_seq, y_seq = [], []
    for i in range(window_size, len(X)):
        X_seq.append(X[i - window_size:i])
        if y is not None:
            y_seq.append(y[i])
    return np.array(X_seq), (np.array(y_seq) if y is not None else None)

window_size = 7
X_rnn_seq, y_rnn_seq = create_rnn_sequence(X_train_rnn_scaled, y_train_rnn_scaled, window_size)
X_test_seq, _ = create_rnn_sequence(X_test_rnn_scaled, None, window_size)

# 10. Build RNN Model
model = Sequential([
    SimpleRNN(64, activation='tanh', input_shape=(window_size, X_rnn_seq.shape[2])),
    Dropout(0.3),
    Dense(32, activation='relu'),
    Dense(1)
])
model.compile(optimizer='adam', loss='mse')
print("Training RNN on residuals...")
model.fit(X_rnn_seq, y_rnn_seq, epochs=100, batch_size=16, verbose=1)

# 11. Predict residuals on test
rnn_residuals_scaled = model.predict(X_test_seq)
rnn_residuals = scaler_y.inverse_transform(rnn_residuals_scaled).flatten()

# Align ARIMA predictions (skip window_size days)
arima_pred_aligned = arima_pred.iloc[window_size:].values
dates_test = test.iloc[window_size:]['Waktu'].values
y_test_aligned = y_test.iloc[window_size:].values

# 12. Final Hybrid Forecast
hybrid_forecast = arima_pred_aligned + rnn_residuals

# 13. Evaluation
mae = mean_absolute_error(y_test_aligned, hybrid_forecast)
rmse = np.sqrt(mean_squared_error(y_test_aligned, hybrid_forecast))
mape = np.mean(np.abs((y_test_aligned - hybrid_forecast) / y_test_aligned)) * 100

print("\nEvaluasi Model Hybrid ARIMA + RNN:")
print(f"MAE  : {mae:.2f}")
print(f"RMSE : {rmse:.2f}")
print(f"MAPE : {mape:.2f}%")

# 14. Plot
plt.figure(figsize=(14,6))
plt.plot(dates_test, y_test_aligned, label='Actual')
plt.plot(dates_test, arima_pred_aligned, label='ARIMA', linestyle='--')
plt.plot(dates_test, hybrid_forecast, label='Hybrid ARIMA + RNN', color='red')
plt.title('Forecasting Pengangkutan Penumpang - Hybrid ARIMA + RNN')
plt.xlabel('Tanggal')
plt.ylabel('Jumlah Penumpang')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
