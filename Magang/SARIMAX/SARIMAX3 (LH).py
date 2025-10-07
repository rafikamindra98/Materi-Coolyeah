# 1. Import library
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import holidays
from pmdarima import auto_arima
from statsmodels.tsa.statespace.sarimax import SARIMAX
from sklearn.metrics import mean_absolute_error, mean_squared_error

# 2. Load data
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code (0,1,2,3)/Dataset/Dataset.xlsx')

# 3. Pra-pemrosesan
data['Waktu'] = pd.to_datetime(data['Waktu'])
data = data.set_index('Waktu')
data = data.sort_index()

# Tambahkan fitur 'Libur' jika belum ada
if 'Libur' not in data.columns:
    data['Libur'] = data.index.weekday
    data['Libur'] = data['Libur'].apply(lambda x: 1 if x >= 5 else 0)

# Tambahkan fitur 'Is_Holiday' (libur nasional Indonesia)
indonesia_holidays = holidays.Indonesia(years=[2023, 2024, 2025])
data['Is_Holiday'] = data.index.to_series().apply(lambda x: 1 if x in indonesia_holidays else 0)

# 4. Split data
train = data.loc['2023-01-01':'2024-12-31']
test = data.loc['2025-01-01':'2025-02-28']

y_train = train['Pengangkutan']
X_train = train[['Libur', 'Is_Holiday']]

y_test = test['Pengangkutan']
X_test = test[['Libur', 'Is_Holiday']]

# 5. Auto-ARIMA dengan seasonality mingguan
print("Mencari parameter SARIMAX terbaik...")

auto_model = auto_arima(
    y_train,
    exogenous=X_train,
    seasonal=True,
    m=7,
    stepwise=True,
    suppress_warnings=True,
    trace=True,
    error_action='ignore'
)

print(f"\nModel terbaik: {auto_model.order} seasonal_order={auto_model.seasonal_order}")

# 6. Fit model SARIMAX
model = SARIMAX(
    endog=y_train,
    exog=X_train,
    order=auto_model.order,
    seasonal_order=auto_model.seasonal_order,
    enforce_stationarity=False,
    enforce_invertibility=False
)
model_fit = model.fit(disp=False)

# 7. Forecasting
y_pred = model_fit.predict(start=y_test.index[0], end=y_test.index[-1], exog=X_test)

# 8. Evaluasi
mae = mean_absolute_error(y_test, y_pred)
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100

print('\nEvaluasi Model SARIMAX (Libur + Is_Holiday):')
print(f'MAE  : {mae:.2f}')
print(f'RMSE : {rmse:.2f}')
print(f'MAPE : {mape:.2f}%')

# 9. Visualisasi
plt.figure(figsize=(14,6))
plt.plot(y_train.index, y_train, label='Training Data')
plt.plot(y_test.index, y_test, label='Actual Test Data', color='black')
plt.plot(y_pred.index, y_pred, label='Forecasted Data', color='red', linestyle='--')
plt.title('Forecasting Pengangkutan Penumpang SARIMAX (Libur + Is_Holiday)')
plt.xlabel('Waktu')
plt.ylabel('Jumlah Pengangkutan')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()
