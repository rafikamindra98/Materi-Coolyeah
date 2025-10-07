# 1. Import library
import pandas as pd
import numpy as np
import holidays
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_absolute_error, mean_squared_error
from xgboost import XGBRegressor

# 2. Load & Rename
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

# 5. Feature & Target Definition
features = ['Libur', 'Is_Holiday', 'Month', 'DayOfWeek', 'Lag1', 'Lag2', 'Lag3']
target = 'Pengangkutan'

# 6. Train-Test Split
train = data[(data['Waktu'] >= '2023-01-01') & (data['Waktu'] <= '2024-12-31')]
test = data[(data['Waktu'] >= '2025-01-01') & (data['Waktu'] <= '2025-02-28')]

X_train = train[features]
y_train = train[target]
X_test = test[features]
y_test = test[target]
dates_test = test['Waktu']

# 7. XGBoost Model
model = XGBRegressor(n_estimators=200, learning_rate=0.1, max_depth=4, random_state=42)
model.fit(X_train, y_train)

# 8. Prediction
y_pred = model.predict(X_test)

# 9. Evaluation
mae = mean_absolute_error(y_test, y_pred)
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100

print("\nEvaluasi Model XGBoost:")
print(f"MAE  : {mae:.2f}")
print(f"RMSE : {rmse:.2f}")
print(f"MAPE : {mape:.2f}%")

# 10. Plot
plt.figure(figsize=(14,6))
plt.plot(dates_test, y_test.values, label='Actual')
plt.plot(dates_test, y_pred, label='XGBoost Prediction', color='green')
plt.title('Forecasting Pengangkutan Penumpang - XGBoost')
plt.xlabel('Tanggal')
plt.ylabel('Jumlah Penumpang')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
