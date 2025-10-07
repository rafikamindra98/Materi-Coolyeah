# 1. Import library
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pmdarima import auto_arima
from statsmodels.tsa.statespace.sarimax import SARIMAX
from sklearn.metrics import mean_absolute_error, mean_squared_error

# 2. Load data
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# 3. Pre-processing
# Assume columns named 'Time' for time and 'Transportation' for transportation
data['Time'] = pd.to_datetime(data['Time'])
data = data.set_index('Time')
data = data.sort_index()

# Make sure there's a 'Holiday' column (0=weekday, 1=weekend)
if 'Holiday' not in data.columns:
    data['Holiday'] = data.index.weekday
    data['Holiday'] = data['Holiday'].apply(lambda x: 1 if x >= 5 else 0)

# 4. Split data
train = data.loc['2023-01-01':'2024-12-31']
test = data.loc['2025-01-01':'2025-02-28']

y_train = train['Transportation']
X_train = train[['Holiday']]  # Exogenous variable in DataFrame format

y_test = test['Transportation']
X_test = test[['Holiday']]

# 5. Auto-ARIMA untuk menentukan (p,d,q)
print("Mencari parameter ARIMAX terbaik dengan auto_arima...")

auto_model = auto_arima(
    y_train, 
    exogenous=X_train, 
    seasonal=False,  
    stepwise=True,
    suppress_warnings=True,
    trace=True,
    error_action='ignore'
)

print(f"Model terbaik: {auto_model.order}")

# 6. Fit model SARIMAX dengan parameter terbaik
model = SARIMAX(
    endog=y_train, 
    exog=X_train, 
    order=auto_model.order, 
    enforce_stationarity=False, 
    enforce_invertibility=False
)
model_fit = model.fit(disp=False)

# 7. Forecasting
y_pred = model_fit.predict(start=y_test.index[0], end=y_test.index[-1], exog=X_test)

# 8. Evaluasi model
mae = mean_absolute_error(y_test, y_pred)
mse = mean_squared_error(y_test, y_pred)  
rmse = np.sqrt(mse)
mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100

print('\nEvaluasi Model:')
print(f'MAE  : {mae:.2f}')
print(f'MSE  : {mse:.2f}')  
print(f'RMSE : {rmse:.2f}')
print(f'MAPE : {mape:.2f}%')

# 9. Plot hasil
plt.figure(figsize=(14,6))
plt.plot(y_train.index, y_train, label='Training Data')
plt.plot(y_test.index, y_test, label='Actual Test Data', color='black')
plt.plot(y_pred.index, y_pred, label='Forecasted Data', color='red', linestyle='--')
plt.title('Passenger Transportation Forecasting Using Auto-ARIMAX')
plt.xlabel('Time')
plt.ylabel('Transportation Amount')
plt.legend()
plt.grid()
plt.show()
