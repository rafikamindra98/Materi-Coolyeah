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
data['Time'] = pd.to_datetime(data['Time'])
data = data.set_index('Time')
data = data.sort_index()

# Check and create 'Holiday' column if not exists
if 'Holiday' not in data.columns:
    data['Holiday'] = data.index.weekday
    data['Holiday'] = data['Holiday'].apply(lambda x: 1 if x >= 5 else 0)

# 4. Split data: training & testing
train = data.loc['2023-01-01':'2024-12-31']
test = data.loc['2025-01-01':'2025-02-28']

y_train = train['Transportation']
X_train = train[['Holiday']]

y_test = test['Transportation']
X_test = test[['Holiday']]

# 5. Auto-ARIMA dengan seasonality
print("Finding best SARIMAX parameters with auto_arima...")

auto_model = auto_arima(
    y_train,
    exogenous=X_train,
    seasonal=True,     # Use seasonal component
    m=7,               # Weekly seasonality (7 days)
    stepwise=True,
    suppress_warnings=True,
    trace=True,
    error_action='ignore'
)

print(f"\nBest model: {auto_model.order} seasonal_order={auto_model.seasonal_order}")

# 6. Fit SARIMAX model with best parameters
model = SARIMAX(
    endog=y_train,
    exog=X_train,
    order=auto_model.order,
    seasonal_order=auto_model.seasonal_order,
    enforce_stationarity=False,
    enforce_invertibility=False
)
model_fit = model.fit(disp=False)

# 7. Forecasting to test set
y_pred = model_fit.predict(start=y_test.index[0], end=y_test.index[-1], exog=X_test)

# 8. Performance evaluation
mae = mean_absolute_error(y_test, y_pred)
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
mape = np.mean(np.abs((y_test - y_pred) / y_test)) * 100

print('\nSARIMAX Model Evaluation:')
print(f'MAE  : {mae:.2f}')
print(f'RMSE : {rmse:.2f}')
print(f'MAPE : {mape:.2f}%')

# 9. Visualization
plt.figure(figsize=(14,6))
plt.plot(y_train.index, y_train, label='Training Data')
plt.plot(y_test.index, y_test, label='Actual Test Data', color='black')
plt.plot(y_pred.index, y_pred, label='Forecasted Data', color='red', linestyle='--')
plt.title('Passenger Transportation Forecasting Using SARIMAX (Seasonal)')
plt.xlabel('Time')
plt.ylabel('Transportation Amount')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()
