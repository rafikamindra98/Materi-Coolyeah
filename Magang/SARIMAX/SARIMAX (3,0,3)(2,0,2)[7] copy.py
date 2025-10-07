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

# 5. Assumption Tests
from statsmodels.stats.diagnostic import acorr_ljungbox
from statsmodels.tsa.stattools import adfuller
from scipy import stats
import statsmodels.api as sm

# Stationarity Test (Augmented Dickey-Fuller Test)
print("\nStationarity Test (ADF Test):")
adf_result = adfuller(y_train)
print(f'ADF Statistic: {adf_result[0]:.4f}')
print(f'p-value: {adf_result[1]:.4f}')
print('Critical values:')
for key, value in adf_result[4].items():
    print(f'\t{key}: {value:.4f}')
if adf_result[1] < 0.05:
    print("Conclusion: Series is stationary (reject H0)")
else:
    print("Conclusion: Series is non-stationary (fail to reject H0)")

# Seasonal Decomposition
decomposition = sm.tsa.seasonal_decompose(y_train, period=12)
plt.figure(figsize=(12, 10))
plt.subplot(411)
plt.plot(y_train, label='Original')
plt.legend()
plt.subplot(412)
plt.plot(decomposition.trend, label='Trend')
plt.legend()
plt.subplot(413)
plt.plot(decomposition.seasonal, label='Seasonal')
plt.legend()
plt.subplot(414)
plt.plot(decomposition.resid, label='Residual')
plt.legend()
plt.tight_layout()
plt.show()

# 6. Define SARIMAX model parameters
# ARIMA(3,0,3)(1,0,0)[12] specification
order = (3, 0, 3)           # non-seasonal part (p,d,q)
seasonal_order = (1, 0, 0, 12)  # seasonal part (P,D,Q,s)

print(f"Using SARIMAX model with parameters:")
print(f"Non-seasonal order (p,d,q): {order}")
print(f"Seasonal order (P,D,Q,s): {seasonal_order}")

# 6. Fit SARIMAX model with specified parameters
model = SARIMAX(
    endog=y_train,
    exog=X_train,
    order=order,
    seasonal_order=seasonal_order,
    enforce_stationarity=False,
    enforce_invertibility=False
)
model_fit = model.fit(disp=False)

# 7. Forecasting to test set
y_pred = model_fit.predict(start=y_test.index[0], end=y_test.index[-1], exog=X_test)

# 8. Model Diagnostics and Performance Evaluation
# Get residuals
residuals = model_fit.resid

# Normality Test (Jarque-Bera Test)
jb_test = stats.jarque_bera(residuals)
print("\nNormality Test (Jarque-Bera):")
print(f"Statistic: {jb_test[0]:.4f}")
print(f"p-value: {jb_test[1]:.4f}")
if jb_test[1] < 0.05:
    print("Conclusion: Residuals are not normally distributed (reject H0)")
else:
    print("Conclusion: Residuals are normally distributed (fail to reject H0)")

# Autocorrelation Test (Ljung-Box Test)
lb_test = acorr_ljungbox(residuals, lags=[10], return_df=True)
print("\nAutocorrelation Test (Ljung-Box):")
print(f"Statistic: {lb_test['lb_stat'].values[0]:.4f}")
print(f"p-value: {lb_test['lb_pvalue'].values[0]:.4f}")
if lb_test['lb_pvalue'].values[0] < 0.05:
    print("Conclusion: Residuals show significant autocorrelation (reject H0)")
else:
    print("Conclusion: Residuals show no significant autocorrelation (fail to reject H0)")

# Q-Q Plot for residuals
plt.figure(figsize=(10, 6))
stats.probplot(residuals, dist="norm", plot=plt)
plt.title("Q-Q Plot of Residuals")
plt.show()

# ACF and PACF of residuals
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8))
sm.graphics.tsa.plot_acf(residuals, lags=40, ax=ax1)
ax1.set_title("Autocorrelation Function (ACF) of Residuals")
sm.graphics.tsa.plot_pacf(residuals, lags=40, ax=ax2)
ax2.set_title("Partial Autocorrelation Function (PACF) of Residuals")
plt.tight_layout()
plt.show()

# Performance Metrics
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
