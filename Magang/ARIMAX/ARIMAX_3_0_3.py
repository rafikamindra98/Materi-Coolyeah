import pandas as pd
import numpy as np
from statsmodels.tsa.arima.model import ARIMA
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, mean_absolute_error
from statsmodels.stats.diagnostic import acorr_ljungbox, lilliefors
from statsmodels.tsa.stattools import adfuller
import matplotlib.pyplot as plt

# Step 1: Determine trend, seasonal, calendar dummy
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')
data['Time'] = pd.to_datetime(data['Time'])
data.set_index('Time', inplace=True)
data['Holiday'] = data.index.isin(pd.to_datetime(['2023-01-01', '2023-12-25', '2025-01-01', '2025-12-25'])) * 1

# Step 2: Time series regression
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']
X_train = train[['Holiday']]
y_train = train['Transportation']
reg = LinearRegression().fit(X_train, y_train)
train['Regression'] = reg.predict(X_train)
print("Regression coefficients:", reg.coef_)
print("Intercept:", reg.intercept_)

# Step 3: Residual → ARIMA model identification
train['Residual'] = y_train - train['Regression']
adf_result = adfuller(train['Residual'])
print("ADF Test (stationarity):", adf_result)
if adf_result[1] > 0.05:
    train['Residual_diff'] = train['Residual'].diff().dropna()

# Identify ARIMA model with ACF and PACF plots
# (This step can be done visually using ACF and PACF plots)

# Step 4: Form ARIMAX model (combination of regression & ARIMA)
p, d, q = 3, 0, 3  
model = ARIMA(train['Transportation'], order=(p, d, q), exog=train[['Holiday']])
model_fit = model.fit()
print(model_fit.summary())

# Step 5: Estimate & eliminate insignificant parameters
# (Insignificant parameters can be identified from ARIMA model summary output)

# Step 6: Residual tests: white noise & normality
ljungbox_result = acorr_ljungbox(model_fit.resid, lags=[10], return_df=True)
print("Ljung-Box Test:", ljungbox_result)
ks_stat, ks_pvalue = lilliefors(model_fit.resid)
print("Kolmogorov-Smirnov Test:", ks_stat, ks_pvalue)

# Step 7: Choose best ARIMAX model → ARIMA with (p=3, d=0, q=3)
forecast = model_fit.forecast(steps=len(test), exog=test[['Holiday']])
test['Forecast'] = forecast

# Evaluate model
mse = mean_squared_error(test['Transportation'], test['Forecast'])
rmse = np.sqrt(mse)
mae = mean_absolute_error(test['Transportation'], test['Forecast'])
mape = np.mean(np.abs((test['Transportation'] - test['Forecast']) / test['Transportation'])) * 100
accuracy = 100 - mape

print(f"Forecasting Results:")
print(f"Root Mean Squared Error (RMSE): {rmse}")
print(f"Mean Absolute Error (MAE): {mae}")
print(f"Mean Absolute Percentage Error (MAPE): {mape}%")
print(f"Accuracy: {accuracy}%")
print(f"Mean Squared Error (MSE): {mse}")  

# Plot results
plt.figure(figsize=(12, 6))
plt.plot(train['Transportation'], label='Train Data')
plt.plot(test['Transportation'], label='Test Data', color='orange')
plt.plot(test['Forecast'], label='Forecast', color='green')
plt.axvline(x=pd.to_datetime('2025-01-01'), color='red', linestyle='--', label='Start of Test Data')
plt.legend()
plt.title('Passenger Transportation Forecasting with ARIMAX')
plt.xlabel('Time')
plt.ylabel('Transportation')
plt.grid()
plt.show()