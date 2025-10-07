import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.statespace.sarimax import SARIMAX
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error
import pmdarima as pm

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# Dataset has columns: 'Waktu', 'Pengangkutan', 'Libur'
data['Waktu'] = pd.to_datetime(data['Waktu'])
data.set_index('Waktu', inplace=True)

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Feature scaling (include 'Libur' as an additional feature)
scaler = MinMaxScaler(feature_range=(0, 1))
train_scaled = scaler.fit_transform(train)
test_scaled = scaler.transform(test)

# Prepare the target (Pengangkutan) and include exogenous variable 'Libur'
X_train = train_scaled[:, 1]  # 'Libur' is the exogenous variable
y_train = train_scaled[:, 0]  # 'Pengangkutan' is the target
X_test = test_scaled[:, 1]
y_test = test_scaled[:, 0]

# Use auto_arima to find the best parameters
print("Finding best SARIMAX parameters using auto_arima...")
auto_arima = pm.auto_arima(y=y_train,
                          exogenous=X_train.reshape(-1, 1),
                          start_p=0, start_q=0,
                          max_p=3, max_q=3, max_d=2,
                          start_P=0, start_Q=0,
                          max_P=2, max_Q=2, max_D=1,
                          m=12,  # Monthly seasonal pattern
                          seasonal=True,
                          trace=True,
                          error_action='ignore',
                          suppress_warnings=True,
                          stepwise=True)

# Get the best parameters
best_order = auto_arima.order
best_seasonal_order = auto_arima.seasonal_order
print(f"\nBest SARIMAX parameters found:")
print(f"Order (p,d,q): {best_order}")
print(f"Seasonal Order (P,D,Q,s): {best_seasonal_order}\n")

# Build SARIMAX model with the best parameters
model = SARIMAX(y_train, 
                exog=X_train, 
                order=best_order, 
                seasonal_order=best_seasonal_order)

# Fit the model
sarimax_model = model.fit(disp=False)

# Forecasting on the training set and test set
train_pred = sarimax_model.predict(start=0, end=len(y_train)-1, exog=X_train)
test_pred = sarimax_model.predict(start=len(y_train), end=len(y_train)+len(y_test)-1, exog=X_test)

# Inverse transform predictions
train_pred = scaler.inverse_transform(np.concatenate((train_pred.reshape(-1, 1), np.zeros((train_pred.shape[0], train_scaled.shape[1] - 1))), axis=1))[:, 0]
test_pred = scaler.inverse_transform(np.concatenate((test_pred.reshape(-1, 1), np.zeros((test_pred.shape[0], test_scaled.shape[1] - 1))), axis=1))[:, 0]

# Inverse transform actual values
y_train_actual = scaler.inverse_transform(np.concatenate((y_train.reshape(-1, 1), np.zeros((y_train.shape[0], train_scaled.shape[1] - 1))), axis=1))[:, 0]
y_test_actual = scaler.inverse_transform(np.concatenate((y_test.reshape(-1, 1), np.zeros((y_test.shape[0], test_scaled.shape[1] - 1))), axis=1))[:, 0]

# Calculate MAPE (Mean Absolute Percentage Error)
train_mape = np.mean(np.abs((y_train_actual - train_pred) / y_train_actual)) * 100
test_mape = np.mean(np.abs((y_test_actual - test_pred) / y_test_actual)) * 100

# Calculate MAE (Mean Absolute Error)
train_mae = np.mean(np.abs(y_train_actual - train_pred))
test_mae = np.mean(np.abs(y_test_actual - test_pred))

# Evaluate model
train_rmse = np.sqrt(mean_squared_error(y_train_actual, train_pred))
test_rmse = np.sqrt(mean_squared_error(y_test_actual, test_pred))

# Display metrics
print(f'Train RMSE: {train_rmse}')
print(f'Test RMSE: {test_rmse}')
print(f'Train MAPE: {train_mape:.2f}%')
print(f'Test MAPE: {test_mape:.2f}%')
print(f'Train MAE: {train_mae}')
print(f'Test MAE: {test_mae}')

# Plot results
plt.figure(figsize=(12, 6))

# Plot Train Actual and Train Predicted (2023-01-01 to 2025-01-01)
train_dates = data['2023-01-01':'2025-01-01'].index[0:len(y_train_actual)]
plt.plot(train_dates, y_train_actual, label='Train Actual')
plt.plot(train_dates, train_pred[:len(train_dates)], label='Train Predicted')

# Plot Test Actual and Test Predicted (2025-01-01 to 2025-02-28)
test_dates = data['2025-01-01':'2025-02-28'].index[:len(test_pred)]
plt.plot(test_dates, y_test_actual, label='Test Actual')
plt.plot(test_dates, test_pred, label='Test Predicted')

# Add details to the plot
plt.title('SARIMAX Model Predictions vs Actual Data')
plt.xlabel('Date')
plt.ylabel('Pengangkutan')
plt.xticks(rotation=45)
plt.grid()
plt.legend()
plt.tight_layout()
plt.show()
