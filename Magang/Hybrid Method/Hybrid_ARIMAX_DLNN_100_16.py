import pandas as pd
import numpy as np
from statsmodels.tsa.statespace.sarimax import SARIMAX
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, LSTM
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error, mean_absolute_error
from math import sqrt

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset.xlsx')

# Preprocessing
data['Waktu'] = pd.to_datetime(data['Waktu'])
data.set_index('Waktu', inplace=True)

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# ARIMAX Model
exog_train = train[['Libur']]
exog_test = test[['Libur']]
arimax_model = SARIMAX(train['Pengangkutan'], order=(3, 0, 3), exog=exog_train)
arimax_result = arimax_model.fit(disp=False)

# Predict with ARIMAX
arimax_pred = arimax_result.predict(start=test.index[0], end=test.index[-1], exog=exog_test)

# Prepare data for DLNN
scaler = MinMaxScaler()
scaled_train = scaler.fit_transform(train[['Pengangkutan']])
scaled_test = scaler.transform(test[['Pengangkutan']])

# Create sequences for LSTM
def create_sequences(data, n_steps):
    X, y = [], []
    for i in range(len(data) - n_steps):
        X.append(data[i:i + n_steps])
        y.append(data[i + n_steps])
    return np.array(X), np.array(y)

n_steps = 10
X_train, y_train = create_sequences(scaled_train, n_steps)
X_test, y_test = create_sequences(scaled_test, n_steps)

# DLNN Model
model = Sequential([
    LSTM(50, activation='relu', input_shape=(n_steps, 1)),
    Dense(1)
])
model.compile(optimizer='adam', loss='mse')

# Train DLNN
model.fit(X_train, y_train, epochs=100, batch_size=16, verbose=1)

# Predict with DLNN
dlnn_pred = model.predict(X_test)

# Combine ARIMAX and DLNN predictions
combined_pred = (arimax_pred.values[:len(dlnn_pred)] + scaler.inverse_transform(dlnn_pred).flatten()) / 2

# Adjust test index for DLNN predictions
adjusted_test_index = test.index[n_steps:]  # Offset by n_steps to match DLNN predictions

# Save predictions
test['ARIMAX_Pred'] = arimax_pred
test.loc[adjusted_test_index, 'DLNN_Pred'] = scaler.inverse_transform(dlnn_pred).flatten()
test.loc[adjusted_test_index, 'Combined_Pred'] = combined_pred

test.to_excel('Predictions.xlsx')
print("Predictions saved to Predictions.xlsx")

# Calculate metrics
actual = test.loc[adjusted_test_index, 'Pengangkutan'].values
arimax_rmse = sqrt(mean_squared_error(actual, test.loc[adjusted_test_index, 'ARIMAX_Pred']))
arimax_mae = mean_absolute_error(actual, test.loc[adjusted_test_index, 'ARIMAX_Pred'])
arimax_mape = np.mean(np.abs((actual - test.loc[adjusted_test_index, 'ARIMAX_Pred']) / actual)) * 100
arimax_accuracy = 100 - arimax_mape
arimax_mse = mean_squared_error(actual, test.loc[adjusted_test_index, 'ARIMAX_Pred'])

combined_rmse = sqrt(mean_squared_error(actual, test.loc[adjusted_test_index, 'Combined_Pred']))
combined_mae = mean_absolute_error(actual, test.loc[adjusted_test_index, 'Combined_Pred'])
combined_mape = np.mean(np.abs((actual - test.loc[adjusted_test_index, 'Combined_Pred']) / actual)) * 100
combined_accuracy = 100 - combined_mape
combined_mse = mean_squared_error(actual, test.loc[adjusted_test_index, 'Combined_Pred'])

# Print metrics
print("ARIMAX Metrics:")
print(f"RMSE: {arimax_rmse}")
print(f"MAE: {arimax_mae}")
print(f"MAPE: {arimax_mape}%")
print(f"Accuracy: {arimax_accuracy}%")
print(f"MSE: {arimax_mse}")

print("\nCombined Metrics:")
print(f"RMSE: {combined_rmse}")
print(f"MAE: {combined_mae}")
print(f"MAPE: {combined_mape}%")
print(f"Accuracy: {combined_accuracy}%")
print(f"MSE: {combined_mse}")

# Plot predictions
plt.figure(figsize=(12, 6))
plt.plot(test.index, test['Pengangkutan'], label='Actual Data', color='blue')
plt.plot(test.index, test['ARIMAX_Pred'], label='ARIMAX Prediction', color='orange')
plt.plot(adjusted_test_index, scaler.inverse_transform(dlnn_pred), label='DLNN Prediction', color='green')
plt.plot(adjusted_test_index, combined_pred, label='Combined Prediction', color='red')
plt.axvline(x=test.index[0], color='black', linestyle='--', label='Start of Test Data')
plt.legend()
plt.title('Hybrid Forecasting: ARIMAX + DLNN')
plt.xlabel('Waktu')
plt.ylabel('Pengangkutan')
plt.grid()
plt.show()