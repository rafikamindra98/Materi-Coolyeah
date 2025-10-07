import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error, mean_absolute_error
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, SimpleRNN

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# Preprocessing
data['Date'] = pd.to_datetime(data['Date'])
data.set_index('Date', inplace=True)

# Split data into train and test
train = data['2023-01-01':'2024-12-31']
test = data['2025-01-01':'2025-02-28']

# Prepare features and target
train_features = train[['Holiday']]  # Using only Holiday feature (0=weekdays, 1=weekend)
test_features = test[['Holiday']]

# Convert target variable to float
train['Transportation'] = train['Transportation'].astype(float)
test['Transportation'] = test['Transportation'].astype(float)

# Normalize target variable using MinMaxScaler
scaler = MinMaxScaler()
train_scaled = scaler.fit_transform(train[['Transportation']])
test_scaled = scaler.transform(test[['Transportation']])

# Prepare data for RNN
X_train = train_scaled
y_train = train_scaled
X_test = test_scaled
y_test = test_scaled

# Reshape data for RNN input (samples, timesteps, features)
X_train = X_train.reshape((X_train.shape[0], 1, 1))
X_test = X_test.reshape((X_test.shape[0], 1, 1))

# Build RNN model using SimpleRNN
model = Sequential([
    SimpleRNN(50, activation='relu', input_shape=(1, 1)),  
    Dense(1)
])
model.compile(optimizer='adam', loss='mse')

# Train the model
history = model.fit(X_train, y_train, epochs=10, batch_size=8, verbose=1, 
                   validation_split=0.2)  # Added validation split to monitor performance

# Forecast
y_pred = model.predict(X_test)
y_pred_rescaled = scaler.inverse_transform(y_pred)
y_test_rescaled = scaler.inverse_transform(y_test.reshape(-1, 1))

# Evaluate model
mse = mean_squared_error(y_test_rescaled, y_pred_rescaled)
rmse = np.sqrt(mse)
mae = mean_absolute_error(y_test_rescaled, y_pred_rescaled)
mape = np.mean(np.abs((y_test_rescaled - y_pred_rescaled) / y_test_rescaled)) * 100
accuracy = 100 - mape

print("\nForecasting Results:")
print(f"Root Mean Squared Error (RMSE): {rmse:.2f}")
print(f"Mean Absolute Error (MAE): {mae:.2f}")
print(f"Mean Absolute Percentage Error (MAPE): {mape:.2f}%")
print(f"Accuracy: {accuracy:.2f}%")
print(f"Mean Squared Error (MSE): {mse:.2f}")

# Plot results
plt.figure(figsize=(16, 8))
plt.plot(test.index, y_test_rescaled, label='Test Data', color='orange', linewidth=2)
plt.plot(test.index, y_pred_rescaled, label='Forecast', color='green', linestyle='--', linewidth=2)
plt.axvline(x=pd.to_datetime('2025-01-01'), color='red', linestyle='--', label='Start of Test Data', linewidth=1.5)
plt.plot(train.index, train['Transportation'], label='Train Data', color='blue', linewidth=2)
plt.legend(fontsize=12)
plt.title('Transportation Forecasting with RNN (Holiday Feature Only)', fontsize=16)
plt.xlabel('Date', fontsize=14)
plt.ylabel('Transportation', fontsize=14)
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)
plt.show()

# Plot training history
plt.figure(figsize=(12, 6))
plt.plot(history.history['loss'], label='Training Loss', color='blue')
plt.plot(history.history['val_loss'], label='Validation Loss', color='orange')
plt.title('Model Training History', fontsize=16)
plt.xlabel('Epoch', fontsize=14)
plt.ylabel('Loss', fontsize=14)
plt.legend(fontsize=12)
plt.grid(True, linestyle='--', alpha=0.7)
plt.show()