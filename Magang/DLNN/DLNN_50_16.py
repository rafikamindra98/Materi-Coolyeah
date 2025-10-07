import pandas as pd
import numpy as np
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error, mean_absolute_error

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset.xlsx')

# Preprocessing
data['Waktu'] = pd.to_datetime(data['Waktu'])
data.set_index('Waktu', inplace=True)

# Split data into training and testing sets
train_data = data['2023-01-01':'2024-12-31']
test_data = data['2025-01-01':'2025-02-28']

# Features and target
X_train = train_data[['Libur']]
y_train = train_data['Pengangkutan']
X_test = test_data[['Libur']]
y_test = test_data['Pengangkutan']

# Normalize the data
scaler_X = MinMaxScaler()
scaler_y = MinMaxScaler()

X_train_scaled = scaler_X.fit_transform(X_train)
y_train_scaled = scaler_y.fit_transform(y_train.values.reshape(-1, 1))
X_test_scaled = scaler_X.transform(X_test)
y_test_scaled = scaler_y.transform(y_test.values.reshape(-1, 1))

# Build the DLNN model
model = Sequential([
    Dense(64, input_dim=X_train_scaled.shape[1], activation='relu'),
    Dense(32, activation='relu'),
    Dense(1, activation='linear')
])

model.compile(optimizer='adam', loss='mse', metrics=['mae'])

# Train the model
model.fit(X_train_scaled, y_train_scaled, epochs=50, batch_size=16, validation_split=0.2)

# Evaluate the model
loss, mae = model.evaluate(X_test_scaled, y_test_scaled)
print(f"Test Loss: {loss}, Test MAE: {mae}")

# Predict
predictions_scaled = model.predict(X_test_scaled)
predictions = scaler_y.inverse_transform(predictions_scaled)

# Save predictions to a new Excel file
test_data['Predicted_Pengangkutan'] = predictions
test_data.to_excel('Predicted_Pengangkutan.xlsx')

print("Predictions saved to 'Predicted_Pengangkutan.xlsx'")

# Calculate evaluation metrics
mse = mean_squared_error(test_data['Pengangkutan'], test_data['Predicted_Pengangkutan'])
rmse = np.sqrt(mse)
mae = mean_absolute_error(test_data['Pengangkutan'], test_data['Predicted_Pengangkutan'])
mape = np.mean(np.abs((test_data['Pengangkutan'] - test_data['Predicted_Pengangkutan']) / test_data['Pengangkutan'])) * 100
accuracy = 100 - mape

# Print evaluation metrics
print("Forecasting Results:")
print(f"Root Mean Squared Error (RMSE): {rmse}")
print(f"Mean Absolute Error (MAE): {mae}")
print(f"Mean Absolute Percentage Error (MAPE): {mape}%")
print(f"Accuracy: {accuracy}%")
print(f"Mean Squared Error (MSE): {mse}")

# Plot results
plt.figure(figsize=(12, 6))
plt.plot(train_data['Pengangkutan'], label='Train Data', color='blue')
plt.plot(test_data['Pengangkutan'], label='Test Data', color='orange')
plt.plot(test_data['Predicted_Pengangkutan'], label='Forecast', color='green')
plt.axvline(x=pd.to_datetime('2025-01-01'), color='red', linestyle='--', label='Start of Test Data')
plt.legend()
plt.title('Pengangkutan Penumpang Forecasting with DLNN')
plt.xlabel('Waktu')
plt.ylabel('Pengangkutan')
plt.grid()
plt.show()