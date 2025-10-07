# Library Imports
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Set style for better visualization
plt.style.use('seaborn-v0_8-darkgrid')  # Using a valid style name
sns.set_theme(style="darkgrid")  # Set seaborn theme
sns.set_palette("husl")

# Load dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# Convert 'Time' to datetime
data['Time'] = pd.to_datetime(data['Time'])
data.set_index('Time', inplace=True)

# Display basic information about the dataset
print("\nDataset Info:")
print("="*50)
print(data.info())

print("\nFirst 5 rows of data:")
print("="*50)
print(data.head())

print("\nLast 5 rows of data:")
print("="*50)
print(data.tail())

print("\nStatistical Summary:")
print("="*50)
print(data.describe())

# Create visualizations
# 1. Time Series Plot
plt.figure(figsize=(15, 6))
plt.plot(data.index, data['Transportation'], label='Transportation')
plt.title('Time Series Plot of Transportation')
plt.xlabel('Time')
plt.ylabel('Transportation')
plt.xticks(rotation=45)
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

# 2. Box Plot for monthly patterns
plt.figure(figsize=(15, 6))
data['Month'] = data.index.month
sns.boxplot(x='Month', y='Transportation', data=data)
plt.title('Monthly Box Plot of Transportation')
plt.xlabel('Month')
plt.ylabel('Transportation')
plt.grid(True)
plt.tight_layout()
plt.show()

# 3. Distribution Plot
plt.figure(figsize=(12, 6))
sns.histplot(data['Transportation'], kde=True)
plt.title('Distribution of Transportation')
plt.xlabel('Transportation')
plt.ylabel('Frequency')
plt.grid(True)
plt.tight_layout()
plt.show()

# 4. Scatter plot between Transportation and Holiday
plt.figure(figsize=(10, 6))
plt.scatter(data['Holiday'], data['Transportation'], alpha=0.5)
plt.title('Scatter Plot: Transportation vs Holiday')
plt.xlabel('Holiday')
plt.ylabel('Transportation')
plt.grid(True)
plt.tight_layout()
plt.show()

# 5. Monthly average plot
monthly_avg = data.groupby(data.index.month)['Transportation'].mean()
plt.figure(figsize=(12, 6))
monthly_avg.plot(kind='bar')
plt.title('Average Monthly Transportation')
plt.xlabel('Month')
plt.ylabel('Average Transportation')
plt.grid(True)
plt.tight_layout()
plt.show()

# Calculate correlation
print("\nCorrelation between Transportation and Holiday:")
print("="*50)
print(data['Transportation'].corr(data['Holiday']))