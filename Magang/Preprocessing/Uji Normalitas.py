# Import required libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from statsmodels.stats.diagnostic import lilliefors
import warnings
warnings.filterwarnings('ignore')

# Load the dataset
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1).xlsx')

# Convert 'Time' column to datetime and set as index
data['Time'] = pd.to_datetime(data['Time'])
data.set_index('Time', inplace=True)

# 1. Descriptive Statistics
print("\n1. Descriptive Statistics:")
desc_stats = data['Transportation'].describe()
print(desc_stats)

# Calculate skewness and kurtosis
skewness = data['Transportation'].skew()
kurtosis = data['Transportation'].kurtosis()
print(f"\nSkewness: {skewness:.4f}")
print(f"Kurtosis: {kurtosis:.4f}")

# 2. Statistical Tests for Normality
print("\n2. Statistical Tests for Normality:")

# Shapiro-Wilk Test
shapiro_stat, shapiro_p = stats.shapiro(data['Transportation'])
print("\nShapiro-Wilk Test:")
print(f"Statistic: {shapiro_stat:.4f}")
print(f"p-value: {shapiro_p:.4f}")
print("Conclusion: Data is", "normal" if shapiro_p > 0.05 else "not normal", "at 5% significance level")

# Kolmogorov-Smirnov Test (Lilliefors variant)
ks_stat, ks_p = lilliefors(data['Transportation'])
print("\nKolmogorov-Smirnov Test (Lilliefors):")
print(f"Statistic: {ks_stat:.4f}")
print(f"p-value: {ks_p:.4f}")
print("Conclusion: Data is", "normal" if ks_p > 0.05 else "not normal", "at 5% significance level")

# D'Agostino-Pearson Test
agostino_stat, agostino_p = stats.normaltest(data['Transportation'])
print("\nD'Agostino-Pearson Test:")
print(f"Statistic: {agostino_stat:.4f}")
print(f"p-value: {agostino_p:.4f}")
print("Conclusion: Data is", "normal" if agostino_p > 0.05 else "not normal", "at 5% significance level")

# 3. Visual Methods
plt.figure(figsize=(15, 10))

# Histogram with KDE
plt.subplot(2, 2, 1)
sns.histplot(data['Transportation'], kde=True)
plt.title('Histogram with Kernel Density Estimation')
plt.xlabel('Transportation')
plt.ylabel('Frequency')

# Q-Q Plot
plt.subplot(2, 2, 2)
stats.probplot(data['Transportation'], dist="norm", plot=plt)
plt.title('Q-Q Plot')

# Box Plot
plt.subplot(2, 2, 3)
sns.boxplot(x=data['Transportation'])
plt.title('Box Plot')
plt.xlabel('Transportation')

# Time Series Plot
plt.subplot(2, 2, 4)
plt.plot(data.index, data['Transportation'])
plt.title('Time Series Plot')
plt.xlabel('Time')
plt.ylabel('Transportation')
plt.xticks(rotation=45)

plt.tight_layout()
plt.show()

# Additional visualization: Monthly distribution
plt.figure(figsize=(12, 6))
data['Month'] = data.index.month
sns.boxplot(x='Month', y='Transportation', data=data)
plt.title('Monthly Distribution of Transportation')
plt.xlabel('Month')
plt.ylabel('Transportation')
plt.show()

# Summary of normality assessment
print("\n4. Summary of Normality Assessment:")
tests = {
    'Shapiro-Wilk': shapiro_p,
    'Kolmogorov-Smirnov (Lilliefors)': ks_p,
    "D'Agostino-Pearson": agostino_p
}

print("\nTest Summary (α = 0.05):")
print("-" * 50)
print(f"{'Test':<30} {'p-value':<10} {'Conclusion'}")
print("-" * 50)
for test, p_value in tests.items():
    conclusion = "Normal" if p_value > 0.05 else "Not Normal"
    print(f"{test:<30} {p_value:>10.4f} {conclusion:>10}")