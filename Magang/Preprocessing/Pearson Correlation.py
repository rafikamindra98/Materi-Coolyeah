# 1. Import necessary libraries
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# 2. Load the Dataset
# Path to Excel file
file_path = '/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1) Correlation.xlsx'

try:
    df = pd.read_excel(file_path, engine='openpyxl')
    print("✅ Dataset loaded successfully.")
except FileNotFoundError:
    print(f"❌ Error: File not found at '{file_path}'.")
    exit()

# 3. Data Preprocessing
print("\n🔄 Starting data preprocessing...")
# Drop the first row if it's irrelevant (based on previous analysis)
df_cleaned = df.drop(index=0).reset_index(drop=True)

# Remove the unnecessary column
if 'Unnamed: 4' in df_cleaned.columns:
    df_cleaned = df_cleaned.drop(columns=['Unnamed: 4'])

# Rename the 'Correlation' column to be more descriptive
df_cleaned = df_cleaned.rename(columns={'Correlation': 'Movement_Trend'})

# Convert the 'Date' column to a datetime format
df_cleaned['Date'] = pd.to_datetime(df_cleaned['Date'], errors='coerce')

# Create a numerical column from the movement trend ('Up'/'Down')
def encode_movement(trend):
    if isinstance(trend, str):
        if trend.lower() == 'up':
            return 1
        elif trend.lower() == 'down':
            return -1
    return 0

df_cleaned['Movement_Encoded'] = df_cleaned['Movement_Trend'].apply(encode_movement)

# Convert columns to numeric types and drop rows with nulls after conversion
for col in ['Transportation', 'Holiday']:
    df_cleaned[col] = pd.to_numeric(df_cleaned[col], errors='coerce')
df_cleaned.dropna(inplace=True)
print("✅ Data preprocessing complete.")


# 4. Calculate Pearson Correlation
print("\n🧮 Calculating the correlation matrix using the Pearson method...")
# Select the numerical columns for the analysis
correlation_columns = ['Transportation', 'Holiday', 'Movement_Encoded']
df_final = df_cleaned[correlation_columns]

# Explicitly calculate the correlation matrix using the 'pearson' method
# Note: 'pearson' is the default method, so df_final.corr() would also work.
pearson_correlation_matrix = df_final.corr(method='pearson')

print("\n--- Pearson Correlation Matrix Results ---")
print(pearson_correlation_matrix)
print("----------------------------------------")


# 5. Visualize the Results
print("\n🎨 Generating heatmap visualization...")
plt.figure(figsize=(10, 7))
sns.heatmap(
    pearson_correlation_matrix,
    annot=True,          # Display the numbers on the heatmap
    cmap='viridis',      # Color scheme
    fmt=".3f",           # Format numbers to 3 decimal places
    linewidths=.5
)
plt.title('Pearson Correlation Heatmap', fontsize=16)
plt.show()

print("\n✅ Analysis complete.")