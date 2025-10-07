# 1. Import required libraries
import os
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# 2. Load Dataset
# Path to Excel file
file_path = '/Users/user/Downloads/Magang/VS Code/Dataset/Dataset (0,1) Correlation.xlsx'

try:
    print(f"Attempting to load file from: {file_path}")
    df = pd.read_excel(file_path, engine='openpyxl')
    print(f"Dataset successfully loaded. Shape: {df.shape}")
    print("Available columns:", df.columns.tolist())
except FileNotFoundError:
    print(f"Error: File not found at '{file_path}'.")
    print("Make sure the file name is correct and the file is in the same folder as this script.")
    exit()
except Exception as e:
    print(f"Error loading file: {str(e)}")
    print("Additional details:")
    print(f"- Error type: {type(e).__name__}")
    print(f"- File exists?: {os.path.exists(file_path)}")
    print(f"- File readable?: {os.access(file_path, os.R_OK)}")
    exit()

# 3. Data Cleaning
print("\nStarting data cleaning...")
# Remove first row (index 0) which might contain header or irrelevant data from original file
# Based on previous analysis, first row is an unstructured header
if not df.empty and df.iloc[0].isnull().all() or "Transportation" in str(df.iloc[0].values): # Check if first row is header or empty
    df_cleaned = df.drop(index=0).reset_index(drop=True)
else:
    df_cleaned = df.copy()


# Remove 'Unnamed: 4' column if it exists, as it usually appears from unclean Excel exports
if 'Unnamed: 4' in df_cleaned.columns:
    df_cleaned = df_cleaned.drop(columns=['Unnamed: 4'])
    print("- Column 'Unnamed: 4' removed.")

# Rename 'Correlation' column to 'Movement_Trend' for better descriptiveness
# ('Movement' is good, but 'Movement_Trend' might be more descriptive)
if 'Correlation' in df_cleaned.columns:
    df_cleaned = df_cleaned.rename(columns={'Correlation': 'Movement_Trend'})
    print("- Column 'Correlation' renamed to 'Movement_Trend'.")
elif 'Movement' in df_cleaned.columns: # If already 'Movement' from previous run
    df_cleaned = df_cleaned.rename(columns={'Movement': 'Movement_Trend'})
    print("- Column 'Movement' renamed to 'Movement_Trend'.")


# Convert 'Date' column from text (object) to datetime data type
# This is important for time-based analysis if needed later
if 'Date' in df_cleaned.columns:
    df_cleaned['Date'] = pd.to_datetime(df_cleaned['Date'], errors='coerce') # errors='coerce' will convert incorrect date formats to NaT
    print("- Column 'Date' converted to datetime format.")

# Remove rows with missing values (NaN) after conversion or cleaning
df_cleaned.dropna(subset=['Transportation', 'Holiday', 'Movement_Trend', 'Date'], inplace=True)
print(f"- Rows with missing values removed. Number of rows after cleaning: {len(df_cleaned)} rows.")

print("\nData cleaning complete. Here are the first 5 rows after cleaning:")
print(df_cleaned.head())
print("\nDataset info after cleaning:")
df_cleaned.info()


# 4. Feature Engineering
# Create new numeric column 'Movement_Encoded' from 'Movement_Trend' column
# Rules: 'Up' -> 1, 'Down' -> -1, others (e.g., NaN or other text) -> 0
print("\nStarting feature engineering...")
def encode_movement(movement_text):
    if isinstance(movement_text, str):
        if movement_text.lower() == 'up':
            return 1
        elif movement_text.lower() == 'down':
            return -1
    return 0 # Default for unrecognized values or non-string input

if 'Movement_Trend' in df_cleaned.columns:
    df_cleaned['Movement_Encoded'] = df_cleaned['Movement_Trend'].apply(encode_movement)
    print("- Column 'Movement_Encoded' created from 'Movement_Trend'.")
else:
    print("Error: Column 'Movement_Trend' not found for encoding. Please check column name.")
    exit()

print("Example of movement encoding results:")
print(df_cleaned[['Movement_Trend', 'Movement_Encoded']].head())


# 5. Correlation Analysis
print("\nStarting correlation analysis...")
# Select only relevant numeric columns for correlation analysis
# Ensure 'Transportation' and 'Holiday' columns are numeric
df_cleaned['Transportation'] = pd.to_numeric(df_cleaned['Transportation'], errors='coerce')
df_cleaned['Holiday'] = pd.to_numeric(df_cleaned['Holiday'], errors='coerce')
df_cleaned.dropna(subset=['Transportation', 'Holiday'], inplace=True) # Remove rows if conversion fails

columns_for_correlation = ['Transportation', 'Holiday', 'Movement_Encoded']
df_corr = df_cleaned[columns_for_correlation]

# Calculate correlation matrix
correlation_matrix = df_corr.corr()

print("\nCorrelation Matrix:")
print(correlation_matrix)


# 6. Data Visualization
print("\nCreating heatmap visualization...")
# Create heatmap to visualize correlation matrix
plt.figure(figsize=(10, 7)) # Set figure size for better clarity
sns.set_theme(style="whitegrid") # Set seaborn theme
heatmap = sns.heatmap(
    correlation_matrix,
    annot=True,           # Show correlation values inside each cell
    cmap='coolwarm',      # Use 'coolwarm' color scheme (blue for negative, red for positive)
    fmt=".2f",            # Format numbers to 2 decimal places
    linewidths=.5,        # Add thin lines between cells
    cbar_kws={"shrink": .8} # Adjust color bar size
)
plt.title('Correlation Heatmap between Transportation, Holiday, and Movement Trends', fontsize=15, pad=20)
plt.xticks(rotation=45, ha='right') # Rotate x-axis labels for better readability
plt.yticks(rotation=0)
plt.tight_layout() # Adjust layout to show all elements properly
plt.show() # Display the plot

print("\nAnalysis complete. Heatmap visualization has been displayed.")
print("Make sure the plot window appears. If not, check your Matplotlib configuration in VS Code.")

