# library
import pandas as pd
from sklearn.impute import KNNImputer

# Load the dataset
dataset = pd.read_excel('/Users/user/Downloads/Magang/VS Code/Missing Value/Dataset.xlsx')  # Gunakan path absolut
print("Dataset awal:")
print(dataset)

# Check for missing values
missing_values = dataset.isnull().sum()
print("\nMissing values in each column:")
print(missing_values)
print("Total missing values in the dataset:", dataset.isnull().sum().sum())

# Pisahkan kolom numerik dan non-numerik
numeric_columns = dataset.select_dtypes(include=['number'])
non_numeric_columns = dataset.select_dtypes(exclude=['number'])

# Gunakan KNN Imputer untuk kolom numerikcle
if not numeric_columns.empty:
    imputer = KNNImputer(n_neighbors=5)  # Menggunakan 5 tetangga terdekat
    numeric_columns_imputed = pd.DataFrame(imputer.fit_transform(numeric_columns), columns=numeric_columns.columns)
    dataset[numeric_columns.columns] = numeric_columns_imputed

# Isi missing value di kolom non-numerik dengan nilai tertentu (misalnya, 'Unknown')
for col in non_numeric_columns.columns:
    dataset[col] = dataset[col].fillna('Unknown')

# Periksa kembali missing value setelah imputasi
missing_values_after = dataset.isnull().sum()
print("\nMissing values after KNN imputation:")
print(missing_values_after)

# Simpan dataset yang sudah diisi missing value ke file Excel
dataset.to_excel('/Users/user/Downloads/Magang/VS Code/Missing Value/Dataset_Filled_KNN.xlsx', index=False)

print("\nDataset yang sudah diisi missing value berhasil disimpan ke 'Dataset_Filled_KNN.xlsx'")