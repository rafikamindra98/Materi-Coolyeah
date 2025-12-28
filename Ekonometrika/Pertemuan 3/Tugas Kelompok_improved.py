import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import os

# --- Konfigurasi Awal ---
# Mengatur gaya plot agar terlihat modern
sns.set_theme(style="whitegrid")
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = 'Arial'
plt.rcParams['axes.labelweight'] = 'bold'
plt.rcParams['axes.titleweight'] = 'bold'

# --- Membaca dan Membersihkan Data ---
try:
    # Path file Excel
    file_path = 'Basic Econometrics.xlsx'
    
    # Memastikan file ada
    if not os.path.exists(file_path):
        print(f"Error: Pastikan file '{file_path}' berada di folder yang sama.")
        print(f"\nPath lengkap yang dicari: {os.path.abspath(file_path)}")
        exit()
    
    # Membaca file Excel
    print(f"\nMembaca file: {file_path}")
    df = pd.read_excel(file_path)
    
    # Print informasi tentang sheet yang tersedia
    print("\nSheet yang tersedia dalam file Excel:")
    print(pd.ExcelFile(file_path).sheet_names)
    
    # Print preview data mentah
    print("\nPreview data mentah:")
    print(df.head())
    print("\nKolom yang tersedia:")
    print(df.columns.tolist())
    
    # Tanyakan input dari pengguna
    sheet_name = input("\nMasukkan nama sheet yang berisi data (dari daftar di atas): ")
    skiprows = int(input("Masukkan jumlah baris yang ingin dilewati di awal (0 jika tidak ada): "))
    
    # Baca ulang dengan parameter yang sesuai
    df = pd.read_excel(file_path, sheet_name=sheet_name, skiprows=skiprows)
    
    print("\nPreview data setelah membaca sheet yang dipilih:")
    print(df.head())
    print("\nKolom yang tersedia:")
    print(df.columns.tolist())
    
    # Konfirmasi nama kolom
    print("\nApakah kolom sudah sesuai dengan format yang diharapkan?")
    print("Format yang diharapkan: Year, Verbal_Males, Verbal_Females, Verbal_Total, Math_Males, Math_Females, Math_Total")
    rename = input("Apakah perlu mengubah nama kolom? (y/n): ").lower()
    
    if rename == 'y':
        df.columns = ['Year', 'Verbal_Males', 'Verbal_Females', 'Verbal_Total',
                     'Math_Males', 'Math_Females', 'Math_Total']
    
    # Membersihkan data
    df = df.dropna(subset=[df.columns[0]])  # Hapus baris dengan nilai tahun kosong
    df = df[~df[df.columns[0]].astype(str).str.contains('Source|Year', na=False)]  # Hapus baris dengan kata 'Source' atau 'Year'
    
    # Konversi ke numerik
    for col in df.columns:
        df[col] = pd.to_numeric(df[col], errors='coerce')
    
    # Hapus baris dengan nilai NaN
    df = df.dropna()
    
    print("\nData final setelah dibersihkan:")
    print(df.head())
    print("\nInformasi data:")
    print(df.info())

except Exception as e:
    print(f"\nError: {str(e)}")
    exit()

# --- Visualisasi Data ---

# 1. Scatter Plot Data
plt.figure(figsize=(12, 6))

# Plot Verbal Scores
plt.subplot(1, 2, 1)
plt.scatter(df['Year'], df['Verbal_Males'], label='Males', color='blue', alpha=0.6)
plt.scatter(df['Year'], df['Verbal_Females'], label='Females', color='red', alpha=0.6)
plt.title('SAT Verbal Scores Over Time')
plt.xlabel('Year')
plt.ylabel('Verbal Score')
plt.legend()
plt.grid(True, alpha=0.3)

# Plot Math Scores
plt.subplot(1, 2, 2)
plt.scatter(df['Year'], df['Math_Males'], label='Males', color='blue', alpha=0.6)
plt.scatter(df['Year'], df['Math_Females'], label='Females', color='red', alpha=0.6)
plt.title('SAT Math Scores Over Time')
plt.xlabel('Year')
plt.ylabel('Math Score')
plt.legend()
plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

# 2. Line Plot dengan Trend
plt.figure(figsize=(12, 6))

# Plot Verbal Scores dengan Trend
plt.subplot(1, 2, 1)
for col in ['Verbal_Males', 'Verbal_Females']:
    plt.plot(df['Year'], df[col], marker='o', label=col.split('_')[1])
    # Tambahkan trend line
    z = np.polyfit(df['Year'], df[col], 1)
    p = np.poly1d(z)
    plt.plot(df['Year'], p(df['Year']), linestyle='--', alpha=0.8)

plt.title('SAT Verbal Scores Trend')
plt.xlabel('Year')
plt.ylabel('Verbal Score')
plt.legend()
plt.grid(True, alpha=0.3)

# Plot Math Scores dengan Trend
plt.subplot(1, 2, 2)
for col in ['Math_Males', 'Math_Females']:
    plt.plot(df['Year'], df[col], marker='o', label=col.split('_')[1])
    # Tambahkan trend line
    z = np.polyfit(df['Year'], df[col], 1)
    p = np.poly1d(z)
    plt.plot(df['Year'], p(df['Year']), linestyle='--', alpha=0.8)

plt.title('SAT Math Scores Trend')
plt.xlabel('Year')
plt.ylabel('Math Score')
plt.legend()
plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

# 3. Box Plot untuk Distribusi Skor
plt.figure(figsize=(12, 6))

# Prepare data for box plot
verbal_data = [df['Verbal_Males'], df['Verbal_Females']]
math_data = [df['Math_Males'], df['Math_Females']]

# Plot Verbal Scores Distribution
plt.subplot(1, 2, 1)
plt.boxplot(verbal_data, labels=['Males', 'Females'])
plt.title('Distribution of SAT Verbal Scores')
plt.ylabel('Score')

# Plot Math Scores Distribution
plt.subplot(1, 2, 2)
plt.boxplot(math_data, labels=['Males', 'Females'])
plt.title('Distribution of SAT Math Scores')
plt.ylabel('Score')

plt.tight_layout()
plt.show()

# 4. Statistik Deskriptif
print("\nStatistik Deskriptif:")
print("\nVerbal Scores:")
print(df[['Verbal_Males', 'Verbal_Females']].describe())
print("\nMath Scores:")
print(df[['Math_Males', 'Math_Females']].describe())

# 5. Correlation Analysis
correlation_matrix = df[['Verbal_Males', 'Verbal_Females', 'Math_Males', 'Math_Females']].corr()

plt.figure(figsize=(10, 8))
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', vmin=-1, vmax=1)
plt.title('Correlation Matrix of SAT Scores')
plt.tight_layout()
plt.show()