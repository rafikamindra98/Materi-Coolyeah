import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
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
        raise FileNotFoundError(f"File {file_path} tidak ditemukan!")
    
    print("Membaca data...")
    # Membaca data dari Excel dan skip baris header yang tidak diperlukan
    df = pd.read_excel(file_path, skiprows=2)
    
    # Rename kolom untuk memudahkan analisis
    df.columns = ['Year', 'Verbal_Males', 'Math_Males', 'Total_Males', 
                 'Verbal_Females', 'Math_Females', 'Total_Females']
    
    # Hapus baris yang tidak diperlukan
    df = df.dropna()
    
    # Konversi Year ke integer
    df['Year'] = df['Year'].astype(int)
    
    # Menampilkan info awal
    print("\nInformasi Dataset setelah dibersihkan:")
    print("-" * 50)
    print(df.info())
    print("\nSampel Data:")
    print("-" * 50)
    print(df.head())
    
    # --- Analisis Data ---
    print("\nAnalisis Data SAT Scores")
    print("=" * 50)
    
    # Plot 1: Tren Verbal Scores
    plt.figure(figsize=(15, 7))
    plt.plot(df['Year'], df['Verbal_Males'], marker='o', label='Males')
    plt.plot(df['Year'], df['Verbal_Females'], marker='s', label='Females')
    plt.title('Tren SAT Verbal Scores (1967-1990)', pad=20)
    plt.xlabel('Tahun')
    plt.ylabel('Verbal Score')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.show()
    
    # Plot 2: Tren Math Scores
    plt.figure(figsize=(15, 7))
    plt.plot(df['Year'], df['Math_Males'], marker='o', label='Males')
    plt.plot(df['Year'], df['Math_Females'], marker='s', label='Females')
    plt.title('Tren SAT Math Scores (1967-1990)', pad=20)
    plt.xlabel('Tahun')
    plt.ylabel('Math Score')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.show()
    
    # Plot 3: Total Score Comparison
    plt.figure(figsize=(15, 7))
    plt.plot(df['Year'], df['Total_Males'], marker='o', label='Males')
    plt.plot(df['Year'], df['Total_Females'], marker='s', label='Females')
    plt.title('Tren SAT Total Scores (1967-1990)', pad=20)
    plt.xlabel('Tahun')
    plt.ylabel('Total Score')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.show()
    
    # Plot 4: Gender Gap Analysis
    plt.figure(figsize=(15, 7))
    verbal_gap = df['Verbal_Males'] - df['Verbal_Females']
    math_gap = df['Math_Males'] - df['Math_Females']
    total_gap = df['Total_Males'] - df['Total_Females']
    
    plt.plot(df['Year'], verbal_gap, marker='o', label='Verbal Gap')
    plt.plot(df['Year'], math_gap, marker='s', label='Math Gap')
    plt.plot(df['Year'], total_gap, marker='^', label='Total Gap')
    plt.axhline(y=0, color='r', linestyle='--')
    plt.title('Gender Gap in SAT Scores (Male - Female)', pad=20)
    plt.xlabel('Tahun')
    plt.ylabel('Score Gap')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.show()
    
    # Analisis Statistik
    print("\nAnalisis Statistik:")
    print("-" * 50)
    
    # Statistik deskriptif
    print("\nStatistik Deskriptif:")
    print(df.describe().round(2))
    
    # Analisis trend
    print("\nAnalisis Trend:")
    for col in df.columns[1:]:  # Skip Year column
        start_val = df.iloc[0][col]
        end_val = df.iloc[-1][col]
        change = ((end_val - start_val) / start_val) * 100
        print(f"{col}: Perubahan {change:.1f}% dari {start_val:.0f} ke {end_val:.0f}")
    
    # Analisis gender gap
    print("\nAnalisis Gender Gap:")
    avg_verbal_gap = verbal_gap.mean()
    avg_math_gap = math_gap.mean()
    avg_total_gap = total_gap.mean()
    
    print(f"Rata-rata Gender Gap:")
    print(f"- Verbal: {avg_verbal_gap:.1f} poin")
    print(f"- Math: {avg_math_gap:.1f} poin")
    print(f"- Total: {avg_total_gap:.1f} poin")
    
    # Analisis korelasi
    print("\nKorelasi antar Variabel:")
    correlation_matrix = df.drop('Year', axis=1).corr()
    print(correlation_matrix.round(3))
    
except Exception as e:
    print(f"Terjadi kesalahan: {str(e)}")
    exit(1)