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
    df = pd.read_excel(file_path, sheet_name='2.9')

    # Membersihkan nama kolom dari spasi dan karakter yang tidak perlu
    df.columns = ['Year', 'Verbal_Males', 'Verbal_Females', 'Verbal_Total',
                  'Math_Males', 'Math_Females', 'Math_Total']

    # Menghapus baris terakhir yang mungkin berisi data agregat/kosong
    df = df.dropna(subset=['Year'])
    df = df[df['Year'].str.contains('Source') == False]

    # Mengonversi kolom ke tipe data numerik yang sesuai
    for col in df.columns:
        if col != 'Year':
            df[col] = pd.to_numeric(df[col], errors='coerce')
    df['Year'] = pd.to_numeric(df['Year'])

except FileNotFoundError:
    print("Error: Pastikan file 'Basic Econometrics.xlsx - 2.9.csv' berada di folder yang sama.")
    exit()

# --- a. Membuat plot skor verbal dan matematika dari waktu ke waktu ---
plt.figure(figsize=(14, 8))
plt.plot(df['Year'], df['Verbal_Males'], marker='o', linestyle='-', label='Verbal Pria', color='blue')
plt.plot(df['Year'], df['Verbal_Females'], marker='x', linestyle='--', label='Verbal Wanita', color='red')
plt.plot(df['Year'], df['Math_Males'], marker='s', linestyle='-', label='Matematika Pria', color='green')
plt.plot(df['Year'], df['Math_Females'], marker='^', linestyle='--', label='Matematika Wanita', color='purple')

plt.title('Perkembangan Skor Rata-Rata SAT (1967-1990)', fontsize=16)
plt.xlabel('Tahun', fontsize=12)
plt.ylabel('Skor Rata-Rata SAT', fontsize=12)
plt.legend(title='Kategori Skor')
plt.grid(True, which='both', linestyle='--', linewidth=0.5)
plt.tight_layout()
# Menyimpan plot ke file
plt.savefig('plot_skor_sat_waktu.png', dpi=300)
print("Plot perkembangan skor SAT telah disimpan sebagai 'plot_skor_sat_waktu.png'")


# --- d. Membuat plot skor SAT verbal wanita terhadap skor SAT verbal pria ---
X = df['Verbal_Males']
Y = df['Verbal_Females']

# Melakukan regresi linear sederhana untuk mendapatkan garis tren
slope, intercept = np.polyfit(X, Y, 1)
regression_line = slope * X + intercept

plt.figure(figsize=(10, 8))
# Scatter plot
sns.scatterplot(x=X, y=Y, label='Data Aktual', s=100, color='royalblue', alpha=0.7)
# Garis Regresi
plt.plot(X, regression_line, color='red', linewidth=2, label=f'Garis Regresi (Y = {slope:.2f}X + {intercept:.2f})')

plt.title('Skor Verbal Wanita vs. Skor Verbal Pria (1967-1990)', fontsize=16)
plt.xlabel('Skor Verbal Pria', fontsize=12)
plt.ylabel('Skor Verbal Wanita', fontsize=12)
plt.legend()
plt.grid(True, which='both', linestyle='--', linewidth=0.5)
plt.axis('equal') # Membuat skala sumbu X dan Y sama agar mudah dibandingkan
plt.tight_layout()
# Menyimpan plot ke file
plt.savefig('plot_verbal_wanita_vs_pria.png', dpi=300)
print("Plot skor verbal wanita vs pria telah disimpan sebagai 'plot_verbal_wanita_vs_pria.png'")

# Menampilkan hasil analisis untuk pertanyaan (c) dan (d)
print("\n--- Hasil Analisis Regresi (d) ---")
print(f"Persamaan Garis Regresi: Skor Verbal Wanita = {intercept:.2f} + {slope:.2f} * Skor Verbal Pria")
correlation = np.corrcoef(X, Y)[0, 1]
r_squared = correlation**2
print(f"Koefisien Korelasi (r): {correlation:.4f}")
print(f"Koefisien Determinasi (R²): {r_squared:.4f}")
print("-> Ini menunjukkan hubungan linear positif yang sangat kuat.")

# Tampilkan semua plot di akhir
plt.show()
