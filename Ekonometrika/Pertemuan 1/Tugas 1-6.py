import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os

def plot_scatter_only(df):
    """
    Menampilkan scatter plot data tanpa garis regresi.
    """
    plt.style.use('seaborn-v0_8-whitegrid')
    fig, ax = plt.subplots(figsize=(12, 8))
    sns.scatterplot(x='Expenditure', y='Impressions', data=df, s=100, ax=ax, label='Data Aktual', color='royalblue', edgecolor='black')
    ax.set_title('Scatter Plot Pengeluaran Iklan vs Impresi (Tanpa Garis Regresi)', fontsize=16, fontweight='bold')
    ax.set_xlabel('Expenditure (dalam jutaan dolar 1983)', fontsize=12)
    ax.set_ylabel('Impressions (dalam jutaan)', fontsize=12)
    ax.legend()
    ax.grid(True, which='both', linestyle='--', linewidth=0.5)
    plt.tight_layout()
    plt.show()
    
def clean_data(df):
    """
    Membersihkan DataFrame: mengganti nama kolom dan menghapus baris total.
    """
    # Mengganti nama kolom agar lebih mudah digunakan
    df.columns = ['No', 'Firm', 'Impressions', 'Expenditure']
    
    # Menghapus baris 'Total' dan 'Rata-rata' jika ada
    # dengan mencari baris yang kolom 'Firm'-nya mengandung kata 'Total' atau 'Rata'
    df = df[~df['Firm'].str.contains('Total|Rata', na=False, case=False)]
    
    # Mengonversi kolom ke tipe data numerik, mengubah error menjadi NaN
    df['Impressions'] = pd.to_numeric(df['Impressions'], errors='coerce')
    df['Expenditure'] = pd.to_numeric(df['Expenditure'], errors='coerce')
    
    # Menghapus baris dengan nilai kosong (jika ada setelah konversi)
    df.dropna(subset=['Impressions', 'Expenditure'], inplace=True)
    
    return df

def perform_regression_analysis(df):
    """
    Melakukan analisis regresi linear sederhana pada DataFrame yang sudah bersih.
    """
    Y = df['Impressions']
    X = df['Expenditure']

    # Perhitungan Matematis untuk Regresi OLS
    n = len(df)
    if n == 0:
        print("Tidak ada data valid untuk dianalisis.")
        return None, None, None

    X_mean = np.mean(X)
    Y_mean = np.mean(Y)

    numerator = np.sum((X - X_mean) * (Y - Y_mean))
    denominator = np.sum((X - X_mean)**2)

    # Hitung koefisien
    beta_1 = numerator / denominator
    beta_0 = Y_mean - (beta_1 * X_mean)

    # Hitung R-squared
    y_pred = beta_0 + beta_1 * X
    ss_total = np.sum((Y - Y_mean)**2)
    ss_residual = np.sum((Y - y_pred)**2)
    r_squared = 1 - (ss_residual / ss_total)

    # Tampilkan hasil
    print("--- Analisis Regresi Linear Pengaruh Iklan ---")
    print(f"Jumlah Observasi (n): {n}")
    print(f"Rata-rata Expenditure (X̄): {X_mean:.2f} juta dolar")
    print(f"Rata-rata Impressions (Ȳ): {Y_mean:.2f} juta\n")
    print("Hasil Estimasi Model Regresi:")
    print(f"  Intercept (β₀): {beta_0:.4f}")
    print(f"  Slope (β₁): {beta_1:.4f}\n")
    print("Persamaan Garis Regresi:")
    print(f"  Impressions = {beta_0:.2f} + {beta_1:.4f} * Expenditure\n")
    print(f"Koefisien Determinasi (R²): {r_squared:.4f}")
    print(f"-> Artinya, sekitar {r_squared:.2%} variasi dalam Impressions dapat dijelaskan oleh Expenditure.\n")

    return beta_0, beta_1, y_pred

def plot_results(df, beta_0, beta_1, y_pred):
    """
    Membuat dan menampilkan diagram pencar beserta garis regresinya.
    """
    plt.style.use('seaborn-v0_8-whitegrid')
    fig, ax = plt.subplots(figsize=(12, 8))

    # Scatter plot data aktual
    sns.scatterplot(x='Expenditure', y='Impressions', data=df, s=100, ax=ax, label='Data Aktual', color='royalblue', edgecolor='black')

    # Garis regresi
    sns.lineplot(x=df['Expenditure'], y=y_pred, color='red', linewidth=2.5, ax=ax, label=f'Garis Regresi (Estimasi OLS)')
    
    # Pengaturan plot
    ax.set_title('Hubungan Pengeluaran Iklan (Expenditure) dan Impresi (Impressions)', fontsize=16, fontweight='bold')
    ax.set_xlabel('Expenditure (dalam jutaan dolar 1983)', fontsize=12)
    ax.set_ylabel('Impressions (dalam jutaan)', fontsize=12)
    ax.legend()
    ax.grid(True, which='both', linestyle='--', linewidth=0.5)

    # Tampilkan persamaan di plot
    equation_text = f'$\\widehat{{Impressions}} = {beta_0:.2f} + {beta_1:.4f} \\times Expenditure$\n$R^2 = {np.corrcoef(df["Expenditure"], df["Impressions"])[0,1]**2:.4f}$'
    ax.text(0.05, 0.95, equation_text, transform=ax.transAxes, fontsize=12,
            verticalalignment='top', bbox=dict(boxstyle='round,pad=0.5', fc='wheat', alpha=0.5))

    plt.tight_layout()
    plt.show()

def main():
    """
    Fungsi utama untuk menjalankan seluruh proses analisis.
    """
    # Nama file Excel
    filename = os.path.join(os.path.dirname(__file__), 'Data Pengaruh Pengeluaran Iklan.xlsx')
    
    if not os.path.exists(filename):
        print(f"Error: File '{filename}' tidak ditemukan.")
        print("Pastikan file Excel berada di direktori yang sama dengan script Python ini.")
        return

    try:
        # Baca file Excel
        df = pd.read_excel(filename)
        # Bersihkan data
        df_cleaned = clean_data(df)

        # Tampilkan scatter plot tanpa garis regresi
        plot_scatter_only(df_cleaned)

        # Lakukan analisis regresi
        beta_0, beta_1, y_pred = perform_regression_analysis(df_cleaned)

        # Tampilkan scatter plot beserta garis regresi jika hasil regresi valid
        if beta_0 is not None and beta_1 is not None and y_pred is not None:
            plot_results(df_cleaned, beta_0, beta_1, y_pred)

    except Exception as e:
        print(f"Terjadi error saat memproses file: {e}")

if __name__ == '__main__':
    main()
