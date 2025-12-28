"""
Analisis Regresi Linear Berganda yang Diperbaiki dengan Feature Engineering
Script ini membangun model prediktif yang ditingkatkan dengan feature engineering
dan seleksi fitur untuk memprediksi biaya asuransi dengan akurasi lebih tinggi.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
import statsmodels.api as sm
import warnings
warnings.filterwarnings('ignore')


# =============================================================================
# 1. PERSIAPAN DATA
# =============================================================================
def load_and_prepare_data(filepath):
    """
    Muat dan siapkan data dengan menghapus variabel yang tidak signifikan.
    
    Parameter:
    -----------
    filepath : str
        Lokasi file data (Excel atau CSV).
    
    Return:
    --------
    df : pandas.DataFrame
        Dataset yang sudah disiapkan.
    """
    print("=" * 80)
    print("LANGKAH 1: PERSIAPAN DATA")
    print("=" * 80)
    
    # Muat data
    if filepath.endswith('.xlsx'):
        df = pd.read_excel(filepath)
    else:
        df = pd.read_csv(filepath)
    
    print(f"\nDataset asli:")
    print(f"  Bentuk: {df.shape}")
    print(f"  Kolom: {df.columns.tolist()}")
    print(f"\nBeberapa baris pertama:\n{df.head()}")
    print(f"\nStatistik deskriptif:\n{df.describe()}")
    
    return df


def preprocess_data_improved(df):
    """
    Praproses data dengan:
    - Membuat dummy variable untuk 'smoker'
    - Menghapus variabel tidak signifikan ('sex' dan 'region')
    
    Parameter:
    -----------
    df : pandas.DataFrame
        Dataset asli.
    
    Return:
    --------
    df_processed : pandas.DataFrame
        Dataset yang sudah diproses.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 1b: PEMBUATAN DUMMY VARIABLE DAN SELEKSI FITUR")
    print("=" * 80)
    
    # Buat salinan dataset
    df_processed = df.copy()
    
    # Buat dummy variable untuk 'smoker' (drop_first=True untuk menghindari multicollinearity)
    # Ini menghasilkan kolom 'smoker_yes'
    smoker_dummy = pd.get_dummies(df_processed['smoker'], prefix='smoker', drop_first=True)
    # Konversi ke integer (0 dan 1)
    smoker_dummy = smoker_dummy.astype(int)
    df_processed = pd.concat([df_processed, smoker_dummy], axis=1)
    
    print(f"\nDummy variable 'smoker' dibuat:")
    print(f"  Kolom baru: {smoker_dummy.columns.tolist()}")
    
    # Hapus variabel yang tidak signifikan secara statistik: 'sex' dan 'region'
    # Juga hapus kolom 'smoker' asli karena sudah diganti dengan dummy variable
    columns_to_drop = ['sex', 'region', 'smoker']
    df_processed = df_processed.drop(columns=columns_to_drop)
    
    print(f"\nVariabel tidak signifikan dihapus: {columns_to_drop}")
    print(f"\nDataset setelah pemrosesan:")
    print(f"  Bentuk: {df_processed.shape}")
    print(f"  Kolom: {df_processed.columns.tolist()}")
    print(f"\nBeberapa baris pertama:\n{df_processed.head()}")
    
    return df_processed


# =============================================================================
# 2. FEATURE ENGINEERING - INTERAKSI
# =============================================================================
def create_interaction_features(df):
    """
    Membuat fitur interaksi untuk meningkatkan akurasi model.
    
    Fitur interaksi yang dibuat:
    - bmi_smoker: Interaksi antara BMI dan status perokok
      Alasan: Efek obesitas pada biaya asuransi mungkin berbeda antara perokok dan non-perokok
    
    Parameter:
    -----------
    df : pandas.DataFrame
        Dataset yang sudah diproses.
    
    Return:
    --------
    df_engineered : pandas.DataFrame
        Dataset dengan fitur interaksi tambahan.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 2: FEATURE ENGINEERING - PEMBUATAN FITUR INTERAKSI")
    print("=" * 80)
    
    # Buat salinan
    df_engineered = df.copy()
    
    # Buat fitur interaksi: bmi_smoker = bmi * smoker_yes
    df_engineered['bmi_smoker'] = df_engineered['bmi'] * df_engineered['smoker_yes']
    
    print(f"\nFitur interaksi dibuat:")
    print(f"  Nama fitur: 'bmi_smoker'")
    print(f"  Formula: bmi × smoker_yes")
    print(f"  Alasan: Menguji hipotesis bahwa efek obesitas lebih drastis pada perokok")
    
    print(f"\nDataset dengan fitur interaksi:")
    print(f"  Bentuk: {df_engineered.shape}")
    print(f"  Kolom baru: {[col for col in df_engineered.columns if col not in df.columns]}")
    print(f"  Semua kolom: {df_engineered.columns.tolist()}")
    print(f"\nBeberapa baris pertama:\n{df_engineered.head()}")
    
    return df_engineered


# =============================================================================
# 3. DEFINISI FITUR DAN TARGET
# =============================================================================
def define_features_and_target_improved(df_engineered):
    """
    Tentukan variabel fitur (X) dan target (y) dengan fitur-fitur yang dipilih.
    
    Parameter:
    -----------
    df_engineered : pandas.DataFrame
        Dataset dengan fitur interaksi.
    
    Return:
    --------
    X : pandas.DataFrame
        Matriks fitur (age, bmi, children, smoker_yes, bmi_smoker).
    y : pandas.Series
        Variabel target (charges).
    """
    print("\n" + "=" * 80)
    print("LANGKAH 3: MENDEFINISIKAN FITUR DAN TARGET")
    print("=" * 80)
    
    # Fitur yang dipilih (setelah seleksi fitur)
    selected_features = ['age', 'bmi', 'children', 'smoker_yes', 'bmi_smoker']
    
    X = df_engineered[selected_features]
    y = df_engineered['charges']
    
    print(f"\nFitur yang dipilih (X):")
    print(f"  {selected_features}")
    print(f"  Bentuk: {X.shape}")
    
    print(f"\nTarget (y):")
    print(f"  Variabel: 'charges'")
    print(f"  Bentuk: {y.shape}")
    
    print(f"\nStatistik deskriptif fitur:")
    print(X.describe())
    
    return X, y


# =============================================================================
# 4. PEMBAGIAN DATA
# =============================================================================
def split_data_improved(X, y, test_size=0.2, random_state=42):
    """
    Bagi data menjadi set pelatihan dan pengujian.
    
    Parameter:
    -----------
    X : pandas.DataFrame
        Matriks fitur.
    y : pandas.Series
        Variabel target.
    test_size : float
        Proporsi data untuk pengujian.
    random_state : int
        Benih acak untuk reproduksibilitas.
    
    Return:
    --------
    X_train, X_test, y_train, y_test : tuple
        Set pelatihan dan pengujian.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 4: PEMBAGIAN DATA (80% PELATIHAN, 20% PENGUJIAN)")
    print("=" * 80)
    
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=test_size, random_state=random_state
    )
    
    print(f"\nUkuran set pelatihan: {X_train.shape[0]} sampel (80%)")
    print(f"Ukuran set pengujian: {X_test.shape[0]} sampel (20%)")
    print(f"Jumlah fitur: {X_train.shape[1]}")
    
    return X_train, X_test, y_train, y_test


# =============================================================================
# 5. PEMODELAN DAN PELATIHAN
# =============================================================================
def build_and_train_model_improved(X_train, y_train):
    """
    Bangun dan latih model regresi linear berganda menggunakan statsmodels OLS.
    
    Parameter:
    -----------
    X_train : pandas.DataFrame
        Fitur pelatihan.
    y_train : pandas.Series
        Target pelatihan.
    
    Return:
    --------
    results : statsmodels.regression.linear_model.RegressionResults
        Model yang sudah dilatih.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 5: PEMODELAN DAN PELATIHAN")
    print("=" * 80)
    
    # Tambahkan konstanta (intercept)
    X_train_const = sm.add_constant(X_train)
    
    # Bangun model OLS
    model = sm.OLS(y_train, X_train_const)
    results = model.fit()
    
    print(f"\nModel berhasil dilatih!")
    print(f"  Jumlah observasi: {int(results.nobs)}")
    print(f"  Jumlah fitur: {int(results.df_model)}")
    print(f"  Derajat kebebasan residual: {int(results.df_resid)}")
    
    return results


# =============================================================================
# 6. EVALUASI MODEL
# =============================================================================
def evaluate_model_improved(results, X_test, y_test):
    """
    Evaluasi model pada set pengujian dan hitung metrik performa.
    
    Parameter:
    -----------
    results : statsmodels.regression.linear_model.RegressionResults
        Model yang sudah dilatih.
    X_test : pandas.DataFrame
        Fitur pengujian.
    y_test : pandas.Series
        Target pengujian.
    
    Return:
    --------
    metrics : dict
        Kamus yang berisi metrik evaluasi.
    y_pred : numpy.ndarray
        Prediksi pada set pengujian.
    residuals : numpy.ndarray
        Residual (error) pada set pengujian.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 6: EVALUASI MODEL")
    print("=" * 80)
    
    # Tambahkan konstanta ke fitur pengujian
    X_test_const = sm.add_constant(X_test)
    
    # Buat prediksi
    y_pred = results.predict(X_test_const)
    
    # Hitung residual
    residuals = y_test.values - y_pred.values
    
    # Hitung metrik evaluasi
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    mae = np.mean(np.abs(residuals))
    r_squared_test = r2_score(y_test, y_pred)
    
    metrics = {
        'RMSE': rmse,
        'MAE': mae,
        'R_squared_test': r_squared_test
    }
    
    print(f"\nMetrik Performa Model pada Set Pengujian:")
    print(f"  Akar Kesalahan Kuadrat Rerata (RMSE): ${rmse:.2f}")
    print(f"  Kesalahan Absolut Rerata (MAE): ${mae:.2f}")
    print(f"  R-squared (Test Set): {r_squared_test:.4f}")
    
    return metrics, y_pred, residuals


# =============================================================================
# 7. VISUALISASI DAN DIAGNOSIS
# =============================================================================
def create_diagnostic_plots(y_test, y_pred, residuals):
    """
    Buat scatter plot sederhana untuk analisis residual dan heteroskedastisitas.
    
    Parameter:
    -----------
    y_test : pandas.Series
        Nilai target aktual.
    y_pred : numpy.ndarray
        Nilai prediksi.
    residuals : numpy.ndarray
        Residual (error).
    """
    print("\n" + "=" * 80)
    print("LANGKAH 7: VISUALISASI DAN DIAGNOSA")
    print("=" * 80)
    
    # Buat figure dengan dua subplot
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    
    # Plot 1: Nilai Prediksi vs Nilai Aktual
    axes[0].scatter(y_test, y_pred, alpha=0.6, edgecolors='k')
    axes[0].plot([y_test.min(), y_test.max()], [y_test.min(), y_test.max()], 'r--', lw=2)
    axes[0].set_xlabel('Nilai Aktual (Charges)', fontsize=11)
    axes[0].set_ylabel('Nilai Prediksi', fontsize=11)
    axes[0].set_title('Nilai Prediksi vs Nilai Aktual', fontsize=12, fontweight='bold')
    axes[0].grid(True, alpha=0.3)
    
    # Plot 2: Nilai Prediksi vs Residual (untuk mendeteksi heteroskedastisitas)
    axes[1].scatter(y_pred, residuals, alpha=0.6, edgecolors='k')
    axes[1].axhline(y=0, color='r', linestyle='--', lw=2)
    axes[1].set_xlabel('Nilai Prediksi', fontsize=11)
    axes[1].set_ylabel('Residual (Error)', fontsize=11)
    axes[1].set_title('Nilai Prediksi vs Residual (Heteroskedastisitas)', fontsize=12, fontweight='bold')
    axes[1].grid(True, alpha=0.3)
    
    plt.tight_layout()
    
    # Simpan plot
    plot_path = '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/model_diagnostics.png'
    plt.savefig(plot_path, dpi=300, bbox_inches='tight')
    print(f"\nPlot diagnostik tersimpan di: {plot_path}")
    
    plt.show()
    
    # Analisis heteroskedastisitas
    print(f"\nAnalisis Residual:")
    print(f"  Mean Residual: {np.mean(residuals):.6f} (seharusnya ≈ 0)")
    print(f"  Std Residual: {np.std(residuals):.2f}")
    print(f"  Min Residual: ${np.min(residuals):.2f}")
    print(f"  Max Residual: ${np.max(residuals):.2f}")


# =============================================================================
# 8. RINGKASAN DAN OUTPUT HASIL
# =============================================================================
def print_comprehensive_summary(results, metrics):
    """
    Cetak ringkasan komprehensif model termasuk statistik dan metrik evaluasi.
    
    Parameter:
    -----------
    results : statsmodels.regression.linear_model.RegressionResults
        Model yang sudah dilatih.
    metrics : dict
        Kamus yang berisi metrik evaluasi.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 8: RINGKASAN MODEL DAN HASIL STATISTIK")
    print("=" * 80)
    
    print("\n" + results.summary().as_text())
    
    print("\n" + "=" * 80)
    print("PERBANDINGAN MODEL LAMA vs MODEL BARU")
    print("=" * 80)
    print("\nModel Lama (Tanpa Feature Engineering):")
    print("  R-squared: 0.7417")
    print("  RMSE: $5,796.28")
    print("  Variabel tidak signifikan: sex, region")
    
    print("\nModel Baru (Dengan Feature Engineering):")
    print(f"  R-squared (Training): {results.rsquared:.4f}")
    print(f"  R-squared (Testing): {metrics['R_squared_test']:.4f}")
    print(f"  RMSE (Testing): ${metrics['RMSE']:.2f}")
    print(f"  MAE (Testing): ${metrics['MAE']:.2f}")
    
    # Hitung peningkatan
    rmse_improvement = (5796.28 - metrics['RMSE']) / 5796.28 * 100
    rsquared_improvement = (metrics['R_squared_test'] - 0.7417) / 0.7417 * 100
    
    print(f"\nPeningkatan Performa:")
    print(f"  Penurunan RMSE: {rmse_improvement:.2f}%")
    print(f"  Peningkatan R-squared: {rsquared_improvement:.2f}%")
    
    if metrics['R_squared_test'] > 0.80:
        print(f"\n✓ BERHASIL! R-squared > 0.80")
    else:
        print(f"\n⚠ Target R-squared > 0.80 belum tercapai")
    
    print("\n" + "=" * 80)


# =============================================================================
# FUNGSI UTAMA
# =============================================================================
def main():
    """
    Fungsi utama untuk menjalankan seluruh pipeline analisis regresi yang diperbaiki.
    """
    print("\n")
    print("╔" + "=" * 78 + "╗")
    print("║" + " ANALISIS REGRESI LINEAR BERGANDA YANG DITINGKATKAN ".center(78) + "║")
    print("║" + " dengan Feature Engineering dan Seleksi Fitur ".center(78) + "║")
    print("╚" + "=" * 78 + "╝")
    
    # Lokasi file data
    # Coba berbagai format file
    possible_paths = [
        '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance.xlsx',
        '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance.csv',
        '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance - Sheet1.csv'
    ]
    
    filepath = None
    for path in possible_paths:
        try:
            import os
            if os.path.exists(path):
                filepath = path
                break
        except:
            continue
    
    if filepath is None:
        print("Kesalahan: File data 'insurance' tidak ditemukan!")
        return
    
    print(f"\nFile data yang digunakan: {filepath}")
    
    # Langkah 1: Persiapan data
    df = load_and_prepare_data(filepath)
    df_processed = preprocess_data_improved(df)
    
    # Langkah 2: Feature engineering
    df_engineered = create_interaction_features(df_processed)
    
    # Langkah 3: Definisi fitur dan target
    X, y = define_features_and_target_improved(df_engineered)
    
    # Langkah 4: Pembagian data
    X_train, X_test, y_train, y_test = split_data_improved(X, y, test_size=0.2, random_state=42)
    
    # Langkah 5: Pemodelan dan pelatihan
    results = build_and_train_model_improved(X_train, y_train)
    
    # Langkah 6: Evaluasi model
    metrics, y_pred, residuals = evaluate_model_improved(results, X_test, y_test)
    
    # Langkah 7: Visualisasi
    create_diagnostic_plots(y_test, y_pred, residuals)
    
    # Langkah 8: Ringkasan
    print_comprehensive_summary(results, metrics)


if __name__ == '__main__':
    main()
