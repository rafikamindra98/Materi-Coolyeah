"""
Analisis Regresi Linear Berganda untuk Prediksi Biaya Asuransi
Script ini membangun model prediktif untuk biaya asuransi menggunakan Regresi Linear Berganda.
"""

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, mean_absolute_error
import statsmodels.api as sm
import warnings
warnings.filterwarnings('ignore')


# =============================================================================
# 1. DATA LOADING
# =============================================================================
def load_data(filepath):
    """
    Memuat dataset asuransi dari file Excel.
    
    Parameter:
    -----------
    filepath : str
        Lokasi file Excel yang berisi data asuransi.
    
    Return:
    --------
    df : pandas.DataFrame
        Dataset yang sudah dimuat.
    """
    print("=" * 80)
    print("LANGKAH 1: MEMUAT DATA")
    print("=" * 80)
    
    df = pd.read_excel(filepath)
    print(f"Bentuk dataset: {df.shape}")
    print(f"\nBeberapa baris pertama:\n{df.head()}")
    print(f"\nJenis tipe data:\n{df.dtypes}")
    print(f"\nStatistik deskriptif:\n{df.describe()}")
    
    return df


# =============================================================================
# 2. DATA PREPROCESSING
# =============================================================================
def preprocess_data(df):
    """
    Praproses data dengan mengkonversi variabel kategori menjadi variabel dummy.
    Menggunakan One-Hot Encoding dengan drop_first=True untuk menghindari Dummy Variable Trap.
    
    Parameter:
    -----------
    df : pandas.DataFrame
        Dataset asli dengan variabel kategori.
    
    Return:
    --------
    df_processed : pandas.DataFrame
        Dataset yang sudah diproses dengan variabel dummy.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 2: PRAPROSES DATA - PENYANDIAN SATU-PANAS (ONE-HOT ENCODING)")
    print("=" * 80)
    
    # Buat salinan untuk menghindari modifikasi pada dataframe asli
    df_processed = df.copy()
    
    # Identifikasi variabel kategori
    categorical_cols = df_processed.select_dtypes(include=['object']).columns.tolist()
    print(f"Kolom kategori yang diidentifikasi: {categorical_cols}")
    
    # Konversi variabel kategori menjadi variabel dummy (drop_first=True untuk menghindari multicollinearity)
    df_processed = pd.get_dummies(df_processed, columns=categorical_cols, drop_first=True)
    
    # Konversi variabel dummy menjadi integer (0 dan 1)
    dummy_cols = [col for col in df_processed.columns if col not in df.columns]
    df_processed[dummy_cols] = df_processed[dummy_cols].astype(int)
    
    print(f"\nBentuk dataset yang diproses: {df_processed.shape}")
    print(f"Kolom yang diproses:\n{df_processed.columns.tolist()}")
    print(f"\nBeberapa baris pertama data yang diproses:\n{df_processed.head()}")
    
    return df_processed


# =============================================================================
# 3. FEATURE AND TARGET DEFINITION
# =============================================================================
def define_features_and_target(df_processed, target_col='charges'):
    """
    Tentukan variabel fitur (X) dan target (y).
    
    Parameter:
    -----------
    df_processed : pandas.DataFrame
        Dataset yang sudah diproses.
    target_col : str
        Nama variabel target.
    
    Return:
    --------
    X : pandas.DataFrame
        Matriks fitur.
    y : pandas.Series
        Variabel target.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 3: MENDEFINISIKAN FITUR DAN TARGET")
    print("=" * 80)
    
    y = df_processed[target_col]
    X = df_processed.drop(columns=[target_col])
    
    print(f"Bentuk fitur (X): {X.shape}")
    print(f"Bentuk target (y): {y.shape}")
    print(f"\nKolom fitur:\n{X.columns.tolist()}")
    
    return X, y


# =============================================================================
# 4. DATA SPLITTING
# =============================================================================
def split_data(X, y, test_size=0.2, random_state=42):
    """
    Bagi data menjadi set pelatihan dan pengujian.
    
    Parameter:
    -----------
    X : pandas.DataFrame
        Matriks fitur.
    y : pandas.Series
        Variabel target.
    test_size : float
        Proporsi data yang digunakan untuk pengujian (default: 0.2 atau 20%).
    random_state : int
        Benih acak untuk reproduksibilitas.
    
    Return:
    --------
    X_train, X_test, y_train, y_test : tuple
        Set pelatihan dan pengujian untuk fitur dan target.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 4: MEMBAGI DATA (80% PELATIHAN, 20% PENGUJIAN)")
    print("=" * 80)
    
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=test_size, random_state=random_state
    )
    
    print(f"Ukuran set pelatihan: {X_train.shape[0]} sampel ({100-test_size*100:.0f}%)")
    print(f"Ukuran set pengujian: {X_test.shape[0]} sampel ({test_size*100:.0f}%)")
    print(f"Jumlah fitur: {X_train.shape[1]}")
    
    return X_train, X_test, y_train, y_test


# =============================================================================
# 5. MODEL BUILDING AND TRAINING
# =============================================================================
def build_and_train_model(X_train, y_train):
    """
    Bangun dan latih model Regresi Linear Berganda menggunakan statsmodels OLS.
    
    Parameter:
    -----------
    X_train : pandas.DataFrame
        Fitur pelatihan.
    y_train : pandas.Series
        Variabel target pelatihan.
    
    Return:
    --------
    model : statsmodels.regression.linear_model.RegressionResults
        Model regresi OLS yang sudah dilatih.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 5: MEMBANGUN DAN MELATIH MODEL")
    print("=" * 80)
    
    # Tambahkan konstanta (intercept) ke fitur
    X_train_with_const = sm.add_constant(X_train)
    
    # Bangun model OLS
    model = sm.OLS(y_train, X_train_with_const)
    
    # Latih model
    results = model.fit()
    
    print("Model telah berhasil dilatih menggunakan statsmodels OLS.")
    print(f"Jumlah observasi: {results.nobs}")
    print(f"Derajat kebebasan: {results.df_model}")
    
    return results


# =============================================================================
# 6. MODEL EVALUATION
# =============================================================================
def evaluate_model(results, X_test, y_test):
    """
    Evaluasi model pada set pengujian dan hitung RMSE dan MAE.
    
    Parameter:
    -----------
    results : statsmodels.regression.linear_model.RegressionResults
        Model regresi OLS yang sudah dilatih.
    X_test : pandas.DataFrame
        Fitur pengujian.
    y_test : pandas.Series
        Variabel target pengujian.
    
    Return:
    --------
    metrics : dict
        Kamus yang berisi RMSE dan MAE.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 6: EVALUASI MODEL")
    print("=" * 80)
    
    # Tambahkan konstanta ke fitur pengujian (harus sesuai dengan pelatihan)
    X_test_with_const = sm.add_constant(X_test)
    
    # Buat prediksi
    y_pred = results.predict(X_test_with_const)
    
    # Hitung metrik evaluasi
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    mae = mean_absolute_error(y_test, y_pred)
    
    metrics = {
        'RMSE': rmse,
        'MAE': mae
    }
    
    print(f"Akar Kesalahan Kuadrat Rerata (RMSE): ${rmse:.2f}")
    print(f"Kesalahan Absolut Rerata (MAE): ${mae:.2f}")
    
    return metrics, y_pred


# =============================================================================
# 7. OUTPUT AND SUMMARY
# =============================================================================
def print_model_summary(results, metrics):
    """
    Cetak ringkasan model komprehensif termasuk detail statistik dan metrik evaluasi.
    
    Parameter:
    -----------
    results : statsmodels.regression.linear_model.RegressionResults
        Model regresi OLS yang sudah dilatih.
    metrics : dict
        Kamus yang berisi metrik evaluasi (RMSE, MAE).
    """
    print("\n" + "=" * 80)
    print("LANGKAH 7: RINGKASAN MODEL DAN HASIL STATISTIK")
    print("=" * 80)
    
    print(results.summary())
    
    print("\n" + "=" * 80)
    print("METRIK EVALUASI PADA SET PENGUJIAN")
    print("=" * 80)
    print(f"Akar Kesalahan Kuadrat Rerata (RMSE): ${metrics['RMSE']:.2f}")
    print(f"Kesalahan Absolut Rerata (MAE): ${metrics['MAE']:.2f}")
    print(f"\nR-squared Model (Pelatihan): {results.rsquared:.4f}")
    print(f"R-squared yang Disesuaikan (Pelatihan): {results.rsquared_adj:.4f}")
    print("=" * 80)


# =============================================================================
# MAIN EXECUTION
# =============================================================================
def main():
    """
    Fungsi utama untuk menjalankan seluruh pipeline analisis Regresi Linear Berganda.
    """
    # Lokasi file
    filepath = '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance.xlsx'
    
    # Langkah 1: Muat data
    df = load_data(filepath)
    
    # Langkah 2: Praproses data
    df_processed = preprocess_data(df)
    
    # Langkah 3: Tentukan fitur dan target
    X, y = define_features_and_target(df_processed, target_col='charges')
    
    # Langkah 4: Bagi data
    X_train, X_test, y_train, y_test = split_data(X, y, test_size=0.2, random_state=42)
    
    # Langkah 5: Bangun dan latih model
    results = build_and_train_model(X_train, y_train)
    
    # Langkah 6: Evaluasi model
    metrics, y_pred = evaluate_model(results, X_test, y_test)
    
    # Langkah 7: Cetak ringkasan
    print_model_summary(results, metrics)


if __name__ == '__main__':
    main()
