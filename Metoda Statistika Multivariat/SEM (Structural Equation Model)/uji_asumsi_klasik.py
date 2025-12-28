import os
import sys
import numpy as np
import pandas as pd
from scipy import stats
from scipy.stats import chi2
import statsmodels.api as sm
from statsmodels.stats.outliers_influence import variance_inflation_factor


def load_data(path):
    if not os.path.exists(path):
        raise FileNotFoundError(f"File tidak ditemukan: {path}")
    return pd.read_excel(path)


def normality_checks(df, vars_list):
    results = []
    for v in vars_list:
        series = df[v].dropna()
        sk = float(stats.skew(series))
        kt = float(stats.kurtosis(series, fisher=False))
        conclusion = "Tidak Normal"
        if -2.58 <= sk <= 2.58:
            conclusion = "Data Berdistribusi Normal (Dapat Diterima)"
        results.append({
            'variable': v,
            'n': len(series),
            'skewness': round(sk, 4),
            'kurtosis': round(kt, 4),
            'kesimpulan_skewness': conclusion
        })
    return pd.DataFrame(results).set_index('variable')


def compute_vif(df, vars_for_vif):
    # VIF requires a matrix of regressors. We'll include the reference variable 'log_gdp'.
    X = df[vars_for_vif].dropna()
    X_const = sm.add_constant(X, has_constant='add')
    vif_data = []
    for i, col in enumerate(X_const.columns):
        try:
            vif = variance_inflation_factor(X_const.values, i)
        except Exception:
            vif = np.nan
        vif_data.append({'variable': col, 'VIF': float(np.round(vif, 4)) if not np.isnan(vif) else np.nan})
    return pd.DataFrame(vif_data).set_index('variable')


def mahalanobis_outliers(df, vars_list, alpha=0.001):
    data = df[vars_list].dropna()
    x = data.values
    mean = np.mean(x, axis=0)
    cov = np.cov(x, rowvar=False)
    # use pseudo-inverse for numerical stability
    inv_cov = np.linalg.pinv(cov)
    diffs = x - mean
    m_dist = np.einsum('ij,jk,ik->i', diffs, inv_cov, diffs)
    df_count = x.shape[1]
    crit = chi2.ppf(1 - alpha, df=df_count)
    outlier_mask = m_dist > crit
    return pd.Series(m_dist, index=data.index, name='mahalanobis_distance'), int(outlier_mask.sum()), float(crit)


def main():
    # --- Configuration ---
    path = r'/Users/user/Downloads/Semester 7/Metoda Statistika Multivariat/SEM (Structural Equation Model)/VSCode 2/billionaires_sem_ready.xlsx'
    vars_to_test = ['log_worth', 'log_gdp', 'gross_tertiary_education_enrollment', 'total_tax_rate_country', 'age']

    print('\nUJI ASUMSI KLASIK - Skripsi Bab 4')
    print('Data file:', path)

    # Load
    try:
        df = load_data(path)
    except Exception as e:
        print('Gagal memuat data:', e)
        sys.exit(1)

    missing_vars = [v for v in vars_to_test if v not in df.columns]
    if missing_vars:
        print('\nERROR: Variabel berikut tidak ditemukan di data:', missing_vars)
        sys.exit(1)

    # 1) Normality
    print('\n1) Uji Normalitas (Skewness & Kurtosis)')
    norm_df = normality_checks(df, vars_to_test)
    print(norm_df.to_string())

    # 2) Multicollinearity (VIF)
    print('\n2) Uji Multikolinearitas (VIF)')
    # We compute VIF among ['log_gdp', 'gross_tertiary_education_enrollment', 'total_tax_rate_country']
    vif_vars = ['log_gdp', 'gross_tertiary_education_enrollment', 'total_tax_rate_country']
    missing_vif = [v for v in vif_vars if v not in df.columns]
    if missing_vif:
        print('Variabel untuk VIF tidak lengkap:', missing_vif)
    else:
        vif_df = compute_vif(df, vif_vars)
        # show only the two exogenous variables' VIF
        print(vif_df.loc[[ 'log_gdp', 'gross_tertiary_education_enrollment', 'total_tax_rate_country']].to_string())
        # Conclusion
        conclusions = []
        for var in ['gross_tertiary_education_enrollment', 'total_tax_rate_country']:
            v = vif_df.loc[var, 'VIF']
            if pd.notnull(v) and v < 5:
                conclusions.append(f"{var}: Tidak terjadi Multikolinearitas (VIF={v})")
            else:
                conclusions.append(f"{var}: Kemungkinan Multikolinearitas (VIF={v})")
        print('\nKesimpulan VIF:')
        for c in conclusions:
            print('-', c)

    # 3) Outlier (Mahalanobis Distance)
    print('\n3) Uji Outlier (Mahalanobis Distance)')
    md_series, n_outliers, crit = mahalanobis_outliers(df, vars_to_test, alpha=0.001)
    print(f'Critical value (Chi-square, df={len(vars_to_test)}, alpha=0.001): {crit:.4f}')
    print(f'Jumlah data yang melebihi critical value (outlier): {n_outliers}')
    print('\nContoh 10 baris pertama Mahalanobis distance:')
    md_print = md_series.rename('mahalanobis_distance').to_frame()
    print(md_print.head(10).to_string())

    print('\nCatatan: Outlier tidak dihapus — hanya dilaporkan untuk keperluan Bab 4.')


if __name__ == '__main__':
    main()
