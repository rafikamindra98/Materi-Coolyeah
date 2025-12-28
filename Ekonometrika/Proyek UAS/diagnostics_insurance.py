"""Diagnostics script for insurance.xlsx
Generates regression with bmi*smoker interaction and runs:
- Normality tests (Shapiro-Wilk, Jarque-Bera)
- Multicollinearity (VIF)
- Heteroskedasticity (Breusch-Pagan)
Saves outputs to the project folder.
"""
from pathlib import Path
import sys

PROJECT_DIR = Path(__file__).resolve().parent
DATA_PATH = PROJECT_DIR / 'insurance.xlsx'
OUT_DIR = PROJECT_DIR

# Optional: adjust python interpreter if using venv

def main():
    try:
        import pandas as pd
        import numpy as np
        import statsmodels.api as sm
        import statsmodels.formula.api as smf
        from statsmodels.stats.outliers_influence import variance_inflation_factor
        from statsmodels.stats.diagnostic import het_breuschpagan
        from statsmodels.stats.diagnostic import acorr_ljungbox
        from statsmodels.stats.stattools import jarque_bera
        from statsmodels.stats.stattools import durbin_watson
        from scipy.stats import shapiro
        import patsy
        import matplotlib.pyplot as plt
        import seaborn as sns
    except Exception as e:
        print('Missing Python packages. Install required packages with:')
        print('pip install pandas numpy statsmodels scipy matplotlib seaborn openpyxl patsy')
        raise

    print('Loading data from:', DATA_PATH)
    df = pd.read_excel(DATA_PATH)
    print('Columns:', list(df.columns))

    possible_targets = ["charges","cost","expenses","medical_cost","total_charge","charge","amount"]
    target = None
    for t in possible_targets:
        if t in df.columns:
            target = t
            break
    if target is None:
        numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
        if len(numeric_cols) >= 1:
            target = numeric_cols[0]
            print('No standard target found; using first numeric column:', target)
        else:
            raise ValueError('Tidak menemukan kolom target numerik di file.')

    if 'bmi' not in df.columns or 'smoker' not in df.columns:
        raise ValueError("File harus berisi kolom 'bmi' dan 'smoker'. Tolong periksa nama kolom pada file Anda.")

    exclude = {target, 'bmi', 'smoker'}
    other_cols = [c for c in df.columns if c not in exclude]
    categorical = [c for c in other_cols if df[c].dtype == 'object' or df[c].dtype.name=='category']
    numeric = [c for c in other_cols if c not in categorical]

    terms = []
    for c in numeric:
        terms.append(c)
    for c in categorical:
        terms.append(f"C({c})")

    formula = target + ' ~ bmi * smoker'
    if terms:
        formula += ' + ' + ' + '.join(terms)

    print('Using formula:', formula)

    used_cols = [target, 'bmi', 'smoker'] + numeric + categorical
    df2 = df[used_cols].dropna()

    model = smf.ols(formula=formula, data=df2)
    results = model.fit()

    with open(OUT_DIR / 'regression_summary.txt', 'w', encoding='utf-8') as f:
        f.write(results.summary().as_text())
    print('Regression summary saved to regression_summary.txt')

    resid = results.resid
    fitted = results.fittedvalues

    sh_stat, sh_p = shapiro(resid)
    jb_stat, jb_p, jb_skew, jb_kurt = jarque_bera(resid)
    with open(OUT_DIR / 'normality_tests.txt', 'w', encoding='utf-8') as f:
        f.write(f'Shapiro-Wilk: stat={sh_stat:.6f}, p={sh_p:.6g}\n')
        f.write(f'Jarque-Bera: stat={jb_stat:.6f}, p={jb_p:.6g}\n')
    print('Normality tests saved to normality_tests.txt')

    # Residual hist
    plt.figure(figsize=(6,4))
    sns.histplot(resid, kde=True)
    plt.title('Residuals Histogram')
    plt.xlabel('Residual')
    plt.tight_layout()
    plt.savefig(OUT_DIR / 'residuals_hist.png')
    plt.close()

    sm.qqplot(resid, line='45')
    plt.title('QQ plot of residuals')
    plt.tight_layout()
    plt.savefig(OUT_DIR / 'residuals_qq.png')
    plt.close()

    # VIF
    exog = results.model.exog
    exog_names = results.model.exog_names
    vif_data = []
    for i in range(exog.shape[1]):
        try:
            vif = variance_inflation_factor(exog, i)
            vif_data.append((exog_names[i], vif))
        except Exception:
            vif_data.append((exog_names[i], float('nan')))
    import pandas as pd
    vif_df = pd.DataFrame(vif_data, columns=['variable','VIF']).sort_values('VIF', ascending=False)
    vif_df.to_csv(OUT_DIR / 'vif.csv', index=False)

    plt.figure(figsize=(8,4))
    sns.barplot(data=vif_df, x='VIF', y='variable')
    plt.title('VIF per variable')
    plt.tight_layout()
    plt.savefig(OUT_DIR / 'vif_bar.png')
    plt.close()

    # Breusch-Pagan
    bp_test = het_breuschpagan(resid, results.model.exog)
    bp_stat, bp_pvalue, fvalue, fpvalue = bp_test
    with open(OUT_DIR / 'heteroskedasticity_tests.txt', 'w', encoding='utf-8') as f:
        f.write(f'Breusch-Pagan LM stat={bp_stat:.6f}, p={bp_pvalue:.6g}\n')
        f.write(f'Breusch-Pagan F stat={fvalue:.6f}, p={fpvalue:.6g}\n')

    plt.figure(figsize=(6,4))
    sns.scatterplot(x=fitted, y=resid)
    plt.axhline(0, color='red', linestyle='--')
    plt.xlabel('Fitted values')
    plt.ylabel('Residuals')
    plt.title('Residuals vs Fitted')
    plt.tight_layout()
    plt.savefig(OUT_DIR / 'residuals_vs_fitted.png')
    plt.close()

    # Autocorrelation tests: Durbin-Watson and Ljung-Box, plus ACF plot
    dw = durbin_watson(resid)
    try:
        lb_df = acorr_ljungbox(resid, lags=[10], return_df=True)
    except Exception:
        # fallback for older statsmodels versions
        from statsmodels.stats.diagnostic import acorr_ljungbox as _ac
        lb = _ac(resid, lags=[10])
        import pandas as pd
        lb_df = pd.DataFrame({'lb_stat': [lb[0][-1]], 'lb_pvalue': [lb[1][-1]]}, index=[10])

    with open(OUT_DIR / 'autocorrelation_tests.txt', 'w', encoding='utf-8') as f:
        f.write(f'Durbin-Watson: {dw:.6f}\n')
        f.write('\nLjung-Box test (lag=10):\n')
        f.write(lb_df.to_string())

    # ACF plot
    try:
        from statsmodels.graphics.tsaplots import plot_acf
        plt.figure(figsize=(6,4))
        plot_acf(resid, lags=30)
        plt.tight_layout()
        plt.savefig(OUT_DIR / 'residuals_acf.png')
        plt.close()
    except Exception:
        pass

    print('\nOutputs saved to project folder:')
    for fn in ['regression_summary.txt','normality_tests.txt','residuals_hist.png','residuals_qq.png','vif.csv','vif_bar.png','heteroskedasticity_tests.txt','residuals_vs_fitted.png','autocorrelation_tests.txt','residuals_acf.png']:
        print('-', OUT_DIR / fn)

if __name__ == '__main__':
    main()
