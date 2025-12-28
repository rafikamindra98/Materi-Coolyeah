"""
Create figures and summary table for SEM MGA report.

Outputs saved next to the input file:
- mga_comparison.png
- correlation_heatmap.png

This script:
- Loads cleaned data `billionaires_sem_ready.xlsx`.
- Fits the simple path model on Full sample and separately for Self-Made and Inherited.
- Plots a bar chart comparing `log_gdp -> log_worth` coefficient between groups.
- Plots correlation heatmap for model variables.
- Computes indirect effect a*b (education -> log_gdp -> log_worth).
- Prints a formatted summary table (Full / Self-Made / Inherited) for the `log_gdp -> log_worth` path.

Run:
    python "VSCode 2/sem_report_billionaires.py"
"""

import os
import sys
import subprocess
import pandas as pd
import numpy as np

# File paths
DATA_PATH = r"/Users/user/Downloads/Semester 7/Metoda Statistika Multivariat/SEM (Structural Equation Model)/VSCode 2/billionaires_sem_ready.xlsx"
OUT_DIR = os.path.dirname(DATA_PATH)


def ensure_packages():
    """Ensure semopy, seaborn and matplotlib are installed."""
    try:
        import semopy  # noqa: F401
        import seaborn  # noqa: F401
        import matplotlib  # noqa: F401
    except Exception:
        print("Installing required packages (semopy, seaborn, matplotlib)...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", "semopy", "seaborn", "matplotlib", "openpyxl"])


def normalize_selfmade(series: pd.Series) -> pd.Series:
    """Normalize `selfMade` to 'Self-Made' or 'Inherited'."""
    s = series.copy()
    if pd.api.types.is_bool_dtype(s) or s.dropna().isin([True, False]).all():
        return s.map({True: 'Self-Made', False: 'Inherited'})
    s = s.astype(str).str.strip().str.lower()
    def map_val(v):
        if pd.isna(v) or v == 'nan':
            return np.nan
        if 'self' in v or v in ('yes', 'y', 'true', '1'):
            return 'Self-Made'
        return 'Inherited'
    return s.map(map_val)


def safe_inspect(model):
    """Return a normalized parameter table with columns lhs, op, rhs, Estimate, p-value (if available)."""
    df = model.inspect()
    df = df.rename(columns={c: c.lower() for c in df.columns})
    # heuristics
    lhs_col = next((c for c in ('lval','lhs','left') if c in df.columns), None)
    rhs_col = next((c for c in ('rval','rhs','right') if c in df.columns), None)
    op_col = next((c for c in ('op','operator') if c in df.columns), None)
    est_col = next((c for c in ('estimate','est','value') if c in df.columns), None)
    p_col = next((c for c in ('p-value','p_value','pvalue','p') if c in df.columns), None)
    se_col = next((c for c in ('se','std.err','std_error') if c in df.columns), None)

    if lhs_col and rhs_col and est_col:
        out = pd.DataFrame()
        out['lhs'] = df[lhs_col]
        out['op'] = df[op_col] if op_col in df.columns else '~'
        out['rhs'] = df[rhs_col]
        out['Estimate'] = df[est_col]
        if se_col:
            out['SE'] = df[se_col]
        if p_col:
            out['p-value'] = df[p_col]
        return out
    return df


def get_path(params_df, lhs, rhs):
    """Return (estimate, pvalue) for a given lhs ~ rhs path from params_df."""
    if params_df is None or params_df.empty:
        return (None, None)
    mask = (params_df['lhs'].astype(str) == lhs) & (params_df['rhs'].astype(str) == rhs)
    if mask.any():
        row = params_df.loc[mask].iloc[0]
        est = row.get('Estimate', None)
        p = row.get('p-value', None)
        return (est, p)
    return (None, None)


def fit_model_and_inspect(model_spec, df):
    """Fit semopy Model and return (model, params_df)."""
    from semopy import Model
    m = Model(model_spec)
    m.fit(df)
    params = safe_inspect(m)
    return m, params


def plot_mga_bar(est_sm, est_in, out_path):
    import matplotlib.pyplot as plt
    import seaborn as sns

    sns.set(style='whitegrid')
    groups = ['Self-Made', 'Inherited']
    estimates = [est_sm, est_in]
    colors = ['#1f77b4', '#d62728']

    plt.figure(figsize=(6, 5))
    ax = sns.barplot(x=groups, y=estimates, palette=colors)
    ax.set_title('Perbandingan Sensitivitas Kekayaan terhadap Ekonomi Negara (GDP)')
    ax.set_ylabel('Koefisien Jalur (GDP ke Kekayaan)')
    ax.set_ylim(min(estimates) - abs(min(estimates))*0.3 if min(estimates) is not None else -1,
                max(estimates) + abs(max(estimates))*0.3 if max(estimates) is not None else 1)

    # Annotate values
    for i, v in enumerate(estimates):
        if v is None or (isinstance(v, float) and np.isnan(v)):
            label = 'NA'
        else:
            label = f"{v:.3f}"
        ax.text(i, v + (0.01 if v>=0 else -0.02), label, ha='center', va='bottom', fontsize=10)

    plt.tight_layout()
    plt.savefig(out_path, dpi=200)
    plt.close()
    print(f"Saved MGA comparison bar chart to: {out_path}")


def plot_correlation_heatmap(df, vars_list, out_path):
    import seaborn as sns
    import matplotlib.pyplot as plt

    num_df = df[vars_list].apply(lambda c: pd.to_numeric(c, errors='coerce'))
    corr = num_df.corr()
    plt.figure(figsize=(8, 6))
    sns.set(style='white')
    sns.heatmap(corr, annot=True, fmt='.2f', cmap='coolwarm', vmin=-1, vmax=1)
    plt.title('Matriks Validitas Hubungan Antar Variabel')
    plt.tight_layout()
    plt.savefig(out_path, dpi=200)
    plt.close()
    print(f"Saved correlation heatmap to: {out_path}")


def print_summary_table(rows):
    """Print a formatted table with columns: Kelompok, Estimate, P-Value, Kesimpulan."""
    header = f"{'Kelompok':<12} | {'Estimate (Koefisien)':>20} | {'P-Value':>8} | {'Kesimpulan':>12}"
    sep = '-' * len(header)
    print('\n' + header)
    print(sep)
    for r in rows:
        kelompok = r['group']
        est = r['estimate']
        p = r['pvalue']
        if est is None or (isinstance(est, float) and np.isnan(est)):
            est_s = 'NA'
        else:
            est_s = f"{est:.3f}"
        if p is None or (isinstance(p, float) and np.isnan(p)):
            p_s = 'NA'
        else:
            try:
                p_s = f"{float(p):.3f}"
            except Exception:
                p_s = str(p)
        kes = 'Signifikan' if (p is not None and not np.isnan(p) and float(p) < 0.05) else 'Tidak Signifikan'
        print(f"{kelompok:<12} | {est_s:>20} | {p_s:>8} | {kes:>12}")


def main():
    ensure_packages()
    from semopy import Model

    if not os.path.exists(DATA_PATH):
        raise FileNotFoundError(f"Data file not found: {DATA_PATH}")

    # 1) Load data
    df = pd.read_excel(DATA_PATH)
    # Normalize selfMade
    df['selfMade_std'] = normalize_selfmade(df.get('selfMade', pd.Series([np.nan]*len(df))))

    # Model specification
    model_spec = '''
log_gdp ~ gross_tertiary_education_enrollment + total_tax_rate_country
log_worth ~ log_gdp + age
'''

    # 2) Split groups
    df_sm = df[df['selfMade_std'] == 'Self-Made']
    df_in = df[df['selfMade_std'] == 'Inherited']

    print(f"Full sample: n={len(df)}, Self-Made: n={len(df_sm)}, Inherited: n={len(df_in)}")

    # 3) Fit models per group
    print('\nFitting model on Self-Made group...')
    model_sm, params_sm = fit_model_and_inspect(model_spec, df_sm)
    print('Fitting model on Inherited group...')
    model_in, params_in = fit_model_and_inspect(model_spec, df_in)

    # 4) Extract estimates for path log_worth ~ log_gdp
    est_sm, p_sm = get_path(params_sm, 'log_worth', 'log_gdp')
    est_in, p_in = get_path(params_in, 'log_worth', 'log_gdp')

    # 5) Plot MGA comparison bar chart
    bar_out = os.path.join(OUT_DIR, 'mga_comparison.png')
    plot_mga_bar(est_sm if est_sm is not None else 0.0, est_in if est_in is not None else 0.0, bar_out)

    # 6) Plot correlation heatmap for model variables
    vars_for_corr = ['log_worth', 'log_gdp', 'gross_tertiary_education_enrollment', 'total_tax_rate_country', 'age']
    heat_out = os.path.join(OUT_DIR, 'correlation_heatmap.png')
    plot_correlation_heatmap(df, vars_for_corr, heat_out)

    # 7) Fit full sample and compute mediation indirect effect
    print('\nFitting model on Full sample...')
    model_full, params_full = fit_model_and_inspect(model_spec, df)
    a_est, a_p = get_path(params_full, 'log_gdp', 'gross_tertiary_education_enrollment')
    b_est, b_p = get_path(params_full, 'log_worth', 'log_gdp')
    indirect = None
    if a_est is not None and b_est is not None:
        try:
            indirect = float(a_est) * float(b_est)
        except Exception:
            indirect = None

    print('\nBesaran pengaruh tidak langsung Pendidikan ke Kekayaan melalui GDP adalah: {}'.format(
        f"{indirect:.6f}" if indirect is not None else 'NA'))

    # 8) Print summary table rows for Full, Self-Made, Inherited for path log_gdp -> log_worth
    # For Full sample, extract from params_full
    est_full, p_full = get_path(params_full, 'log_worth', 'log_gdp')

    rows = [
        {'group': 'Full Sample', 'estimate': est_full, 'pvalue': p_full},
        {'group': 'Self-Made', 'estimate': est_sm, 'pvalue': p_sm},
        {'group': 'Inherited', 'estimate': est_in, 'pvalue': p_in},
    ]

    print_summary_table(rows)

    print('\nAll outputs saved to: {}'.format(OUT_DIR))


if __name__ == '__main__':
    main()
