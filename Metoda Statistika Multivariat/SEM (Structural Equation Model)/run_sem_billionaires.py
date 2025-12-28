"""
SEM analysis for Billionaires dataset using semopy.

This script fits a path model where `log_gdp` is a mediator between
education/tax variables and `log_worth`. It runs the model on the full
sample and separately for Self-Made vs Inherited billionaires.

Comments are provided for thesis documentation.
"""

import os
import sys
import subprocess
import pandas as pd
import numpy as np

# Data path (cleaned and ready)
DATA_PATH = r"/Users/user/Downloads/Semester 7/Metoda Statistika Multivariat/SEM (Structural Equation Model)/VSCode 2/billionaires_sem_ready.xlsx"


def ensure_semopy():
    """Ensure semopy is installed; install if missing."""
    try:
        import semopy  # noqa: F401
    except Exception:
        print("semopy not found — installing via pip...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", "semopy"])


def normalize_selfmade(series: pd.Series) -> pd.Series:
    """Normalize `selfMade` column to two categories: 'Self-Made' or 'Inherited'.

    Accepts booleans or strings like 'Yes', 'No', 'Self', 'Inherited'.
    """
    s = series.copy()
    # If boolean dtype, map True->Self-Made, False->Inherited
    if pd.api.types.is_bool_dtype(s) or s.dropna().isin([True, False]).all():
        return s.map({True: 'Self-Made', False: 'Inherited'})

    # Otherwise, map using string matching
    s = s.astype(str).str.strip().str.lower()
    def map_val(v):
        if pd.isna(v) or v == 'nan':
            return np.nan
        if 'self' in v or v in ('yes', 'y', 'true', '1'):
            return 'Self-Made'
        return 'Inherited'

    return s.map(map_val)


def safe_inspect(model):
    """Return normalized parameter table (lhs, op, rhs, Estimate, SE, p-value).

    semopy versions differ; try to pick commonly available names.
    """
    df = model.inspect()
    # Lowercase columns
    df = df.rename(columns={c: c.lower() for c in df.columns})

    # heuristics for names
    col_map = {}
    for c in ('lval', 'lhs', 'left'):
        if c in df.columns:
            col_map['lhs'] = c
            break
    for c in ('rval', 'rhs', 'right'):
        if c in df.columns:
            col_map['rhs'] = c
            break
    for c in ('op', 'operator'):
        if c in df.columns:
            col_map['op'] = c
            break
    for c in ('estimate', 'est', 'value'):
        if c in df.columns:
            col_map['est'] = c
            break
    for c in ('se', 'std.err', 'std_error'):
        if c in df.columns:
            col_map['se'] = c
            break
    for c in ('p-value', 'p_value', 'pvalue', 'p'):
        if c in df.columns:
            col_map['p'] = c
            break

    out = pd.DataFrame()
    if 'lhs' in col_map and 'rhs' in col_map and 'est' in col_map:
        out['lhs'] = df[col_map['lhs']]
        out['op'] = df[col_map['op']] if 'op' in col_map else '~'
        out['rhs'] = df[col_map['rhs']]
        out['Estimate'] = df[col_map['est']]
        if 'se' in col_map:
            out['SE'] = df[col_map['se']]
        if 'p' in col_map:
            out['p-value'] = df[col_map['p']]
        return out
    return df


def extract_fit_stats(model, data=None):
    """Compute common fit indices and return dict with scalars.

    Returns keys: 'CFI', 'RMSEA', 'TLI', 'chi2'
    """
    from semopy import calc_stats

    try:
        stats = calc_stats(model)
    except Exception:
        stats = calc_stats(model, data)

    # helper to get scalar
    def get_scalar(d, keys):
        for k in keys:
            if k in d:
                val = d[k]
                # if Series/array, take first element
                try:
                    if hasattr(val, 'size') and np.asarray(val).size >= 1:
                        return float(np.asarray(val).flat[0])
                except Exception:
                    pass
                try:
                    return float(val)
                except Exception:
                    return None
        return None

    out = {}
    out['CFI'] = get_scalar(stats, ['CFI', 'cfi'])
    out['RMSEA'] = get_scalar(stats, ['RMSEA', 'rmsea'])
    out['TLI'] = get_scalar(stats, ['TLI', 'tli'])
    out['chi2'] = get_scalar(stats, ['chi2', 'chisq', 'chi-square', 'chi2_value'])
    return out


def print_fit_summary(model, data=None):
    """Print fit indices and parameter table for a fitted semopy Model."""
    stats = extract_fit_stats(model, data)
    print("Fit indices:")
    for k in ('CFI', 'RMSEA', 'TLI', 'chi2'):
        print(f"- {k}: {stats.get(k)}")

    params = safe_inspect(model)
    print("\nParameter estimates:")
    print(params)
    return stats, params


def main():
    # 1) Setup
    ensure_semopy()
    from semopy import Model

    if not os.path.exists(DATA_PATH):
        raise FileNotFoundError(f"Data file not found: {DATA_PATH}")

    # Load data
    df = pd.read_excel(DATA_PATH)

    # Ensure selfMade column is present and normalized
    if 'selfMade' not in df.columns:
        raise KeyError("Column 'selfMade' not found in dataset")
    df['selfMade_standard'] = normalize_selfmade(df['selfMade'])

    # Define SEM path model (semopy syntax)
    model_desc = """
    # Mediator equation: GDP affected by education and tax
    log_gdp ~ gross_tertiary_education_enrollment + total_tax_rate_country

    # Outcome equation: wealth affected by GDP and age (control)
    log_worth ~ log_gdp + age
    """

    # Fit model on full data
    print("\nFitting model on full sample...")
    model_full = Model(model_desc)
    model_full.fit(df)

    stats_full, params_full = print_fit_summary(model_full, df)

    # Check mediation path significance: a = education->log_gdp, b = log_gdp->log_worth
    def find_path_est(params_df, lhs, rhs):
        mask = (params_df['lhs'] == lhs) & (params_df['rhs'] == rhs)
        if mask.any():
            row = params_df.loc[mask].iloc[0]
            est = row.get('Estimate', None)
            p = row.get('p-value', None)
            return est, p
        return None, None

    a_est, a_p = find_path_est(params_full, 'log_gdp', 'gross_tertiary_education_enrollment')
    # note: inspect lhs/rhs orientation may differ; try reversed if None
    if a_est is None:
        a_est, a_p = find_path_est(params_full, 'log_gdp', 'gross_tertiary_education_enrollment')

    # find b: log_worth ~ log_gdp
    b_est, b_p = find_path_est(params_full, 'log_worth', 'log_gdp')

    print('\nMediation path checks:')
    print(f"- Education -> log_gdp: est={a_est}, p={a_p}")
    print(f"- log_gdp -> log_worth: est={b_est}, p={b_p}")

    if a_p is not None and b_p is not None:
        try:
            a_sig = float(a_p) < 0.05
            b_sig = float(b_p) < 0.05
            if a_sig and b_sig:
                print("Mediation plausible: both a and b paths are significant (p<0.05)")
            else:
                print("Mediation not strongly supported: one or both paths not significant at p<0.05")
        except Exception:
            print("Could not interpret p-values for mediation check")
    else:
        print("Mediation p-values not available in parameter table")

    # 4) Multi-group analysis: Self-Made vs Inherited
    print('\nRunning multi-group analysis (Self-Made vs Inherited)...')
    groups = {
        'Self-Made': df[df['selfMade_standard'] == 'Self-Made'],
        'Inherited': df[df['selfMade_standard'] == 'Inherited'],
    }

    group_results = {}
    for name, gdf in groups.items():
        if gdf.shape[0] < 20:
            print(f"Skipping group {name}: too few observations ({gdf.shape[0]})")
            continue
        print(f"\nFitting model for group: {name} (n={gdf.shape[0]})")
        m = Model(model_desc)
        m.fit(gdf)
        stats_g, params_g = print_fit_summary(m, gdf)
        group_results[name] = {'model': m, 'stats': stats_g, 'params': params_g}

    # Compare key path estimates side-by-side if both groups fitted
    if 'Self-Made' in group_results and 'Inherited' in group_results:
        print('\nComparing key path estimates between groups:')
        gm = group_results['Self-Made']['params']
        gi = group_results['Inherited']['params']

        # focus on two paths: education -> log_gdp and log_gdp -> log_worth
        def get_est(df_params, lhs, rhs):
            mask = (df_params['lhs'] == lhs) & (df_params['rhs'] == rhs)
            if mask.any():
                row = df_params.loc[mask].iloc[0]
                return row.get('Estimate'), row.get('p-value')
            return (None, None)

        a_sm = get_est(gm, 'log_gdp', 'gross_tertiary_education_enrollment')
        b_sm = get_est(gm, 'log_worth', 'log_gdp')
        a_in = get_est(gi, 'log_gdp', 'gross_tertiary_education_enrollment')
        b_in = get_est(gi, 'log_worth', 'log_gdp')

        comp = pd.DataFrame([
            {'path': 'education -> log_gdp', 'Self-Made_est': a_sm[0], 'Self-Made_p': a_sm[1], 'Inherited_est': a_in[0], 'Inherited_p': a_in[1]},
            {'path': 'log_gdp -> log_worth', 'Self-Made_est': b_sm[0], 'Self-Made_p': b_sm[1], 'Inherited_est': b_in[0], 'Inherited_p': b_in[1]},
        ])
        print(comp)

    # 5) Optional: plot model diagram if semopy provides semplot
    try:
        from semopy import semplot
        png = os.path.join(os.path.dirname(DATA_PATH), 'sem_model_diagram.png')
        semplot(model_full, png)
        print(f"Saved SEM diagram to {png}")
    except Exception:
        print("semopy semplot not available or failed — skipping diagram generation")

    print('\nSEM analysis completed.')


if __name__ == '__main__':
    main()
