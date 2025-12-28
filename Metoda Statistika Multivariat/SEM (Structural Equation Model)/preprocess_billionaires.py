"""
Preprocessing script for Billionaires_Statistics_Dataset.xlsx

Steps performed:
1) Load data from Excel
2) Clean numeric column `gdp_country` by removing '$' and ',' then convert to float
3) Create log-transformed columns `log_worth` and `log_gdp`
4) Drop rows with missing values only in SEM-relevant variables
5) Plot and save correlation heatmap for numeric SEM variables
6) Save cleaned dataframe to 'billionaires_sem_ready.xlsx'

Commented for thesis clarity.
"""

import os
import sys
import subprocess
import pandas as pd
import numpy as np

# Path to the source Excel file (dataset chosen by user)
DATA_PATH = r"/Users/user/Downloads/Semester 7/Metoda Statistika Multivariat/SEM (Structural Equation Model)/VSCode 2/Billionaires_Statistics_Dataset.xlsx"


def ensure_packages():
    """Install plotting/writing packages if missing (openpyxl, seaborn, matplotlib)."""
    try:
        import seaborn  # noqa: F401
        import matplotlib  # noqa: F401
        import openpyxl  # noqa: F401
    except Exception:
        print("Installing required packages: seaborn, matplotlib, openpyxl...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", "seaborn", "matplotlib", "openpyxl"])


def clean_gdp_column(df, col='gdp_country'):
    """Remove currency symbols and thousands separators, convert to float.

    Expects strings like '$1,234,567'. Non-numeric entries become NaN.
    """
    if col not in df.columns:
        raise KeyError(f"Column '{col}' not found in the dataset")

    # Convert to string, strip whitespace
    s = df[col].astype(str).str.strip()

    # Remove dollar sign and commas
    s = s.str.replace('\$', '', regex=True).str.replace(',', '', regex=True)

    # Coerce to numeric, invalid parsing -> NaN
    df[col] = pd.to_numeric(s, errors='coerce')
    return df


def add_log_columns(df):
    """Add log-transformed columns for finalWorth and gdp_country.

    We guard against non-positive values by setting them to NaN before log.
    """
    # finalWorth -> log_worth
    if 'finalWorth' not in df.columns:
        raise KeyError("Column 'finalWorth' not found in dataset")

    # Convert finalWorth to numeric just in case
    df['finalWorth'] = pd.to_numeric(df['finalWorth'], errors='coerce')

    # Replace non-positive with NaN to avoid log errors
    df.loc[df['finalWorth'] <= 0, 'finalWorth'] = np.nan
    df['log_worth'] = np.log(df['finalWorth'])

    # gdp_country -> log_gdp (assumes gdp_country already numeric)
    if 'gdp_country' not in df.columns:
        raise KeyError("Column 'gdp_country' not found in dataset")
    df.loc[df['gdp_country'] <= 0, 'gdp_country'] = np.nan
    df['log_gdp'] = np.log(df['gdp_country'])

    return df


def plot_heatmap(df, numeric_vars, out_dir):
    """Plot correlation heatmap for provided numeric variables and save PNG and CSV."""
    ensure_packages()
    import seaborn as sns
    import matplotlib.pyplot as plt

    num_df = df[numeric_vars].apply(lambda col: pd.to_numeric(col, errors='coerce'))
    corr = num_df.corr(method='pearson')

    plt.figure(figsize=(8, 6))
    sns.set(style='white')
    mask = np.triu(np.ones_like(corr, dtype=bool))
    sns.heatmap(
        corr,
        mask=mask,
        cmap='coolwarm',
        vmin=-1,
        vmax=1,
        center=0,
        annot=True,
        fmt='.2f',
        linewidths=.5,
        cbar_kws={"shrink": .75},
    )
    plt.title('Correlation Heatmap (SEM numeric variables)')
    plt.tight_layout()

    png_path = os.path.join(out_dir, 'billionaires_correlation_heatmap.png')
    csv_path = os.path.join(out_dir, 'billionaires_correlation_matrix.csv')
    plt.savefig(png_path, dpi=200)
    corr.to_csv(csv_path)
    print(f"Saved heatmap to: {png_path}")
    print(f"Saved correlation matrix to: {csv_path}")
    plt.close()


def main():
    # 1) Load Data
    if not os.path.exists(DATA_PATH):
        raise FileNotFoundError(f"Data file not found: {DATA_PATH}")

    print("Loading data from:", DATA_PATH)
    df = pd.read_excel(DATA_PATH)

    # 2) Clean `gdp_country` (remove '$' and ',') and convert to numeric
    print("Cleaning 'gdp_country' column (remove $ and commas, convert to numeric)...")
    df = clean_gdp_column(df, col='gdp_country')

    # 3) Create log-transformed columns 'log_worth' and 'log_gdp'
    print("Creating log-transformed columns 'log_worth' and 'log_gdp'...")
    df = add_log_columns(df)

    # 4) Handle missing values for SEM variables: drop rows with NA in the listed columns
    sem_vars = [
        'log_worth',
        'log_gdp',
        'gross_tertiary_education_enrollment',
        'total_tax_rate_country',
        'age',
        'gender',
        'selfMade',
    ]

    missing_cols = [c for c in sem_vars if c not in df.columns]
    if missing_cols:
        raise KeyError(f"These SEM variables are missing from the dataset: {missing_cols}")

    # Report missing counts before dropping
    print("Missing counts for SEM-relevant variables before drop:")
    print(df[sem_vars].isna().sum())

    # Drop rows that have NA in any of the SEM variables (subset)
    df_clean = df.dropna(subset=sem_vars).copy()

    # 5) Validation: correlation heatmap for numeric SEM vars and print final info
    numeric_for_heatmap = [
        'log_worth',
        'log_gdp',
        'gross_tertiary_education_enrollment',
        'total_tax_rate_country',
        'age',
    ]

    out_dir = os.path.dirname(DATA_PATH)
    print("Plotting correlation heatmap for numeric SEM variables...")
    plot_heatmap(df_clean, numeric_for_heatmap, out_dir)

    # Print final data info
    print("Final dataset info:")
    print(f"Rows before cleaning: {len(df):,}")
    print(f"Rows after dropping NA in SEM variables: {len(df_clean):,}")

    # 6) Save cleaned dataframe to Excel
    out_path = os.path.join(out_dir, 'billionaires_sem_ready.xlsx')
    try:
        df_clean.to_excel(out_path, index=False)
    except Exception:
        # Try installing openpyxl and retry
        print("Failed to write Excel file: attempting to install openpyxl and retry...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", "openpyxl"])
        df_clean.to_excel(out_path, index=False)

    print(f"Saved cleaned SEM-ready data to: {out_path}")


if __name__ == '__main__':
    main()
