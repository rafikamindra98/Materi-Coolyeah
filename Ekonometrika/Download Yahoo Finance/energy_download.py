import yfinance as yf
import pandas as pd
import warnings
warnings.filterwarnings('ignore')

# Daftar ticker sektor Energi (PXD dan HES dihapus karena masalah dengan Yahoo Finance)
tickers = [
    "XOM", "CVX", "COP", "EOG", "SLB", 
    "OXY", "MPC", "PSX", "VLO", "HAL", 
    "DVN", "BKR", "APA"
]

start_date = "2020-01-01"
end_date = "2026-01-01"

print(f"Mengunduh data untuk {len(tickers)} saham sektor Energi...")

file_name = "Energy_Stocks_2020_2025.xlsx"
failed_tickers = []

with pd.ExcelWriter(file_name, engine='openpyxl') as writer:
    for ticker in tickers:
        try:
            print(f"Proses: {ticker}...")
            # Menambahkan threads=False dan progress=False untuk menghindari issues
            df = yf.download(ticker, start=start_date, end=end_date, threads=False, progress=False)
            
            if df is not None and not df.empty:
                df.to_excel(writer, sheet_name=ticker)
                print(f"  ✓ {ticker} berhasil diunduh")
            else:
                print(f"  ✗ Data {ticker} kosong atau tidak tersedia")
                failed_tickers.append(ticker)
        except Exception as e:
            print(f"  ✗ Error pada {ticker}: {str(e)[:50]}")
            failed_tickers.append(ticker)

print(f"\nSelesai! File disimpan: {file_name}")
if failed_tickers:
    print(f"Ticker yang gagal: {', '.join(failed_tickers)}")
    print(f"Catatan: PXD dan HES mungkin sudah delisted atau tidak tersedia di Yahoo Finance")
else:
    print("Semua data berhasil diunduh!")