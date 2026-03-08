import yfinance as yf
import pandas as pd

# Daftar ticker saham yang diminta
tickers = [
    "XOM", "CVX", "COP", "EOG", "SLB", 
    "OXY", "MPC", "PSX", "VLO", "HAL", 
    "DVN", "PXD", "BKR", "APA", "HES"
]

start_date = "2020-01-01"
end_date = "2026-01-01"  # Data sampai 31 Des 2025

print(f"Memulai proses unduhan untuk {len(tickers)} saham...")

# Gunakan ExcelWriter untuk menyimpan banyak sheet dalam satu file
file_name = "Energy_Historical_Data_2020_2025.xlsx"

with pd.ExcelWriter(file_name) as writer:
    for ticker in tickers:
        try:
            print(f"Mengambil data: {ticker}...")
            # Ambil data
            df = yf.download(ticker, start=start_date, end=end_date)
            
            # Simpan ke sheet yang berbeda berdasarkan nama ticker
            df.to_excel(writer, sheet_name=ticker)
        except Exception as e:
            print(f"Gagal mengunduh {ticker}: {e}")

print(f"\nSelesai! Semua data telah disimpan di: {file_name}")