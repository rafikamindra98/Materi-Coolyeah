import yfinance as yf

# 1. Tentukan Ticker (AAPL untuk Apple)
ticker = "AAPL"

# 2. Tentukan Rentang Tanggal
# Catatan: Tanggal 'end' di yfinance bersifat eksklusif (data diambil sampai sehari sebelumnya), 
# jadi gunakan 2026-01-01 untuk mendapatkan data hingga 31 Desember 2025.
start_date = "2020-01-01"
end_date = "2026-01-01"

print(f"Sedang mengunduh data {ticker}...")

# 3. Ambil data dari Yahoo Finance
data = yf.download(ticker, start=start_date, end=end_date)

# 4. Simpan ke format Excel
file_name = "AAPL_Historical_Data_2020_2025.xlsx"
data.to_excel(file_name)

print(f"Selesai! Data telah disimpan dalam file: {file_name}")