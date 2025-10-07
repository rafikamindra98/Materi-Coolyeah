import pandas as pd

# Membaca file Excel
data = pd.read_excel('/Users/user/Downloads/Magang/VS Code (0,1,2,3)/Dataset/Dataset.xlsx')

# Pastikan kolom yang diperlukan ada
if 'Pengangkutan' in data.columns and 'Libur' in data.columns:
    # Menghitung korelasi
    correlation = data['Pengangkutan'].corr(data['Libur'])
    print(f"Korelasi antara Pengangkutan dan Libur: {correlation}")

    # Menentukan kesimpulan berdasarkan nilai korelasi
    if correlation > 0.7:
        print("Kesimpulan: Pengangkutan dan Libur memiliki korelasi positif yang signifikan.")
    elif correlation < -0.7:
        print("Kesimpulan: Pengangkutan dan Libur memiliki korelasi negatif yang signifikan.")
    else:
        print("Kesimpulan: Pengangkutan dan Libur tidak memiliki korelasi yang signifikan.")
else:
    print("Kolom 'Pengangkutan' atau 'Libur' tidak ditemukan dalam dataset.")