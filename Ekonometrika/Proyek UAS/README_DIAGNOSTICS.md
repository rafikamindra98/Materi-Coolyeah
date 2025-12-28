Petunjuk menjalankan skrip diagnostik untuk insurance.xlsx

1) Pastikan file `insurance.xlsx` berada di folder proyek:
   - /Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance.xlsx

2) (Opsional) Aktifkan environment yang sudah dikonfigurasi:
   - Jika Anda menggunakan venv yang dikonfigurasi oleh tool, jalankan:

```bash
/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/.venv/bin/python diagnostics_insurance.py
```

3) Jika tidak menggunakan venv, install dependensi lalu jalankan:

```bash
python -m pip install pandas numpy statsmodels scipy matplotlib seaborn openpyxl patsy
python diagnostics_insurance.py
```

4) Output akan disimpan di folder proyek:
- regression_summary.txt
- normality_tests.txt
- heteroskedasticity_tests.txt
- vif.csv
- residuals_hist.png
- residuals_qq.png
- vif_bar.png
- residuals_vs_fitted.png

Interpretasi singkat:
- Normalitas: lihat `normality_tests.txt` (Shapiro dan Jarque-Bera) dan QQ plot.
- Multikolinearitas: lihat `vif.csv` (VIF > 5 atau >10 dapat menandakan masalah).
- Heteroskedastisitas: lihat `heteroskedasticity_tests.txt` (p-value Breusch-Pagan).
- Autokorelasi: lihat `autocorrelation_tests.txt` (Durbin–Watson dan Ljung–Box) dan `residuals_acf.png`.

Jika Anda mau, saya bisa menjalankan skrip ini untuk Anda (jika environment mengizinkan), atau membantu membaca/menafsirkan hasil setelah Anda menjalankannya.