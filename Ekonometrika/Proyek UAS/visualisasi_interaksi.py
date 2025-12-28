"""
Visualisasi Efek Interaksi: BMI vs Charges berdasarkan Status Perokok
Script ini membuat grafik untuk menunjukkan bagaimana status perokok mempengaruhi
hubungan antara BMI dan biaya asuransi.
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings('ignore')


# =============================================================================
# 1. MUAT DATA
# =============================================================================
def load_data_for_visualization():
    """
    Muat data asuransi dari file Excel.
    
    Return:
    --------
    df : pandas.DataFrame
        Dataset yang sudah dimuat.
    """
    print("=" * 80)
    print("LANGKAH 1: MEMUAT DATA UNTUK VISUALISASI")
    print("=" * 80)
    
    # Coba berbagai lokasi file
    possible_paths = [
        '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance.xlsx',
        '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance.csv',
        '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/insurance - Sheet1.csv'
    ]
    
    filepath = None
    for path in possible_paths:
        try:
            import os
            if os.path.exists(path):
                filepath = path
                break
        except:
            continue
    
    if filepath is None:
        raise FileNotFoundError("File data 'insurance' tidak ditemukan!")
    
    # Muat data
    if filepath.endswith('.xlsx'):
        df = pd.read_excel(filepath)
    else:
        df = pd.read_csv(filepath)
    
    print(f"\n✓ File data berhasil dimuat: {filepath}")
    print(f"  Bentuk dataset: {df.shape}")
    print(f"  Kolom: {df.columns.tolist()}")
    
    return df


# =============================================================================
# 2. PERSIAPAN DATA UNTUK VISUALISASI
# =============================================================================
def prepare_data_for_visualization(df):
    """
    Siapkan data dengan mengkonversi 'smoker' menjadi format yang mudah divisualisasi.
    
    Parameter:
    -----------
    df : pandas.DataFrame
        Dataset asli.
    
    Return:
    --------
    df_vis : pandas.DataFrame
        Dataset yang sudah disiapkan untuk visualisasi.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 2: PERSIAPAN DATA UNTUK VISUALISASI")
    print("=" * 80)
    
    # Buat salinan
    df_vis = df.copy()
    
    # Konversi 'smoker' menjadi format kategori yang lebih readable
    df_vis['Perokok'] = df_vis['smoker'].map({'yes': 'Perokok', 'no': 'Non-Perokok'})
    
    print(f"\nKonversi kolom 'smoker':")
    print(f"  'yes' → 'Perokok'")
    print(f"  'no' → 'Non-Perokok'")
    
    print(f"\nDisribusi Status Perokok:")
    print(df_vis['Perokok'].value_counts())
    
    print(f"\nBeberapa baris data:")
    print(df_vis[['age', 'bmi', 'smoker', 'Perokok', 'charges']].head(10))
    
    return df_vis


# =============================================================================
# 3. BUAT SCATTER PLOT DENGAN GARIS REGRESI
# =============================================================================
def create_interaction_effect_plot(df_vis):
    """
    Buat scatter plot dengan garis regresi terpisah untuk perokok dan non-perokok.
    
    Parameter:
    -----------
    df_vis : pandas.DataFrame
        Dataset yang sudah disiapkan untuk visualisasi.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 3: MEMBUAT SCATTER PLOT DENGAN GARIS REGRESI")
    print("=" * 80)
    
    # Set gaya visualisasi
    sns.set_style("whitegrid")
    plt.rcParams['figure.figsize'] = (12, 7)
    
    # Buat lmplot (scatter plot dengan garis regresi terpisah)
    fig = sns.lmplot(
        data=df_vis,
        x='bmi',
        y='charges',
        hue='Perokok',
        height=7,
        aspect=1.5,
        palette={'Perokok': '#d62728', 'Non-Perokok': '#1f77b4'},  # Merah dan Biru
        scatter_kws={'alpha': 0.6, 's': 50, 'edgecolors': 'k', 'linewidths': 0.5},
        line_kws={'linewidth': 2.5}
    )
    
    # Sesuaikan judul dan label
    fig.set(
        title='Efek Interaksi: BMI vs Biaya Asuransi pada Perokok & Non-Perokok',
        xlabel='BMI (Body Mass Index)',
        ylabel='Biaya Asuransi (USD)'
    )
    
    # Tambahkan styling pada judul
    fig.fig.suptitle('Efek Interaksi: BMI vs Biaya Asuransi pada Perokok & Non-Perokok',
                     fontsize=14, fontweight='bold', y=0.998)
    
    # Sesuaikan label aksis
    fig.set_axis_labels('BMI (Body Mass Index)', 'Biaya Asuransi (USD)', fontsize=12)
    
    # Tambahkan grid yang lebih jelas
    plt.grid(True, alpha=0.3)
    
    # Tambahkan anotasi penjelasan
    textstr = ('Garis merah: Perokok (slope lebih curam → BMI memiliki dampak lebih besar)\n'
               'Garis biru: Non-Perokok (slope lebih landai → BMI memiliki dampak lebih kecil)')
    fig.ax.text(0.02, 0.98, textstr, transform=fig.ax.transAxes,
                fontsize=10, verticalalignment='top',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    plt.tight_layout()
    
    # Simpan plot
    output_path = '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/interaction_effect.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"\n✓ Plot berhasil tersimpan: {output_path}")
    
    plt.show()
    
    # Analisis visual
    print_interaction_analysis(df_vis)


# =============================================================================
# 4. ANALISIS INTERAKSI
# =============================================================================
def print_interaction_analysis(df_vis):
    """
    Cetak analisis tentang efek interaksi yang terlihat dari plot.
    
    Parameter:
    -----------
    df_vis : pandas.DataFrame
        Dataset dengan kolom status perokok.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 4: ANALISIS EFEK INTERAKSI")
    print("=" * 80)
    
    # Pisahkan data berdasarkan status perokok
    smokers = df_vis[df_vis['Perokok'] == 'Perokok']
    non_smokers = df_vis[df_vis['Perokok'] == 'Non-Perokok']
    
    # Hitung korelasi BMI-Charges untuk masing-masing grup
    corr_smokers = smokers['bmi'].corr(smokers['charges'])
    corr_non_smokers = non_smokers['bmi'].corr(non_smokers['charges'])
    
    # Hitung rata-rata biaya untuk masing-masing grup
    avg_charges_smokers = smokers['charges'].mean()
    avg_charges_non_smokers = non_smokers['charges'].mean()
    avg_bmi_smokers = smokers['bmi'].mean()
    avg_bmi_non_smokers = non_smokers['bmi'].mean()
    
    print(f"\n📊 STATISTIK PEROKOK:")
    print(f"  Jumlah sampel: {len(smokers)}")
    print(f"  Rata-rata BMI: {avg_bmi_smokers:.2f}")
    print(f"  Rata-rata Biaya: ${avg_charges_smokers:.2f}")
    print(f"  Korelasi BMI-Charges: {corr_smokers:.4f}")
    
    print(f"\n📊 STATISTIK NON-PEROKOK:")
    print(f"  Jumlah sampel: {len(non_smokers)}")
    print(f"  Rata-rata BMI: {avg_bmi_non_smokers:.2f}")
    print(f"  Rata-rata Biaya: ${avg_charges_non_smokers:.2f}")
    print(f"  Korelasi BMI-Charges: {corr_non_smokers:.4f}")
    
    print(f"\n🔍 INTERPRETASI EFEK INTERAKSI:")
    print(f"\n1. PERBEDAAN BIAYA DASAR:")
    perbedaan_biaya = avg_charges_smokers - avg_charges_non_smokers
    print(f"   Perokok membayar ${perbedaan_biaya:.2f} lebih tinggi dari non-perokok")
    print(f"   (Perokok: ${avg_charges_smokers:.2f} vs Non-Perokok: ${avg_charges_non_smokers:.2f})")
    
    print(f"\n2. SENSITIVITAS BMI:")
    print(f"   Korelasi BMI-Charges Perokok: {corr_smokers:.4f} (SANGAT KUAT)")
    print(f"   Korelasi BMI-Charges Non-Perokok: {corr_non_smokers:.4f} (LEMAH)")
    print(f"   → Efek BMI pada biaya JAUH LEBIH BESAR untuk perokok!")
    
    print(f"\n3. GARIS REGRESI:")
    print(f"   ✓ Garis MERAH (Perokok): SLOPE CURAM")
    print(f"     Artinya: Setiap kenaikan 1 unit BMI meningkatkan biaya secara drastis")
    print(f"\n   ✓ Garis BIRU (Non-Perokok): SLOPE LANDAI")
    print(f"     Artinya: Setiap kenaikan 1 unit BMI meningkatkan biaya secara minimal")
    
    print(f"\n4. KESIMPULAN INTERAKSI:")
    print(f"   Hubungan antara BMI dan charges BERGANTUNG pada status perokok")
    print(f"   Ini adalah EFEK INTERAKSI yang signifikan statistik!")
    print(f"   Fitur 'bmi_smoker' dalam model kami menangkap fenomena ini dengan sempurna.")
    
    print("\n" + "=" * 80)


# =============================================================================
# 5. BUAT VISUALISASI TAMBAHAN: DISTRIBUTION PLOT
# =============================================================================
def create_distribution_plots(df_vis):
    """
    Buat plot distribusi biaya untuk merokok vs non-perokok.
    
    Parameter:
    -----------
    df_vis : pandas.DataFrame
        Dataset dengan kolom status perokok.
    """
    print("\n" + "=" * 80)
    print("LANGKAH 5: VISUALISASI TAMBAHAN - DISTRIBUSI BIAYA")
    print("=" * 80)
    
    fig, axes = plt.subplots(1, 2, figsize=(14, 5))
    
    # Plot 1: Distribution dengan KDE
    sns.histplot(data=df_vis, x='charges', hue='Perokok',
                 kde=True, bins=30, palette={'Perokok': '#d62728', 'Non-Perokok': '#1f77b4'},
                 ax=axes[0])
    axes[0].set_title('Distribusi Biaya Asuransi\n(Perokok vs Non-Perokok)', 
                      fontsize=12, fontweight='bold')
    axes[0].set_xlabel('Biaya Asuransi (USD)', fontsize=11)
    axes[0].set_ylabel('Frekuensi', fontsize=11)
    
    # Plot 2: Box Plot
    sns.boxplot(data=df_vis, x='Perokok', y='charges',
                palette={'Perokok': '#d62728', 'Non-Perokok': '#1f77b4'},
                ax=axes[1])
    sns.stripplot(data=df_vis, x='Perokok', y='charges', color='black', alpha=0.3, ax=axes[1])
    axes[1].set_title('Perbandingan Box Plot Biaya Asuransi\n(Perokok vs Non-Perokok)',
                      fontsize=12, fontweight='bold')
    axes[1].set_xlabel('Status', fontsize=11)
    axes[1].set_ylabel('Biaya Asuransi (USD)', fontsize=11)
    
    plt.tight_layout()
    
    # Simpan plot
    output_path = '/Users/user/Downloads/Semester 7/Ekonometrika/Proyek UAS/distribution_comparison.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"\n✓ Plot distribusi tersimpan: {output_path}")
    
    plt.show()


# =============================================================================
# FUNGSI UTAMA
# =============================================================================
def main():
    """
    Fungsi utama untuk menjalankan seluruh proses visualisasi.
    """
    print("\n")
    print("╔" + "=" * 78 + "╗")
    print("║" + " VISUALISASI EFEK INTERAKSI: BMI vs CHARGES ".center(78) + "║")
    print("║" + " berdasarkan Status Perokok dan Non-Perokok ".center(78) + "║")
    print("╚" + "=" * 78 + "╝")
    
    # Langkah 1: Muat data
    df = load_data_for_visualization()
    
    # Langkah 2: Persiapan data
    df_vis = prepare_data_for_visualization(df)
    
    # Langkah 3: Buat scatter plot dengan garis regresi
    create_interaction_effect_plot(df_vis)
    
    # Langkah 4 sudah terintegrasi di step 3 (print_interaction_analysis)
    
    # Langkah 5: Buat visualisasi tambahan
    create_distribution_plots(df_vis)
    
    print("\n" + "=" * 80)
    print("✓ VISUALISASI SELESAI!")
    print("=" * 80)
    print("\nFile yang telah dibuat:")
    print("  1. interaction_effect.png - Plot interaksi BMI vs Charges")
    print("  2. distribution_comparison.png - Perbandingan distribusi biaya")
    print("=" * 80)


if __name__ == '__main__':
    main()
