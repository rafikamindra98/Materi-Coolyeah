# Kalkulator Sederhana dengan GUI menggunakan Tkinter di Python
# Jalankan ini di VSCode: Pastikan Tkinter terinstal (biasanya sudah ada di Python standar)
# Buka terminal di VSCode dan jalankan: python nama_file.py

import tkinter as tk
from tkinter import messagebox

# Fungsi untuk menghitung hasil
def hitung():
    try:
        num1 = float(entry1.get())
        num2 = float(entry2.get())
        operasi = operator_var.get()

        if operasi == "+":
            hasil = num1 + num2
        elif operasi == "-":
            hasil = num1 - num2
        elif operasi == "*":
            hasil = num1 * num2
        elif operasi == "/":
            if num2 == 0:
                messagebox.showerror("Error", "Pembagian dengan nol tidak diperbolehkan")
                return
            hasil = num1 / num2
        else:
            messagebox.showerror("Error", "Operasi tidak valid")
            return

        label_hasil.config(text=f"Hasil: {hasil}")
    except ValueError:
        messagebox.showerror("Error", "Masukkan angka yang valid")

# Membuat window utama
root = tk.Tk()
root.title("Kalkulator Sederhana")
root.geometry("300x200")

# Label dan Entry untuk angka pertama
tk.Label(root, text="Angka Pertama:").grid(row=0, column=0, padx=10, pady=5)
entry1 = tk.Entry(root)
entry1.grid(row=0, column=1, padx=10, pady=5)

# Label dan Entry untuk angka kedua
tk.Label(root, text="Angka Kedua:").grid(row=1, column=0, padx=10, pady=5)
entry2 = tk.Entry(root)
entry2.grid(row=1, column=1, padx=10, pady=5)

# Pilihan operasi menggunakan OptionMenu
operator_var = tk.StringVar(root)
operator_var.set("+")  # default value
tk.Label(root, text="Operasi:").grid(row=2, column=0, padx=10, pady=5)
operator_menu = tk.OptionMenu(root, operator_var, "+", "-", "*", "/")
operator_menu.grid(row=2, column=1, padx=10, pady=5)

# Tombol Hitung
tk.Button(root, text="Hitung", command=hitung).grid(row=3, column=0, columnspan=2, pady=10)

# Label untuk menampilkan hasil
label_hasil = tk.Label(root, text="Hasil: ")
label_hasil.grid(row=4, column=0, columnspan=2, pady=5)

# Menjalankan aplikasi
root.mainloop()