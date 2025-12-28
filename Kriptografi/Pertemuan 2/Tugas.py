import tkinter as tk
# Membuat Window Utama
root = tk.Tk()
root.title('Shift Cipher')
canvas1 = tk.Canvas(root, width=600, height=500)
canvas1.pack()
# Label dan Entry untuk Teks
label1 = tk.Label(root, text='Teks:')
canvas1.create_window(100, 50, window=label1)
entry1 = tk.Entry(root)
canvas1.create_window(200, 50, window=entry1)
# Label dan Entry untuk Key
label2 = tk.Label(root, text='Key:')
canvas1.create_window(100, 80, window=label2)
entry2 = tk.Entry(root)
canvas1.create_window(200, 80, window=entry2)
# Label untuk Hasil di samping masing-masing tombol
label_hasil_enkrip = tk.Label(root, text=' ')
canvas1.create_window(300, 150, window=label_hasil_enkrip)
label_hasil_deskrip = tk.Label(root, text=' ')
canvas1.create_window(300, 200, window=label_hasil_deskrip)
# Text Widget untuk Kriptanalisis
text_hasil_kriptanalisis = tk.Text(root, height=10, width=50, wrap='word')
text_hasil_kriptanalisis.place(x=100, y=280)
# Fungsi Enkripsi
def enkrip():
    plainA = entry1.get().lower()
    try:
        key = int(entry2.get())
    except ValueError:
        label_hasil_enkrip.config(text='Masukkan key')
        return
    
    chiperA = ''
    for i in range(len(plainA)):
        if plainA[i].isalpha():
            chiperA += chr((ord(plainA[i]) + key - 97) % 26 + 97)
        else:
            chiperA += plainA[i] # Biarkan karakter non-huruf tetap sama
    
    label_hasil_enkrip.config(text=f'Chiperteks: {chiperA}')
# Fungsi Dekripsi
def deskrip():
    chiperA = entry1.get().lower()
    try:
        key = int(entry2.get())
    except ValueError:
        label_hasil_deskrip.config(text='Masukkan Key')
        return
    
    plainA = ''
    for i in range(len(chiperA)):
        if chiperA[i].isalpha():
            plainA += chr((ord(chiperA[i]) - key - 97) % 26 + 97)
        else:
            plainA += chiperA[i]
    
    label_hasil_deskrip.config(text=f'Hasil: {plainA}')
# Fungsi Kriptanalisis
def kriptanalis():
    chiperA = entry1.get().lower()
    text_hasil_kriptanalisis.config(state='normal')  # Aktifkan Text Widget
    text_hasil_kriptanalisis.delete(1.0, tk.END)  # Hapus teks sebelumnya
    hasil = "Kriptanalisis:\n"
    
    for key in range(1, 26):
        plainA = ''
        for i in range(len(chiperA)):
            if chiperA[i].isalpha():
                plainA += chr((ord(chiperA[i]) - key - 97) % 26 + 97)
            else:
                plainA += chiperA[i]
        hasil += f'Key {key}: {plainA}\n'
    
    text_hasil_kriptanalisis.insert(tk.END, hasil)
    text_hasil_kriptanalisis.config(state='disabled')  # Nonaktifkan Text Widget # Nonaktifkan Text Widget
# Tombol Enkripsi
button1 = tk.Button(text='Enkripsi', command=enkrip)
canvas1.create_window(150, 150, window=button1)
# Tombol Dekripsi
button2 = tk.Button(text='Deskripsi', command=deskrip)
canvas1.create_window(150, 200, window=button2)

# Tombol Kriptanalisis
button3 = tk.Button(text='Kriptanalis', command=kriptanalis)
canvas1.create_window(150, 250, window=button3)
root.mainloop()