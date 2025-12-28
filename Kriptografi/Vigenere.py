import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from PIL import Image, ImageTk
import itertools

# ==============================================================================
# BAGIAN 1: LOGIKA KRIPTOGRAFI
# ==============================================================================

# --- Logika Vigenère Cipher ---
def vigenere_cipher(text, key, mode):
    """
    Enkripsi atau dekripsi teks menggunakan Vigenère Cipher.
    :param text: Teks input.
    :param key: Kunci rahasia.
    :param mode: 'encrypt' atau 'decrypt'.
    :return: Teks hasil.
    """
    if not key:
        raise ValueError("Kunci tidak boleh kosong.")
        
    key_cycle = itertools.cycle(key.lower())
    result = ""
    
    for char in text:
        if 'a' <= char <= 'z':
            shift = ord(next(key_cycle)) - ord('a')
            if mode == 'decrypt':
                shift = -shift
            
            encrypted_char_code = (ord(char) - ord('a') + shift) % 26
            result += chr(encrypted_char_code + ord('a'))
        elif 'A' <= char <= 'Z':
            shift = ord(next(key_cycle)) - ord('a')
            if mode == 'decrypt':
                shift = -shift
            
            encrypted_char_code = (ord(char) - ord('A') + shift) % 26
            result += chr(encrypted_char_code + ord('A'))
        else:
            # Karakter selain huruf tidak diubah
            result += char
            
    return result

# --- Logika Steganografi LSB ---
DELIMITER = "||END||" # Penanda akhir pesan

def message_to_binary(message):
    """Mengubah pesan string menjadi string biner."""
    return ''.join(format(ord(char), '08b') for char in message)

def hide_message_in_image(image_path, secret_message):
    """Menyembunyikan pesan rahasia di dalam gambar menggunakan LSB."""
    try:
        img = Image.open(image_path).convert("RGB")
    except FileNotFoundError:
        raise FileNotFoundError("File gambar tidak ditemukan.")
    
    full_message = secret_message + DELIMITER
    binary_message = message_to_binary(full_message)
    
    if len(binary_message) > img.width * img.height * 3:
        raise ValueError("Pesan terlalu panjang untuk disembunyikan di gambar ini.")
    
    data_index = 0
    img_data = list(img.getdata())
    new_img_data = []

    for pixel in img_data:
        if data_index < len(binary_message):
            new_pixel = []
            # Ubah LSB dari setiap channel warna (R, G, B)
            for i in range(3): # R, G, B
                if data_index < len(binary_message):
                    # Ubah bit terakhir dari channel warna
                    new_val = list(format(pixel[i], '08b'))
                    new_val[-1] = binary_message[data_index]
                    new_pixel.append(int("".join(new_val), 2))
                    data_index += 1
                else:
                    new_pixel.append(pixel[i])
            new_img_data.append(tuple(new_pixel))
        else:
            new_img_data.append(pixel)
            
    new_img = Image.new(img.mode, img.size)
    new_img.putdata(new_img_data)
    return new_img

def extract_message_from_image(image_path):
    """Mengekstrak pesan rahasia dari gambar LSB."""
    try:
        img = Image.open(image_path).convert("RGB")
    except FileNotFoundError:
        raise FileNotFoundError("File gambar tidak ditemukan.")

    binary_data = ""
    img_data = img.getdata()

    for pixel in img_data:
        for val in pixel:
            binary_data += format(val, '08b')[-1] # Ambil LSB
    
    # Konversi data biner kembali ke string
    all_bytes = [binary_data[i: i+8] for i in range(0, len(binary_data), 8)]
    decoded_message = ""
    for byte in all_bytes:
        if len(byte) == 8:
            decoded_message += chr(int(byte, 2))
    
    if DELIMITER in decoded_message:
        return decoded_message.split(DELIMITER)[0]
    else:
        return None # Tidak ada pesan tersembunyi yang ditemukan

# ==============================================================================
# BAGIAN 2: GUI (Graphical User Interface)
# ==============================================================================

class CryptoApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Aplikasi Kriptografi & Steganografi")
        self.root.geometry("650x500")

        self.image_path = None

        # --- Buat Tab Kontrol ---
        tab_control = ttk.Notebook(root)
        
        # Tab 1: Vigenère Cipher
        tab_vigenere = ttk.Frame(tab_control)
        tab_control.add(tab_vigenere, text='Vigenère Cipher')
        self.setup_vigenere_tab(tab_vigenere)

        # Tab 2: Steganografi LSB
        tab_steganography = ttk.Frame(tab_control)
        tab_control.add(tab_steganography, text='Steganografi LSB')
        self.setup_steganography_tab(tab_steganography)
        
        tab_control.pack(expand=1, fill='both', padx=10, pady=10)

    def setup_vigenere_tab(self, tab):
        # Frame untuk Vigenère
        vigenere_frame = ttk.LabelFrame(tab, text="Enkripsi / Dekripsi Teks")
        vigenere_frame.pack(padx=10, pady=10, fill="both", expand=True)

        # Input Teks
        ttk.Label(vigenere_frame, text="Teks:").grid(row=0, column=0, padx=5, pady=5, sticky="w")
        self.vigenere_text = tk.Text(vigenere_frame, height=10, width=60)
        self.vigenere_text.grid(row=1, column=0, columnspan=2, padx=5, pady=5)

        # Input Kunci
        ttk.Label(vigenere_frame, text="Kunci:").grid(row=2, column=0, padx=5, pady=5, sticky="w")
        self.vigenere_key = ttk.Entry(vigenere_frame, width=50)
        self.vigenere_key.grid(row=2, column=1, padx=5, pady=5, sticky="ew")

        # Tombol
        button_frame = ttk.Frame(vigenere_frame)
        button_frame.grid(row=3, column=0, columnspan=2, pady=10)
        ttk.Button(button_frame, text="Enkripsi", command=self.encrypt_vigenere).pack(side="left", padx=10)
        ttk.Button(button_frame, text="Dekripsi", command=self.decrypt_vigenere).pack(side="left", padx=10)

    def setup_steganography_tab(self, tab):
        # Frame untuk Steganografi
        stego_frame = ttk.LabelFrame(tab, text="Sembunyikan / Ekstrak Pesan dari Gambar")
        stego_frame.pack(padx=10, pady=10, fill="both", expand=True)
        
        # Tombol Pilih Gambar
        self.image_label = ttk.Label(stego_frame, text="Pilih gambar terlebih dahulu...")
        self.image_label.pack(pady=5)
        ttk.Button(stego_frame, text="Pilih Gambar...", command=self.select_image).pack(pady=5)
        
        # Input Pesan Rahasia
        ttk.Label(stego_frame, text="Pesan Rahasia:").pack(pady=(10, 0))
        self.stego_text = tk.Text(stego_frame, height=8, width=60)
        self.stego_text.pack(pady=5, padx=10)

        # Tombol Aksi
        button_frame = ttk.Frame(stego_frame)
        button_frame.pack(pady=10)
        ttk.Button(button_frame, text="Sembunyikan Pesan", command=self.encode_image).pack(side="left", padx=10)
        ttk.Button(button_frame, text="Ekstrak Pesan", command=self.decode_image).pack(side="left", padx=10)

    # --- Fungsi-fungsi untuk Vigenère Tab ---
    def encrypt_vigenere(self):
        try:
            plain_text = self.vigenere_text.get("1.0", "end-1c")
            key = self.vigenere_key.get()
            encrypted_text = vigenere_cipher(plain_text, key, 'encrypt')
            self.vigenere_text.delete("1.0", "end")
            self.vigenere_text.insert("1.0", encrypted_text)
        except ValueError as e:
            messagebox.showerror("Error", str(e))

    def decrypt_vigenere(self):
        try:
            cipher_text = self.vigenere_text.get("1.0", "end-1c")
            key = self.vigenere_key.get()
            decrypted_text = vigenere_cipher(cipher_text, key, 'decrypt')
            self.vigenere_text.delete("1.0", "end")
            self.vigenere_text.insert("1.0", decrypted_text)
        except ValueError as e:
            messagebox.showerror("Error", str(e))
            
    # --- Fungsi-fungsi untuk Steganografi Tab ---
    def select_image(self):
        path = filedialog.askopenfilename(
            filetypes=[("Image files", "*.png;*.jpg;*.jpeg;*.bmp"), ("All files", "*.*")]
        )
        if path:
            self.image_path = path
            self.image_label.config(text=f"File: {path.split('/')[-1]}")

    def encode_image(self):
        if not self.image_path:
            messagebox.showwarning("Peringatan", "Silakan pilih gambar terlebih dahulu.")
            return
        
        secret_message = self.stego_text.get("1.0", "end-1c")
        if not secret_message:
            messagebox.showwarning("Peringatan", "Masukkan pesan rahasia yang ingin disembunyikan.")
            return

        try:
            new_image = hide_message_in_image(self.image_path, secret_message)
            save_path = filedialog.asksaveasfilename(
                defaultextension=".png",
                filetypes=[("PNG Image", "*.png")]
            )
            if save_path:
                new_image.save(save_path)
                messagebox.showinfo("Sukses", f"Pesan berhasil disembunyikan dan gambar disimpan di:\n{save_path}")
        except Exception as e:
            messagebox.showerror("Error", f"Terjadi kesalahan: {e}")

    def decode_image(self):
        if not self.image_path:
            messagebox.showwarning("Peringatan", "Silakan pilih gambar terlebih dahulu.")
            return
        
        try:
            extracted_message = extract_message_from_image(self.image_path)
            if extracted_message:
                self.stego_text.delete("1.0", "end")
                self.stego_text.insert("1.0", extracted_message)
                messagebox.showinfo("Sukses", "Pesan berhasil diekstrak dari gambar.")
            else:
                messagebox.showinfo("Informasi", "Tidak ditemukan pesan tersembunyi di dalam gambar ini.")
        except Exception as e:
            messagebox.showerror("Error", f"Terjadi kesalahan: {e}")


if __name__ == "__main__":
    root = tk.Tk()
    app = CryptoApp(root)
    root.mainloop()