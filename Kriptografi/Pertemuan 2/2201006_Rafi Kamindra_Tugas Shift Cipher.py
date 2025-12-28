import tkinter as tk
from tkinter import messagebox, simpledialog, scrolledtext

class ShiftCipherGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Shift Cipher")
        self.root.geometry("450x350")

        # Label dan Entry untuk Teks
        tk.Label(root, text="Teks:", font=("Arial", 12)).grid(row=0, column=0, padx=10, pady=10, sticky="w")
        self.text_entry = tk.Entry(root, width=40, font=("Arial", 12))
        self.text_entry.grid(row=0, column=1, padx=10, pady=10)

        # Label dan Entry untuk Kunci
        tk.Label(root, text="Key:", font=("Arial", 12)).grid(row=1, column=0, padx=10, pady=10, sticky="w")
        self.key_entry = tk.Entry(root, width=10, font=("Arial", 12))
        self.key_entry.grid(row=1, column=1, padx=10, pady=10, sticky="w")

        # Frame untuk Tombol
        button_frame = tk.Frame(root)
        button_frame.grid(row=2, column=0, columnspan=2, pady=10)

        # Tombol Enkripsi, Dekripsi, Kriptanalisis
        self.encrypt_button = tk.Button(button_frame, text="Enkripsi", command=self.encrypt_message, font=("Arial", 12))
        self.encrypt_button.pack(side=tk.LEFT, padx=5)

        self.decrypt_button = tk.Button(button_frame, text="Dekripsi", command=self.decrypt_message, font=("Arial", 12))
        self.decrypt_button.pack(side=tk.LEFT, padx=5)

        self.cryptanalysis_button = tk.Button(button_frame, text="Kriptanalisis", command=self.cryptanalysis, font=("Arial", 12))
        self.cryptanalysis_button.pack(side=tk.LEFT, padx=5)

        # Label dan Area Hasil
        tk.Label(root, text="Hasil:", font=("Arial", 12)).grid(row=3, column=0, padx=10, pady=10, sticky="w")
        self.result_text = scrolledtext.ScrolledText(root, width=50, height=8, font=("Arial", 10), wrap=tk.WORD)
        self.result_text.grid(row=4, column=0, columnspan=2, padx=10)
        self.result_text.config(state=tk.DISABLED)

    def process_text(self, text, key, mode):
        """Fungsi utama untuk enkripsi dan dekripsi."""
        result = ""
        for char in text:
            if 'a' <= char <= 'z':
                start = ord('a')
                if mode == 'encrypt':
                    # Rumus Enkripsi: e(x) = (x + K) mod 26
                    new_ord = (ord(char) - start + key) % 26
                else: # decrypt
                    # Rumus Dekripsi: d(y) = (y - K) mod 26
                    new_ord = (ord(char) - start - key + 26) % 26
                result += chr(start + new_ord)
            elif 'A' <= char <= 'Z':
                start = ord('A')
                if mode == 'encrypt':
                    new_ord = (ord(char) - start + key) % 26
                else: # decrypt
                    new_ord = (ord(char) - start - key + 26) % 26
                result += chr(start + new_ord)
            else:
                result += char # Karakter selain huruf tidak diubah
        return result

    def get_key(self):
        """Mendapatkan dan memvalidasi kunci."""
        try:
            key = int(self.key_entry.get())
            if 0 <= key <= 25:
                return key
            else:
                messagebox.showerror("Error", "Kunci harus berada di antara 0 dan 25.")
                return None
        except ValueError:
            messagebox.showerror("Error", "Kunci harus berupa angka integer.")
            return None

    def encrypt_message(self):
        key = self.get_key()
        if key is not None:
            plaintext = self.text_entry.get().lower()
            ciphertext = self.process_text(plaintext, key, 'encrypt')
            self.display_result(f"Enkripsi Cipherteks: {ciphertext}")
            # Menyimpan ciphertext ke file
            with open("Cipher.txt", "w") as f:
                f.write(ciphertext)
            messagebox.showinfo("Info", "Cipherteks berhasil disimpan ke Cipher.txt")

    def decrypt_message(self):
        key = self.get_key()
        if key is not None:
            ciphertext = self.text_entry.get().lower()
            plaintext = self.process_text(ciphertext, key, 'decrypt')
            self.display_result(f"Dekripsi Plainteks: {plaintext}")

    def cryptanalysis(self):
        """Melakukan serangan brute-force (ciphertext-only attack)."""
        ciphertext = self.text_entry.get().lower()
        if not ciphertext:
            messagebox.showerror("Error", "Masukkan cipherteks untuk dianalisis.")
            return
            
        results = "Hasil Kriptanalisis (Brute-Force Attack):\n" + "="*40 + "\n"
        # Mencoba semua kemungkinan kunci dari 0 hingga 25 
        for key in range(26):
            plaintext = self.process_text(ciphertext, key, 'decrypt')
            results += f"Key = {key:2d}: {plaintext}\n"
        
        self.display_result(results)

    def display_result(self, text):
        """Menampilkan hasil di area teks."""
        self.result_text.config(state=tk.NORMAL)
        self.result_text.delete(1.0, tk.END)
        self.result_text.insert(tk.END, text)
        self.result_text.config(state=tk.DISABLED)

if __name__ == "__main__":
    root = tk.Tk()
    app = ShiftCipherGUI(root)
    root.mainloop()