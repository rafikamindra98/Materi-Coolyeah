import tkinter as tk
from tkinter import messagebox, ttk
import random

class TicTacToe:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Tic Tac Toe - Game Seru!")
        self.root.geometry("400x600")  # Ukuran jendela diperbesar untuk menu tambahan
        self.root.configure(bg="#f0f8ff")  # Latar belakang biru muda untuk tampilan menarik
        
        self.show_main_menu()
    
    def show_main_menu(self):
        # Bersihkan window
        for widget in self.root.winfo_children():
            widget.destroy()
        
        # Title
        self.title_label = tk.Label(
            self.root, 
            text="Tic Tac Toe", 
            font=("Arial", 24, "bold"), 
            bg="#f0f8ff", 
            fg="#333"
        )
        self.title_label.pack(pady=20)
        
        # Frame untuk menu
        menu_frame = tk.Frame(self.root, bg="#f0f8ff")
        menu_frame.pack(pady=20)
        
        # Tombol menu
        tk.Button(
            menu_frame,
            text="Bermain dengan Teman",
            font=("Arial", 14, "bold"),
            bg="#90ee90",
            fg="#333",
            width=20,
            height=2,
            command=lambda: self.start_game("friend")
        ).pack(pady=10)
        
        tk.Button(
            menu_frame,
            text="Bermain dengan Komputer",
            font=("Arial", 14, "bold"),
            bg="#add8e6",
            fg="#333",
            width=20,
            height=2,
            command=self.show_difficulty_menu
        ).pack(pady=10)
    
    def show_difficulty_menu(self):
        # Bersihkan window
        for widget in self.root.winfo_children():
            widget.destroy()
            
        # Title
        self.title_label = tk.Label(
            self.root, 
            text="Pilih Level", 
            font=("Arial", 24, "bold"), 
            bg="#f0f8ff", 
            fg="#333"
        )
        self.title_label.pack(pady=20)
        
        # Frame untuk menu
        menu_frame = tk.Frame(self.root, bg="#f0f8ff")
        menu_frame.pack(pady=20)
        
        # Tombol level
        difficulties = [
            ("Mudah", "#90ee90"),
            ("Sedang", "#ffd700"),
            ("Sulit", "#ff6b6b")
        ]
        
        for diff, color in difficulties:
            tk.Button(
                menu_frame,
                text=f"Level {diff}",
                font=("Arial", 14, "bold"),
                bg=color,
                fg="#333",
                width=20,
                height=2,
                command=lambda d=diff.lower(): self.start_game("computer", d)
            ).pack(pady=10)
        
        # Tombol kembali
        tk.Button(
            menu_frame,
            text="Kembali",
            font=("Arial", 12),
            bg="#d3d3d3",
            fg="#333",
            width=15,
            command=self.show_main_menu
        ).pack(pady=20)
    
    def start_game(self, mode, difficulty=None):
        self.mode = mode
        self.difficulty = difficulty
        self.current_player = "X"
        self.board = [""] * 9
        self.game_over = False
        
        # Bersihkan window
        for widget in self.root.winfo_children():
            widget.destroy()
        
        # Setup UI
        self.setup_game_ui()
        
        # Jika komputer mulai duluan (random)
        if mode == "computer" and random.choice([True, False]):
            self.current_player = "O"
            self.status_label.config(text="Giliran: Komputer")
            self.root.after(1000, self.make_computer_move)
    
    def setup_game_ui(self):
        # Title
        self.title_label = tk.Label(
            self.root, 
            text="Tic Tac Toe", 
            font=("Arial", 24, "bold"), 
            bg="#f0f8ff", 
            fg="#333"
        )
        self.title_label.pack(pady=10)
        
        # Mode info
        mode_text = "Mode: Bermain dengan " + ("Teman" if self.mode == "friend" else f"Komputer (Level {self.difficulty.title()})")
        self.mode_label = tk.Label(
            self.root,
            text=mode_text,
            font=("Arial", 12),
            bg="#f0f8ff",
            fg="#333"
        )
        self.mode_label.pack(pady=5)
        
        # Status
        self.status_label = tk.Label(
            self.root, 
            text=f"Giliran: {self.current_player}", 
            font=("Arial", 16), 
            bg="#f0f8ff", 
            fg="#333"
        )
        self.status_label.pack(pady=5)
        
        # Frame untuk papan
        self.board_frame = tk.Frame(self.root, bg="#f0f8ff")
        self.board_frame.pack(pady=20)
        
        # Button frame
        button_frame = tk.Frame(self.root, bg="#f0f8ff")
        button_frame.pack(pady=10)
        
        # Tombol Reset
        tk.Button(
            button_frame,
            text="Reset Game",
            font=("Arial", 12, "bold"),
            bg="#90ee90",
            fg="#333",
            width=12,
            command=lambda: self.start_game(self.mode, self.difficulty)
        ).pack(side=tk.LEFT, padx=5)
        
        # Tombol Menu Utama
        tk.Button(
            button_frame,
            text="Menu Utama",
            font=("Arial", 12, "bold"),
            bg="#ffd700",
            fg="#333",
            width=12,
            command=self.show_main_menu
        ).pack(side=tk.LEFT, padx=5)
        
        # Buat tombol papan 3x3
        self.buttons = []
        for i in range(3):
            row = []
            for j in range(3):
                btn = tk.Button(
                    self.board_frame, 
                    text="", 
                    font=("Arial", 32, "bold"), 
                    width=5, 
                    height=3,
                    bg="#add8e6",  # Biru muda untuk tombol
                    fg="#333",
                    relief="raised",
                    bd=3,
                    command=lambda idx=i*3+j: self.make_move(idx)
                )
                btn.grid(row=i, column=j, padx=2, pady=2)
                row.append(btn)
            self.buttons.append(row)
        
        # Tombol Reset
        self.reset_btn = tk.Button(
            self.root,
            text="Reset Game",
            font=("Arial", 14, "bold"),
            bg="#90ee90",  # Hijau muda
            fg="#333",
            width=15,
            height=2,
            command=self.reset_game
        )
        self.reset_btn.pack(pady=20)
        
        # Bind tombol ESC untuk reset (bonus fitur)
        self.root.bind("<Escape>", lambda e: self.reset_game())
    
    def make_move(self, index):
        if self.board[index] == "" and not self.game_over:
            # Langkah pemain
            self.board[index] = self.current_player
            self.buttons[index // 3][index % 3].config(
                text=self.current_player,
                fg="#ff0000" if self.current_player == "X" else "#0000ff",  # Merah untuk X, Biru untuk O
                state="disabled"  # Nonaktifkan tombol setelah klik
            )
            
            if self.check_winner():
                self.game_over = True
                messagebox.showinfo("Selamat!", f"Pemain {self.current_player} Menang!")
                self.status_label.config(text=f"{self.current_player} Menang! Game Selesai.")
                return
            elif "" not in self.board:
                self.game_over = True
                messagebox.showinfo("Seri!", "Game Berakhir Seri!")
                self.status_label.config(text="Game Seri! Tekan Reset untuk Main Lagi.")
                return
            
            # Ganti pemain
            self.current_player = "O" if self.current_player == "X" else "X"
            self.status_label.config(text=f"Giliran: {'Komputer' if self.mode == 'computer' and self.current_player == 'O' else self.current_player}")
            
            # Jika bermain dengan komputer dan sekarang giliran komputer
            if self.mode == "computer" and self.current_player == "O":
                self.root.after(1000, self.make_computer_move)
    
    def make_computer_move(self):
        if self.game_over:
            return
            
        move = None
        
        if self.difficulty == "sulit":
            # Coba menang
            move = self.find_winning_move("O")
            if not move:
                # Blok kemenangan lawan
                move = self.find_winning_move("X")
                if not move:
                    # Ambil tengah jika kosong
                    if self.board[4] == "":
                        move = 4
                    else:
                        # Ambil sudut atau sisi yang kosong
                        corners = [0, 2, 6, 8]
                        random.shuffle(corners)
                        for i in corners:
                            if self.board[i] == "":
                                move = i
                                break
                        if move is None:
                            edges = [1, 3, 5, 7]
                            random.shuffle(edges)
                            for i in edges:
                                if self.board[i] == "":
                                    move = i
                                    break
        
        elif self.difficulty == "sedang":
            if random.random() < 0.7:  # 70% kemungkinan bermain cerdas
                move = self.find_winning_move("O")
                if not move:
                    move = self.find_winning_move("X")
            
            if move is None:
                empty_spots = [i for i, spot in enumerate(self.board) if spot == ""]
                move = random.choice(empty_spots)
        
        else:  # mudah
            empty_spots = [i for i, spot in enumerate(self.board) if spot == ""]
            move = random.choice(empty_spots)
        
        self.make_move(move)
    
    def find_winning_move(self, player):
        # Cek setiap posisi kosong
        for i in range(9):
            if self.board[i] == "":
                # Coba langkah
                self.board[i] = player
                # Cek apakah menang
                if self.check_winner():
                    self.board[i] = ""  # Kembalikan ke kosong
                    return i
                self.board[i] = ""  # Kembalikan ke kosong
        return None
    
    def check_winner(self):
        # Kombinasi kemenangan: baris, kolom, diagonal
        winning_combos = [
            [0, 1, 2], [3, 4, 5], [6, 7, 8],  # Baris
            [0, 3, 6], [1, 4, 7], [2, 5, 8],  # Kolom
            [0, 4, 8], [2, 4, 6]  # Diagonal
        ]
        for combo in winning_combos:
            if (self.board[combo[0]] == self.board[combo[1]] == self.board[combo[2]] != ""):
                # Highlight kotak pemenang
                for pos in combo:
                    self.buttons[pos // 3][pos % 3].config(bg="#90ee90")  # Highlight hijau
                return True
        return False
    
    def reset_game(self):
        self.current_player = "X"
        self.board = [""] * 9
        self.game_over = False
        self.status_label.config(text=f"Giliran: {self.current_player}")
        
        # Reset tombol
        for i in range(3):
            for j in range(3):
                idx = i * 3 + j
                self.buttons[i][j].config(
                    text="",
                    fg="#333",
                    state="normal",
                    bg="#add8e6"
                )
        
        messagebox.showinfo("Reset", "Game Direset! Mulai Main Lagi.")
    
    def run(self):
        self.root.mainloop()

# Jalankan game
if __name__ == "__main__":
    game = TicTacToe()
    game.run()
