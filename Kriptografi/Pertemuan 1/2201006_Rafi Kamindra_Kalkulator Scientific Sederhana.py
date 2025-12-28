import tkinter as tk
from tkinter import ttk
import math

# Inisialisasi window
root = tk.Tk()
root.title('Kalkulator Scientific')
root.configure(bg='#f0f0f0')

# Frame utama
main_frame = ttk.Frame(root, padding="10")
main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))

# Style
style = ttk.Style()
style.configure('TLabel', font=('Helvetica', 12))
style.configure('TButton', font=('Helvetica', 10))
style.configure('TEntry', font=('Helvetica', 12))

# Judul
title_label = ttk.Label(main_frame, text='Kalkulator Scientific', font=('Helvetica', 16, 'bold'))
title_label.grid(row=0, column=0, columnspan=4, pady=10)

# Entry fields
input_frame = ttk.Frame(main_frame)
input_frame.grid(row=1, column=0, columnspan=4, pady=5)

label1 = ttk.Label(input_frame, text='X = ')
label1.grid(row=0, column=0, padx=5)
entry1 = ttk.Entry(input_frame, width=15)
entry1.grid(row=0, column=1, padx=5)

label2 = ttk.Label(input_frame, text='Y = ')
label2.grid(row=0, column=2, padx=5)
entry2 = ttk.Entry(input_frame, width=15)
entry2.grid(row=0, column=3, padx=5)

# Label hasil
result_frame = ttk.Frame(main_frame)
result_frame.grid(row=2, column=0, columnspan=4, pady=10)

label3 = ttk.Label(result_frame, text='Hasil = ')
label3.grid(row=0, column=0)
result_label = ttk.Label(result_frame, text='0', font=('Helvetica', 14))
result_label.grid(row=0, column=1)

# History operasi
history_label = ttk.Label(main_frame, text='History Operasi:', font=('Helvetica', 10))
history_label.grid(row=4, column=0, columnspan=4, pady=(10,0), sticky='w')
history_text = tk.Text(main_frame, height=3, width=40, font=('Helvetica', 10))
history_text.grid(row=5, column=0, columnspan=4, pady=5)
# Fungsi untuk menambahkan operasi ke history
def add_to_history(operation, x, y, result):
    history_text.insert('1.0', f'{x} {operation} {y} = {result}\n')
    history_text.see('1.0')

# Fungsi untuk clear input dan hasil
def clear():
    entry1.delete(0, tk.END)
    entry2.delete(0, tk.END)
    result_label.config(text='0')

# Fungsi operasi matematika
def calculate(operation):
    try:
        x = float(entry1.get())
        y = float(entry2.get())
        
        if operation == '+':
            result = x + y
            symbol = '+'
        elif operation == '-':
            result = x - y
            symbol = '-'
        elif operation == '×':
            result = x * y
            symbol = '×'
        elif operation == '÷':
            if y == 0:
                result = 'Error: Dibagi 0'
                symbol = '÷'
            else:
                result = x / y
                symbol = '÷'
        elif operation == '^':
            result = x ** y
            symbol = '^'
        elif operation == '√':
            result = x ** (1/y)
            symbol = '√'
        elif operation == '%':
            result = x % y
            symbol = '%'
            
        if isinstance(result, float):
            result_str = f'{result:.4f}'.rstrip('0').rstrip('.')
        else:
            result_str = str(result)
            
        result_label.config(text=result_str)
        add_to_history(symbol, x, y, result_str)
        
    except ValueError:
        result_label.config(text='Error: Input Invalid')
    except Exception as e:
        result_label.config(text=f'Error: {str(e)}')

# Button Frame
button_frame = ttk.Frame(main_frame)
button_frame.grid(row=3, column=0, columnspan=4, pady=10)

# Buttons dengan style yang lebih menarik
buttons = [
    ('+', 0, 0), ('-', 0, 1), ('×', 0, 2), ('÷', 0, 3),
    ('^', 1, 0), ('√', 1, 1), ('%', 1, 2), ('C', 1, 3)
]

for (text, row, col) in buttons:
    if text == 'C':
        btn = ttk.Button(button_frame, text=text, command=clear)
    else:
        btn = ttk.Button(button_frame, text=text, 
                        command=lambda t=text: calculate(t))
    btn.grid(row=row, column=col, padx=5, pady=5)

# Konfigurasi grid layout
for i in range(4):
    main_frame.columnconfigure(i, weight=1)
    button_frame.columnconfigure(i, weight=1)

# Jalankan aplikasi
root.mainloop()