% 1. Masukkan fungsi secara simbolik
syms x;
y = input('Masukkan fungsi y = ', 's');     % Contoh: x^3 - x - 2
f = str2sym(y);
f_func = matlabFunction(f);                 % Konversi ke fungsi numerik

% 2. Masukkan batas bawah (a)
a = input('Masukkan nilai a (batas bawah): ');

% 3. Masukkan batas atas (b)
b = input('Masukkan nilai b (batas atas): ');

% 4. Masukkan toleransi (E)
E = input('Masukkan toleransi (contoh: 1e-5): ');

% 5. Masukkan jumlah iterasi maksimum
N = input('Masukkan jumlah iterasi maksimum: ');

% Validasi awal
fa = f_func(a);
fb = f_func(b);
if fa * fb >= 0
    error('f(a) * f(b) harus < 0 (akar harus berada di antara a dan b)');
end

% 6. Inisialisasi
iter = 0;
akar = [];

% 7. Iterasi
while iter < N
    % Hitung c dengan metode posisi palsu
    c = (a * f_func(b) - b * f_func(a)) / (f_func(b) - f_func(a));
    fc = f_func(c);
    
    % Simpan nilai akar
    akar = [akar; c];
    
    % Cek toleransi
    if abs(fc) < E
        break;
    end

    % Update interval berdasarkan tanda
    if fa * fc < 0
        b = c;
        fb = fc;
    else
        a = c;
        fa = fc;
    end
    
    % Tambah iterasi
    iter = iter + 1;
end

% 8. Cetak hasil
fprintf('\nAkar ditemukan: %.10f\n', c);
fprintf('Jumlah iterasi: %d\n', iter);
fprintf('Daftar akar tiap iterasi:\n');
disp(akar);