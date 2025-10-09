% 1. Masukkan fungsi f(x)
syms x;
f_str = input('Masukkan fungsi f(x) = ', 's');     % Contoh: x^3 - x - 2
f = str2sym(f_str);
f_func = matlabFunction(f);                        % Konversi ke fungsi numerik

% 2. Masukkan dua tebakan awal x0 dan x1
x0 = input('Masukkan nilai awal x0: ');
x1 = input('Masukkan nilai awal x1: ');

% 3. Masukkan toleransi error
E = input('Masukkan toleransi (contoh: 1e-5): ');

% 4. Masukkan jumlah iterasi maksimum
N = input('Masukkan jumlah iterasi maksimum: ');

% 5. Inisialisasi
iter = 0;
akar = [];

% 6. Iterasi metode secant
while iter < N
    f0 = f_func(x0);
    f1 = f_func(x1);
    
    % Cek pembagi nol
    if (f1 - f0) == 0
        fprintf('Pembagi nol terdeteksi. Iterasi dihentikan.\n');
        break;
    end
    
    % Hitung x selanjutnya
    x2 = x1 - f1 * (x1 - x0) / (f1 - f0);
    
    akar = [akar; x2];       % Simpan nilai akar
    iter = iter + 1;
    
    % Cek konvergensi
    if abs(x2 - x1) < E
        break;
    end
    
    % Update nilai x
    x0 = x1;
    x1 = x2;
end

% 7. Cetak hasil
if iter == N
    fprintf('Iterasi tidak konvergen dalam %d iterasi.\n', N);
else
    fprintf('\nAkar ditemukan: %.10f\n', x2);
    fprintf('Jumlah iterasi: %d\n', iter);
    fprintf('Daftar akar tiap iterasi:\n');
    disp(akar);
end