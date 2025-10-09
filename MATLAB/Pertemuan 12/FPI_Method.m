% 1. Masukkan fungsi g(x) = x, yaitu bentuk iteratif
syms x;
gx_str = input('Masukkan fungsi iterasi g(x) = ', 's');
g = str2sym(gx_str);
g_func = matlabFunction(g);  % Konversi ke fungsi numerik

% 2. Masukkan nilai awal x0
x0 = input('Masukkan nilai awal x0: ');

% 3. Masukkan toleransi error (E)
E = input('Masukkan toleransi (contoh: 1e-5): ');

% 4. Masukkan jumlah iterasi maksimum
N = input('Masukkan jumlah iterasi maksimum: ');

% 5. Inisialisasi
iter = 0;
akar = [];

% 6. Iterasi titik tetap
while iter < N
    x1 = g_func(x0);              % Hitung x selanjutnya
    akar = [akar; x1];            % Simpan nilai akar
    iter = iter + 1;

    if abs(x1 - x0) < E           % Cek konvergensi
        break;
    end
    
    x0 = x1;                      % Perbarui x0 untuk iterasi berikutnya
end

% 7. Cek hasil akhir
if iter == N
    fprintf('Iterasi tidak konvergen dalam batas %d iterasi.\n', N);
else
    fprintf('\nAkar ditemukan: %.10f\n', x1);
    fprintf('Jumlah iterasi: %d\n', iter);
    fprintf('Daftar nilai akar tiap iterasi:\n');
    disp(akar);
end