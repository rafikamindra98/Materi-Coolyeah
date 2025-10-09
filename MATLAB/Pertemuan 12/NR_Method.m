% 1-4. Masukkan fungsi
syms x;
y = input('Masukkan fungsi y = ', 's');
f = str2sym(y);           % Konversi string ke simbolik
f_diff = diff(f, x);      % 10. Hitung turunan

% 5. Masukkan nilai awal
x0 = input('Masukkan tebakan awal x0: ');

% 6. Masukkan toleransi
E = input('Masukkan toleransi (galat): ');

% 7. Masukkan batas iterasi maksimum
N = input('Masukkan jumlah iterasi maksimum: ');

% 8-9. Inisialisasi
iter = 1;
akar = [];

% 11. Hitung f(x0)
f0 = double(subs(f, x, x0));

% 12. Proses iterasi
while abs(f0) > E
    f0 = double(subs(f, x, x0));         % 12.1 Hitung f(x0)
    df0 = double(subs(f_diff, x, x0));   % 12.2 Hitung f'(x0)

    % 12.3 Cek pembagi nol
    if df0 == 0
        fprintf('Pembagi dengan nol. Proses dihentikan.\n');
        break;
    end

    % 12.5 Hitung x selanjutnya
    x_new = x0 - (f0 / df0);

    % 12.6 Update x0
    x0 = x_new;

    % 12.7 Cek iterasi maksimum
    if iter > N
        fprintf('Iterasi tidak konvergen.\n');
        break;
    end

    % 12.9 Simpan nilai akar
    akar = [akar; x0];

    % 12.10 Tambah iterasi
    iter = iter + 1;
end

% 14-15. Cetak hasil
fprintf('\nDaftar akar per iterasi:\n');
disp(akar);
fprintf('Jumlah iterasi: %d\n', iter - 1);