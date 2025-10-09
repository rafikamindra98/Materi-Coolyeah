% 1. Baca fungsi f(x) dari input sebagai string lalu konversi ke anonymous function
fungsi_str = input('Masukkan fungsi f(x) (contoh: x^3-x-2): ', 's');
f = str2func(['@(x)', fungsi_str]);

% 2. Baca nilai a
a = input('Masukkan nilai a (batas bawah): ');

% 3. Baca nilai b
b = input('Masukkan nilai b (batas atas): ');

% 4. Baca toleransi E
E = input('Masukkan toleransi (galat): ');

% Validasi awal
if f(a) * f(b) >= 0
    error('f(a) * f(b) harus < 0. Fungsi harus memiliki tanda berlawanan di a dan b.');
end

% 5. Inisialisasi iterasi
iterasi = 0;

% 6. Tempat menyimpan nilai akar (c) tiap iterasi
akar = [];

% 7-8. Inisialisasi nilai awal c dan f(c)
c = (a + b) / 2;
fc = f(c);

% 9. Iterasi
while abs(fc) > E
    iterasi = iterasi + 1;     % Tambah iterasi
    c = (a + b) / 2;           % Hitung titik tengah
    akar(end+1) = c;           % Simpan nilai akar
    fa = f(a);                 % Hitung f(a)
    fc = f(c);                 % Hitung f(c)

    if fa * fc < 0
        b = c;
    else
        a = c;
    end
end

% 11-12. Cetak hasil
fprintf('\nAkar ditemukan: %.10f\n', c);
fprintf('Jumlah iterasi: %d\n', iterasi);
fprintf('Nilai akar setiap iterasi:\n');
disp(akar');