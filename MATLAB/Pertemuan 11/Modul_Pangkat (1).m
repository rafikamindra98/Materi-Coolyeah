% Input
a = input('Masukkan basis (a): ');
b = input('Masukkan pangkat (b): ');

% Inisialisasi
hasil = 1;

% Hitung pangkat
if b == 0
    hasil = 1;
elseif b > 0
    for i = 1:b
        hasil = hasil * a;
    end
else
    for i = 1:abs(b)
        hasil = hasil * a;
    end
    hasil = 1 / hasil;
end

% Output
disp(['Hasil dari ', num2str(a), '^', num2str(b), ' adalah: ', num2str(hasil)]);
