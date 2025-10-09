% Input jumlah data
n = input('Masukkan jumlah data: ');

% Validasi jumlah data
if n <= 0 || floor(n) ~= n
    disp('Jumlah data harus bilangan bulat positif.');
else
    % Inisialisasi array dan total
    data = zeros(1, n); % Menyimpan semua data
    total = 0;

    % Input data satu per satu
    for i = 1:n
        data(i) = input(['Masukkan data ke-', num2str(i), ': ']);
        total = total + data(i);
    end

    % Hitung rata-rata
    rata_rata = total / n;

    % Hitung simpangan baku (standard deviation)
    jumlah_selisih_kuadrat = 0;
    for i = 1:n
        selisih = data(i) - rata_rata;
        jumlah_selisih_kuadrat = jumlah_selisih_kuadrat + selisih^2;
    end

    simpangan_baku = sqrt(jumlah_selisih_kuadrat / n);  % populasi
    % simpangan_baku = sqrt(jumlah_selisih_kuadrat / (n - 1)); % jika sampel

    % Tampilkan hasil
    disp(['Rata-rata: ', num2str(rata_rata)]);
    disp(['Simpangan baku: ', num2str(simpangan_baku)]);
end

