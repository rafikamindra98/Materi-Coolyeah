% Meminta input jumlah data
n = input('Masukkan jumlah data: ');

% Validasi jumlah data
if n <= 0 || floor(n) ~= n
    disp('Jumlah data harus bilangan bulat positif.');
else
    % Inisialisasi array dan jumlah total
    total = 0;

    % Loop untuk input data satu per satu
    for i = 1:n
        nilai = input(['Masukkan data ke-', num2str(i), ': ']);
        total = total + nilai;
    end

    % Hitung rata-rata
    rata_rata = total / n;

    % Tampilkan hasil
    disp(['Rata-rata dari ', num2str(n), ' data adalah: ', num2str(rata_rata)]);
end
