% Input
n = input('Masukkan bilangan bulat non-negatif: ');

% Cek validitas input
if n < 0 || floor(n) ~= n
    disp('Input tidak valid. Harus bilangan bulat non-negatif.');
else
    % Inisialisasi
    hasil = 1;

    % Proses faktorial
    for i = 2:n
        hasil = hasil * i;
    end

    % Output
    disp(['Faktorial dari ', num2str(n), ' adalah: ', num2str(hasil)]);
end
