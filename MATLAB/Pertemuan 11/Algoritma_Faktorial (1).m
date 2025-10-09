% Input
n = 5;  % bisa ganti nilainya sesuai kebutuhan

% Cek validitas input
if n < 0 || floor(n) ~= n
    disp('Input harus bilangan bulat non-negatif');
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
