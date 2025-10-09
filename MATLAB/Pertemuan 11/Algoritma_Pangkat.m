a = 2; % Basis
b = 3; % Pangkat

hasil = 1;
for i = 1:b
    hasil = hasil * a;
end

disp(['Hasil ', num2str(a), '^', num2str(b), ' = ', num2str(hasil)]);