n = input('Masukkan bilangan bulat non-negatif: ');

hasil = 1;
for i = 1:n
    hasil = hasil * i;
end

disp(['Faktorial dari ', num2str(n), ' adalah ', num2str(hasil)]);