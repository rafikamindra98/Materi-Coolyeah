//PROGRAM KalkulatorSederhana
#include <iostream>
#include <math.h>
using namespace std;
int main() {
	
	//Tampilkan Nama Program
	cout << "				========================================" << endl;
	cout << "				##-----------Kalkulator Ajaib---------##" << endl;
	cout << "				##-------------Rafi Kamindra----------##" << endl;
	cout << "				##----------------2201006-------------##" << endl;
	cout << "				========================================" << endl;
	cout << endl;
	
	//Deklarasi
	int pilihan;
	float num1, num2, num3, num4;
	float luas, keliling;
	float Pi=3.14;
	
	//Algortima
	cout << "1. Penjumlahan"	<< endl;
	cout << "2. Pengurangan"	<< endl;
	cout << "3. Perkalian"  	<< endl;
	cout << "4. Pembagian"  	<< endl;
	cout << "5. Modulus"    	<< endl;
	cout << "6. Pangkat" 		<< endl;
	cout << "7. Akar"			<< endl;
	cout << "8. Luas Segitiga"	<< endl;
	cout << "9. Luas Persegi"	<< endl;
	cout << "10. Luas Persegi Panjang" << endl;
	cout << "11. Luas Jajar Genjang" << endl;
	cout << "12. Luas Trapesium" << endl;
	cout << "13. Luas Belah Ketupat" << endl;
	cout << "14. Luas Layang-layang" << endl;
	cout << "15. Luas Lingkaran" << endl;
	cout << "16. Keliling Segitiga" << endl;
	cout << "17. Keliling Persegi" << endl;
	cout << "18. Keliling Persegi Panjang" << endl;
	cout << "19. Keliling Jajar Genjang" << endl;
	cout << "20. Keliling Trapesium" << endl;
	cout << "21. Keliling Belah Ketupat" << endl;
	cout << "22. Keliling Layang-layang" << endl;
	cout << "23. Keliling Lingkaran" << endl;
	cout << "24. Konversi Suhu Celcius" << endl;
	cout << "25. Konversi Suhu Farenheit" << endl;
	cout << "26. Konversi Suhu Kelvin" << endl;
	cout << "27. Konversi Suhu Reamur" << endl;
	cout << "28. Konversi Suhu Rankine" << endl;
	cout << endl;
	
	cout << "Input pilihan operasi: "; cin >> pilihan; cout << endl;
	
	switch (pilihan) {
		
	case 1:
	//1. Penjumlahan
		cout << "Angka pertama: "; cin >> num1;
		cout << "Angka kedua: "; cin >> num2; 
		cout << endl;
		cout << "Hasil dari " << num1 << " + " << num2 << " = " << num1+num2;
		break;
		
   case 2:
   	//2. Pengurangan
   		cout << "Angka pertama: "; cin >> num1;
		cout << "Angka kedua: "; cin >> num2; 
		cout << endl;
   		cout << "Hasil dari " << num1 << " - " << num2 << " = " << num1-num2;
		break;
		
   case 3:
   	//3. Perkalian
   		cout << "Angka pertama: "; cin >> num1;
		cout << "Angka kedua: "; cin >> num2; 
		cout << endl;
	   	cout << "Hasil dari " << num1 << " * " << num2 << " = " << num1*num2;
	   	break;
	   	
   case 4:
   	//4. Pembagian
   		cout << "Angka pertama: "; cin >> num1;
		cout << "Angka kedua: "; cin >> num2; 
		cout << endl;
		cout << "Hasil dari " << num1 << " / " << num2 << " = " << num1/num2;
		break;
		
   case 5:
   	//5. Modulus
   		cout << "Angka pertama: "; cin >> num1;
		cout << "Angka kedua: "; cin >> num2; 
		cout << endl;
    	cout << "Hasil dari " << num1 << " % " << num2 << " = " << (int)num1 % (int)num2;
		break;
		
	case 6:
	//6. Pangkat
		cout << "Angka pertama: "; cin >> num1;
		cout << "Derajat pangkat: "; cin >> num2; 
		cout << endl;
		cout << "Hasil dari " << num1 << " ^ " << num2 << " = " << pow (num1,num2);
		break;
		
	case 7:
	//7. Akar
		cout << "Angka didalam akar: "; cin >> num1; 
		cout << endl;
		cout << "Hasil dari akar " << num1 << " = " << sqrt (num1);
		break;
		
	case 8:
	//8. Luas Segitiga
		cout << "Panjang alas: "; cin >> num1;
		cout << "Tinggi segitiga: "; cin >> num2; 
		cout << endl;
		cout << "Luas Segitiga = " << (num1*num2)/2;
		break;
	
	case 9:
	//9. Luas Persegi
		cout << "Panjang sisi: "; cin >> num1; 
		cout << endl;
		cout << "Luas Persegi = " << num1*num1;
		break;
	
	case 10:
	//10. Luas Persegi Panjang
		cout << "Panjang alas: "; cin >> num1;
		cout << "Tinggi persegi panjang: "; cin >> num2; 
		cout << endl;
		cout << "Luas Persegi Panjang = " << num1*num2;
		break;
		
	case 11:
	//11. Luas Jajar Genjang
		cout << "Panjang alas: "; cin >> num1;
		cout << "Tinggi jajar genjang: "; cin >> num2;
		cout << endl;
		cout << "Luas Jajar Genjang = " << num1*num2;
		break;
		
	case 12:
	//12. Luas Trapesium
		cout << "Panjang sisi sejajar 1: "; cin >> num1;
		cout << "Panjang sisi sejajar 2: "; cin >> num2;
		cout << "Tinggi trapesium: "; cin >> num3;
		cout << endl;
		cout << "Luas Trapesium = " << ((num1*num2)/2)*num3;
		break;
	
	case 13:
	//13. Luas Belah Ketupat
		cout << "Panjang diagonal 1: "; cin >> num1;
		cout << "Panjang diagonal 2: "; cin >> num2;
		cout << endl;
		cout << "Luas Belah Ketupat = " << (num1*num2)/2;
		break;
		
	case 14:
	//14. Luas Layang-layang
		cout << "Panjang diagonal 1: "; cin >> num1;
		cout << "Panjang diagonal 2: "; cin >> num2;
		cout << endl;
		cout << "Luas Layang-layang = " << (num1*num2)/2;
		break;
		
	case 15:
	//15. Luas Lingkaran
		cout << "Jari-jari lingkaran: "; cin >> num1;
		cout << endl;
		cout << "Luas Lingkaran dengan jari-jari " << num1 << " = " << Pi*num1*num1;
		break;
		
	case 16:
	//16. Keliling Segitiga
		cout << "Panjang sisi segitiga: "; cin >> num1;
		cout << endl;
		cout << "Keliling Segitiga = " << 3*num1;
		break;
		
	case 17:
	//17. Keliling Persegi
		cout << "Panjang sisi: "; cin >> num1; cout << endl;
		cout << endl;
		cout << "Keliling Persegi = " << 4*num1;
		break;
	
	case 18:
	//18. Keliling Persegi Panjang
		cout << "Panjang alas: "; cin >> num1;
		cout << "Tinggi persegi panjang: "; cin >> num2; 
		cout << endl;
		cout << "Keliling Persegi Panjang = " << 2*(num1+num2);
		break;
		
	case 19:
	//19. Keliling Jajar Genjang
		cout << "Panjang alas: "; cin >> num1;
		cout << "Tinggi jajar genjang: "; cin >> num2;
		cout << endl;
		cout << "Keliling Jajar Genjang = " << 2*(num1+num2);
		break;
		
	case 20:
	//20. Keliling Trapesium
		cout << "Panjang sisi 1: "; cin >> num1;
		cout << "Panjang sisi 2: "; cin >> num2;
		cout << "Panjang sisi 3: "; cin >> num3;
		cout << "Panjang sisi 4: "; cin >> num4;
		cout << endl;
		cout << "Keliling Trapesium = " << num1+num2+num3+num4;
		break;
		
	case 21:
	//21. Keliling Belah Ketupat 
		cout << "Panjang sisi 1: "; cin >> num1;
		cout << endl;
		cout << "Keliling Belah Ketupat = " << 4*num1;
		break;
		
	case 22:
	//22. Keliling Layang-layang
		cout << "Panjang sisi terpanjang "; cin >> num1;
		cout << "Panjang sisi terpendek: "; cin >> num2;
		cout << endl;
		cout << "keliling Belah Ketupat = " << 2*(num1+num2);
		break;
		
	case 23:
	//23. Keliling Lingkaran
		cout << "Jari-jari lingkaran: "; cin >> num1;
		cout << endl;
		cout << "Keliling Lingkaran dengan jari-jari " << num1 << " = " << 2*Pi*num1;
		break;
		
	case 24:
	//24. Konversi Suhu Celcius
		float celcius, farenheit, kelvin, reamur, rankine;
		cout << "Suhu dalam satuan Celcius: "; cin >> celcius;
		farenheit = 9*celcius/5+32;
		kelvin = celcius+273.15;
		reamur = 4*celcius/5;
		rankine = 9*(celcius+273.15)/5;
		cout << endl;
		cout << celcius << " derajat celcius sama dengan:" << endl;
		cout << "Farenheit (F) = " << farenheit << endl;
		cout << "Kelvin (K) = " << kelvin << endl;
		cout << "Reamur (R) = " << reamur << endl;
		cout << "Rankine (Ra) = " << rankine <<  endl;
		break;
		
	case 25:
	//25. Konversi Suhu Farenheit
		cout << "Suhu dalam satuan Farenheit: "; cin >> farenheit;
		celcius = 5*(farenheit-32)/9;
		kelvin = 5*(farenheit+459.67)/9;
		reamur = 4*(farenheit-32)/9;
		rankine = farenheit+459.67;
		cout << endl;
		cout << farenheit << " derajat farenheit sama dengan:" << endl;
		cout << "Celcius (C) = " << celcius << endl;
		cout << "Kelvin (K) = " << kelvin << endl;
		cout << "Reamur (R) = " << reamur << endl;
		cout << "Rankine (Ra) = " << rankine <<  endl;
		break;
		
	case 26:
	//26. Konversi Suhu Kelvin
		cout << "Suhu dalam satuan Kelvin: "; cin >> kelvin;
		celcius = kelvin-273.15;
		farenheit = (kelvin*9)/5-459.67;
		reamur = 4*(kelvin-273)/5;
		rankine = 9*kelvin/5;
		cout << endl;
		cout << kelvin << " derajat farenheit sama dengan:" << endl;
		cout << "Celcius (C) = " << celcius << endl;
		cout << "Farenheit (F) = " << farenheit << endl;
		cout << "Reamur (R) = " << reamur << endl;
		cout << "Rankine (Ra) = " << rankine <<  endl;
		break;
		
	case 27:
	//27. Konversi Suhu Reamur
		cout << "Suhu dalam satuan Reamur: "; cin >> reamur;
		celcius = reamur/0.8;
		farenheit = (reamur*2.25)+32;
		kelvin = (reamur/0.8)+273.25;
		rankine = (reamur*2.25)+491.67;
		cout << endl;
		cout << reamur << " derajat reamur sama dengan:" << endl;
		cout << "Celcius (C) = " << celcius << endl;
		cout << "Farenheit (F) = " << farenheit << endl;
		cout << "Kelvin (K) = " << kelvin << endl;
		cout << "Rankine (Ra) = " << rankine <<  endl;
		break;
		
	case 28:
	//28. Konversi Suhu Rankine
		cout << "Suhu dalam satuan Rankine: "; cin >> rankine;
		celcius = 5*(rankine-491.67)/9;
		farenheit = rankine-459.67;
		kelvin = 5*rankine/9;
		cout << endl;
		cout << rankine << " derajat rankine sama dengan:" << endl;
		cout << "Celcius (C) = " << celcius << endl;
		cout << "Farenheit (F) = " << farenheit << endl;
		cout << "Kelvin (K) = " << kelvin << endl;
		break;
		
   default :
     printf("Maaf, pilihan menu tidak tersedia");
  }
 
  cout << endl;
  return 0;
}
