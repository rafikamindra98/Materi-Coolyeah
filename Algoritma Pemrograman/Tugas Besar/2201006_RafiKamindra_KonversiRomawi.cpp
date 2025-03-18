//Program Konversi_ke_Romawi(1-4999)
#include <iostream>
#include <conio.h>
using namespace std;
int main() {
	
	//Tampilkan Nama Program
	cout << "				========================================" << endl;
	cout << "				##------------Konversi Romawi---------##" << endl;
	cout << "				##-------------Rafi Kamindra----------##" << endl;
	cout << "				##----------------2201006-------------##" << endl;
	cout << "				========================================" << endl;
	cout << endl;
	
	//Deklarasi
    int romawi;
    
    //Algoritma
    cout << "Masukkan Angka: ";cin >> romawi;

    if(romawi < 1 || romawi > 4999)
    	cout << "romawi hanya diantara 1 - 4999";
    else{
         while(romawi >= 1000){
           cout << "M";
           romawi -= 1000;
         }

         if(romawi >= 500){
           if(romawi >= 900){
             cout << "CM";
             romawi -= 900;
           }
           else{
             cout << "D";
             romawi -= 500;
           }
         }


         while(romawi >= 100){
           if(romawi >= 400){
            cout << "CD";
            romawi -= 400;
           }
           else{
             cout << "C";
             romawi -= 100;
           }
         }

         if(romawi >= 50){
           if(romawi >= 90){
             cout << "XC";
             romawi -= 90;
           }
           else{
             cout << "L";
             romawi -= 50;
           }
         }

         while(romawi >= 10){
           if(romawi >= 40){
             cout << "XL";
             romawi -= 40;
           }
           else{
             cout << "X";
             romawi -= 10;
           }
         }

         if(romawi >= 5){
           if(romawi==9){
             cout << "IX";
             romawi -= 9;
           }
           else{
             cout << "V";
             romawi -= 5;
           }
         }

         while(romawi >= 1){
           if(romawi==4){
             cout << "IV";
             romawi -= 4;
           }
           else{
             cout << "I";
             romawi -= 1;
           }
         }
    }
    cout << endl;
    system("pause");
}
