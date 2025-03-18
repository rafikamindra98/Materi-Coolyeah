#include <iostream>
using namespace std;
int main(){
  cout << "##  Program C++ Hitung Faktorial ##" << endl;
  cout << "===================================" << endl;
  cout << endl;
  
  //DEKLARASI
  int angka,hasil,i;
  
  //ALGORITMA
  cout << "Masukkan Angka: "; cin >> angka;
 
  hasil = 1;
  for(i=1;i<=angka;i++) {
    hasil = hasil * i;
  }
  cout << angka << "! = " << hasil << endl;
}
