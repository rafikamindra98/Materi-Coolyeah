/***PROGRAM MENENTUKAN BILANGAN GENAP***/
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int bilangan;
	//ALGORITMA
	cout<<"Masukkan suatu bilangan bulat: ";cin>>bilangan;
	if (bilangan%2==0)
	cout<<"Bilangan Genap."<<endl;
	else
	cout<<"Bilangan Ganjil."<<endl;
}
