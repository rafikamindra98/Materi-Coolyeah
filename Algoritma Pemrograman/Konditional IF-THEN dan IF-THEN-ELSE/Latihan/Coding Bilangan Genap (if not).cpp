//PROGRAM Genap_if_not
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int bilangan;
	//ALGORITMA
	cout<<"Masukkan suatu bilangan bulat: ";cin>>bilangan;
	if (not (bilangan%2==1))
	cout<<"Bilangan Genap"<<endl;
}
