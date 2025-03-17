#include <iostream>
using namespace std;
int main() {
	/*PROGRAM NamaLengkap
	DEKLARASI*/
	string NamaDepan, NamaBelakang, NamaLengkap;
	//ALAGORITMA
	cout<<"Nama Depan=";cin>>NamaDepan;
	cout<<"Nama Belakang=";cin>>NamaBelakang;
	NamaLengkap=NamaDepan + NamaBelakang;
	cout<<"Nama Lengkap="+NamaLengkap<<endl;
	cin.get();
}
