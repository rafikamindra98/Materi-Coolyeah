//PROGRAM DurasiDetik
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int jam, menit, detik, DurasiDetik;
	//ALGORITMA
	cout<<"Masukkan durasi waktu"<<endl;
	cout<<"Jam=";cin>>jam;
	cout<<"Menit=";cin>>menit;
	cout<<"Detik=";cin>>detik;
	DurasiDetik=jam*3600+menit*60+detik;
	cout<<"Durasi dalam detik=" <<DurasiDetik<<endl;
	cin.get();
}
