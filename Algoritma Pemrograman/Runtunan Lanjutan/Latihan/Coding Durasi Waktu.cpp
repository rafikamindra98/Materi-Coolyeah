////PROGRAM DurasiWaktu
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int DurasiDetik, jam, menit, detik, sisa;
	//ALGORITMA
	cout<<"Masukkan durasi waktu dalam detik=";
	cin>>DurasiDetik;
	jam=DurasiDetik/3600;
	sisa=DurasiDetik%3600;
	menit=sisa/60;
	detik=menit%60;
	cout<<jam<<"Jam"<<menit<<"Menit"<<detik<<"Detik"<<endl;
	system("pause");
}
