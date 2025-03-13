//PROGRAM DurasiAntara
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int Jam1,Jam2,Menit1,Menit2,Detik1,Detik2,Durasi1,Durasi2,DurasiAkhir,Jam,Menit,Detik,Sisa;
	//ALGORITMA
	cout<<"Durasi Awal"<<endl;
	cout<<"Jam=";cin>>Jam1;
	cout<<"Menit=";cin>>Menit1;
	cout<<"Detik=";cin>>Detik1;
	cout<<""<<endl;
	cout<<"Durasi Akhir"<<endl;
	cout<<"Jam=";cin>>Jam2;
	cout<<"Menit=";cin>>Menit2;
	cout<<"Detik=";cin>>Detik2;
	Durasi1 = (Jam1*3600) + (Menit1*60) + Detik1;
	Durasi2 = (Jam2*3600) + (Menit2*60) + Detik2;
	DurasiAkhir=Durasi2-Durasi1;
	Jam=DurasiAkhir/3600;
	Sisa=DurasiAkhir%3600;
	Menit=Sisa/60;
	Detik=Sisa%60;
	cout<<"Durasi Antara="<<Jam<<"Jam"<<Menit<<"Menit"<<Detik<<"Detik"<<endl;
}
