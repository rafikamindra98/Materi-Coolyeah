#include <iostream>
using namespace std;
int main(){
	//PROGRAM BiayaPenggunaan
	//DEKLARASI
	float BiayaPerPulsa=1000;
	int jam1, jam2, jam3, menit1, menit2, menit3, detik1, detik2, detik3, durasi1, durasi2, durasi3, sisa, biaya, menitpulsa;
	//ALGORITMA
	cout<<"Waktu Awal"<<endl;
	cout<<"Jam=";cin>>jam1;
	cout<<"Menit=";cin>>menit1;
	cout<<"Detik=";cin>>detik1;
	cout<<""<<endl;
	cout<<"Waktu Akhir"<<endl;
	cout<<"Jam=";cin>>jam2;
	cout<<"Menit=";cin>>menit2;
	cout<<"Detik=";cin>>detik2;
	durasi1 = (jam1*3600) + (menit1*60) + detik1;
	durasi2 = (jam2*3600) + (menit2*60) + detik2;
	durasi3 = durasi2 - durasi1;
	menitpulsa = durasi3/60;
	biaya = (menitpulsa/5)*BiayaPerPulsa+BiayaPerPulsa;
	jam3 = durasi3/3600;
	sisa = durasi3%3600;
	menit3 = sisa/60;
	detik3 = sisa%60;
	cout<<""<<endl;
	cout<<"Lama Penggunaan="<<jam3<<"Jam"<<menit3<<"Menit"<<detik3<<"Detik"<<endl;
	cout<<"Biaya="<<biaya<<endl;
} 
