#include <iostream>
using namespace std;
int main(){
	//PROGRAM UangKembalian
	//DEKLARASI
	int beli, bayar, kembali, sisa, jumlah;
	//ALGORITMA
	cout<<"Total Pembelian : ";cin>>beli;
	cout<<"Jumlah Pembayaran : ";cin>>bayar;
	kembali = bayar-beli;
	sisa = kembali;
	cout<<"\n";
	cout<<"Total Kembalian : "<<kembali<<endl;
	cout<<"Pecahan Kembalian\n";
	if(sisa>=50000){
		jumlah = sisa/50000;
		cout<<""<<jumlah;
		cout<<"Lembar 50.000\n";
		sisa = sisa%50000;
	}
	if(sisa>=20000){
		jumlah = sisa/20000;
		cout<<""<<jumlah;
		cout<<"Lembar 20.000\n";
		sisa = sisa%20000;
	}
	if(sisa>=10000){
		jumlah = sisa/10000;
		cout<<""<<jumlah;
		cout<<"Lembar 10.000\n";
		sisa = sisa%10000;
	}
	if(sisa>=5000){
		jumlah = sisa/5000;
		cout<<""<<jumlah;
		cout<<"Lembar 5.000\n";
		sisa = sisa%5000;
	}
	if(sisa>=2000){
		jumlah = sisa/2000;
		cout<<""<<jumlah;
		cout<<"Lembar 2.000\n";
		sisa = sisa%2000;
	}
	if(sisa>=1000){
		jumlah = sisa/1000;
		cout<<""<<jumlah;
		cout<<"Lembar 1.000\n";
		sisa = sisa%1000;
	}
}
