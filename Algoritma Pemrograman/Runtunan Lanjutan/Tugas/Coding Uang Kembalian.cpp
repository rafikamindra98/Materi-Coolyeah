#include <iostream>
using namespace std;
int main(){
	//PROGRAM UangKembalian
	//DEKLARASI
	int harga,bayar,kembali;
	int a,a1,b,b1,c,c1,d,d1,e,e1,f,f1;
	
	//ALGORITMA
	cout << "MASUKKAN JUMLAH HARGA : ";cin>>harga;
	cout << "MASUKKAN JUMLAH PEMBAYARAN : ";cin>>bayar;
	
	kembali = bayar - harga;
	
	cout << "" << endl;
	cout << "TOTAL KEMBALIAN : " << kembali << endl;
	cout << "PECAHAN KEMBALIAN : " <<  endl;
    
	a  = kembali%50000;
	a1 = kembali/50000;
	b  = a%20000;
	b1 = a/20000;
	c  = b%10000;
	c1 = b/10000;
	d  = c%5000;
	d1 = c/5000;
	e  = d%2000;
	e1 = d/2000;
	f  = e%1000;
	f1 = e/1000;
    
	cout << a1 << " Lembar Rp.50000" << endl;
	cout << b1 << " Lembar Rp.20000" << endl;
	cout << c1 << " Lembar Rp.10000" << endl;
	cout << d1 << " Lembar Rp.5000"  << endl;
	cout << e1 << " Lembar Rp.2000"  << endl;
    cout << f1 << " Lembar Rp.1000"  << endl;
    
    cin.get();
}
