//PROGRAM InputNamaArray
#include<iostream>
using namespace std;
int main() {
	//DEKLARASI
	string nama, flag, cari;
	int hasilcari;
	string A[100];
	//ALGORITMA
	//INPUT NAMA
	cout<<"Masukkan nama: "<<endl;
	cout<<"(tuliskan 'stop' untuk berhenti)"<<endl;
	flag = "stop";
	int n = 0;
	while (nama != flag) {
		n++;
		A[n] = nama;
		cout<<n<<". "; cin>>nama;
	}	cout<<endl;
	
	//SEARCHING DATA
	cout<<"Nama yang dicari adalah "; cin>>cari;
	for (int a = 1; a <= n; a++) {
		if(cari == A[a]) {
			hasilcari++;
		}
	}
	
	//TAMBAH DATA
	if(hasilcari==0) {
		cout<<endl;
		cout<<"Nama tidak ditemukan."<<endl;
		cout<<"Nama "<<cari<<" akan ditambahkan ke dalam data."<<endl;
		A[n+1] = cari;
		for(int a = 1; a <= n+1; a++) {
			cout<<A[a]<<endl;
		}
	} else {
		cout<<"Nama sudah ada dalam daftar."<<endl;
	}
}
