//PROGRAM BintangDiamond
#include <iostream>
using namespace std;
int main () {
	//DEKLARASI
	int i, j, n;
	//ALGORITMA
	cout << "Masukkan Nilai N (Bilangan Ganjil)= "; cin >>n;
	
	while (n%2==0) {
		cout << "N harus bilangan ganjil, silakan coba lagi";
		cout << "Masukkan Nilai N (Bilangan Ganjil): "; cin >> n;
	}
	
	i=1;
	while (i<=n) {
		//Bagian atas Diamond
		if (i<=(n/2)+1) {
			j=1;
			while (j<=n) {
				if ((j>=(n/2)+2-i) and (j<=(n/2)+1)) {
					cout << "*";}
					else
						cout << " ";
				}
				j=j+1;
			}
			//Bagian bawah Diamond
			else
			j=1;
			while (j<=n) {
				if ((j>=(n/2)) and (j<=(n-(i-(n/2))))) {
					cout << "*";}
				else
					cout << " ";
				}
				j=j+1;
	}
	cout << endl;
	i=i+1;
}
