//PROGRAM Bintang_Diamond
#include <iostream>
using namespace std;
int main (){
	
	//DEKLARASI
	int n;
	
	//ALGORITMA
	cout << "Masukkan Nilai N="; cin >> n;
	
	//Bagian atas Diamond
	for (int a=1; a<=n; a++) {
		for (int b=n; b>a; b--) {
			cout << " ";
		}
		for (int c=1; c<=(2*a-1); c++) {
			cout << "*";
		}
		cout << endl;
	}
	
	//Bagian bawah Diamond
	for (int a=2; a<=n; a++) {
		for (int b=1; b<a; b++) {
			cout << " ";
		}
		for (int c=n; c>=(2*a-n); c--) {
			cout << "*";
		}
		cout << endl;
	}
	cout << endl;
}
