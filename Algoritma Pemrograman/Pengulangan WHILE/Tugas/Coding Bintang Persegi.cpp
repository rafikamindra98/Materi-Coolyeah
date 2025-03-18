//PROGRAM BintangPersegi
#include <iostream>
using namespace std;
int main () {
	//DEKLARASI
	int i, j, n;
	//ALGORITMA
	cout << "Masukkan Nilai N= "; cin >> n;
	
	i=1;
	while (i<=n) {
		j=1;
		while (j<=n) {
			cout << "*";
			j=j+1;
		}
	cout << endl;
	i=i+1;
	}
}
