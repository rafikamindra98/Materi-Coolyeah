//PROGRAM Faktorial
#include <iostream>
using namespace std;
int main () {
	//DEKLARASI
	int i, n, fak;
	//ALGORITMA
	cout << "Masukkan Bilangan ="; cin >> n;
	i=1;
	fak=1;
	while (i<=n) {
		fak=fak*i;
		i=i+1;
	}
	cout << fak << endl;
	system ("pause");
}
