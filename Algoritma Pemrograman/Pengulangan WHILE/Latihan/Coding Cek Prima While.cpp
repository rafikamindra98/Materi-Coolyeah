//PROGRAM CekPrima
#include <iostream>
#include <math.h>
using namespace std;
int main () {
	//DEKLARASI
	int n, i;
	string flag;
	//ALGORITMA
	cout << "Masukkan Bilangan ="; cin >> n;
	flag="Prima";
	i=2;
	while (i<=n-1) {
		if (n%i==0) {
			flag="Bukan Prima";
		i=i+1;
		}
	}
	cout << flag << endl;
	system ("pause");
}
