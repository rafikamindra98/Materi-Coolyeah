//PROGRAM nilai_xyz
#include<iostream>
using namespace std;
int main() {
	//DEKLARASI
	int x, y, z, n, hasil;
	//ALGORITMA
	cout << "(n merupakan suatu nilai yang merupakan penyelesaian persamaan x+2y+3z)" << endl;
	cout << endl;
	cout << "Masukkan nilai n = "; cin >> n;
	cout << endl;
	cout << "Semua bilangan bulat positif x, y, z: "<<endl;
	hasil = 0;
	for (x=0; x<=n; x++) {
		for (y=0; y<=n; y++) {
			for (z=0; z<=n; z++) {
				if (x+2*y+3*z==n) {
					hasil = hasil+1;
					cout << "x = " << x << endl;
					cout << "y = " << y << endl;
					cout << "z = " << z << endl;
					cout << endl;
				}
			}
		}
	} cout << "Banyaknya kemungkinan solusi untuk nilai x, y, z adalah " << hasil << endl;
	return 0;
}
