//PROGRAM BilanganPrima_KurangdariN
#include <iostream>
using namespace std;
int main (){
	//DEKLARASI
	int n;
	//ALGORITMA
	cout << "Masukkan Nilai N: "; cin >> n;
	cout << "Bilangan Prima Kurang dari" << n << "adalah" << endl;
	for (int i=2; i<n; i++) {
		int mod = 0;
		//Menghitung Modulus
		for (int j=1; j<=i; j++) {
			if (i%j == 0) {
				mod++;
			}
		}
		//Menampilkan bilangan prima
		if (mod==2) {
			cout << i <<" ";
		}
	}
}
