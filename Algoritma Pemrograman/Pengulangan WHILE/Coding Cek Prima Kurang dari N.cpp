//PROGRAM CekPrima_KurangdariN
#include <iostream>
using namespace std;
int main () {
	//DEKLARASI
	int i, j, n;
	bool prima;
	//ALGORITMA
	cout << "Masukkan Nilai N= "; cin >> n;
	while (i<n) {
		prima = true;
		for (j=2; j<=(i/2); j++) {
			if (i%j==0) {
				prima = false;
			}
		}
		if (prima) {
			cout << (i+"");
		}
		i=i+1;
	}
}
