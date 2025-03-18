//PROGRAM CekPrima1
#include <iostream>
using namespace std;
int main (){
	//DEKLARASI
	int N, i, flag;
	//ALGORITMA
	cout << "Masukkan Bilangan: "; cin >> N;
	flag = 0;
	for (i=2; i<=N-1; i++) {
		if (N%i==0) {
			flag = 1;
		}
	}
	if (flag = 0) {
		cout << "Prima";
	}
	else 
		cout << "Bukan Prima";
} 
