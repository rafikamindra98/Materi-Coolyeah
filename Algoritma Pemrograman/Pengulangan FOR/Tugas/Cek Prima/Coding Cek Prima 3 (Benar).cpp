//PROGRAM CekPrima3
#include <iostream>
using namespace std;
int main (){
	//DEKLARASI
	int N, i;
	string flag;
	
	//ALGORITMA
	cout << "Masukkan Bilangan: "; cin >> N;
	flag = "Prima";
	for (i=2; i<=N-1; i++){
		if (N%i==0) {
			flag="Bukan Prima";
		}
	}
	cout << flag;
}
