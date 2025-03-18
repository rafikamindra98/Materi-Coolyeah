//PROGRAM CekPrima 2
#include <iostream>
using namespace std;
int main (){
	//DEKLARASI
	int N, i;
	bool flag;
	//ALGORITMA
	cout << "Masukkan Bilangan: "; cin >> N;
	flag = false;
	for (i=2; i<=N-1; i++){
		if (N%i==0){
			flag = true;
		}
	}
	if (flag=true){
		cout << "Prima";
	}
	else
		cout << "Bukan Prima";
}
