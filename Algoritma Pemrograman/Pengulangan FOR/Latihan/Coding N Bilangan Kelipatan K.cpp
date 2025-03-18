//PROGRAM N_Kelipatan_K
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int i, n, k;
	
	//ALGORITMA
	cout << "Masukkan Bilangan ="; cin >> k;
	cout << "Banyak Kelipatan ="; cin >> n;
	
	for(i=1; i<=n; i++){
		cout << i*k << endl;
	}
}
