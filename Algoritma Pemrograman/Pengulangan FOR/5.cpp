//PROGRAM DaftarBilanganKuadrat
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int i, n;
	
	//ALGORITMA
	cout << "Daftar Bilangan ="; cin >> n;
	cout << endl;
	
	for (i=1; i<=n; i++){
		cout << (i*(i+1))/2 <<endl;
	}
}
