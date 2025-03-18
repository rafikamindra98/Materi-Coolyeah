//PROGRAM DaftarBilanganKuadrat
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int i, n, x;
	
	//ALGORITMA
	cout << "Daftar Bilangan ="; cin >> n;
	cout << endl;
	
	x=0;
	for (i=1; i<=n; i++){
		x = x+i;
		cout << x <<endl;
	}
}
