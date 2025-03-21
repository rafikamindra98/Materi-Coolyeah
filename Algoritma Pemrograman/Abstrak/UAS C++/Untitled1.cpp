//PROGRAM Pytagoras
#include<iostream>
using namespace std;
int main() {
	//DEKLARASI
	int i, j, k, n;
	//ALGORITMA
	cout<<"Masukkan nilai N = ";cin>>n;
	cout<<endl;
	cout<<"Semua bilangan bulat positif x, y, z: "<<endl;
	for (i=1; i<=n; i++) {
		for (j=1; j<=n; j++) {
			for (k=1; k<=n; k++) {
				if (i*i+j*j==k*k) {
					cout<<"k = "<<k<<endl;
					cout<<"j = "<<j<<endl;
					cout<<"i = "<<i<<endl;
					cout<<endl;
				}
			}
		}
	}
	return 0;
}
//true
