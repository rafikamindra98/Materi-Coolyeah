#include <iostream>
using namespace std;
int main(){
	/*Program Pertukaran*/
	/* DEKLARASI */
	int A, B, C, D;
	/* ALGORITMA */
	cout<<"A=";cin>>A;
	cout<<"B=";cin>>B;
	cout<<"C=";cin>>C;
	D=A;
	A=B;
	B=C;
	C=D;
	cout<<"Hasil pertukaran:"<<endl;
	cout<<"A="<<A<<endl;
	cout<<"B="<<B<<endl;
	cout<<"C="<<C<<endl;
}
