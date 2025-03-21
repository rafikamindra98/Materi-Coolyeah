#include <iostream>
using namespace std;
int main(){
	/*Progrm Pertukaran*/
	/* DEKLARASI */
	int a, b;
	/* ALGORITMA */
	cout<<"A=";cin>>a;
	cout<<"B=";cin>>b;
	a=a+b;
	b=a-b;
	a=a-b;
	cout<<"Hasil pertukaran:"<<endl;
	cout<<"A="<<a<<endl;
	cout<<"B="<<b<<endl;
	cin.get();
}
