//PROGRAM AkarReal
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int a,b,c,D;
	//ALGORITMA
	cout<<"ax2+bx+c=0"<<endl;
	cout<<"a: ";cin>>a;
	cout<<"b: ";cin>>b;
	cout<<"c: ";cin>>c;
	D=b*b-4*a*c;
	if (D>=0){
		cout<<"Memiliki akar real"<<endl;
		cout<<"Diskriminan: "<<D<<endl;
	}
	else 
	cout<<"Tidak memiliki akar real"<<endl;
}
