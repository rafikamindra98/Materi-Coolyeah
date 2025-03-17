//PROGRAM PersamaanKuadrat_Imajiner
#include <iostream>
#include <math.h>
using namespace std;
int main(){
	//DEKLARASI
	float a, b, c, D, p, q, x1, x2;
	//ALGORIMA
	cout<<"Format Persamaan: ax2 + bx + c = 0"<<endl;
	cout<<"Masukkan nilai a: ";cin>>a;
	cout<<"Masukkan nilai b: ";cin>>b;
	cout<<"Masukkan nilai c: ";cin>>c;
	
	cout<<endl;
	D = b*b-4*a*c;
	cout<<"Diskriminan= "<<D<<endl;
	
	if (D>0){
		cout<<"Akar real dan berbeda"<<endl;
		x1 = (-b + sqrt(D))/(2*a);
		x2 = (-b - sqrt(D))/(2*a);
		cout<<"x1= "<<x1<<endl;
		cout<<"x2= "<<x2<<endl;
	}
	else 
		if (D==0){
			cout<<"Akar real dan sama"<<endl;
			x1 = x2 = (-b + sqrt(D))/(2*a);
			cout<<"x1= "<<x1<<endl;
			cout<<"x2= "<<x2<<endl;
		}
		else {
			p = -b/(2*a);
			q = sqrt(-D)/(2*a);
			cout<<"x1= "<<p<<"+"<<1<<q<<"i"<<endl;
			cout<<"x2= "<<p<<"-"<<1<<q<<"i"<<endl;
		}
	return 0;
}
