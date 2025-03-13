#include <iostream>
using namespace std;
int main(){
	/*Program Pertukaran Konversi Suhu*/
	/* DEKLARASI */
	int Celcius,Reamur,Fahrenheit,Kelvin;
	/* ALGORITMA */
	cout<<"Celcius=";cin>>Celcius;
	Reamur=4*Celcius/5;
	Fahrenheit=9*Celcius/5+32;
	Kelvin=Celcius+273;
	cout<<"Reamur="<<Reamur<<endl;
	cout<<"Fahrenheit="<<Fahrenheit<<endl;
	cout<<"Kelvin="<<Kelvin<<endl;
}
