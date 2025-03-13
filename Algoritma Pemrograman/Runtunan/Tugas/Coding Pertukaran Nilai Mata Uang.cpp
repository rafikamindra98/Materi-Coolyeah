#include <iostream>
using namespace std;
int main(){
	/*Program Pertukaran Nilai Mata Uang*/
	/* DEKLARASI */
	int Rupiah,Dolar,Euro,Yen,Real;
	/* ALGORITMA */
	cout<<"Rupiah=";cin>>Rupiah;
	Dolar=Rupiah*0.065;
	Euro=Rupiah*0.061;
	Yen=Rupiah*8.82;
	Real=Rupiah*0.24;
	cout<<"Dolar="<<Dolar<<endl;
	cout<<"Euro="<<Euro<<endl;
	cout<<"Yen="<<Yen<<endl;
	cout<<"Real="<<Real<<endl;
}
