//PROGRAM Grade
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int nilai;
	//ALGORIMA
	cout<<"Masukkan Nilai: ";cin>>nilai;
	if (nilai>=80)
		cout<<"Nilai A"<<endl;
	else if (nilai>=70)
		cout<<"Nilai B"<<endl;
	else if (nilai>=60)
		cout<<"Nilai C"<<endl;
	else if(nilai>=50)
		cout<<"Nilai D"<<endl;
	else
		cout<<"Nilai E"<<endl;	
	return 0;
}
