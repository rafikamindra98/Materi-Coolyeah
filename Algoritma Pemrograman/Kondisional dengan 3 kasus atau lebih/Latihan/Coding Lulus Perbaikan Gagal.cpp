//PROGRAM LulusPerbaikanGagal
#include <iostream>
using namespace std;
int main(){
	//DEKLARASI
	int nilai;
	//ALGORITMA
	cout<<"Masukkan Nilai: ";cin>>nilai;
	if ((nilai>75)and(nilai<=100))
		cout<<"LULUS"<<endl;
	else 
		if ((nilai>=50)and(nilai<=75))
			cout<<"PERBAIKAN"<<endl;
	else
			cout<<"GAGAL"<<endl;
	return 0;
}
