//PROGRAM TrafficLightB
#include <iostream>
using namespace std;
int main () {
	//DEKLARASI 
	int LampuMenyala, LampuMerah;
	//ALGORITMA
	cout<<"Ketik 1 untuk YA dan 0 untuk TIDAK"<<endl;
	cout<<"Lampu Menyala ="; cin>>LampuMenyala;
	
	if (LampuMenyala==1) {
		cout<<"Lampu Merah ="; cin>>LampuMerah;
		if (LampuMerah==1) 
			cout<<"Berhenti"<<endl;
	}
	else 
		cout<<"Jalan"<<endl;
}
	
