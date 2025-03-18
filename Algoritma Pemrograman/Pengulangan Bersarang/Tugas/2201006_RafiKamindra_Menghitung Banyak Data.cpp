//PROGRAM BanyakJumlahRataMaxMin_Data
#include<iostream>
#include<climits>
using namespace std;
int main() {
	//DEKLARASI
	int x, jml, n, flag, min, max;
	float rata;
	
	//ALGORITMA
	cout << "Ketikkan angka '9999' untuk berhenti!" <<endl;
	cout << endl;
	cout << "Masukkan datanya: " << endl;
	x = 0;
	jml = 0;
	n = 0;
	flag = 9999;
	min = InT_MAX;
	max = InT_MIn;
	
	while (cin>>x and x!= flag) {
		n++;
		jml = jml+x;
		if (x>max) {
			max = x;} 
		if (x<min or n==0) {
			min = x;}
	} 
	
	if (n>0) {
		rata = jml/n;
        cout<<endl;
		cout<<"- Banyak data \t \t = "<<n<<endl;
        cout<<"- Jumlah data \t \t = "<<jml<<endl;
        cout<<"- nilai rata-rata \t = "<<rata<<endl;
        cout<<"- nilai maksimum \t = "<<max<<endl;
        cout<<"- nilai minimum \t = "<<min<<endl;
	}  else {
		cout<<"Tidak bisa diproses"<<endl;
	} 
	return 0;
} 
