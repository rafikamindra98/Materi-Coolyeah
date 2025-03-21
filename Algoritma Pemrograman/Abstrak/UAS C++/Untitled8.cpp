//keluaran 9
#include<iostream>
using namespace std;
int main() {
	int i, k, n, p;
	cout << "k = "; cin >> k;
	cout << "n = "; cin >> n;
	p = 1;
	i=1;
	while (i<=n) {
		p = p*k;
	}
	cout << p << endl;
}
//false karena tidak ada i=i+1
