//keluaran 9
#include<iostream>
using namespace std;
int main() {
	int i, k, n, p;
	cout << "k = "; cin >> k;
	cout << "n = "; cin >> n;
	p = 1;
	for (i=1; i<=n; i++) {
		p = p*k;
	}
	cout << p << endl;
}
//true
