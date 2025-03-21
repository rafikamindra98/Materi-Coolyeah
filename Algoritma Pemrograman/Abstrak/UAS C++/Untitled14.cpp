//prima
#include<iostream>
using namespace std;
int main() {
	int n, i, fak;
	cout << "n = "; cin >> n;
	fak = 1;
	for (i=1; i<=n; i++) {
		fak = fak*i;
	}
	cout << fak << endl;
}
