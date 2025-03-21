//fibonacci
#include<iostream>
using namespace std;
int main() {
	int n, a, b, c, i;
	cout << "n = "; cin >> n;
	a = 1;
	b = 1;
	for (i=3; i<=n; i++){
		cout << a << endl;
		cout << b << endl;
		c = a+b;
		cout << c << endl;
		a=b;
		b=c;
	}
}
//salah karena di deklarasi ngga ada i, i=3
