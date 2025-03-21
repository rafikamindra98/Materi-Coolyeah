#include<iostream>
using namespace std;
int main() {
	int flag, x, jml;
	flag = 999;
	cout << "x = "; cin >> x;
	jml=x;
	while (x!=flag) {
		cout << "x = "; cin >> x;
		jml=jml+x;
	}
	cout << jml << endl;
}
