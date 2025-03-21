//prima
#include<iostream>
using namespace std;
int main() {
	string flag;
	int i, n;
	cout << "n = "; cin >> n;
	i = 2;
	while (n%i!=0){
		i=i+1;
	}
	if (i<n-1){
		flag = "bukan prima";
	}
	cout << n << "," << flag << endl;
}
//false karena ngga ada flag = prima
