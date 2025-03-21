//keluaran 9
#include<iostream>
using namespace std;
int main() {
	char i,j,k,l;
for (i='A';i<='D';i++)
	for (j='A';j<='D';j++)
		if (i!=j)
		for (k='A';k<='D';k++)
			if ((k!=i) and (k!=j))
			for (l='A';l<='D';l++)
				if ((l!=i)and(l!=j)and(l!=k))
					cout<<string(1,i)+string(1,j)+string(1,k)+string(1,l)<<endl;
				cin.get();
			}
