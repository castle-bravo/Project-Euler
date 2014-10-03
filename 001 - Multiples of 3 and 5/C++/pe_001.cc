/*
 * file: pe_001.cc
 * title: Multiples of 3 and 5
 * author: Alexander Gosselin
 * e-mail: alexandergosselin@gmail.com
 * date: July 18, 2014
 * 
 */

#include <iostream>
using namespace std;

int main()
{
	int sum = 0;
	for(int f = 3; f < 1000; f += 3) {
		sum += f;
	}
	for(int f = 5; f < 1000; f += 5) {
		if(f % 3 == 0) continue;
		sum += f;
	}
	cout << sum << endl;
}
