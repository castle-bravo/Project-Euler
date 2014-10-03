/*
 * file: pe_003.cc
 * title: Largest Prime Factor
 * author: Alexander Gosselin
 * e-mail: alexandergosselin@gmail.com
 * date: July 18, 2014
 */

#include <iostream>
using namespace std;

int main() 
{
    int64_t number = 600851475143;
    for(int divisor = 3; ; divisor += 2) {
        if(number == divisor) break;
        while(number % divisor == 0) {
            number /= divisor;
        }
    }
    cout << number << endl;
}
