/*
 * file:    pe_007.cc
 * title:   10001st prime
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 13, 2014
 */

#include <iostream>
#include <vector>
using namespace std;

const int prime_index = 10001;

int main()
{
  vector<bool> is_prime(1, false);
  is_prime.push_back(true);
  int prime_count = 2;
  for(int n = 2; true; n++) {
    is_prime.push_back(true);
    for(int m = 1; m*m < n; m++) {
      if(is_prime[m] == false) continue;
      if((2*n + 1)%(2*m + 1) == 0) {
        is_prime[n] = (false);
        break;
      }
    }
    if(is_prime[n]) prime_count++;
    if(prime_count == prime_index) break;
  }
  cout << 2*is_prime.size() - 1 << endl;
}
