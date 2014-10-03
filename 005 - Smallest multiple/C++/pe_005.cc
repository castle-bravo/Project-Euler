/*
 * file:    pe_005.cc
 * title:   Smallest multiple
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 29, 2014
 * 
 * math:    The smallest multiple of the numbers between 0 and 20 has
 *          a prime factorization such that each prime exponent is equal
 *          to the maximum exponent on that prime in the factorizations 
 *          of the numbers 1 to 20.
 */

#include <iostream>
using namespace std;

const int N = 20;

int main() {
  int smallest_multiple = 1;
  bool not_prime[N + 1] = {false}; // a prime sieve
  not_prime[0] = true;             // 0 is not prime
  not_prime[1] = true;             // 1 is not prime
  for (int i = 0; i <= N; i++) {
    // find the prime numbers and ignore composite numbers
    if (not not_prime[i]) {
      for (int c = 2*i; c <= N; c += i) {
        not_prime[c] = true;
      }
      // find largest power of prime i less than N
      int power = i;
      while (power*i <= N) {
        power *= i;
      }
      smallest_multiple *= power;
    }
  }
  cout << smallest_multiple << endl;
}
