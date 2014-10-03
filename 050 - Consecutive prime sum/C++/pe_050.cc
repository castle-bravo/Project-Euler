/*
 * file:    pe_049.cc
 * title:   Consecutive prime sum
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 25, 2014
 */

#include <bitset>
#include <iostream>
#include <vector>

const int  PRIMES_BELOW = 1000000;

using namespace std;
int main() {
  // initialize a list of primes
  vector<int> primes (1, 2);
  // initialize and fill prime number sieve
  bitset<PRIMES_BELOW/2> not_prime; // odd numbers
  not_prime[0] = true; // 1 is not prime
  for (size_t i = 1; i < not_prime.size(); i++) {
    if (!not_prime[i]) {
      primes.push_back(2*i + 1);
      for (size_t j = 3*i + 1; j < not_prime.size(); 
        j += 2*i + 1) {
        not_prime[j] = true;
      }
    }
  }
  int max_sequence_length = 0;
  int max_sequence_prime = 2;
  for (vector<int>::iterator it_prime = primes.begin(); 
       it_prime != primes.end(); ++it_prime) {
    int consecutive_prime_sum = 0;
    vector<int>::iterator it_add = primes.begin();
    vector<int>::iterator it_subtract = primes.begin();
    while (consecutive_prime_sum != *it_prime) {
      while (consecutive_prime_sum < *it_prime) {
        consecutive_prime_sum += *it_add;
        ++it_add;
      }
      while (consecutive_prime_sum > *it_prime) {
        consecutive_prime_sum -= *it_subtract;
        ++it_subtract;
      }
    }
    if (consecutive_prime_sum == *it_prime) {
      int sequence_length = it_add - it_subtract;
      if (sequence_length > max_sequence_length) {
        max_sequence_length = sequence_length;
        max_sequence_prime = *it_prime;
      }
    }
  }
  cout << max_sequence_prime << endl;
}
