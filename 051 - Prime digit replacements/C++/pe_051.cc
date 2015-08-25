/*
 * file:    pe_051.cc
 * title:   Prime digit replacements
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 *          alexander.gosselin@alumni.ubc.ca
 * date:    August 21, 2015
 * 
 * link:    https://projecteuler.net/problem=51
 * 
 * math:
 *   By the rule of 3 and 9, if the digit sum of a number is divisible
 *   by 3, then so is the number itself.
 * 
 * license: GNU General Public License (v3)
 *          <http://www.gnu.org/licenses/gpl-3.0.en.html>
 */

#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>
using namespace std;

const int N = 1000000; // generate primes up to N

int main() {
  
  // Sieve of Eratosthenes
  vector<bool> is_prime((N - 1)/2, true); // odd numbers
  is_prime[0] = false;                    // 1 is not prime
  vector<size_t> primes;
  for (uint i = 1; i < is_prime.size(); i++) {
    if (is_prime[i]) {
      size_t p = 2*i + 1;
      if (p > N/10) primes.push_back(p); // keep primes with 6 digits
      for (size_t j = i*p + i; j < is_prime.size(); j += p) {
        is_prime[j] = false;
      }
    }
  }

  // find 8 prime value family
  for (auto p : primes) {
    string prime_string = to_string(p);
    string digit_string(prime_string.length(), '0');
    for (int i = 0; i <= 2; i++) {       // at least 3 of 0, 1, or 2
      if (count(prime_string.begin(), prime_string.end(), 
          i + '0') == 3) {
        for (size_t j = 0; j < prime_string.length(); j++) {
          if (prime_string[j] == i + '0') digit_string[j] = '1';
        }
        int digits = stoi(digit_string);
        int family_size = 1;
        int composites = i;
        for (int k = 1; composites < 3 && k <= 9 - i; k++) {
          size_t candidate_prime = p + digits*k;
          if (binary_search(primes.begin(), primes.end(),
              candidate_prime)) {
            family_size++;
          } else {
            composites++;
          }
        }
        if (family_size == 8) {
          cout << p << endl;
          return 0;
        }
      }
    }
  }
  return 0;
}

