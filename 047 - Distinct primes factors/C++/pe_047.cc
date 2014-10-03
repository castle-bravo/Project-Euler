/*
 * file:    pe_047.cc
 * title:   Distinct Primes Factors
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 24, 2014
 */

#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

void expand_sieve();
vector<int> factorize(int);
bool common_prime_exponent(vector<int> &, vector<int> &);
bool is_positive(int i) { return (i > 0); }

// initialize prime number sieve
vector<bool> is_prime;

int main() {
  is_prime.push_back(false); // 1 is not prime
  is_prime.push_back(true); // 3 is prime
  // initialize vector of factorizations
  vector<vector<int>> factorizations;
  int i;
  for (i = 1; factorizations.size() < 4; i++) {
    vector<int> factorization = factorize(i);
    if (count_if(factorization.begin(), factorization.end(),
                 is_positive) != 4) {
      factorizations.clear();
      continue;
    }
    //cout << i << '\t' << factorizations.size() << endl;
    for (size_t i = factorizations.size() - 1; i <= 0; i--) {
      if (common_prime_exponent(factorization, factorizations[i])) {
        factorizations.erase(factorizations.begin(), 
                             factorizations.begin() + i);
        break;
      }
    }
    factorizations.push_back(factorization);
  }
  cout << i - 4 << endl;
}

void expand_sieve() {
  for (size_t n = is_prime.size(); true; n++) {
    is_prime.push_back(true);
    for (size_t i = 0; i*i < n; i++) {
      if (!is_prime[i])
        continue;
      if ((2*n + 1)%(2*i + 1) == 0) {
        is_prime[n] = false;
        break;
      }
    }
    if (is_prime[n]) {
      break;
    }
  }
}

vector<int> factorize(int number) {
  vector<int> factorization(1, 0);
  // if number is known to be prime, fill in the factorization without
  // testing for divisibility
  if (is_prime[(number-1)/2]) {
    for (int i = 0; i < (number - 1) / 2; i++) {
      if(is_prime[i])
        factorization.push_back(0);
    }
    factorization.push_back(1);
    return factorization;
  }
  // otherwise, test number for divisibility with each prime and divide
  // it by that prime until it is no longer divisible by that prime
  while (number%2 == 0) {
    factorization[0] += 1;
    number /= 2;
  }
  for (size_t i = 0; i < is_prime.size(); i++) {
    if (!is_prime[i])
      continue;
    factorization.push_back(0);
    while (number%(2*i + 1) == 0) {
      factorization.back()++;
      number /= 2*i + 1;
    }
    // if the number has been divided down to 1, then it is factored
    if (number == 1)
      break;
    else if (i == is_prime.size() - 1)
      expand_sieve();
  }
  return factorization;
}

bool common_prime_exponent(vector<int> &prime_exponents_a, 
                           vector<int> &prime_exponents_b) {
  if (prime_exponents_a.size() < prime_exponents_a.size()) {
    for (size_t i = 0; i < prime_exponents_a.size(); i++) {
      if (prime_exponents_a[i] == 0)
        continue;
      if (prime_exponents_a[i] == prime_exponents_b[i])
        return true;
    }
  }
  else
      for (size_t i = 0; i < prime_exponents_b.size(); i++) {
      if (prime_exponents_b[i] == 0)
        continue;
      if (prime_exponents_b[i] == prime_exponents_a[i])
        return true;
    }
  return false;
}
