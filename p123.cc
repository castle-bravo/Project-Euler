/*
file:   p123.cc
title:  Prime square remainders
date:   August 9, 2017
author: Alexander Gosselin
e-mail: <alexandergosselin@gmail.com>
        <alexander.gosselin@alumni.ubc.ca>

link:   <https://projecteuler.net/problem=123>

explanation:
  Applying the binomial theorem [0] yields:
    (p + 1)^n = p^n + n*p^{n-1} + ... + n*p + 1
    (p - 1)^n = p^n - n*p^{n-1} + ... - n*p + 1 (if n is even)
              = p^n - n*p^{n-1} + ... + n*p - 1 (if n is odd)
  So the sum of the two above terms is:
    (p + 1)^n + (p - 1)^n = 
        2*p^n + n*(n - 1)*p^{n - 2} + ... + 2     (if n is even)
	2*p^n + n*(n - 1)*p^{n - 2} + ... + 2*n*p (if n is odd)
  Where all terms except the rightmost contain p^2 as a factor.
  Since p^2 is a factor, each of these terms is congruent to 0 modulo p^2 [1].
  This implies that the remainder of the above expression divided by p^2 is
  2 when n is even and congruent to 2*n*p when n is odd.
  Here, n is equal to the prime counting function [2] evaluated at p. The prime
  counting function is well approximated by p/ln(p), so we can say that: 
    2*n*p < p^2 for all p > e^2 > 7
  This means that the remainder isn't just congruent to 2*n*p, it is exactly
  2*n*p, all we need to do is find the smallest odd n for which 2*n*p_n is 
  greater than some given N.
  
references:
[0]: <https://en.wikipedia.org/wiki/Binomial_theorem>
[1]: <https://en.wikipedia.org/wiki/Modular_arithmetic>
[2]: <https://en.wikipedia.org/wiki/Prime-counting_function>
*/

#include <iostream>
#include <vector>
using namespace std;

const size_t N = 10000000000;

void extend_primes(vector<size_t>&);

int main() { // solution in terms of N
  auto primes = vector<size_t>({2,3,5});
  auto p = primes.back();
  auto n = primes.size();
  while (p*p < N) {
    extend_primes(primes);
    extend_primes(primes);
    p = primes.back();
  }
  while (2*p*n < N) {
    extend_primes(primes);
    extend_primes(primes);
    p = primes.back();
    n = primes.size();
  }
  //cout << p << endl;
  cout << n << endl; 
}

void extend_primes(vector<size_t>& primes) { // trial division
  for (auto q = primes.back() + 2; true; q += 2) {
    for (auto p : primes) {
      if (p*p > q) {
	primes.push_back(q);
	return;
      }
      if (q % p == 0) break;
    }
  }
}
