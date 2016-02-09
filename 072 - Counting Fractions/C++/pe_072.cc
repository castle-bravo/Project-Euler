/*
file:   pe_072.hs
title:  Counting fractions
author: Alexander Gosselin
e-mail: alexandergosselin@gmail.com
        alexander.gosselin@alumni.ubc.ca
date:   February 8, 2016

link:   <https://projecteuler.net/problem=72>

copyright:  (C) 2016 Alexander Gosselin
license:    GNU General Public License v3.0
            <http://www.gnu.org/licenses/gpl-3.0.en.html>
*/

#include <iostream>
#include <vector>
using namespace std;

static const int N = 1000000;

int main() {
  unsigned long int totient_sum = 0;
  vector<int> sieve; // sieve generating the totients
  for (int i = 0; i <= N; sieve.push_back(i++));
  for (int i = 2; i <= N; i++) {
    if (i == sieve[i]) { // i is prime
      sieve[i] -= 1;
      for (int j = 2*i; j <= N; j += i) {
        sieve[j] /= i;
        sieve[j] *= i - 1;
      }
    }
    totient_sum += sieve[i];
  }
  cout << totient_sum << endl;
  return 0;
}

