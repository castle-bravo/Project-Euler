/*
 * file:    pe_069.cc
 * title:   Powerful digit counts
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 9, 2014
 * 
 * math:    1. No nth power of 10 or greater can have fewer than n + 1
 *          digits.
 *          2. The number of digits in a number n base b is given by:
 *            floor(log_b(n)) + 1
 *          3. log(b^e) = e*log(b)
 */

#include <cmath>
#include <iostream>
using namespace std;

int main() {
  int n_digit_nth_powers = 0;
  int bases[9] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  for (int base : bases) {
    for (int n = 1; floor(n*log10(base)) + 1 == n; n++) {
      n_digit_nth_powers++;
    }
  }
  cout << n_digit_nth_powers << endl;
  return 0;
}
