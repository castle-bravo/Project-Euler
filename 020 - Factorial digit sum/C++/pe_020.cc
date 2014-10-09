/*
 * file:    pe_020.cc
 * title:   Factorial digit sum
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 8, 2014
 */

#include <iostream>
#include <vector>
using namespace std;

const int N = 100; // digit sum of N!

int main() {
  int factorial_digit_sum = 0;
  vector<int16_t> digits({1});
  digits.reserve(200);
  for (int16_t i = 2; i < N; i++) { // multiplication by 100 does not
                                    // increase the digit sum
    int j = i; 
    div_t d = div(j, 10);
    if (d.rem == 0) { // if i is a multiple of 10, use i/10
      j = d.quot;
    }
    for (vector<int16_t>::iterator it = digits.begin(); 
         it != digits.end(); ++it) {
      *it *= j;
    }
    for (vector<int16_t>::iterator it = digits.begin(); 
         it != digits.end() - 1; ++it) {
      d = div(*it, 10);
      if (d.quot > 0) {
        *(it + 1) += d.quot;
        *it = d.rem;
      }
    }
    while (digits.back() > 9) {
      d = div(digits.back(), 10);
      digits.back() = d.rem;
      digits.push_back(d.quot);
    }
  }
  for (int digit : digits) {
    factorial_digit_sum += digit;
  }
  cout << factorial_digit_sum << endl;
  return 0;
}
