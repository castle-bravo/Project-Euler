/*
 * file:    pe_065.cc
 * title:   Convergents of e
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    November 14, 2014
 */

#include <iostream>
#include <vector>
#include <boost/multiprecision/cpp_int.hpp> 
using namespace std;

int N = 100; // find numerator of Nth convergent

int main() {
  int numerator_digit_sum = 0;
  vector<uint128_t> continued_fraction({2});
  continued_fraction.reserve(N);
  for (int i = 1, k = 1; i < N; i++) {
    if (i % 3 == 2) {
      continued_fraction.push_back(2*k);
      k++;
    } else {
      continued_fraction.push_back(1);
    }
  }
  continued_fraction.push_back(1);
  for (vector<int>::reverse_iterator rit = 
           continued_fraction.rbegin() + 2;
       rit != continued_fraction.rend(); ++rit) {
    *rit *= *(rit - 1);
    *rit += *(rit - 2);
  }
  string digits = to_string(continued_fraction[0]);
  for (char d : digits) {
    numerator_digit_sum += d - '0';
  }
  cout << numerator_digit_sum << endl;
  return 0;
}
