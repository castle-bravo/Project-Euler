/*
 * file:    pe_052.cc
 * title:   Permuted multiples
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 1, 2014
 * 
 * math:    The first sixth of numbers in a range [0, 10^n - 1] yield
 *          a number with no more than n digits when multiplied by six. 
 *          For example, 6*1 = 6 has only 1 digit, but 6*2 = 12 has two.
 *          It follows that we can skip any integer i if:
 *            i >= (10^ceiling(log_10(i)))/6
 */

#include <algorithm>
#include <cmath>
#include <iostream>
using namespace std;

int main() {
  uint64_t cap = 1;
  for (uint64_t i = 1; true; i++) {
    if (i >= cap) {
      cap = static_cast<uint64_t>(pow(10, ceil(log10(i)) + 1)/6);
      i = static_cast<uint64_t>(pow(10, ceil(log10(i))));
      continue;
    }
    string digits_i = to_string(i);
    for (int j = 2; j <= 6; j++) {
      string digits_j = to_string(i*j);
      if (not is_permutation(digits_i.begin(), digits_i.end(),
                             digits_j.begin())) {
        break;
      } else if (j == 6) {
        cout << i << endl;
        return 0;
      }
    }
  }
}
