/*
 * file:    pe_048.cc
 * title:   Self powers
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 24, 2014
 * 
 * math:    This solution relies on the identity:
 *          a^b % n = (a % n)^b % n
 */

#include <iostream>

const uint64_t B_10 = 10000000000; // ten billion

using namespace std;
int main() {
  uint64_t self_power_sum = 0;
  for (size_t i = 1; i <= 1000; i++) {
    uint64_t power = i;
    for (size_t j = 1; j < i; j++) {
      power *= i;
      if (power >= B_10) {
        power %= B_10;
      }
    }
    self_power_sum += power;
    if (self_power_sum >= B_10) {
        self_power_sum %= B_10;
    }
  }
  cout << self_power_sum << endl;
}
