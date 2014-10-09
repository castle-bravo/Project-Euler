/*
 * file:    pe_021.cc
 * title:   Amicable numbers
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 8, 2014
 * 
 * note:    Use of array<bool> shaves 10-15ms off of the execution time
 *          of this code over vector<bool> for non_abundant_sum.
 */

#include <array>
#include <iostream>
#include <set>
#include <vector>
using namespace std;

const int MAX = 28123; // largest number not a sum of abundant numbers

int main() {
  uint64_t non_abundant_sum = 0;
  //vector<bool> not_abundant_sum(MAX, true);
  array<bool, MAX> not_abundant_sum;
  not_abundant_sum.fill(true);
  vector<int> abundant;
  vector<int> primes({2}); // 2 is the first prime
  for (int i = 3; i < MAX; i++) { // collect abundant numbers
    int n = i;
    int divisor_sum = 0;
    set<int> divisors({1}); // 1 is guaranteed to be a proper divisor
    for (vector<int>::iterator it = primes.begin(); 
         (*it <= n) && (it != primes.end()); ++it) {
      while (n % (*it) == 0) {
        // make a copy of divisors to avoid adding infinite powers of 
        // the divisors to the set of divisors
        vector<int> known_divisors(divisors.begin(), divisors.end());
        for (int divisor : known_divisors) {
          divisors.insert((*it) * divisor);
        }
        n /= (*it);
      }
    }
    divisors.erase(i);
    for (int divisor : divisors) {
      divisor_sum += divisor;
    }
    if (divisor_sum == 1) { // i is prime
      primes.push_back(i);
    }
    if (divisor_sum > i) { // i is abundant
      abundant.push_back(i);
    }
  }
  for (vector<int>::iterator it1 = abundant.begin(); 
       it1 != abundant.end(); ++it1) {
    for (vector<int>::iterator it2 = it1; it2 != abundant.end(); ++it2) 
    {
      int abundant_sum = *it1 + *it2;
      if (abundant_sum < MAX) {
        not_abundant_sum[abundant_sum] = false;
      } else {
        break;
      }
    }
  }
  for (size_t i = 1; i < not_abundant_sum.size(); i++) {
    if (not_abundant_sum[i]) non_abundant_sum += i;
  }
  cout << non_abundant_sum << endl;
  return 0;
}
