/*
 * file:    pe_021.cc
 * title:   Amicable numbers
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 8, 2014
 */

#include <iostream>
#include <set>
#include <vector>
using namespace std;

const int N = 10000; // sum of amicable numbers less than N

int main() {
  int amicable_sum = 0;
  vector<int> divisor_sums({0, 1, 1});
  vector<int> primes({2}); // 2 is the first prime
  for (int i = 3; i <= N; i++) {
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
    if (divisors.size() == 1) { // i is prime
      primes.push_back(i);
    }
    divisors.erase(i);
    for (int divisor : divisors) {
      divisor_sum += divisor;
    }
    divisor_sums.push_back(divisor_sum);
    if ((divisor_sum < i) && (divisor_sums[divisor_sum] == i)) {
      amicable_sum += i + divisor_sum;
    }
  }
  cout << amicable_sum << endl;
  return 0;
}
