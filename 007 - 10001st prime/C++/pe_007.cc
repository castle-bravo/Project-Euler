/*
 * file:    pe_007.cc
 * title:   10001st prime
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 24, 2014
 * 
 * math:    This solution uses the sieve of Sundaram to find the nth
 *          prime p_n. The sieve is initialized using the fact that p_n
 *          is less than n*(ln(n) + ln(ln(n))).
 * 
 *          References:
 *          en.wikipedia.org/wiki/Sieve_of_Sundaram
 *          en.wikipedia.org/wiki/Prime_number_theorem
 *              #Approximations_for_the_nth_prime_number
 */

#include <cmath>
#include <iostream>
#include <vector>
using namespace std;

const int N = 10001; // Nth prime

int main()
{
  int max = floor(N*(log(N) + log(log(N))));
  vector<bool> marked(max/2, false);
  for (size_t i = 1; i < marked.size(); i++) {
    for (size_t j = 1; j <= i; j++) {
      size_t m = i + j + 2*i*j;
      if (m < marked.size()) {
        marked[m] = true;
      } else {
        break;
      }
    }
  }
  int n = 1;
  for (size_t m = 1; m < marked.size(); m++) {
    if (not marked[m]) n++;
    if (n == N) {
      cout << 2*m + 1 << endl;
      break;
    }
  }
  return 0;
}
