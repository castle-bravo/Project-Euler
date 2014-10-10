/*
 * file:    pe_025.cc
 * title:   1000-digit Fibonacci number
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 9, 2014
 * 
 * math:    Binet's formula for the nth Fibonacci number:
 *            F_n = ((1 + sqrt(5))^n - (1 - sqrt(5))^n)/sqrt(5)
 * 
 *          For large n, we can ignore (1 - sqrt(5))^n, since 
 *          (1 - sqrt(6)) < 1. Then F_n is approximately equal to 
 *            (1 + sqrt(5))^n/(sqrt(5)*2^n)
 * 
 *          The number of digits in a number n is given by:
 *            N_digits = floor(log_10(n)) + 1
 * 
 *          So the number of digits in F_n is:
 *            floor(n(log_10(1 + sqrt(5)) - log_10(2)) 
 *                  - log_10(sqrt(5))) + 1
 * 
 *          Since we're after the term in the sequence that first
 *          generates 1000 digits, we have:
 *            n = ceil((1000 - 1 + log_10(sqrt(5)))/
 *                     (log_10(1 + sqrt(5)) - log_10(2)))
 * 
 *          The reason that we use the ceiling function instead of 
 *          floor is that the number inside the floor function is
 *          greater than 1000 - 1, so if we divide out the log terms,
 *          we'll get a number less than n.
 */

#include <cmath>
#include <iostream>
#include <vector>
using namespace std;

const int N = 1000; // first Fibonacci number with N digits

int main() {
  double n = ceil((1000 - 1 + log10(sqrt(5)))/
                  (log10(1 + sqrt(5)) - log10(2)));
  cout << n << endl;
  return 0;
}
