/*
 * file:    pe_066.cc
 * title:   Diophantine Equation
 * author:  Alexander Gosselin
 * e-mail:  <alexandergosselin@gmail.com>
 *          <alexander.gosselin@alumni.ubc.ca>
 * date:    September 1, 2015
 * 
 * license: GNU General Public License v3
 *          <http://www.gnu.org/licenses/gpl-3.0.html>
 */

#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

#include "uint128_t.hh"

const int N = 1000;

// isqrt(n) returns the integer square root of n.
// Reference:
//    <https://en.wikipedia.org/wiki/Integer_square_root>
uint isqrt(uint n) {
  uint x0 = n;
  uint x1 = (n + 1)/2;
  while (x0 > x1) {
    x0 = x1;
    x1 = (x0 + n/x0)/2;
  }
  return x0;
}

// is_square(n) returns true if n is a perfect square.
// taken with modification from:
//    <http://www.johndcook.com/blog/2008/11/17/
//        fast-way-to-test-whether-a-number-is-a-square/>
bool is_square(uint n) {
  char hex = n & 0xF; // least significant hex digit of n
  if (hex > 9) return false;
  if (hex == 0 || hex == 1 || hex == 4 || hex == 9) {
    uint root = isqrt(n);
    return root*root == n;
  }
  return false;
}

struct ContinuedFraction {
  uint a0;
  std::vector<uint> as;
  ContinuedFraction() {
    a0 = 0;
    as = std::vector<uint>();
  }
  ContinuedFraction(uint a) {
    a0 = a;
    as = std::vector<uint>();
  }
  ContinuedFraction(uint a, std::vector<uint> v) {
    a0 = a;
    as = v;
  }
};

// square_root(n) returns a continued fraction representation of the 
// square root of n. If n is a perfect square, the continued fraction
// won't have any periodic elements, i.e. period of 0.
// Reference:
//    <https://en.wikipedia.org/wiki/Methods_of_computing_square_roots
//        #Continued_fraction_expansion>
ContinuedFraction square_root(uint n) {
  if (is_square(n)) {
    return ContinuedFraction(isqrt(n));
  }
  uint m = 0;
  uint d = 1;
  uint a = isqrt(n);
  ContinuedFraction cf = ContinuedFraction(a);
  uint m1 = a*d;
  uint d1 = (n - m1*m1)/d;
  uint a1 = (a + m1)/d1;
  m = m1; 
  d = d1,
  a = a1;
  do {
    cf.as.push_back(a);
    m = d*a - m;
    d = (n - m*m)/d;
    a = (cf.a0 + m)/d;;
  } while (m != m1 || d != d1 || a != a1);
  return cf;
}


// solve_pell(n) returns the x value in the fundamental solution to
// the Pell equation:
//    x^2 - n*y^2 = 1
// References:
//  Hendrik Lenstra - Solving the Pell Equation
//    <http://www.math.leidenuniv.nl/~psh/ANTproc/01lenstra.pdf>
//  Recursive formula for the nth convergent of a continued fraction:
//    <https://en.wikipedia.org/wiki/Continued_fraction
//        #Infinite_continued_fractions>
uint128_t solve_pell(uint n) {
  ContinuedFraction cf = square_root(n);
  size_t period = cf.as.size();
  if (period == 0) return 0; // n is a perfect square
  if (period % 2 == 1) {     // repeat as
    for (size_t i = 0; i < period; i++) {
      cf.as.push_back(cf.as[i]);
    }
  }
  // recurrence relation for numerator of convergent of n
  uint128_t h0 = 1;
  uint128_t h1 = cf.a0;
  uint128_t h2;
  for (auto a = cf.as.begin(); a != cf.as.end() - 1; ++a) {
    h2 = uint64_t(*a)*h1 + h0;
    h0 = h1;
    h1 = h2;
  }
  return h2;
}

int main() {
  uint128_t x_max = 0;
  uint n_max = 0;
  for (uint n = 2; n <= N; n++) {
    uint128_t pell_n = solve_pell(n);
    if (pell_n > x_max) {
      x_max = pell_n;
      n_max = n;
    }
  }
  std::cout << n_max << std::endl;
  return 0;
}
