/*
 * file:    pe_045.cc
 * title:   Triangular, pentagonal, hexagonal
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 23, 2014
 * 
 * math:    Triangular, pentagonal, and hexagonal numbers have the forms
 *          n(n + 1)/2, n(3n - 1)/2, and n(2n - 1) respectively.
 *          Every hexagonal number is also a triangular number, since
 *          n(2n - 1) = (2n-1)[(2n - 1) + 1]/2
 * 
 *          http://mathworld.wolfram.com/HexagonalNumber.html
 */

#include <cmath>
#include <iostream>

bool is_pentagonal(size_t);

const int min_hexagonal_index = 144;

using namespace std;
int main() {
  for (size_t n = min_hexagonal_index; true; n++) {
    size_t hexagonal = n*(2*n - 1);
    if (is_pentagonal(hexagonal)) {
      cout << hexagonal << endl;
      break;
    }
  }
}

bool is_pentagonal(size_t n) {
  double x = (1 + sqrt(1 + 24*n))/6.0;
  if (floor(x) == x) 
    return true;
  else 
    return false;
}
