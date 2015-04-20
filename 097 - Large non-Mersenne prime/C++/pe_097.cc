/*
 * file:    pe_97.cc
 * title:   Large non-Mersenne prime
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    April 20, 2015
 * 
 * link:    https://projecteuler.net/problem=97
 * 
 * note:    This solution uses bit shifting to repeatedly multiply the
 *  result by factors of 2. The modulo operator is only applied after
 *  the most significant bit of result has been shifted into the 64th
 *  bit of the integer.
 * 
 * reference:
 *  Modular Multiplication, Exponentiation:
 *  http://en.wikipedia.org/wiki/Modular_arithmetic
 *  http://en.wikipedia.org/wiki/Modular_exponentiation
 * 
 *  Find the log base 2 of an N-bit integer in O(lg(N)) operations :
 *  https://graphics.stanford.edu/~seander/bithacks.html#IntegerLog
 * 
 * copyright: (C) 2015 Alexander Gosselin
 * license:   GNU General Public License <http://www.gnu.org/licenses/>
 */

#include <iostream>
using namespace std;

const uint64_t bits[] = {0x8000000000000000,
                         0xC000000000000000,
                         0xF000000000000000,
                         0xFF00000000000000,
                         0xFFFF000000000000,
                         0xFFFFFFFF00000000};
const uint64_t shift[] = {1, 2, 4, 8, 16, 32};

int main() {
  uint64_t result = 28433;
  uint64_t exponent = 7830457;
  uint64_t modulus = 10000000000;
  while (exponent > 0) {
    for (int i = 5; i >= 0; i--) {
      if (!(result & bits[i]) && exponent >= shift[i]) {
        result <<= shift[i];
        exponent -= shift[i];
      }
    }
    result %= modulus;
  }
  result += 1;
  cout << result << endl;
  return 0;
}
