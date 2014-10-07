/*
 * file:    pe_058.cc
 * title:   Square root convergents
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 6, 2014
 * 
 * link:    https://projecteuler.net/problem=58
 * 
 * math:    The diagonal values for each layer are given by:
 *            4i^2 - 2i + 1 + 2ij
 *          Where i is the index of the layer, and j is the corner
 *          of the spiral:
 *            1-0
 *            ---
 *            2-3
 *          Since the spiral starts at one and forms a square, it is
 *          clear that the bottom right element corner of the spiral
 *          will always be square and therefore composite.
 * 
 * note:    miller-rabin.h only supports checking the primality of
 *          numbers up to 2^32.
 */

#include <iostream>
#include "miller-rabin.h"
using namespace std;

int main() {
  float prime_count = 0;
  float corner_count = 2; // 1 is a corner for both diagonals
  for (uint32_t i = 1; ; i++) {
    uint32_t corner_0 = 4*i*i - 2*i + 1;
    uint32_t corners[3] = { corner_0, corner_0 + 2*i, corner_0 + 4*i };
    for (uint32_t corner : corners) {
      if (is_prime(corner)) {
        prime_count++;
        //cout << corner << endl;
      }
    }
    corner_count += 4;
    if (prime_count/corner_count < 0.1) {
      cout << 2*i + 1 << endl;
      break;
    }
  }
  return 0;
}
