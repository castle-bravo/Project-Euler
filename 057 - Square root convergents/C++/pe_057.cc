/*
 * file:    pe_057.cc
 * title:   Square root convergents
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 6, 2014
 * 
 * link:    https://projecteuler.net/problem=57
 * 
 * note:    I cheated a little an used the fact that the 1000th
 *          expansion of the numerator has 384 digits to reserve that
 *          much memory for the numerator and denominator vecints.
 *          The vecints were pushing back into random blocks of memory
 *          and corrupting themselsves, resulting in random values of
 *          size(). I didn't look any further into this, but it could
 *          be an issue in the future.
 */

#include <iostream>
#include <vector>
using namespace std;

const int MAX_EXPANSIONS = 1000;
const int MAX_DIGITS = 384;

typedef vector<int8_t> vecint_t;

vecint_t from_int(int i);
void multiply_by_2(vecint_t &vi);
void add_to(vecint_t &lhs, vecint_t &rhs);
void iterate(vecint_t &curr, vecint_t &prev);

int main () {
  int numerator_more_digits_count = 0;
  vecint_t numerator_curr = from_int(7);
  vecint_t numerator_prev = from_int(3);
  numerator_curr.reserve(MAX_DIGITS);
  numerator_prev.reserve(MAX_DIGITS);
  vecint_t denominator_curr = from_int(5);
  vecint_t denominator_prev = from_int(2);
  denominator_curr.reserve(MAX_DIGITS);
  denominator_prev.reserve(MAX_DIGITS);
  for (int expansion = 2; expansion <= MAX_EXPANSIONS; expansion++) {
    iterate(numerator_curr, numerator_prev);
    iterate(denominator_curr, denominator_prev);
    if (numerator_curr.size() > denominator_curr.size()) {
      numerator_more_digits_count++;
    }
  }
  cout << numerator_more_digits_count << endl;
  return 0;
}

vecint_t from_int(int i) {
  vecint_t vi;
  while (i > 0) {
    vi.push_back(i % 10);
    i /= 10;
  }
  return vi;
}

void multiply_by_2(vecint_t &vi) {
  for (vecint_t::iterator it = vi.begin(); it != vi.end(); 
       ++it) {
    *it += *it; 
  }
  for (vecint_t::iterator it = vi.begin(); it != vi.end() - 1; 
       ++it) {
    if (*it > 9) {
      *it %= 10;
      *(it + 1) += 1;
    } 
  }
  if (vi.back() > 9) {
    vi.back() %= 10;
    vi.push_back(1);
  }
  return;
}

void add_to(vecint_t &lhs, vecint_t &rhs) {
  while (lhs.size() < rhs.size()) {
    lhs.push_back(0);
  }
  for (struct { vecint_t::iterator l; vecint_t::iterator r; }
       it = { lhs.begin(), rhs.begin() }; it.l != lhs.end();
       ++it.l, ++it.r) {
    *it.l += *it.r;
  }
  for (vecint_t::iterator it = lhs.begin(); it != lhs.end() - 1; ++it ) 
  {
    if (*it > 9) {
      *it %= 10;
      *(it + 1) += 1;
    }
  }
  if (lhs.back() > 9) {
    lhs.back() %= 10;
    lhs.push_back(1);
  }
  return;
}

void iterate(vecint_t &curr, vecint_t &prev) {
  vecint_t next(curr);
  multiply_by_2(next);
  add_to(next, prev);
  prev = curr;
  curr = next;
  return;
}
