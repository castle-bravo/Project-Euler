/*
 * file:    pe_055.cc
 * title:   Lychrel numbers
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 5, 2014
 */

#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

typedef vector<int8_t> vecint_t;

vecint_t from_int(int i);
void add_to(vecint_t &lhs, vecint_t &rhs);
bool is_palindromic(vecint_t &vi);

const int RANGE_END = 10000;
const int ITERATIONS = 50;

int main () {
  int lychrel_number_count = 0;
  for (int i = 1; i < RANGE_END; i++) {
    vecint_t vi = from_int(i);
    bool is_lychrel = true;
    for (int iteration = 0; iteration < ITERATIONS; iteration++) {
      vecint_t vi_reversed = vi;
      reverse(vi_reversed.begin(), vi_reversed.end());
      add_to(vi, vi_reversed);
      if (is_palindromic(vi)) {
        is_lychrel = false;
        break;
      }
    }
    if (is_lychrel) lychrel_number_count++;
  }
  cout << lychrel_number_count << endl;
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

bool is_palindromic(vecint_t &vi) {
  size_t half = vi.size()/2;
  for (struct { vecint_t::iterator f; vecint_t::reverse_iterator r; } 
       it = { vi.begin(), vi.rbegin() }; 
       (it.f != vi.begin() + half) && (it.r != vi.rbegin() + half); 
       ++it.f, ++it.r) {
    if (*it.f != *it.r) return false;
  }
  return true;
}
