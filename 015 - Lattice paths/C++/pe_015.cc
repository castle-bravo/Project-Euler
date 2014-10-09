/*
 * file:    pe_015.cc
 * title:   Lattice paths
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 8, 2014
 * 
 * note:    This solution uses two vectors rather than a 20*40 array,
 *          but is probably not faster than the old version of the
 *          solution.
 */

#include <iostream>
#include <vector>
using namespace std;

const int L = 20; // side length of the square grid

int main() {
  vector<uint64_t> row(1, 1); // 0 choose 0 = 1
  row.reserve(2*L);
  for (int i = 1; i <= 2*L; i++) {
    vector<uint64_t> row_prev(row);
    for (size_t j = 1; j < row.size(); j++) {
      row[j] = row_prev[j] + row_prev[j - 1];
    }
    if (i % 2 == 0) {
      row.push_back(2*row_prev.back());
    }
  }
  cout << row.back() << endl;
  return 0;
}
