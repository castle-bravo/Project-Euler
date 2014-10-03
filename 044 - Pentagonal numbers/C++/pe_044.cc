/*
 * file:    pe_044.cc
 * title:   Pentagonal numbers
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 23, 2014
 */

#include <cmath>
#include <iostream>
#include <vector>

bool is_pentagonal(int);

using namespace std;
int main() {
  vector<int> pentagonal_numbers;
  for(int n = 1; true; n++) {
    pentagonal_numbers.push_back((n*(3*n-1))/2);
    vector<int>::reverse_iterator a = pentagonal_numbers.rbegin();
    for (++a; a != pentagonal_numbers.rend(); ++a) {
      //cout << pentagonal_numbers.back() << '\t' << *a << endl;
      vector<int>::iterator b = pentagonal_numbers.begin();
      for (vector<int>::iterator b = pentagonal_numbers.begin();
           b != pentagonal_numbers.end(); ++b) {
        if (*b == *a)
          break;
        int sum_ab = *a + *b;
        if (sum_ab > pentagonal_numbers.back()) 
          break;
        if (sum_ab == pentagonal_numbers.back()) {
          int difference_ab = *a - *b;
          if (is_pentagonal(*a - *b)) {
            cout << difference_ab << endl;
            goto end;
          }
        }
      }
    }
  }
  end:;
}

bool is_pentagonal(int n) {
  double x = (1 + sqrt(1 + 24*n))/6.0;
  if (floor(x) == x) 
    return true;
  else 
    return false;
}
