/*
 * file:    pe_014.cc
 * title:   Longest Collatz sequence
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 3, 2014 (modified)
 *          Original date unknown
 */

#include <array>
#include <vector>
#include <iostream>
using namespace std;

const unsigned int N = 999999;

int main() {
  array<int, N> seq_l;
  seq_l.fill(0);
  int max_l = 0;
  int n_max = 0;
  vector<int> add_l;
  vector<int>::iterator it;
  unsigned int i;
  unsigned long long int n;
  for (i = 1; i < N; i++) {
    if (seq_l[i] == 0) {
      n = i + 1;
      add_l.push_back(i);
      while (n > 1) {
        if (n % 2 == 0) {
          n = n/2;
        } else {
          n = 3*n + 1;
        }
        for (it = add_l.begin(); it < add_l.end(); ++it) {
          seq_l[*it]++;
        }
        if (n <= N) {
          if (seq_l[n-1] == 0) {
             add_l.push_back(n-1);
          } else {
            for (it = add_l.begin(); it < add_l.end(); ++it) {
              seq_l[*it] += seq_l[n-1];
            }
            break;
          }
        }
      }
      add_l.erase(add_l.begin(), add_l.end());
      if (seq_l[i] > max_l) {
        max_l = seq_l[i];
        n_max = i + 1;
      }
    } else continue;
  }
  cout << n_max << '\n';
  return 0;
}

