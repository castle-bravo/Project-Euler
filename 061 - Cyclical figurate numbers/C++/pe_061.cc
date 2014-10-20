/*
 * file:    pe_061.cc
 * Title:   Cyclical figurate numbers
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 19, 2014
 * 
 * link:    https://projecteuler.net/problem=61
 */

#include <cmath>
#include <iostream>
#include <map>
#include <set>
#include <vector>
using namespace std;

int polygonal_number(int i, int n);
void find_cycle(vector<char> &cycle, char first, char link, 
                set<multimap<char, char>*> &to_check);

int main() {
  int polygonal_digit_cycle_sum = 0;
  vector<multimap<char, char>> polygonal_maps;
  for (int i = 0; i < 6; i++) {
    multimap<char, char> polygonal_map;
    for (int n = 1; true; n++) {
      int p = polygonal_number(i, n);
      int digits = floor(log10(p)) + 1;
      if (digits < 4) continue;
      if (digits > 4) break;
      div_t d = div(p, 100); // first two and last two digits of p
      if (d.rem < 10) continue; // no 4-digit number starts with 0
      polygonal_map.insert(pair<char, char>(d.quot, d.rem));
    }
    polygonal_maps.push_back(polygonal_map);
  }
  set<multimap<char, char>*> to_check;
  for (int i = 0; i < 5; i++) { // don't check octagonal numbers
    to_check.insert(&polygonal_maps[i]);
  }
  vector<char> cycle;
  for (auto octagonal : polygonal_maps[5]) { // start looking for cycle
                                             // from octagonal numbers
    find_cycle(cycle, octagonal.first, octagonal.second, to_check);
    if (not cycle.empty()) break;
  }
  for (auto link : cycle) {
    polygonal_digit_cycle_sum += 101*link; 
  }
  cout << polygonal_digit_cycle_sum << endl;
  return 0;
}

// This problem only requires triangular, square, pentagonal, hexagonal,
// heptagonal, and octagonal numbers. 
int polygonal_number(int i, int n) {
  switch (i) {
    case 0: // triangular
      return (n*(n + 1))/2;
    case 1: // square
      return n*n;
    case 2: // pentagonal
      return (n*(3*n - 1))/2;
    case 3: // hexagonal
      return n*(2*n - 1);
    case 4: // heptagonal
      return (n*(5*n - 3))/2;
    case 5: // octagonal
      return n*(3*n - 2);
  }
  return 0;
}

void find_cycle(vector<char> &cycle, char first, char link, 
                set<multimap<char, char>*> &not_checked) {
  if (not_checked.empty() && link == first) {
    cycle.push_back(link);
    goto pop;
  }
  for (auto polygon : not_checked) {
    pair<multimap<char, char>::iterator, 
         multimap<char, char>::iterator> next_link_range;
    next_link_range = (*polygon).equal_range(link);
    if (next_link_range.first == next_link_range.second) continue;
    set<multimap<char, char>*> to_check(not_checked);
    to_check.erase(polygon);
    for (multimap<char, char>::iterator it = next_link_range.first;
         it != next_link_range.second; ++it) {
      find_cycle(cycle, first, (*it).second, to_check);
      if (not cycle.empty()) {
        cycle.push_back(link);
        goto pop;
      }
    }
  }
  pop: return;
}
