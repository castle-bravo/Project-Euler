/*
 * file:    pe_096.cc
 * title:   Su Doku
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    April 9, 2015
 * 
 * link:    https://projecteuler.net/problem=96
 * 
 * references:
 *    Sudoku:
 *    http://en.wikipedia.org/wiki/Sudoku
 * 
 *    Exact Cover:
 *    http://en.wikipedia.org/wiki/Exact_cover
 * 
 *    Donald Knuth's Algorithm X:
 *    http://en.wikipedia.org/wiki/Knuth%27s_Algorithm_X
 * 
 *    Python Algorithm X implementation:
 *    http://www.cs.mcgill.ca/~aassaf9/python/algorithm_x.html
 * 
 * copyright: (C) 2015 Alexander Gosselin
 * license:   GNU General Public License <http://www.gnu.org/licenses/>
 */

#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <vector>
using namespace std;

bool solve(map<int, set<int>>* X, map<int, array<int, 4>>* Y,
           vector<int>* solution);
void select_row(map<int, set<int>>* X, map<int, array<int, 4>>* Y,
                vector<set<int>>* cs, int r);
void deselect_row(map<int, set<int>>* X, map<int, array<int, 4>>* Y,
                  vector<set<int>>* cs, int r);

// square boxes only
static const int BOX_SIZE = 3; // standard sudoku grid
static const int SIZE = BOX_SIZE*BOX_SIZE;

int main() {
  int top_left_sum = 0;
  
  // initialize the sparse matrix representation of the sudoku problem
  map<int, set<int>> C; // constraints/columns
  for (int i = 0; i < 4*SIZE*SIZE; i++) {
    C[i] = set<int>();
  }
  map<int, array<int, 4>> R; // subsets/rows
  for (int i = 0; i < SIZE*SIZE*SIZE; i++) {
    // i is the subset index and encodes location and number on grid
    int index = i/SIZE;
    int row = i/(SIZE*SIZE);
    int column = (i/SIZE) % SIZE;
    int box = BOX_SIZE*(row/BOX_SIZE) + column/BOX_SIZE;
    int value = i % SIZE;
    
    // there are 4 constaints satisfied by each number placement
    array<int, 4> subset;  
    // insert the keys of constraints that subset satisfies
    subset[0] = (index); // row-column
    subset[1] = (SIZE*SIZE + SIZE*row + value); // row-number
    subset[2] = (2*SIZE*SIZE + SIZE*column + value); // column-number
    subset[3] = (3*SIZE*SIZE + SIZE*box + value); // box-number
    
    R.insert(pair<int, array<int, 4>>(i, subset));
    
    for (auto c : subset) {
      C[c].insert(i);
    }
  }

  ifstream ifs("../sudoku.txt");
  
  string line;
  while (getline(ifs, line)) {
    if (line[0] == 'G') {
      map<int, set<int>> X = C;
      map<int, array<int, 4>> Y = R;
      vector<int> solution;
      for (int i = 0; i < SIZE; i++) {
        getline(ifs, line);
        for (int j = 0; j < SIZE; j++) {
          if (line[j] != '0') {
            int r = SIZE*SIZE*i + SIZE*j + line[j] - '1';
            solution.push_back(r);
            vector<set<int>> cs;
            select_row(&X, &Y, &cs, r);
          }
        }
      }
      
      solve(&X, &Y, &solution);
      sort(solution.begin(), solution.end());
      
      top_left_sum += 100*(solution[0] % SIZE + 1) 
                      + 10*(solution[1] % SIZE + 1) 
                      + solution[2] % SIZE + 1;

      // display solution
      /*for (size_t i = 0; i < solution.size(); i++) {
        if (i % 9 == 0) cout << endl;
        cout << solution[i] % 9 + 1 << ' ';
      } cout << endl; */
    }
  }
  ifs.close();
  cout << top_left_sum << endl;
  return 0;
}

bool solve(map<int, set<int>>* X, map<int, array<int, 4>>* Y,
           vector<int>* solution) {
  if ((*X).empty()) return true;
  
  // find the column with the minimum number of nonzero elements
  map<int, set<int>>::iterator c_min = (*X).begin();
  for (map<int, set<int>>::iterator c = ++(*X).begin();
       c != (*X).end(); ++c) {
    if ((*c).second.size() < (*c_min).second.size()) {
      c_min = c;
    }
  }
  
  set<int> c = (*c_min).second;
  
  // for each row pointed to by c_min, call solve recursively
  for (auto r : c) {
    (*solution).push_back(r);
    vector<set<int>> cs;
    select_row(X, Y, &cs, r);
    if (solve(X, Y, solution)) return true;
    deselect_row(X, Y, &cs, r);
    (*solution).pop_back();
  }
  return false;
}

void select_row(map<int, set<int>>* X, map<int, array<int, 4>>* Y,
                vector<set<int>>* cs, int r) {
  for (auto j : (*Y)[r]) {
    for (auto i : (*X)[j]) {
      for (auto k : (*Y)[i]) {
        if (k != j) (*X)[k].erase(i);
      }
    }
    (*cs).push_back((*X)[j]);
    (*X).erase(j);
  }
  return;
}

void deselect_row(map<int, set<int>>* X, map<int, array<int, 4>>* Y,
                  vector<set<int>>* cs, int r) {
  for (array<int, 4>::reverse_iterator j = (*Y)[r].rbegin();
       j != (*Y)[r].rend(); ++j) {
    (*X)[*j] = (*cs).back();
    (*cs).pop_back();
    for (auto i : (*X)[*j]) {
      for (auto k : (*Y)[i]) {
        if (k != *j) (*X)[k].insert(i);
      }
    }
  }
  return;
}
