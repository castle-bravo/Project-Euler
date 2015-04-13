/*
 * file:    pe_079.cc
 * title:   Su Doku
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    April 12, 2015
 * 
 * link:    https://projecteuler.net/problem=79
 * 
 * algorithm:  This solution uses the Majority Merge algorithm, which
 *    returns an approximate solution to the minimal supersequence
 *    problem. See the reference for a brief description.
 * 
 * reference:  
 *    Kang Ning and Hong Wai Leong. "Towards A Better Solution to the
 *    Shortest Common Supersequence Problem: The Deposition and 
 *    Reduction Algorithm" BMC Bioinformatics, 2006.
 *    http://www.biomedcentral.com/1471-2105/7/S4/S12
 * 
 * copyright: (C) 2015 Alexander Gosselin
 * license:   GNU General Public License <http://www.gnu.org/licenses/>
 */

#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>
using namespace std;

int main() {
  vector<vector<char>> sequences;
  
  ifstream ifs("../keylog.txt");
  for (string line; getline(ifs, line); ) {
    vector<char> sequence;
    for (string::reverse_iterator rit = line.rbegin(); 
         rit != line.rend(); ++rit) {
      sequence.push_back(*rit);
    }
    sequences.push_back(sequence);
  }
  ifs.close();
  
  string supersequence;
  
  while (!sequences.empty()) {
    map<char, int> frontier;
    for (vector<vector<char>>::iterator i = sequences.begin();
         i != sequences.end(); ++i)  {
      frontier[(*i).back()]++;
    }
    map<char, int>::iterator frontier_max = frontier.begin();
    for (map<char, int>::iterator j = ++frontier.begin(); 
         j != frontier.end(); ++j) {
      if ((*j).second > (*frontier_max).second) {
        frontier_max = j;
      }
    }
    supersequence.push_back((*frontier_max).first);
    for (vector<vector<char>>::iterator i = sequences.begin();
         i != sequences.end(); ++i) {
      if ((*i).back() == (*frontier_max).first) {
        (*i).pop_back();
      }
    }
    for (vector<vector<char>>::reverse_iterator i = sequences.rbegin();
         i != sequences.rend(); ++i) {
      if ((*i).empty()) {
        // "How to call erase with a reverse iterator"
        // http://stackoverflow.com/a/1830240/2738025
        sequences.erase(next(i).base());
      }
    }
  }
  cout << supersequence << endl;
  return 0;
}
