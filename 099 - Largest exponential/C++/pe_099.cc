/*
 * file:    pe_97.cc
 * title:   Large non-Mersenne prime
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    April 20, 2015
 * 
 * link:    https://projecteuler.net/problem=99
 * 
 * math:    If a^b > p^q, then log(a^b) > log(p^q), since log is a
 *  monotonically increasing function. log(a^b) = b*log(a), which we
 *  can easily compute.
 * 
 * copyright: (C) 2015 Alexander Gosselin
 * license:   GNU General Public License <http://www.gnu.org/licenses/>
 */

#include <cmath>
#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main() {
  int max_line = 0;
  double max_value = 0;
  ifstream ifs("../base_exp.txt");
  for (struct { int i; string s; } line = { 1, "" }; 
       getline(ifs, line.s); line.i++) {
    int comma = line.s.find(',');
    double base = stod(line.s.substr(0, comma));
    double exponent = stod(line.s.substr(comma + 1, line.s.length()));
    double value = log2(base)*exponent;
    if (value > max_value) {
      max_value = value;
      max_line = line.i;
    }
  }
  ifs.close();
  cout << max_line << endl;
  return 0;
}
