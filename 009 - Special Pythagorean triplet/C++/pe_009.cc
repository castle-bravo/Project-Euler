/*
 * file:    pe_009.cpp
 * title:   Special Pythagorean triplet
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 3, 2014 (modified)
 *          Original date unknown
 * 
 * math:    given: 
 *            a < b < c, a^2 + b^2 = c^2
 *            a + b + c = 1000
 *          It follows that:
 *            c = 1000 - a - b
 *            a^2 + b^2 = (1000 - a - b)^2
 *            2000(a + b) - 2ab = 1000000 
 *          Subject to 
 *            2b < 1000 - a, and a < b
 *          So the maximum value of a is 332 when b = 333 and c = 335
 */

#include <iostream>
using namespace std;

int main(int argc, char **argv)
{
  int a, b, c;
  for (a = 1; a < 333; a++) {
    b = (1000000 - (2000*a))/(2000 - (2*a));
    c = 1000 - a - b;
    // check if (a,b,c) is a solution
    if (a*a + b*b == c*c) {
      cout << a*b*c << "\n";
      break;
    }
  }
  return 0;
}

