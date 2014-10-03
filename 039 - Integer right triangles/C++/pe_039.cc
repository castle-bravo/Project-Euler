/*
 * file:    pe_039.cc
 * title:   Integer right triangles
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 14, 2014
 * 
 * note:    This is a translation into C++ of Zach Denton's solution
 *          in Haskell available here:
 *          zacharydenton.com/project-euler-solutions/39/
 * 
 * math:    A Pythagorean triple is a right angle triangle with integer
 *          side lengths a, b, c, such that a^2 + b^2 = c^2. The
 *          perimeter of the right angle triangle p = a + b + c. Since
 *          (a, b, c) = (3, 4, 5) is identical to triangle (a, b, c) =
 *          (4, 3, 5), we enforce the condition that a < b < c. It 
 *          follows that a < p/4 for any given p.
 * 
 *          b can be calculated in terms of a and p.
 *          b = (b^2 + bc)/(b + c)
 *            = (b^2 + c^2 - a^2 + 2bc)/2(b + c)
 *            = (a + b + c)(b + c - a)/2(b + c)
 *            = p(p - 2a)/2(p - a)
 *
 *          This solution generates integers a, b for all p, and checks
 *          if c is also an integer.
 * 
 */

#include <cmath>
#include <iostream>
using namespace std;

const int max_p = 1000;

int main()
{
    int solutions [max_p+1] = {0}; 
    for(int p = 1; p <= max_p; p++) {
        for(int a = 1; a <= p/4; a++) {
            int b = p*(p-2*a)/(2*(p-a));
            int c = (int) sqrt(a*a + b*b);
            if(a + b + c == p) solutions[p]++;
        }
    }
    int max_solutions = 0;
    int maximizing_perimeter= 0;
    for(int p = 1; p <= max_p; p++) {
        if(solutions[p] > max_solutions) {
            max_solutions = solutions[p];
            maximizing_perimeter = p;
        }
    }
    cout << maximizing_perimeter << endl;
}
