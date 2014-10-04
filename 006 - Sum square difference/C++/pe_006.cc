/*
 * file:    pe_006.cc
 * title:   Sum square difference
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 4, 2014
 * 
 * math:    The sum of numbers i from i = 1 to n has a closed form:
 *          sum(i, 1 -> n) = n(n + 1)/2, so
 *          sum(i, 1 -> n)^2 = n^2(n + 1)^2/4
 *                           = (n^4 + 2n^3 + n^2)/4
 *          This is also true for i^2 from i = 1 to n:
 *          sum(i^2, 1 -> n) = n(n + 1)(2n + 1)/6
 *          It follows that the sum square difference simplifies to: 
 *          sum(i, 1 -> n)^2 - sum(i^2, 1 -> n) = n(n^2 - 1)(3n + 2)/12
 */

#include <iostream>
using namespace std;

const int N = 100;

int main () {
  int sum_square_difference = N*(N*N - 1)*(3*N + 2)/12;
  cout << sum_square_difference << endl;
}
