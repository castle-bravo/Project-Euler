/*
 * file:    pe_013.cc
 * title:   Large sum
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 3, 2014
 * 
 * note:    The largest sum of any two 10-digit numbers is 
 *          2*(10^10 - 1), and log_2(2*(10**10 - 1)) = ~34.2, so we
 *          will need a 64-bit integer to carry the sum. 
 * 
 * note 2:  Since there are 100 numbers to add, digits in the twelfth
 *          position will affect the value of the 10th digit of the
 *          sum, but digits after the 12th digit do not affect any of 
 *          the first 10 digits of the sum, so we can ignore them.
 *          
 * note 3:  When summing over 100 12-digit numbers, we get a 14-digit 
 *          number. We can divide this by 10000 to shave off the last
 *          4 digits.
 */

#include <fstream>
#include <iostream>
#include <string>
using namespace std;

const int SUM_DIGITS = 10;
const uint64_t B_10 = 10000000000; // 10 billion

int main() {
  uint64_t large_sum = 0;
  ifstream ifs ("../numbers.txt");
  for (string number; getline(ifs, number); ) {
    // use stoull instead of stoi to get 64-bit unsigned integers
    // See note 2 for explanation of + 2 in first argument of erase. 
    number.erase(number.begin() + SUM_DIGITS + 2, number.end());
    large_sum += stoull(number, nullptr);
  }
  ifs.close();
  large_sum /= 10000; // drop last 4 digits in sum (See note 3.)
  cout << large_sum << endl;
  return 0;
}
