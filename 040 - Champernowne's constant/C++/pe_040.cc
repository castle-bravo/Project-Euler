/*
 * file:    pe_040.cc
 * title:   Champernowne's constant
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 15, 2014
 */

#include <cmath>
#include <iostream>
#include <vector>
using namespace std;

// subscripts of d are powers of 10.
const int MAX_EXPONENT = 6;

int main()
{
  int digit_product = 1;
  vector<int> digit_sum; // number of digits in numbers n such that 
                         // log_10(n) < i, where i is the index in
                         // the digit_sum vector
  digit_sum.push_back(1); // the number 0 has one digit
  for(int i = 0; digit_sum[i] < (int)pow(10, MAX_EXPONENT); i++) {
    digit_sum.push_back(digit_sum[i] + (int)(i+1)*9*pow(10, i));
  }
  for(int exponent = 0; exponent <= MAX_EXPONENT; exponent++) {
    int digit = (int)pow(10, exponent);
    for(size_t i = 0; i < digit_sum.size(); i++) {
      if(digit_sum[i+1] > digit) {
        digit -= digit_sum[i];
        int number = digit/(i+1) + (int)pow(10,i);
        string number_string = to_string(number);
        int d = (int)number_string[digit%(i+1)] - '0';
        digit_product *= d;
        break;
      }
    }
  }
  cout << digit_product << endl;
}
