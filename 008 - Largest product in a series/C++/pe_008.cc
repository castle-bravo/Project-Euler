/*
 * file:    pe_011.cc
 * title:   Largest product in a grid
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 2, 2014
 *          October 3, 2014 (modified)
 * 
 * note:    The largest possible product of 13 consecutive digits
 *          in a decimal number is 9^13. log_2(9^13) = ~41.2, so we
 *          will need to use a 64-bit integer to represent the product.
 * 
 * note 2:  Since some of the elements in the series are 0, the product
 *          of the digits in the 13-digit subseries will be 0 if any of
 *          the elements are 0. This means that if we see a 0, we can
 *          skip ahead by 13 digits, but we have to cache the nonzero
 *          digits that we skip over.
 * 
 * note 3:  I submitted this code to codereview.stackexhange.com:
 *          http://codereview.stackexchange.com/q/64615/54309
 *          Jamal corrected my use of while (stream.good()), and
 *          vnp suggested that I refactor the product loop. Thanks!
 * 
 * note 4:  When defining the function window_product, I tried to
 *          execute a ranged for loop on an array passed to the 
 *          function. While this works in the main function, the 
 *          compiler throws an error when the array is passed to the
 *          function as a pointer. I asked for help on stackoverflow:
 *          http://stackoverflow.com/q/26182907/2738025
 *          Apparently this was due to array decaying into a pointer.
 *          Marco A. answered my question and gave me the following
 *          template function to execute a range based for loop on an
 *          array of any size:
 * 
 *            template <std::size_t array_size>
 *            void foo(int (&bar)[array_size]) {
 *              for (int i : bar) {
 *                cout << i << endl;
 *              }
 *            }
 */

#include <fstream>
#include <iostream>
using namespace std;

const size_t DIGITS_IN_PRODUCT = 13;
const size_t DIGITS_IN_NUMBER = 1000;

void refill_window(string &series, size_t &i, uint8_t* window);
uint64_t window_product(uint8_t (&window)[DIGITS_IN_PRODUCT]);

int main() {
  // read series.txt into a string without newline characters
  ifstream ifs("../series.txt");
  string series, line;
  while (getline(ifs, line)) {
    series += line;
  }
  ifs.close();
  uint8_t window[DIGITS_IN_PRODUCT];
  for (size_t i = 0; (i < DIGITS_IN_PRODUCT) && (i < DIGITS_IN_NUMBER);
       i++) {
    if (series[i] == '0') {
      refill_window(series, i, window);
      break;
    }
    window[i] = series[i] - '0';
  }
  uint64_t product = window_product(window);
  uint64_t max_product = product;
  for (size_t i = DIGITS_IN_PRODUCT; i < DIGITS_IN_NUMBER; i++) {
    if (series[i] == '0') {
      refill_window(series, i, window);
      product = window_product(window);
    } else {
      product /= window[i % DIGITS_IN_PRODUCT];
      window[i % DIGITS_IN_PRODUCT] = series[i] - '0';
      product *= window[i % DIGITS_IN_PRODUCT];
    }
    if (product > max_product) {
      max_product = product;
    }
  }
  cout << max_product << endl;
  return 0;
}

void refill_window(string &series, size_t &i, uint8_t* window) {
  i++;
  for (size_t j = 0;
       (j < DIGITS_IN_PRODUCT) && (i < DIGITS_IN_NUMBER); j++, i++) {
    if (series[i] == '0') {
      refill_window(series, i, window);
      return;
    }
    window[i % DIGITS_IN_PRODUCT] = series[i] - '0';
  }
  i--; // correct for final increment in for loop.
}

uint64_t window_product(uint8_t (&window)[DIGITS_IN_PRODUCT]) {
  uint64_t product = 1;
  for (uint8_t element : window) {
    product *= element;
  }
  return product;
}
