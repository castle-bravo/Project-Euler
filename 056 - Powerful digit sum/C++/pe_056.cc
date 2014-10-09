/*
 * file:    pe_055.cc
 * title:   Powerful digit sum
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 5, 2014
 * 
 */

#include <iostream>
#include <vector>
using namespace std;

const int RANGE_END = 100;

typedef vector<int8_t> vecint_t;

vecint_t from_int(int i);
void multiply(vecint_t &a, vecint_t &b);
int digit_sum(vecint_t &vi);

int main() {
  int max_digit_sum = 0;
  for (int a = 2; a < RANGE_END; a++) {
    vecint_t a_digits = from_int(a);
    vecint_t power_digits(a_digits);
    for (int b = 2; b < RANGE_END; b++) {
      multiply(power_digits, a_digits);
      int power_digit_sum = digit_sum(power_digits);
      if (power_digit_sum > max_digit_sum)
        max_digit_sum = power_digit_sum;
    }
  }
  cout << max_digit_sum << endl;
  return 0;
}

vecint_t from_int(int i) {
  vecint_t vi;
  while (i > 0) {
    vi.push_back(i % 10);
    i /= 10;
  }
  return vi;
}

void multiply(vecint_t &a, vecint_t &b) {
  vecint_t product(max(a.size(), b.size()));
  product.reserve(a.size() + b.size());
  for (size_t i = 0; i < a.size(); i++) {
    for (size_t j = 0; j < b.size(); j++) {
      if (i + j == product.size()) {
        product.push_back(0);
      }
      product[i + j] += a[i] * b[j];
    }
    for (size_t k = 0; k < product.size() - 1; k++) {
      if (product[k] > 9) {
        product[k + 1] += product[k]/10;
        product[k] %= 10;
      } 
    }
    if (product.back() > 9) {
      product.push_back(product.back()/10);
      *(product.rbegin() + 1) %= 10;
    }
  }
  a = product;
  return;
}

int digit_sum(vecint_t &vi) {
  int sum = 0;
  for (int8_t digit : vi) {
    sum += digit;
  }
  return sum;
}
