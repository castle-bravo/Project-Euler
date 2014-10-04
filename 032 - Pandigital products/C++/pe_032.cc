/*
 * file:    pe_032.cc
 * title:   Pandigital products
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    November 11, 2013
 * 
 */
 
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

bool is_pandigital(string);

int main(int argc, char **argv) {
  vector<int> pandigital_products;
  for(int a = 1; a < 10; a++) {
    for(int b = 1; b < 10; b++) {
      if(b == a) continue;
      for(int c = 1; c < 10; c++) {
        if((c == a)|(c == b)) continue;
        for(int d = 1; d < 10; d++) {
          if((d == a)|(d == b)|(d == c)) continue;
          for(int e = 1; e < 10; e++) {
            if((e == a)|(e == b)|(e == c)|(e == d)) continue;
            if(a > 1) {
              int product = a*(1000*b+100*c+10*d+e);
              stringstream ss;
              ss << a << b << c << d << e << product;
              if(is_pandigital(ss.str())) {
                bool unique_product = true;
                for(int pandigital_product : pandigital_products) {
                  if(product == pandigital_product) {
                    unique_product = false;
                  }
                }
                if(unique_product) {
                  pandigital_products.push_back(product);
                }
              }
            }
            int product = (10*a+b)*(100*c+10*d+e);
            stringstream ss;
            ss << a << b << c << d << e << product;
            if(is_pandigital(ss.str())) {
              bool unique_product = true;
              for(int pandigital_product : pandigital_products) {
                if(product == pandigital_product) {
                  unique_product = false;
                }
              }
              if(unique_product) {
                pandigital_products.push_back(product);
              }
            }
          }
        }
      }
    }
  }
  int pandigital_product_sum = 0;
  for(int pandigital_product : pandigital_products) {
    pandigital_product_sum += pandigital_product;
  }
  cout << pandigital_product_sum << endl;
}

bool is_pandigital(string number_string) {
  if(number_string.length() != 9) {
    return false;
  } else {
    for(int8_t index_i = 0; index_i < 9; index_i++) {
      if(number_string[index_i] == '0') return false;
      for(int8_t index_j = 0; index_j < 9; index_j++) {
        if(index_i == index_j) break;
        if(number_string[index_i] == number_string[index_j]) return false;
      }
    }
  }
  return true;
}
