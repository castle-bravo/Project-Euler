/*
 * file:    pe_002.cc
 * title:   Even Fibonacci Numbers
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 20, 2014
 */
 
/*
 * The even Fibonnaci numbers also form a sequence that can be defined
 * recursively in terms of itself:
 *    E_{n} = 4*E_{n-1} + E_{n-2}
 * Where E_{n} is the nth even Fibonnaci number.
 */ 

#include <iostream>

const int max_E = 4000000;

using namespace std;
int main() {
  int even_fibonacci_sum = 0;
  int E_prev = 0;
  int E_curr = 2;
  while (E_curr < max_E) {
    even_fibonacci_sum += E_curr;
    int E_next = 4*E_curr + E_prev;
    E_prev = E_curr;
    E_curr = E_next;
  }
  cout << even_fibonacci_sum << endl;
}
