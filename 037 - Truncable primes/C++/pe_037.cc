/*
 * file:    pe_0376.cc
 * title:   Truncable primes
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    December 20, 2013
 */

#include <bitset>
#include <iostream>
#include <string>
using namespace std;

const size_t primes_up_to = 1000000;

int main(int argc, char **argv)
{
  //generate list of primes using sieve of Eratosthenes
  bitset<primes_up_to> is_prime;
  is_prime.set();
  is_prime[0] = false;
  is_prime[1] = false;
  for(size_t number = 2; number < primes_up_to; number++) {
    if(is_prime[number]) {
      for(size_t composite = 2*number; composite < primes_up_to; 
          composite += number) {
        is_prime[composite] = false;
      }
    }
  }
  size_t sum_of_truncable_primes = 0;
  size_t number_of_truncable_primes = 0;
  while(number_of_truncable_primes != 11) {
    for(size_t number = 10; number < primes_up_to; number++) {
      if(is_prime[number]) {
        string prime_string = to_string(number);
        string truncated_string = prime_string;
        bool is_truncable = true;
        while(truncated_string.length() > 1) {
          truncated_string.erase(truncated_string.length()-1);
          if(~is_prime[stoi(truncated_string)]) {
            is_truncable = false;
            break;
          }
        }
        if(is_truncable) {
          truncated_string = prime_string;
          while(truncated_string.length() > 1) {
            truncated_string.assign(truncated_string.begin()+1,
                                    truncated_string.end());
            if(~is_prime[stoi(truncated_string)]) {
              is_truncable = false;
              break;
            }
          }
        }
        if(is_truncable) {
          number_of_truncable_primes += 1;
          sum_of_truncable_primes += number;
        }
        //if(is_truncable) cout << number << endl;
      }
    }
  } 
  cout << sum_of_truncable_primes << endl;
}
