/*
 * file:    pe_010.cc
 * title:   Sum of primes
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 14, 2014
 * 
 * notes:   The sum of primes under 2000000 is larger than a 32 bit
 *          integer can hold - signed or unsigned. size_t is 64 bits
 *          unsigned, so it can manage.
 * 
 *          The bitset.set() operation can be avoided if is_prime
 *          were instead an array not_prime containing true values for 
 *          composites and false values for primes.
 */

#include <bitset>
#include <iostream>
using namespace std;

const int primes_below = 2000000;

int main()
{
    bitset<primes_below/2> is_prime; // odd numbers
    is_prime.set();                  // assume all numbers are prime
    is_prime[0] = false;             // 1 is not prime
    size_t sum_of_primes = 2;        // include 2 in the sum
    for(size_t i = 1; i < is_prime.size(); i++) {
        if(is_prime[i]) {
            sum_of_primes += 2*i+1;
            for(size_t j = 3*i+1; j < is_prime.size(); j += 2*i+1) {
                is_prime[j] = false;
            }
        }
    }
    cout << sum_of_primes << endl;
}
