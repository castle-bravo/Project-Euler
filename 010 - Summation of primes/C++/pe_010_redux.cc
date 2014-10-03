/*
 * file:    pe_010.cc
 * title:   Sum of primes
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 16, 2014
 * 
 * notes:   The sum of primes under 2000000 is larger than a 32 bit
 *          integer can hold - signed or unsigned. size_t is 64 bits
 *          unsigned, so it can manage.
 * 
 *          The bitset.set() operation can be avoided if not_prime
 *          were instead an array not_prime containing true values for 
 *          composites and false values for primes.
 * 
 *          I tried rewriting this solution with an optimized prime
 *          number sieve. It returns the wrong result, but on 
 *          approximately the right order of magnitude. It could be
 *          throwing out every fifth prime.
 * 
 */

#include <bitset>
#include <iostream>
using namespace std;

const int primes_below = 2000000;

int main()
{
    bitset<primes_below/3> not_prime; // numbers without 2 or 3 as a 
                                      // prime factor
    not_prime[0] = true;              // 1 is not prime
    size_t sum_of_primes = 5;         // include 2 and 3 in the sum
    for(size_t i = 1; i < not_prime.size(); i++) {
        if(!not_prime[i]) {
            size_t n, j;
            if(i%2 == 0) {
                n = 3*i+1;
                j = i+n-2;
            }
            else {
                n = 3*i+2;
                j = i+n+2;
            }
            sum_of_primes += n;
            //cout << n << endl;
            while(j < not_prime.size()) {
                not_prime[j] = true;
                if(j%2 == 0) j += n-2;
                else j += n+2;
            }
        }
    }
    cout << sum_of_primes << endl;
}
