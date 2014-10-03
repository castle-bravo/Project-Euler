/*
 * file:    pe_041.cc
 * title:   Pandigital prime
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 16, 2014
 * 
 * note:    This solution uses the fact that numbers are divisible by
 *          3 if and only if their digit sum is also divisible by 3. For 
 *          pangigital numbers, digit sums are: 
 *          sum(1-9) = 45, sum(1-8) = 36, sum(1-7) = 28, sum(1-6) = 21, 
 *          sum(1-5) = 15, sum(1-4) = 10, sum(1-3) = 6, sum(1-2) = 3 
 *          Of all of these numbers, only sum(1-7) and sum(1-4) are not 
 *          divisible by 3, so only pangigital numbers with 4 or 7 
 *          digits can be prime.
 * 
 *          I got this fact from Kristian Edlund's solution:
 *          mathblog.dk/project-euler-41-pandigital-prime/
 */

#include <algorithm>
#include <bitset>
#include <cmath>
#include <iostream>
using namespace std;

void increment(vector<int> &);
int to_number(vector<int> &, string &);

const size_t max_pandigital = 1234567;

int main()
{
    int largest_pandigital_prime = 0;
    // initialize prime number sieve
    bitset<max_pandigital/2> not_prime;
    not_prime[0] = true; // 1 is not prime
    for(size_t i = 1; i < not_prime.size(); i++) {
        if(~not_prime[i]) {
            for(size_t j = 3*i+1; j < not_prime.size(); j += 2*i+1) {
                not_prime[j] = true;
            }
        }
    }
    // generate pandigital numbers of decreasing size
    string digit_string = to_string(max_pandigital);
    while(digit_string.length() > 1) {
        vector<int> permutation(digit_string.length());
        for(size_t i = permutation.size(); i > 0; i--) {
            permutation[i] = digit_string.length()-i;
        }
        for(int i = 0; true; i++) {
            increment(permutation);
            int pandigital = to_number(permutation, digit_string);
            // test pandigital numbers for primality against sieve
            if(pandigital%2 == 0) continue;
            else {
                for(int j = 0; j <= (int)sqrt(pandigital)/2; j++) {
                    if(~not_prime[j] & (pandigital%(2*j+1) == 0)) {
                        break;
                    }
                    if(j == (int)sqrt(pandigital)/2) {
                        largest_pandigital_prime = pandigital;
                        goto pandigital_prime_found;
                    }
                }
            }
            // decrease the number of digits in the pandigital number
            if(all_of(permutation.begin(), permutation.end(), 
                      [](int i){return i==0;})) {
                cout << digit_string.length() << endl;
                digit_string.erase(digit_string.length()-1, 1);
                break;
            }
        }
    }
    pandigital_prime_found:
    cout << largest_pandigital_prime << endl;
}

void increment(vector<int> &permutation)
{
    for(size_t i = permutation.size(); i > 0; i--) {
        permutation[i]--;
        if(permutation[i] == -1) {
            permutation[i] = permutation.size()-i;
        } else break;
    }
}

int to_number(vector<int> &permutation, string &digit_string)
{
    string base_string = digit_string;
    string permutation_string = "";
    for(size_t i = 0; i < permutation.size(); i++) {
        permutation_string += base_string[permutation[i+1]];
        base_string.erase(permutation[i+1], 1);
    }
    return stoi(permutation_string);
}

