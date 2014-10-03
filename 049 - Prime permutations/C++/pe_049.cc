/*
 * file:    pe_049.cc
 * title:   Prime permutations
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 24, 2014
 * 
 * math:    There are (10 4) + (10 3) + (10 2) + (10 1) distinct sets
 *          of digits that can make up a 4-digit number, including 0000.
 *          Here (n k) indicates the binomial coefficient:
 *            n!/k!(n - k)!
 *          This adds up t0 385.
 */

#include <algorithm>
#include <bitset>
#include <iostream>
#include <vector>

const int PRIMES_ABOVE = 999;
const int PRIMES_BELOW = 10000;

using namespace std;
int main() {
  // generate a vector of 4-digit primes
  vector<int> primes;
  // initialize prime number sieve
  bitset<PRIMES_BELOW/2> not_prime; // odd numbers
  not_prime[0] = true; // 1 is not prime
  for (size_t i = 1; i < not_prime.size(); i++) {
    if (!not_prime[i]) {
      if (i > PRIMES_ABOVE/2)
        primes.push_back(2*i + 1);
      for (size_t j = 3*i + 1; j < not_prime.size(); 
        j += 2*i + 1) {
        not_prime[j] = true;
      }
    }
  }
  // put primes that permute to eachother in vectors together
  // note: using std::list to contain the pairs may be more performant
  vector<pair<string, vector<int> > > prime_permutations;
  for (int prime : primes) {
    string permutation_key = to_string(prime);
    sort(permutation_key.begin(), permutation_key.end());
    bool prime_inserted = false;
    for (size_t i = 0; i < prime_permutations.size(); i++) {
      int key_comparison = permutation_key.compare(
          prime_permutations[i].first);
      if (key_comparison == 0) {
        prime_permutations[i].second.push_back(prime);
        prime_inserted = true;
        break;
      } else if (key_comparison < 0) {
        pair<string, vector<int> > new_permutation (
            permutation_key, vector<int> (1, prime));
        prime_permutations.insert(prime_permutations.begin() + i, 
                                  new_permutation);
        prime_inserted = true;
        break;
      }
    }
    if (!prime_inserted) {
      pair<string, vector<int> > new_permutation (
          permutation_key, vector<int> (1, prime));
      prime_permutations.push_back(new_permutation);
    }
  }
  // find the digit sets with evenly spaced primes
  for (pair<string, vector<int> > key : prime_permutations) {
    size_t primes_in_key = key.second.size();
    if (primes_in_key < 3) continue;
    vector<vector<int> > differences;
    for (size_t i = 0; i < primes_in_key - 1; i++) {
      differences.push_back(vector<int> ());
      for (size_t j = i + 1; j < primes_in_key; j++) {
        int difference = key.second[j] - key.second[i];
        differences[i].push_back(difference);
      }
    }
    for (size_t i = 0; i < differences.size(); i++) {
      for (size_t j = 0; j < differences[i].size() - 1; j++) {
        for (size_t k = 0; k < differences[j+1].size(); k++) {
          if (differences[i][j] == differences[i + j + 1][k]) {
            // we're looking for a sequence other than 1487, 4817, 8147
            if (key.second[i] == 1487) goto next_key;
            cout << key.second[i] << key.second[i + j + 1] 
                 << key.second[i + j + k + 2] << endl;
            goto end;
          }
        }
      }
    }
    next_key:;
  }
  end:;
}
