/* 
 * file:    pe_046.cc
 * title:   Goldbach's other conjecture
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    July 12, 2014
 */
 
#include <cmath>
#include <iostream>
#include <vector>
using namespace std;

bool is_square(size_t);

int main(int argc, char **argv)
{
  vector<bool> is_prime;  //odd numbers
  is_prime.push_back(false);
  is_prime.push_back(true);
  bool goldbachs_other_conjecture = true;
  for(size_t odd_index_i = 2; goldbachs_other_conjecture; 
      odd_index_i++)
  {
    size_t odd_number = 2*odd_index_i+1;
    // determine if the odd number is prime
    is_prime.push_back(true);
    for (size_t odd_index_j = 1; odd_index_j < odd_index_i; 
      odd_index_j++) {
      if (is_prime[odd_index_j] && 
          (odd_number%(2*odd_index_j+1) == 0)) {
        is_prime[odd_index_i] = false;
        break;
      }
    }
    // if the number is composite, find out wether it is the sum of a
    // prime and twice a square
    if (!is_prime[odd_index_i]) {
      for (size_t odd_index_j = odd_index_i-1; odd_index_j > 0; 
      odd_index_j--) {
        if (is_prime[odd_index_j]) {
          size_t candidate_square = odd_index_i - odd_index_j;
          if(is_square(candidate_square)) {
            break;
          }
        }
        if (odd_index_j == 1) {
          goldbachs_other_conjecture = false;
          cout << odd_number << endl;
          return false;
        }
      }
    }
  }
}

bool is_square(size_t number) 
{
  float square_root = sqrt((float)number);
  if (fmod(square_root, 1) == 0) {
    return true;
  }
  return false;
}
