/*
 * file:    pe_026.cpp
 * title:   Reciprocal cycles
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 2, 2013
 *
 * note:    It might be worthwhile to limit the space of tested 
 *          denominators to primes. This code can be modified to 
 *          explore the relationship between the primality of the 
 *          denominator and cycle_length.
 */

#include <iostream>
#include <vector>
using namespace std;

const unsigned int N = 1000; //look for integer denominators up to N

int main(int argc, char **argv)
{
	size_t max_cycle_length = 0;
  size_t max_cycle_denominator = 1;
  for(size_t denominator = 2; denominator < N; denominator++) {
    size_t remainder = 1;
    vector<size_t> remainders (1, 1);
    size_t cycle_length = 0;
    while(remainder > 0) {
      //calculate new remainder
      remainder = (10*remainder) 
                  - (((10*remainder)/denominator)*denominator);
      bool cycle_finished = 0;
      //check if cycle is finished
      for(size_t index = 0; index < remainders.size(); index++) {
        if(remainder == remainders[index]) {
          cycle_length = remainders.size() - index;
          cycle_finished++;
          break;
        }
      }
      if(cycle_finished) {
        break;
      } else {
        remainders.push_back(remainder);
      }
    }
    if(cycle_length > max_cycle_length) {
      max_cycle_length = cycle_length;
      max_cycle_denominator = denominator;
    }
    remainders.empty();
  }
  cout << max_cycle_denominator << ", " << max_cycle_length << endl;
  return 0;
}
