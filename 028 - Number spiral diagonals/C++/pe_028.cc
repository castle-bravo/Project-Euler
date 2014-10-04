/*
 * file:    pe_028.cc
 * title:   Number spiral diagonals
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 10, 2013
 */

/* Reasoning:
 * As the spiral becomes larger, the length of a side of the spiral 
 * increases linearly with the distance from the center.
 */

#include <iostream>
using namespace std;

size_t N = 1001; //side length of spiral 

int main(int argc, char **argv)
{
  unsigned int diagonal_sum = 1;
  size_t current_number = 1;
  for (size_t spiral_layer = 1; spiral_layer < N/2 + 1; 
       spiral_layer++) {
    for (int8_t side_number = 0; side_number < 4; side_number++) {
      current_number += 2*spiral_layer;
      diagonal_sum += current_number;
    }
  }
  cout << diagonal_sum << endl;
  return 0;
}
