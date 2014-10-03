/*
 * file: pe_024.cc
 * title: Lexicographic permutations
 * author: Alexander Gosselin
 * date: September 10, 2013
 * e-mail: alexandergosselin@gmail.com
 *
 */

#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

int main(int argc, char **argv)
{
  size_t number_of_permutations = 1000000; //print the Nth permutation
  //initialize string of characters to permute
  string characters = "0123456789";
  number_of_permutations--; //first permutation has just been completed
  const size_t number_of_characters = characters.size();
  string permutation; //initialize string containing permutation of characters
  //create array containing factorial(size of array - index)
  vector<unsigned int> factorials;
  factorials.push_back(1);
  for(size_t index = 2; index < number_of_characters; index++) {
    factorials.insert(factorials.begin(), index*factorials[0]);
  }
  /*//return error if N is too large
  if(number_of_permutations > number_of_characters*factorials[0]) {
    cout << "Error: The number of permutations requested is larger than the"
            "total number of possible permutaions.";
    return 0;
  }*/
  //generate Nth permutation of the string
  for(size_t index = 0; index < number_of_characters-1; index++) {
    size_t index_of_target_character = number_of_permutations/factorials[index];
    permutation.push_back(characters[index_of_target_character]);
    characters.erase(characters.begin()+index_of_target_character);
    number_of_permutations -= factorials[index]*index_of_target_character;
  }
  cout << permutation << characters << endl;
  return 0;
}
