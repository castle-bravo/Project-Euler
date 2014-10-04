/*
 * file:    pe_031.cc
 * title:   Coin sums
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    November 10, 2013
 * 
 */
 
//no memoization, can be made much more efficient
 
#include <iostream>
#include <vector>
using namespace std;

int number_of_combinations(int, vector<int>&);
 
int main(int argc, char **argv) {
  vector<int> coins {1,2,5,10,20,50,100,200};
  cout << number_of_combinations(200, coins) << endl;
}

int number_of_combinations(int sum, vector<int> &coins) {
  int combinations = 0;
  vector<int> coin_subset (coins.begin(), --coins.end());
  if (coin_subset.size() > 0) {
    combinations += number_of_combinations(sum, coin_subset);
    for (sum -= coins.back(); sum >= 0; 
        sum -= coins.back()) {
      if (sum == 0) {
        combinations++;
        break;
      }
      combinations += number_of_combinations(sum, coin_subset);
    }
  } else {
    combinations++;
  }
  return combinations;
}
