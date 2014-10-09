/*
 * file: pe_023.cc
 * title: Non-abundant sums
 * author: Alexander Gosselin
 * e-mail: alexandergosselin@gmail.com
 * date: September 16, 2013
 * 
 * problem description: Find the sum of all the positive integers which cannot 
 * be written as the sum of two abundant numbers.
 */

#include <array>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

const unsigned int upper_bound = 28123; //according to the problem statement
                                       //page, all numbers above 28123 can be
                                       //are a sum of two abundant numbers.

vector<pair<unsigned int, size_t>> prime_factorize(unsigned int);
unsigned int aliquot_sum(unsigned int number);
int integer_power(int, size_t);

vector<unsigned int> list_of_primes;

int main(int argc, char **argv)
{
  //import a list of primes for prime_factorization function to use
  ifstream ifs ("primes_2500.txt");
  for (size_t index = 0; ifs.peek() != EOF; index++) {
    string prime_number;
    getline(ifs, prime_number);
    list_of_primes.push_back(stoi(prime_number));
  }
  ifs.close();
  //create a list of abundant numbers which are less than upper_bound/2
  vector<unsigned int> abundant_numbers;
  for(size_t number = 1; number < upper_bound; number++) {
    if(aliquot_sum(number) > number) {
      abundant_numbers.push_back(number);
    }
  }
  //identify numbers which are not a sum of two abundant numbers by generating
  //sums of abundant numbers and checking those sums off
  array<bool, upper_bound> not_abundant_sum;
  not_abundant_sum.fill(true);
  for(size_t index_i = 0; index_i < abundant_numbers.size(); index_i++) {
    for(size_t index_j = 0; index_j < abundant_numbers.size(); index_j++) {
      unsigned int abundant_sum = 
        abundant_numbers[index_i]+abundant_numbers[index_j];
      if(abundant_sum < upper_bound) {
        not_abundant_sum[abundant_sum] = false;
      }
    }
  }
  //take the sum of non-abundant sums
  unsigned int sum_of_non_abundant_sums = 0;
  for(size_t number = 0; number < not_abundant_sum.size(); number++) {
    sum_of_non_abundant_sums += not_abundant_sum[number]*number;
  }
  cout << sum_of_non_abundant_sums << endl;
  return 0;
}

//the functions below should probably be combined into a single aliquot_sum 
//function
vector<pair<unsigned int, size_t>> prime_factorize(unsigned int number)
{
  vector<pair<unsigned int, size_t>> prime_factorization;
  for(size_t index = 0; index < list_of_primes.size(); index++) {
    if(number % list_of_primes[index] == 0) {
      pair<unsigned int, size_t> prime_and_exponent (list_of_primes[index], 1);
      prime_factorization.push_back(prime_and_exponent);
      number /= list_of_primes[index];
      while(number % list_of_primes[index] == 0) {
        prime_factorization.back().second += 1;
        number /= list_of_primes[index];
      }
    }
    if(number == 1)
      break;
  }
  return prime_factorization;
}

unsigned int aliquot_sum(unsigned int number)
{
  auto prime_factorization = prime_factorize(number);
  unsigned int sigma = 1;
  for(auto &prime_and_exponent : prime_factorization) {
    unsigned int power_series = 0;
    for(size_t index = 0; index <= prime_and_exponent.second; index++) {
      power_series += integer_power(prime_and_exponent.first, index);
    }
    sigma *= power_series;
  }
  return sigma - number;
}

int integer_power(int base, size_t exponent)
{
  int power = 1;
  while(exponent > 0) {
    power *= base;
    exponent--;
  }
  return power;
}
