/*
 * file:    pe_027.cc
 * title:   Quadratic primes
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 30, 2013
 */
 
 #include <cmath>
 #include <cstdlib>
 #include <fstream>
 #include <iostream>
 #include <vector>
 using namespace std;
 
 int main(int argc, char **argv)
{
  // import a list of primes for prime_factorization function to use
  vector<unsigned int> list_of_primes;
  ifstream ifs ("primes_10000.txt");
  for (size_t index = 0; ifs.peek() != EOF; index++) {
    string prime_number;
    getline(ifs, prime_number);
    list_of_primes.push_back(stoi(prime_number));
  }
  ifs.close();
  // initialize some important local variables
  int largest_n = 0;
  int product_ab = 0;
  // b can only be prime
  for (size_t index_b = 0; true; index_b++) {
    int b = list_of_primes[index_b];
    if (b > 1000) { break; }
    //select a such that a + b + 1 is also prime
    for (size_t index_a = 0; true; index_a++) {
      int a = list_of_primes[index_a]-b-1;
      if (abs(a) > 1000) { break; }
      // iterate over n from 2 until n^2 + an + b is not prime
      for (int n = 2; true; n++) {
        int quadratic_value = n*n + a*n + b;
        bool is_prime;
        // check if value is positive
        if (quadratic_value <= 1) {
          is_prime = false;
          break;
        } else {
            is_prime = true;
        }
        // check if quadratic_value is prime
        for (size_t index = 0; 
             list_of_primes[index] < sqrt(quadratic_value+1); 
             index++ ) {
          if (quadratic_value % list_of_primes[index] == 0) {
            is_prime = false;
            break;
          }
        }
        if (is_prime == false) {
          break; 
        }
        if (n > largest_n) {
          largest_n = n;
          product_ab = a*b;
        }
      }
    }
  }
  cout << product_ab << endl;
  return 0;
}
