/*
 * file:    pe_041.cc
 * title:   Sub-string divisibility
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 16, 2014
 */

#include <iostream>
#include <vector>
using namespace std;

void increment(vector<int> &);
string to_string(vector<int> &);
size_t factorial(size_t);

const string digit_string = "0123456789";

int main() {
    size_t substring_divisible_number_sum = 0;
    // initialize pandigital permutation vector
    vector<int> permutation(digit_string.length());
    for(size_t i = 0; i < permutation.size(); i++) {
        permutation[i] = i;
    }
    size_t max_permutations = factorial(digit_string.length());
    for(size_t i = 0; i < max_permutations; i++) {
        string permutation_string = to_string(permutation);
        //cout << permutation_string << endl;
        if(stoi(permutation_string.substr(7,3))%17 == 0
           && stoi(permutation_string.substr(6,3))%13 == 0
           && stoi(permutation_string.substr(5,3))%11 == 0
           && stoi(permutation_string.substr(4,3))%7 == 0
           && stoi(permutation_string.substr(3,3))%5 == 0
           && stoi(permutation_string.substr(2,3))%3 == 0
           && stoi(permutation_string.substr(1,3))%2 == 0) {
            substring_divisible_number_sum += stol(permutation_string);
            //cout << stol(permutation_string) << endl;
        }
        increment(permutation);
    }
    cout << substring_divisible_number_sum << endl;
}

void increment(vector<int> &permutation) {
    for(size_t i = 0; i < permutation.size(); i++) {
        permutation[i]--;
        if(permutation[i] == -1) {
            permutation[i] = i;
        } else break;
    }
}

string to_string(vector<int> &permutation) {
    string base_string = digit_string;
    string permutation_string = "";
    for(size_t i = permutation.size()-1; i > 0; i--) {
        permutation_string += base_string[permutation[i]];
        //cout << permutation_string << endl;
        base_string.erase(permutation[i], 1);
    }
    permutation_string += base_string;
    return permutation_string;
}

size_t factorial(size_t n) {
    size_t n_factorial = n;
    while(n > 1) {
        n--;
        n_factorial *= n;
    }
    return n_factorial;
}
