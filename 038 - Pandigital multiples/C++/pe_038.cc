/*
 * file:    pe_041.cc
 * title:   Pandigital multiples
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 13, 2014
 * 
 */

#include <iostream>
#include <vector>
using namespace std;

void increment(vector<int> &);
string permutation_to_string(vector<int> &);

const int number_of_digits = 9;
const string digit_string = "123456789";

int main()
{
    vector<int> permutation(number_of_digits);
    for(size_t i = permutation.size(); i > 0; i--) {
        permutation[i] = number_of_digits-i;
    }
    string permutation_string;
    bool pandigital_multiple_found = false;
    for(int i = 0; true; i++) {
        increment(permutation);
        permutation_string = permutation_to_string(permutation);
        for(size_t i = 1; i <= number_of_digits/2; i++) {
            string substring = permutation_string.substr(0, i);
            size_t j = i;
            for(int n = 2; n < number_of_digits; n++) {
                int multiple = n*stoi(substring);
                size_t multiple_length = to_string(multiple).length();
                bool compare = multiple == stoi(
                    permutation_string.substr(j, multiple_length));
                if(compare == false) break;
                j += multiple_length;
                if(j == permutation_string.length()) {
                    pandigital_multiple_found = true;
                    break;
                }
            }
            if(i+j > permutation_string.length()) break;
            if(pandigital_multiple_found) break;
        }
        if(pandigital_multiple_found) break;
    }
    cout << permutation_string << endl;
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

string permutation_to_string(vector<int> &permutation)
{
    string base_string = digit_string;
    string permutation_string = "";
    for(size_t i = 0; i < permutation.size(); i++) {
        permutation_string += base_string[permutation[i+1]];
        base_string.erase(permutation[i+1], 1);
    }
    return permutation_string;
}
