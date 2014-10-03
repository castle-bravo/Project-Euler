/*
 * file: pe_004.cc
 * title: Largest Palindrome Pdoduct
 * author: Alexander Gosselin
 * e-mail: alexandergosselin@gmail.com
 * date: August 10, 2014
 * 
 */

#include <iostream>
using namespace std;

bool is_palindrome(int);

int main()
{
    for(int starting_x = 999; starting_x >= 100; starting_x--) {
        for(int x =  starting_x, y = starting_x; 
            x >= 100 && y <= 999; x--, y++) {
            int product = x*y;
            if(is_palindrome(product)) {
                cout << product << endl;
                return 0;
            }
        }
        for(int x =  starting_x, y = starting_x-1; 
            x >= 100 && y <= 999; x--, y++) {
            int product = x*y;
            if(is_palindrome(product)) {
                cout << product << endl;
                return 0;
            }
        }
    }
}

bool is_palindrome(int product)
{
    string product_string = to_string(product);
    int digits = product_string.length();
    for(int depth = digits/2; depth >= 0; depth--) {
        if(product_string[depth] != product_string[digits-depth-1]) {
            return false;
        }
    }
    return true;
}
