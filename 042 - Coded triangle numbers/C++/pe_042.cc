/*
 * file:    pe_042.cc
 * title:   Coded triangle numbers
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 16, 2014
 * 
 * math:    Triangle numbers have the form T_n = n(n+1)/2. A number T is
 *          a triangle number if n^2 + n + 2T = 0 is solved for an 
 *          integer value of n. In other words, (-1 +- sqrt(1 + 8T))/2 
 *          is a positive integer. From this follows:
 *          => sqrt(1 + 8T) - 1 is even
 *          => sqrt(1 + 8T) is odd
 *          => 1 + 8T is an odd perfect square
 *          We can test wether a number n is triangle or not by 
 *          comparing floor(sqrt(1 + 8n))^2 to 1 + 8n. If they are 
 *          equal, then n is a triangle number.
 * 
 */

#include <cmath>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

int main() {
    int number_of_triangle_words = 0;
    // calculate word values for each word in words.txt
    ifstream words;
    words.open ("../words.txt");
    vector<int> word_values;
    while(words.good()) {
        char letter = words.get();
        if(letter < 'A' || letter > 'Z') continue;
        else {
            word_values.push_back(0);
            while(letter >= 'A' && letter <= 'Z') {
                word_values.back() += letter - 'A' + 1;
                letter = words.get();
            }
        }
    }
    words.close();
    // count the number of triangle words in word_values
    for(auto value : word_values) {
        if((int)pow(floor(sqrt(1+8*value)), 2) == 1+8*value) {
            number_of_triangle_words++;
        }
    }
    cout << number_of_triangle_words << endl;
}
