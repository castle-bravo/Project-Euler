/*
 * file:    pe_029.cc
 * title:   Distinct primes
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    September 13, 2014
 * 
 */
 
#include <iostream>
#include <set>
#include <vector>
using namespace std;

const int range_min = 2;
const int range_max = 100;

int main()
{
    int range_size = range_max - range_min + 1;
    int distinct_powers = range_size*range_size;
    int max_exponent = 1;
    int min_power = range_min;
    for(int b = 1; b <= range_max; b++) {
        min_power *= range_min;
        if(min_power > range_max) {
            max_exponent = b;
            break;
        }
    }
    vector<int> common_powers(max_exponent, 0);
    set<int> exponents_in_range;
    for(int b = range_min-1; b <= max_exponent; b++) {
        for(int c = range_min; c <= range_max; c++ ) {
            exponents_in_range.insert(b*c);
        }
        common_powers[b-range_min+1] = b*range_size 
                                       - exponents_in_range.size();
    }
    vector<bool> already_visited(range_size, false);
    for(int a = range_min; a <= range_max; a++) {
        if(already_visited[a-range_min]) continue;
        already_visited[a-range_min] = true;
        int power = a;
        for(int b = 1; b <= range_max; b++) {
            power *= a;
            if(power > range_max) {
                max_exponent = b;
                break;
            } else {
                already_visited[power-range_min] = true;
            }
        }
        if(max_exponent == 1) break;
        distinct_powers -= common_powers[max_exponent-1];
    }
    cout << distinct_powers << endl;
}
