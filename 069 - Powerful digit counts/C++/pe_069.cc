#include <cmath>
#include <iostream>
using namespace std;

int main() {
  int n_digit_nth_powers = 0;
  int bases[9] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  for (int base : bases) {
    for (int n = 1; floor(n*log10(base)) + 1 == n; n++) {
      n_digit_nth_powers++;
    }
  }
  cout << n_digit_nth_powers << endl;
  return 0;
}
