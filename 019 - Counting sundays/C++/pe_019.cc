/*
 * file:    pe_019.txt
 * title:   Counting sundays
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 8, 2014 (modified)
 *          Original date unknown
 */

#include <algorithm>
#include <iostream>
using namespace std;

int main() {
  int day = 1;
  int month;
  int year;
  int sundays = 0;
  const int days31[7] = {1, 3, 5, 7, 8, 10, 12};
  for (year = 0; year < 101; year++) {
    for (month = 1; month < 13; month++) {
      if (month == 2) {
        if (year % 4) {
          day += 29;
        } else {
          day += 28;
        }
      } else {
        if (count(days31, days31 + 6, month)) {
          day += 31;
        } else {
          day += 30;
        }
      }
      if (day % 7 == 0 && year > 0) {
        sundays++;
      }
    }
  }
  cout << sundays <<'\n';
  return 0;
}
