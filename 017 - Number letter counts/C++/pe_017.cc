/*
 * file:    pe_017.cc
 * title:   Number letter counts
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 27, 2014 (modified)
 *          Original date unknown (2013)
 */

#include <iostream>
using namespace std;

int main(int argc, char **argv)
{
  int onedg[10] = {0,3,3,5,4,4,3,5,5,4}; //(zero), one, two, three...
  int teens[10] = {3,6,6,8,8,7,7,9,8,8}; //ten, twelve, thirteen...
  int twodg[10] = {0,0,6,6,5,5,5,7,7,6}; //(zero), (ten), twenty...
  unsigned int count = 0;
  int md100;
  for(int i = 0; i < 1000; i++){
    md100 = i%100;
    if(md100 < 20){
      if(i < 10){ count += onedg[md100]; }
      else{ count += teens[md100-10]; }
    }
    else{ count += onedg[md100%10] + twodg[md100/10]; }
  }
  for(int i = 1; i < 10; i++){
    count += onedg[i] + 7; //(one, two, three...) hundred
    count += 99*(onedg[i] + 10); //(one, two, three...) hundred and
  }
  count += 11; //one thousand
  cout << count << '\n';
  return 0;
}
