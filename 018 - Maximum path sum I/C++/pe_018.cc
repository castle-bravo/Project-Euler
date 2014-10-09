/*
 * file:    pe_018.cc
 * title:   Maximum path sum I
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 2, 2014 (modified)
 *          Original date unknown
 */

#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

int main(int argc, char **argv)
{
  ifstream ifs ("../triangle.txt");
  if (ifs.is_open() == 0) { 
    cout << "Error: unable to open triangle.txt\n";
    return 0;
  }
  //load contents of triangle.txt into a vector of vectors triangle
  vector<vector<int> > triangle(0, vector<int>(0));
  string row;
  string entry;
  int i;
  int j;
  for (i = 0; ifs.peek() != EOF; i++) {
    triangle.push_back(vector<int>(i+1));
    getline(ifs, row);
    for (j = 0; j <= i; j++) {
      entry = {row[3*j], row[3*j+1]};
      triangle[i][j] = stoi(entry);
    }
  }
  i--; //decrement i to prevent segmentation fault
  //calculate the maximum path sum
  while (i > 0) {
    for (j = i; j > 0; j--) {
      if(triangle[i][j] > triangle[i][j-1]) {
        triangle[i-1][j-1] += triangle[i][j];
      } else {
        triangle[i-1][j-1] += triangle[i][j-1];
      }
    }
    i--;
  }
  cout << triangle[0][0] << '\n';
  ifs.close();
  return 0;
}
