/*
 * file:    pe_011.cc
 * title:   Largest product in a grid
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 2, 2014
 * 
 * note:    The old version of this solution correct and performant,
 *          but the each direction in which the product of the grid
 *          elements is taken has its own loop, with lots of code
 *          duplication between them. This is an attempt to solve the
 *          problem using fewer lines of code.
 * 
 * note 2:  When populating the array grid, I use a trick to limit the
 *          scope of two variables to the inside of a for loop:
 *            for ( struct { int i; string s } s = { 0, "" }; ...
 *          Thanks to Georg Fritzsche for this tip:
 *          http://stackoverflow.com/a/2687427/2738025
 * 
 */

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
using namespace std;

const int GRID_WIDTH = 20;
const int GRID_HEIGHT = 20;
const int DIAGONAL_ROWS = GRID_HEIGHT + GRID_WIDTH - 1;

const int DIGITS_IN_PRODUCT = 4;

struct grid_indices {
  int i;
  int j;
};

// grid indexing functions are not necessary for horizontally and
// vertically oriented windows, but they make the code appear more
// consistent.
grid_indices horizontal(int i, int j);
grid_indices vertical(int i, int j);
grid_indices diagonal_tlbr(int i, int j); // top-left to bottom-right
grid_indices diagonal_bltr(int i, int j); // bottom-left to top-right

int diagonal_row_width(int row_index); // square grids only!

int window_product(int (&window)[DIGITS_IN_PRODUCT]);

int main () {
  
  int grid[GRID_WIDTH][GRID_HEIGHT];
  ifstream ifs("../grid.txt");
  // struct v for vertical
  for (struct { int i; string line; } v = { 0, "" };
       getline(ifs, v.line); v.i++) { 
    istringstream iss(v.line);
    // struct h for horizontal
    for (struct { int j; string element; } h = { 0, "" }; 
         getline(iss, h.element, ' '); h.j++) {
      grid[v.i][h.j] = stoi(h.element);
    }
  }
  ifs.close();
  int max_product = 0;
  // horizontal loop
  for (int i = 0; i < GRID_HEIGHT; i++) {
    int window[DIGITS_IN_PRODUCT];
    for (int j = 0; j < GRID_WIDTH; j++) {
      grid_indices x = horizontal(i, j);
      window[j % 4] = grid[x.i][x.j];
      if (j >= DIGITS_IN_PRODUCT) {
        int product = window_product(window);
        if (product > max_product) max_product = product;
      }
    }
  }
  // vertical loop
  for (int i = 0; i < GRID_WIDTH; i++) {
    int window[DIGITS_IN_PRODUCT];
    for (int j = 0; j < GRID_HEIGHT; j++) {
      grid_indices x = vertical(i, j);
      window[j % 4] = grid[x.i][x.j];
      if (j >= DIGITS_IN_PRODUCT) {
        int product = window_product(window);
        if (product > max_product) max_product = product;
      }
    }
  }
  // top-left to bottom-right and bottom-left to top-right diagonal
  // windows can be filled at the same time, since the diagonal row
  // width has the same value for each.
  for (int i = 0; i < DIAGONAL_ROWS; i++) {
    int row_width = diagonal_row_width(i);
    if (row_width < DIGITS_IN_PRODUCT) continue;
    int window_tlbr[DIGITS_IN_PRODUCT];
    int window_bltr[DIGITS_IN_PRODUCT];
    for (int j = 0; j < row_width; j++) {
      grid_indices tlbr = diagonal_tlbr(i, j);
      grid_indices bltr = diagonal_bltr(i, j);
      window_tlbr[j % 4] = grid[tlbr.i][tlbr.j];
      window_bltr[j % 4] = grid[bltr.i][bltr.j];
      if (j >= DIGITS_IN_PRODUCT) {
        int product_tlbr = window_product(window_tlbr);
        int product_bltr = window_product(window_bltr);
        if (product_tlbr > max_product) max_product = product_tlbr;
        if (product_bltr > max_product) max_product = product_bltr;
      }
    }
  }
  cout << max_product << endl;
  return 0;
}

grid_indices horizontal(int i, int j) {
  grid_indices indices = { i, j };
  return indices;
}

grid_indices vertical(int i, int j) {
  grid_indices indices = { j, i };
  return indices;
}

// top-left to bottom-right:
// [x][ ][ ][ ]
// [ ][x][ ][ ]
// [ ][ ][x][ ]
// [ ][ ][ ][x]
// The grid indices move from the bottom left corner of the grid to the
// top right corner along the top-left to bottom-right diagonal rows of
// the grid (shown above).
grid_indices diagonal_tlbr(int i, int j) {
  grid_indices indices;
  if (i < GRID_HEIGHT) {
    indices.i = GRID_HEIGHT - 1 - i + j;
    indices.j = j;
  } else {
    indices.i = j;
    indices.j = i - GRID_HEIGHT + 1 + j;
  }
  return indices;
}

// bottom-left to top-right:
// [ ][ ][ ][x]
// [ ][ ][x][ ]
// [ ][x][ ][ ]
// [x][ ][ ][ ]
// The grid indices move from the top left corner of the grid to the
// bottom right corner along the bottom-left to top-right diagonal rows 
// of the grid (shown above).
grid_indices diagonal_bltr(int i, int j) {
  grid_indices indices;
  if (i < GRID_HEIGHT) {
    indices.i = i - j;
    indices.j = j;
  } else {
    indices.i = GRID_HEIGHT - 1 - j;
    indices.j = i - GRID_HEIGHT + 1 + j;
  }
  return indices;
}

// assumes GRID_HEIGHT == GRID_WIDTH
int diagonal_row_width(int row_index) {
  int row_width;
  if (row_index < GRID_HEIGHT) {
    row_width = row_index + 1;
  } else {
    row_width = 2*GRID_HEIGHT - row_index - 1;
  }
  return row_width;
}

int window_product(int (&window)[DIGITS_IN_PRODUCT]) {
  int product = 1;
  for (int element : window) {
    product *= element;
  }
  return product;
}
