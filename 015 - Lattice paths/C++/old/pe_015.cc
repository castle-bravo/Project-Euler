/*
 * file:    pe_015.cc
 * title:   Lattice paths
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 8, 2014 (modified)
 *          Original date unknown
 * 
 * math:    In order to travel from one corner of the lattice to the
 *          other, the path must traverse 2*n edges, of which n must be
 *          in the e1 direction and n must be in e2. This implies that
 *          the number of possible paths p(n) = 2*n choose n.
 * 
 * note:    I borrowed my method of computing the binomial coefficient
 *          from Ribtoks at:
 *          http://stackoverflow.com/a/11032879/2738025
 *          This may not be the best method of computing the binomial
 *          coefficient, but it is pretty neat.
 */
 
#include <cstring>
#include <iostream>
using namespace std;

const int L = 20;  //side length of the lattice

int main() {
  int N = 2*L;
  int K = L;
  uint64_t C[N+1][K+1];
  memset(C, 0, sizeof(C));
  for(int n = 0; n <= N; n++) C[n][0] = 1;
  for(int k = 1; k <= K; k++){
    for(int n = k; n <= N; n++){
      C[n][k] = C[n - 1][k - 1] + C[n - 1][k];
    }
  }
  cout << C[N][K] << '\n';
  return 0;
}
