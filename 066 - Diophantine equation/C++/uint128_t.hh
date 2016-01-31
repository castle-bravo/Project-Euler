/*
 * file:    uint128_t.h
 * title:   Unsigned 128-bit integer type header
 * author:  Alexander Gosselin
 * e-mail:  <alexandergosselin@gmail.com>
 *          <alexander.gosselin@alumni.ubc.ca>
 * date:    August 31, 2015
 * 
 * license: GNU General Public License v3
 *          <http://www.gnu.org/licenses/gpl-3.0.html>
 */
 
#ifndef UINT128_T_H
#define UINT128_T_H

#include <stdint.h>
#include <string>
#include <sys/types.h>
//#include "bits.h"

typedef uint64_t Block;

class uint128_t {

private:
  Block block0;
  Block block1;

public:
  // constructors
  uint128_t();
  uint128_t(Block i);
  uint128_t(const uint128_t &i);
  uint128_t(Block b0, Block b1);
  
  // type conversion
  inline operator uint64_t() const { return block0; }
  
  /* testing */
  std::string show();
  
  // assignment operators
  void operator =(const Block &i);
  void operator =(const uint128_t &i);
  
  // comparison operators
  // others implemented in terms of == and < 
  friend bool operator ==(const uint128_t &lhs, const uint64_t &rhs);
  friend bool operator ==(const uint64_t &lhs, const uint128_t &rhs);
  friend bool operator ==(const uint128_t &lhs, const uint128_t &rhs);
  friend bool operator <(const uint128_t &lhs, const uint64_t &rhs);
  friend bool operator <(const uint64_t &lhs, const uint128_t &rhs);
  friend bool operator <(const uint128_t &lhs, const uint128_t &rhs);
  
  // bitwise operators
  // bitwise and, or, xor are non-member functions
  friend uint128_t operator~(const uint128_t &i);
  
  // arithmetic operators
  // increment
  uint128_t& operator ++(); // prefix
  uint128_t operator ++(int); // postfix
  // decrement
  uint128_t& operator --(); // prefix
  uint128_t operator --(int); // postfix
  
  // compound assignment operators
  void operator +=(const Block &i);
  void operator +=(const uint128_t &i);
  void operator -=(const Block &i);
  void operator -=(const uint128_t &i);
  void operator *=(const Block &i);
  void operator *=(const uint128_t &i);
  void operator /=(const Block &i);
  void operator /=(const uint128_t &i);
  void operator %=(const Block &i);
  void operator %=(const uint128_t &i);
  void operator &=(const Block &i);
  void operator &=(const uint128_t &i);
  void operator |=(const Block &i);
  void operator |=(const uint128_t &i);
  void operator ^=(const Block &i) ;
  void operator ^=(const uint128_t &i);
  void operator <<=(uint i);
  void operator >>=(uint i);
  
  friend std::string to_string(uint128_t i);
  
  friend uint ilog2(const uint128_t &i); // needed for division and remainder
};

// non-member functions
// comparison operators
bool operator ==(const uint128_t &lhs, const uint64_t &rhs);
bool operator ==(const uint64_t &lhs, const uint128_t &rhs);
bool operator ==(const uint128_t &lhs, const uint128_t &rhs);
bool operator !=(const uint128_t &lhs, const uint64_t &rhs);
bool operator !=(const uint64_t &lhs, const uint128_t &rhs);
bool operator !=(const uint128_t &lhs, const uint128_t &rhs);
bool operator <(const uint128_t &lhs, const uint64_t &rhs);
bool operator <(const uint64_t &lhs, const uint128_t &rhs);
bool operator <(const uint128_t &lhs, const uint128_t &rhs);
bool operator <=(const uint128_t &lhs, const uint64_t &rhs);
bool operator <=(const uint64_t &lhs, const uint128_t &rhs);
bool operator <=(const uint128_t &lhs, const uint128_t &rhs);
bool operator >(const uint128_t &lhs, const uint64_t &rhs);
bool operator >(const uint64_t &lhs, const uint128_t &rhs);
bool operator >(const uint128_t &lhs, const uint128_t &rhs);
bool operator >=(const uint128_t &lhs, const uint64_t &rhs);
bool operator >=(const uint64_t &lhs, const uint128_t &rhs);
bool operator >=(const uint128_t &lhs, const uint128_t &rhs);

// bitwise operators
// bitwise and
uint128_t operator &(uint128_t lhs, const uint64_t &rhs);
uint128_t operator &(const uint64_t &lhs, uint128_t rhs);
uint128_t operator &(uint128_t lhs, const uint128_t &rhs);
// bitwise or
uint128_t operator |(uint128_t lhs, const uint64_t &rhs);
uint128_t operator |(const uint64_t &lhs, uint128_t rhs);
uint128_t operator |(uint128_t lhs, const uint128_t &rhs);
// bitwise xor
uint128_t operator ^(uint128_t lhs, const uint64_t &rhs);
uint128_t operator ^(const uint64_t &lhs, uint128_t rhs);
uint128_t operator ^(uint128_t lhs, const uint128_t &rhs);
// bitwise left shift
uint128_t operator <<(uint128_t lhs, uint bits);
// bitwise right shift
uint128_t operator >>(uint128_t lhs, uint bits);

// arithmetic operators
// addition
uint128_t operator +(uint128_t lhs, const uint64_t &rhs);
uint128_t operator +(const uint64_t &lhs, uint128_t rhs);
uint128_t operator +(uint128_t lhs, const uint128_t &rhs);
// subtraction
uint128_t operator -(uint128_t lhs, const uint64_t &rhs);
uint128_t operator -(const uint64_t &lhs, const uint128_t &rhs);
uint128_t operator -(uint128_t lhs, const uint128_t &rhs);
// multiplication
uint128_t operator *(uint128_t lhs, const uint64_t &rhs);
uint128_t operator *(const uint64_t &lhs, uint128_t rhs);
uint128_t operator *(uint128_t lhs, const uint128_t &rhs);
// division
uint128_t operator /(uint128_t lhs, const uint64_t &rhs);
uint128_t operator /(const uint64_t &lhs, const uint128_t &rhs);
uint128_t operator /(uint128_t lhs, const uint128_t &rhs);
// remainder
uint128_t operator %(uint128_t lhs, const uint64_t &rhs);
uint128_t operator %(const uint64_t &lhs, const uint128_t &rhs);
uint128_t operator %(uint128_t lhs, const uint128_t &rhs);

// additional math functions 
// integer logarithm base 2
uint ilog2(const uint128_t &i);

// printing 
// to_string
std::string to_string(uint128_t i);

// add compatibility with std::cout << operator

#endif /*UINT128_T_H*/
