/*
 * file:    uint128_t.cc
 * title:   Unsigned 128-bit integer function implementations
 * author:  Alexander Gosselin
 * e-mail:  <alexandergosselin@gmail.com>
 *          <alexander.gosselin@alumni.ubc.ca>
 * date:    September 1, 2015
 * 
 * license: GNU General Public License v3
 *          <http://www.gnu.org/licenses/gpl-3.0.html>
 */

#include <iostream>

#include <stdint.h>
#include "uint128_t.hh"

const uint BLOCK_SIZE = 64;
const uint64_t P10_UINT64 = 10000000000000000000ull;

std::string uint128_t::show() {
  std::string s = std::to_string(block0);
  s.append(", ");
  s.append(std::to_string(block1));
  return s;
}

// constructors
uint128_t::uint128_t() {
    block0 = 0;
    block1 = 0;
}
uint128_t::uint128_t(Block i) {
    block0 = i;
    block1 = 0;
}
uint128_t::uint128_t(const uint128_t &i) {
    block0 = i.block0;
    block1 = i.block1;
}
uint128_t::uint128_t(Block b0, Block b1) {
    block0 = b0;
    block1 = b1;
}

// assignment operators
void uint128_t::operator =(const Block &i) {
    block0 = i;
    block1 = 0;
}
void uint128_t::operator =(const uint128_t &i) {
    block0 = i.block0;
    block1 = i.block1;
}

// arithmetic operators

// addition operator
uint128_t operator +(uint128_t lhs, const uint64_t &rhs) {
  lhs += rhs;
  return lhs;
}
uint128_t operator +(const uint64_t &lhs, uint128_t rhs) {
  rhs += lhs;
  return rhs;
}
uint128_t operator +(uint128_t lhs, const uint128_t &rhs) {
  lhs += rhs;
  return lhs;
}

// subtraction
uint128_t operator -(uint128_t lhs, const uint64_t &rhs) {
  lhs -= rhs;
  return lhs;
}
uint128_t operator -(const uint64_t &lhs, const uint128_t &rhs) {
  return uint128_t(lhs) - rhs;
}
uint128_t operator -(uint128_t lhs, const uint128_t &rhs) {
  lhs -= rhs;
  return lhs;
}

// multiplication
uint128_t operator *(uint128_t lhs, const uint64_t &rhs) {
  lhs *= rhs;
  return lhs;
}
uint128_t operator *(const uint64_t &lhs, uint128_t rhs) {
  rhs *= lhs;
  return rhs;
}
uint128_t operator *(uint128_t lhs, const uint128_t &rhs) {
  lhs *= rhs;
  return lhs;
}

// division
uint128_t operator /(uint128_t lhs, const uint64_t &rhs) {
  lhs /= rhs;
  return lhs;
}
uint128_t operator /(const uint64_t &lhs, const uint128_t &rhs) {
  return uint128_t(lhs)/rhs;
}
uint128_t operator /(uint128_t lhs, const uint128_t &rhs) {
  lhs /= rhs;
  return lhs;
}

// remainder
uint128_t operator %(uint128_t lhs, const uint64_t &rhs) {
  lhs %= rhs;
  return lhs;
}
uint128_t operator %(const uint64_t &lhs, const uint128_t &rhs) {
  return uint128_t(lhs)%rhs;
}
uint128_t operator %(uint128_t lhs, const uint128_t &rhs) {
  lhs %= rhs;
  return lhs;
}

// increment
uint128_t& uint128_t::operator ++() { // prefix
  *this += 1;
  return *this;
}
uint128_t uint128_t::operator ++(int) { // postfix
  *this += 1;
  return *this;
}

// decrement
uint128_t& uint128_t::operator --() { // prefix
  *this -= 1;
  return *this;
}
uint128_t uint128_t::operator --(int) { // postfix
  *this -= 1;
  return *this;
}

// comparison operators

// equal
bool operator ==(const uint128_t &lhs, const uint64_t &rhs) {
  return lhs.block1 == 0 && lhs.block0 == rhs;
}
bool operator ==(const uint64_t &lhs, const uint128_t &rhs) {
  return rhs.block1 == 0 && rhs.block0 == lhs;
}
bool operator ==(const uint128_t &lhs, const uint128_t &rhs) {
  return lhs.block0 == rhs.block0 && lhs.block1 == rhs.block1;
}

// not equal
bool operator !=(const uint128_t &lhs, const uint64_t &rhs) {
  return !(lhs == rhs);
}
bool operator !=(const uint64_t &lhs, const uint128_t &rhs) {
  return !(lhs == rhs);
}
bool operator !=(const uint128_t &lhs, const uint128_t &rhs) {
  return !(lhs == rhs);
}

// less than
bool operator <(const uint128_t &lhs, const uint64_t &rhs) {
  return lhs.block1 == 0 && lhs.block0 < rhs;
}
bool operator <(const uint64_t &lhs, const uint128_t &rhs) {
  return rhs.block1 != 0 || lhs < rhs.block0;
}
bool operator <(const uint128_t &lhs, const uint128_t &rhs) {
  if (lhs.block1 == rhs.block1) {
    return lhs.block0 < rhs.block0;
  }
  return lhs.block1 < rhs.block1;
}

// less than or equal to
bool operator <=(const uint128_t &lhs, const uint64_t &rhs) {
  return !(rhs < lhs);
}
bool operator <=(const uint64_t &lhs, const uint128_t &rhs) {
  return !(rhs < lhs);
}
bool operator <=(const uint128_t &lhs, const uint128_t &rhs) {
  return !(rhs < lhs);
}

// greater than
bool operator >(const uint128_t &lhs, const uint64_t &rhs) {
  return rhs < lhs;
}
bool operator >(const uint64_t &lhs, const uint128_t &rhs) {
  return rhs < lhs;
}
bool operator >(const uint128_t &lhs, const uint128_t &rhs) {
  return rhs < lhs;
}

// greater than or equal to
bool operator >=(const uint128_t &lhs, const uint64_t &rhs) {
  return !(lhs < rhs);
}
bool operator >=(const uint64_t &lhs, const uint128_t &rhs) {
  return !(lhs < rhs);
}
bool operator >=(const uint128_t &lhs, const uint128_t &rhs) {
  return !(lhs < rhs);
}

// logical operators
// not implemented

// bitwise operators

// bitwise not
uint128_t operator ~(const uint128_t &i) {
  return uint128_t(~i.block0, ~i.block1);
}

// bitwise and
uint128_t operator &(uint128_t lhs, const uint64_t &rhs) {
  lhs &= rhs;
  return lhs;
}
uint128_t operator &(const uint64_t &lhs, uint128_t rhs) {
  rhs &= lhs;
  return rhs;
}
uint128_t operator &(uint128_t lhs, const uint128_t &rhs) {
  lhs &= rhs;
  return lhs;
}

// bitwise or
uint128_t operator |(uint128_t lhs, const uint64_t &rhs) {
  lhs |= rhs;
  return lhs;
}
uint128_t operator |(const uint64_t &lhs, uint128_t rhs) {
  rhs |= lhs;
  return rhs;
}
uint128_t operator |(uint128_t lhs, const uint128_t &rhs) {
  lhs |= rhs;
  return lhs;
}

// bitwise xor
uint128_t operator ^(uint128_t lhs, const uint64_t &rhs) {
  lhs ^= rhs;
  return lhs;
}
uint128_t operator ^(const uint64_t &lhs, uint128_t rhs) {
  rhs ^= lhs;
  return rhs;
}
uint128_t operator ^(uint128_t lhs, const uint128_t &rhs) {
  lhs ^= rhs;
  return lhs;
}

// bitwise left shift
uint128_t operator <<(uint128_t lhs, uint bits) {
  lhs <<= bits;
  return lhs;
}

// bitwise right shift
uint128_t operator >>(uint128_t lhs, uint bits) {
  lhs >>= bits;
  return lhs;
}

// compound assignment operators

// addition
void uint128_t::operator +=(const Block &i) {
  block0 += i;
  if (block0 < i) {
    block1++;
  }
}
void uint128_t::operator +=(const uint128_t &i) {
  block0 += i.block0;
  if (block0 < i.block0) {
    block1++;
  } 
  block1 += i.block1;
}

// subtraction
void uint128_t::operator -=(const Block &i) {
  if (block0 < i) {
    block1--;
  }
  block0 -= i;
}
void uint128_t::operator -=(const uint128_t &i) {
  if (block0 < i.block0) {
    block1--;
  }
  block0 -= i.block0;
  block1 -= i.block1;
}

// multiplication
void uint128_t::operator *=(const Block &i) {
  Block u0 = block0 & 0xFFFFFFFF;
  Block u1 = block0 >> BLOCK_SIZE/2;
  Block u2 = block1 & 0xFFFFFFFF;
  Block u3 = block1 >> BLOCK_SIZE/2;
  
  Block v0 = i & 0xFFFFFFFF;
  Block v1 = i >> BLOCK_SIZE/2;
  
  Block w0 = u0 * v0; 
  Block w1 = u0 * v1;
  Block w2 = u1 * v0;
  Block w3 = u1 * v1;
  Block w4 = u2 * v0;
  Block w5 = u3 * v0;
  
  this->block0 = w0;
  this->block1 = w3;
  *this += uint128_t(w1 << BLOCK_SIZE/2, w1 >> BLOCK_SIZE/2);
  *this += uint128_t(w2 << BLOCK_SIZE/2, w2 >> BLOCK_SIZE/2);
  this->block1 += w4;
  this->block1 += w5 << BLOCK_SIZE/2;
}
void uint128_t::operator *=(const uint128_t &i) {
  if (i.block1 == 0) {
    *this *= i.block0;
    return;
  }

  Block u0 = block0 & 0xFFFFFFFF;
  Block u1 = block0 >> BLOCK_SIZE/2;
  Block u2 = block1 & 0xFFFFFFFF;
  Block u3 = block1 >> BLOCK_SIZE/2;
  
  Block v0 = i.block0 & 0xFFFFFFFF;
  Block v1 = i.block0 >> BLOCK_SIZE/2;
  Block v2 = i.block1 & 0xFFFFFFFF;
  Block v3 = i.block1 >> BLOCK_SIZE/2;
  
  Block w0 = u0 * v0; 
  Block w1 = u0 * v1;
  Block w2 = u1 * v0;
  Block w3 = u0 * v2;
  Block w4 = u1 * v1;
  Block w5 = u2 * v0;
  Block w6 = u0 * v3;
  Block w7 = u1 * v2;
  Block w8 = u2 * v1;
  Block w9 = u3 * v0;
  
  block0 = w0;
  block1 = w3;
  *this += uint128_t(w1 << BLOCK_SIZE/2, w1 >> BLOCK_SIZE/2);
  *this += uint128_t(w2 << BLOCK_SIZE/2, w2 >> BLOCK_SIZE/2);
  block1 += w3;
  block1 += w4;
  block1 += w5;
  block1 += w6 << BLOCK_SIZE/2;
  block1 += w7 << BLOCK_SIZE/2;
  block1 += w8 << BLOCK_SIZE/2;
  block1 += w9 << BLOCK_SIZE/2;
}

// division
void uint128_t::operator /=(const Block &i) {
  if (block1 == 0) {
    *this = block0 / i;
    return;
  }
  if (i == 1) {
    return;
  }
  uint bits = ilog2(*this) - ilog2(i);
  uint128_t d = uint128_t(i) << bits;
  if (d > *this) {
    d >>= 1u;
    bits--;
  }
  uint128_t n = *this - d;
  uint128_t q = uint128_t(1) << bits;
  *this = q + (n/i);
}

void uint128_t::operator /=(const uint128_t &i) {
  if (i.block1 == 0) {
    *this /= i.block0;
    return;
  }
  if (i == 1ull) {
    return;
  }
  uint bits = ilog2(*this) - ilog2(i);
  uint128_t d = i << bits;
  if (d > *this) {
    d >>= 1u;
    bits--;
  }
  uint128_t n = *this - d;
  uint128_t q = uint128_t(1) << bits;
  *this = q + (n/i);
}

void uint128_t::operator %=(const Block &i) {
  if (block1 == 0) {
    *this = block0 % i;
    return;
  }
  if (i > *this) { return; }
  if (i == *this) { 
    *this = 0;
    return;
  }
  uint bits = ilog2(*this) - ilog2(i);
  uint128_t d = uint128_t(i) << bits;
  if (d > *this) {
    d >>= 1u;
  }
  uint128_t n = uint128_t(*this) - d;
  *this = n % i;
}
void uint128_t::operator %=(const uint128_t &i) {
  if (i.block1 == 0) {
    *this %= i.block0;
    return;
  }
  if (i > *this) { return; }
  if (i == *this) { 
    *this = 0;
    return;
  }
  uint bits = ilog2(*this) - ilog2(i);
  uint128_t d = uint128_t(i) << bits;
  if (d > *this) {
    d >>= 1u;
  }
  uint128_t n = uint128_t(*this) - d;
  *this = n % i;
}

// and equal
void uint128_t::operator &=(const Block &i) {
  block0 &= i;
  block1 = 0;
}
void uint128_t::operator &=(const uint128_t &i) {
  block0 &= i.block0;
  block1 &= i.block1;
}

// or
void uint128_t::operator |=(const Block &i) {
  block0 |= i;
}
void uint128_t::operator |=(const uint128_t &i) {
  block0 |= i.block0;
  block1 |= i.block1;
}

// exclusive or
void uint128_t::operator ^=(const Block &i) {
  block0 ^= i;
}
void uint128_t::operator ^=(const uint128_t &i) {
  block0 ^= i.block0;
  block1 ^= i.block1;
}

// left shift
void uint128_t::operator <<=(uint i) {
  i %= 2*BLOCK_SIZE;
  if (i < BLOCK_SIZE) {
    block1 = (block1 << i) | (block0 >> (BLOCK_SIZE - i));
    block0 <<= i;
  } else {
    block1 = block0 << (i - BLOCK_SIZE);
    block0 = 0;
  }
}

// right shift
void uint128_t::operator >>=(uint i) {
  i %= 2*BLOCK_SIZE;
  if (i < BLOCK_SIZE) {
    block0 = (block0 >> i) | (block1 << (BLOCK_SIZE - i));
    block1 >>= i;
  } else {
    block0 = block1 >> (i - BLOCK_SIZE);
    block1 = 0;
  }
}

// additional math functions 
// integer logarithm base 2
uint ilog2(Block i) { // cut+pasted from bits.h
  uint r = 0;
  if (i & 0xFFFFFFFF00000000ull) {
    i >>= 32;
    r   = 32;
  }
  if (i & 0xFFFF0000) {
    i >>= 16;
    r  |= 16;
  }
  if (i & 0xFF00) {
    i >>= 8;
    r  |= 8;
  }
  if (i & 0xF0) {
    i >>= 4;
    r  |= 4;
  } 
  if (i & 0xC) {
    i >>= 2;
    r  |= 2;
  }
  if (i & 0x2) {
    r  |= 1;
  }
  return r;
}
uint ilog2(const uint128_t &i) {
  if (i.block1 > 0) {
    return 64 + ilog2(i.block1);
  }
  return ilog2(i.block0);
}

// printing
// to_string
std::string to_string(uint128_t i) {
  if (i == uint128_t(0)) { return "0"; }
  std::string s = "";
  while (i > 1ull) {
    std::string is = std::to_string(uint64_t(i % P10_UINT64));
    s.insert(s.begin(), is.begin(), is.end());
    i /= P10_UINT64;
  }
  return s;
}
