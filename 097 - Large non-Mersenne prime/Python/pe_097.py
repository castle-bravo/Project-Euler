#!/usr/bin/env python
'''
file:   pe_97.py
title:  Large non-Mersenne prime
author: Alexander Gosselin
e-mail: alexandergosselin@gmail.com
date:   April 20, 2015

link:   https://projecteuler.net/problem=97

reference:
  Modular Multiplication, Exponentiation:
  http://en.wikipedia.org/wiki/Modular_arithmetic
  http://en.wikipedia.org/wiki/Modular_exponentiation

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
'''

digits = 10
modulus = 10**digits

print 28433 * pow(2, 7830457, modulus) % modulus + 1
