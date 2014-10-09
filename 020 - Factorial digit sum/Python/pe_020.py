#!/usr/bin/python
'''
file:   pe_020.py
title:  Factorial digit sum
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   October 8, 2014
'''

N = 100
digit_sum = 0
factorial = 1
for i in range(2, N + 1):
    factorial *= i
for c in str(factorial):
  digit_sum += int(c)

print digit_sum
