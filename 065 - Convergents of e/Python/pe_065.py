#!/usr/bin/env python
'''
file:   pe_065.py
title:  Convergents of e
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   November 28, 2014

link:   https://projecteuler.net/problem=65
'''

n = 100 # find the nth convergent of e

def main():
  
    digit_sum = 0
    A = [1] * n
    A[0] = 2
    A[2::3] = range(2, 2*len(A[2::3]) + 1, 2)
    A.append(1)
    
    for i in xrange(len(A) - 3, -1, -1):
        A[i] *= A[i + 1]
        A[i] += A[i + 2]
    
    for c in str(A[0]):
        digit_sum += int(c)
    
    print(digit_sum)

main()
