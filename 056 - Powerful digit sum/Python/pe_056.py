#!/usr/bin/python
'''
file:   pe_056.py
title:  Powerful digit sum
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   October 5, 2014
'''

max_digit_sum = 0;

for a in range(2, 100):
    for b in range(1, 100):
        p = a**b
        digit_sum = 0;
        for c in str(p):
            digit_sum += int(c)
        if digit_sum > max_digit_sum:
            max_digit_sum = digit_sum

print max_digit_sum
