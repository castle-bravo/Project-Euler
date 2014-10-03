#|
file:   pe_001.lisp
title:  Multiples of 3 and 5
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   September 29, 2014
|#

(defun sum-of-multiples-of-3-or-5-less-than (N)
  (loop for i from 1 to N
        if (or (= (mod i 3) 0) 
               (= (mod i 5) 0))
        sum i
  )
)

(defun pe_001 ()
  (prin1 (sum-of-multiples-of-3-or-5-less-than 1000))
  (write-char #\linefeed)
)

(pe_001)
