#|
file:   pe_002.lisp
title:  Even Fibonacci numbers
author: Alexander P. Gosselin
mail: alexandergosselin@gmail.com
date:   September 29, 2014
|#

(defun even-fibonacci-sum (N)
  (let ((prev 0)
        (curr 2)
        (current-sum 0))
    (loop while (< curr N) do
          (setf current-sum (+ current-sum curr))
          (let ((next (+ (* 4 curr) prev)))
            (setf prev curr)
            (setf curr next)
          )
    )
    (return-from even-fibonacci-sum current-sum)
  )
)

(defun pe_002 ()
  (prin1 (even-fibonacci-sum 4000000))
  (write-char #\linefeed)
)

(pe_002)
