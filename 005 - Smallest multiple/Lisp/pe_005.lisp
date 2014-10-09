#|
file:   pe_005.lisp
title:  Smallest multiple
author: Alexander P. Gosselin
mail:   alexandergosselin@gmail.com
date:   October 9, 2014

link:   https://projecteuler.net/problem=5
|#

(defparameter *primes* '(2 3 5 7 11 13 17 19))

(defun smallest-common-multiple (n)
  (let ((multiple 1)
        (sqrt-n (sqrt n)))
    (loop for p in *primes*
          if (> p sqrt-n) do (setf multiple (* p multiple))
          else do
            (let ((prime-power p))
              (loop while (< prime-power n) do
                (setf multiple (* p multiple))
                (setf prime-power (* p prime-power))
              )
            )
          finally (return multiple)
    )
  )
)

(defun pe_005 ()
  (prin1 (smallest-common-multiple 20))
  (write-char #\linefeed)
)

(pe_005)
