#|
file:   pe_005.lisp
title:  Smallest common multiple
author: Alexander P. Gosselin
mail:   alexandergosselin@gmail.com
date:   October 9, 2014

link:   https://projecteuler.net/problem=5

note:   I got some help with this solution from the folks over at
        stack overflow:
        http://stackoverflow.com/q/26285189/2738025
|#

(defparameter *primes* '(2 3 5 7 11 13 17 19))

(defun factorize (n)
  (let ((exponents (mapcar (constantly 0) *primes*)))
    (loop for i from 0 to (1- (length *primes*)) do
          (loop while (and (= (mod n (nth i *primes*)) 0) 
                           (not (= n 1))) do
            (incf (nth i exponents))
            (setf n (/ n (nth i *primes*)))
      )
    )
    (return-from factorize exponents)
  )
)

(defun smallest-common-multiple (n)
  (let ((multiple 1)
        (exponents (mapcar (constantly 0) *primes*)))
    (loop for i from 2 to n do
          (let ((factorization (factorize i)))
            ;(print factorization)
            (loop for i from 0 to (1- (length exponents)) do
              (setf (nth i exponents) 
                    (max (nth i exponents) (nth i factorization))
              )
            )
          )
    )
    (loop for i from 0 to (1- (length exponents)) do
      (setf multiple (* multiple (expt (nth i *primes*) (nth i exponents))))
    )
    (return-from smallest-common-multiple multiple)
  )
)

(defun pe_005 ()
  (prin1 (smallest-common-multiple 20))
  (write-char #\linefeed)
)

(pe_005)
