#|
file:   pe_004.lisp
title:  Largest palindrome product
author: Alexander P. Gosselin
mail:   alexandergosselin@gmail.com
date:   October 8, 2014
|#

(defun is-palindromic (x)
  (let ((s (write-to-string x)))
    (if (string= s (reverse s))
        (return-from is-palindromic T)
      (return-from is-palindromic NIL))
  )
)

(defun largest-palindromic-product (max-factor)
  (loop for i from 0 to max-factor do
        (loop for j from 0 to i do
              (let ((p (* (- max-factor (+ i j)) 
                          (- max-factor (- i j)))))
                (if (is-palindromic p)
                    (return-from largest-palindromic-product p)
                  ())
              )
        )
        (loop for k from 0 to (- i 1) do
              (let ((p (* (- max-factor (+ i k)) 
                          (- max-factor (- i k 1)))))
                (if (is-palindromic p)
                    (return-from largest-palindromic-product p)
                  ())
              )
        )
  )
)

(defun pe_004 ()
  (prin1 (largest-palindromic-product 999))
  (write-char #\linefeed)
)

(pe_004)
