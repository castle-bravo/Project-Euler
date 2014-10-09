#|
file:   pe_003.lisp
title:  Largest prime factor
author: Alexander P. Gosselin
mail:   alexandergosselin@gmail.com
date:   October 8, 2014
|#

(defun largest-prime-factor (n)
  (if (< n 0)
      (setf n (* n -1))
    ()
  )
  (loop while (= (mod n 2) 0) do
        (setf n (/ n 2))
  )
  (if (= n 1)
      (return-from largest_prime-factor 2)
    ()
  )
  (let ((d 1)) 
    (loop while (/= n 1) do
          (setf d (+ d 2))
          (if (= (mod n d) 0)
              (setf n (/ n d))
            ()
          )
    )
    (return-from largest-prime-factor d)
  )
)

(defun pe_003 ()
  (prin1 (largest-prime-factor 600851475143))
  (write-char #\linefeed)
)

(pe_003)
