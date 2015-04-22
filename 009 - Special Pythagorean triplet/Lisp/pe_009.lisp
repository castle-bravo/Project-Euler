#|
file:   pe_009.lisp
title:  Special Pythagorean triplet
author: Alexander P. Gosselin
mail:   alexandergosselin@gmail.com
date:   April 21, 2015

link:   https://projecteuler.net/problem=9

math:   
  given a < b < c, a^2 + b^2 = c^2, and a + b + c = 1000
  It follows that:
    c = 1000 - a - b
    a^2 + b^2 = (1000 - a - b)^2
    2000(a + b) - 2ab = 1000000 
  Subject to 
    2b < 1000 - a, and a < b
  So the maximum value of a is 332 when b = 333 and c = 335

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
|#

(defun special-pythagorean-triplet (N)
  (let ((a 1))
    (loop while (< a (/ N (sqrt 2))) do
      (setq b (floor (- (* N N) (* (* 2 N) a)) (- (* 2 N) (* 2 a))))
      (setq c (- 1000 (+ a b)))
      (if (= (+ (* a a) (* b b)) (* c c))
        (return-from special-pythagorean-triplet (* a b c))
      )
      (incf a)
    )
  )
)

(defun pe_009 ()
  (prin1 (special-pythagorean-triplet 1000))
  (write-char #\linefeed)
)

(pe_009)
