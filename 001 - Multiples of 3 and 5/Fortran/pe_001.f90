! file:   pe_001.f90
! author: Alexander Gosselin
! e-mail: alexandergosselin@gmail.com
! date:   May 8, 2015

! link:   https://projecteuler.net/problem=1

! copyright: (C) 2015 Alexander Gosselin
! license:   GNU General Public License <http://www.gnu.org/licenses/>

function sum_divisible_by(x, n) result(y)
  integer :: x ! input
  integer :: y ! output
  integer :: d
  d = n/x
  y = x * (d * (d + 1))/2
end function sum_divisible_by

program pe_001
  implicit none
  integer :: n = 999
  integer :: sum_divisible_by
  print*, sum_divisible_by(3, n) + sum_divisible_by(5, n) &
          - sum_divisible_by(15, n)
end program pe_001
