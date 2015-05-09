! file:   pe_002.f90
! title:  Even Fibbonacci numbers
! author: Alexander Gosselin
! e-mail: alexandergosselin@gmail.com
! date:   May 8, 2015
! 
! link:   https://projecteuler.net/problem=2
! 
! copyright: (C) 2015 Alexander Gosselin
! license:   GNU General Public License <http://www.gnu.org/licenses/>

program pe_002
  implicit none
  integer :: max = 4000000
  integer :: prev = 0, curr = 2, next = 0
  integer :: sum
  do while (next < max)
    sum = sum + curr
    next = 4*curr + prev
    prev = curr
    curr = next
  end do
  print '(I0)', sum
end program pe_002
