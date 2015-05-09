! file:   pe_003.f90
! title:  Largest prime factor
! author: Alexander Gosselin
! e-mail: alexandergosselin@gmail.com
! date:   May 8, 2015
! 
! link:   https://projecteuler.net/problem=1
! 
! copyright: (C) 2015 Alexander Gosselin
! license:   GNU General Public License <http://www.gnu.org/licenses/>

program pe_003
  implicit none 
  integer, parameter :: int64 = selected_int_kind(13)
  integer(int64) :: n = 600851475143_int64
  integer :: i = 3
  do while (i*i < n)
    if (mod(n, i) == 0) then
      n = n/i
    else
      i = i + 2
    end if
  end do
  print '(I0)', n
end program
