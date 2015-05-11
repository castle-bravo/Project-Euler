! file:   pe_004.f90
! title:  Largest palindrome product
! author: Alexander Gosselin
! e-mail: alexandergosselin@gmail.com
! date:   May 8, 2015
! 
! link:   https://projecteuler.net/problem=1
! 
! copyright: (C) 2015 Alexander Gosselin
! license:   GNU General Public License <http://www.gnu.org/licenses/>

function is_palindrome(n) result(tf)
  integer :: n
  logical :: tf
  character(6) :: s
  integer :: l, i
  write (s, '(I0)') n
  tf = .TRUE.
  l = len(trim(s))
  i = 1
  do while (i <= l/2)
    if (s(i:i) /= s(l-i+1:l-i+1)) then
      tf = .FALSE.
      exit
    else
      i = i + 1
    end if
  end do
end function is_palindrome

program pe_003
  logical :: is_palindrome
  integer :: i = 999, j, k
  do while (i > 0)
    k = i
    j = i
    do while ((j > 0).AND.(k <= 999))
      if (is_palindrome(j*k)) then
        print '(I0)', j*k
        return
      else
        j = j - 1
        k = k + 1
      end if
    end do
    k = i
    j = i - 1
    do while ((j > 0).AND.(k <= 999))
      if (is_palindrome(j*k)) then
        print '(I0)', j*k
        return
      else
        j = j - 1
        k = k + 1
      end if
    end do
    i = i - 1
  end do
end program
