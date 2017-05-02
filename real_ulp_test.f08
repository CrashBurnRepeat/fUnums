program real_ulp_test
  use ISO_FORTRAN_ENV, only : REAL32
  implicit none

  real (REAL32) :: sum = 0.0
  integer :: i

  do i = 0, 1000000000
    sum = sum + 1.0_REAL32
  end do

  print *, 'Final value is ', sum

end program real_ulp_test
