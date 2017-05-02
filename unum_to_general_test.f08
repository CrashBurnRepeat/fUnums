program unum_to_general_test
  use ISO_FORTRAN_ENV, only : REAL64, UNUM => INT64
  use unum_to_float_mod
  use unum_to_general_conversion_mod
  use unum_env_mod
  use real_to_unum_mod
  use display_env_mod, only : Display_unum => Display_env
  use display_unum_as_real_mod

  implicit none

  integer (UNUM)               :: unum_exact1
  integer (UNUM)               :: unum_exact2
  integer (UNUM)               :: unum_inexact1
  integer (UNUM)               :: unum_inexact2
  integer (UNUM), dimension(2) :: ubound_closed_closed
  integer (UNUM), dimension(2) :: ubound_closed_open
  integer (UNUM), dimension(2) :: ubound_open_closed
  integer (UNUM), dimension(2) :: ubound_open_open

  real (REAL64) :: real_temp

  real (REAL64), dimension(2, 2) :: g_interval

  call Set_unum_env (4, 5)

  real_temp = 0.1

  unum_exact1 = x2u (real_temp)
  unum_exact2 = x2u (2.0_REAL64)
  print *, 'unum_exact1 is'
  call Display_unum (unum_exact1)
  print *, 'unum_exact1 back to real is: '
  call Display_unum_as_real (unum_exact1)
  print *, 'unum_exact1 should be: ', real_temp
  unum_inexact1 = unum_exact1 !+ Get_ubitmask ()
  unum_inexact2 = unum_exact2 + Get_ubitmask ()
  print *, 'unum_inexact1 is'
  call Display_unum (unum_exact1)

  ubound_closed_closed = [unum_exact1, unum_exact2]
  ubound_closed_open   = [unum_exact1, unum_inexact2]
  ubound_open_closed   = [unum_inexact1, unum_exact2]
  ubound_open_open     = [unum_inexact1, unum_inexact2]

  g_interval = U2g (unum_exact1)

  print *, g_interval(1,1), g_interval(2,1)
  print *, g_interval(1,2), g_interval(2,2)

end program unum_to_general_test
