program unum_to_general_test
  use ISO_FORTRAN_ENV, only : REAL64
  use unum_to_float_mod
  use unum_to_general_conversion_mod
  use unum_env_mod
  use unum_t_mod
  use ubound_t_mod
  use general_interval_t_mod
  use real_to_unum_mod
  use display_env_mod, only : Display_unum => Display_env
  use display_unum_as_real_mod

  implicit none

  type (unum_t)   :: unum_exact1
  type (unum_t)   :: unum_exact2
  type (unum_t)   :: unum_inexact1
  type (unum_t)   :: unum_inexact2
  type (unum_t)   :: ubitmask_local
  type (ubound_t) :: ubound_closed_closed
  type (ubound_t) :: ubound_closed_open
  type (ubound_t) :: ubound_open_closed
  type (ubound_t) :: ubound_open_open

  real (REAL64) :: real_temp1, real_temp2

  type (general_interval_t) :: g_interval

  call Set_unum_env (4, 5)

  real_temp1 = 0.1
  real_temp2 = 2.1
  ubitmask_local = Get_ubitmask ()

  unum_exact1 = Exact(X2u (real_temp1))
  unum_exact2 = Exact(X2u (real_temp2))
  print *, 'unum_exact1 is'
  call Display_unum (unum_exact1)
  print *, 'unum_exact1 back to real is: '
  call Display_unum_as_real (unum_exact1)
  print *, 'unum_exact1 should be: ', real_temp1
  print *, 'unum_exact2 is'
  call Display_unum (unum_exact2)
  print *, 'unum_exact2 back to real is: '
  call Display_unum_as_real (unum_exact2)
  print *, 'unum_exact2 should be: ', real_temp2
  unum_inexact1%u = unum_exact1%u + ubitmask_local%u
  unum_inexact2%u = unum_exact2%u + ubitmask_local%u
  print *, 'unum_inexact1 is'
  call Display_unum (unum_inexact1)
  print *, 'unum_inexact2 is'
  call Display_unum (unum_inexact2)
  call Display_unum_as_real (unum_inexact2)

  ubound_closed_closed%ub = [unum_exact1, unum_exact2]
  ubound_closed_open%ub   = [unum_exact1, unum_inexact2]
  ubound_open_closed%ub   = [unum_inexact1, unum_exact2]
  ubound_open_open%ub     = [unum_inexact1, unum_inexact2]

  g_interval = U2g (ubound_open_open)

  print *, g_interval%endpoints
  print *, g_interval%are_closed

end program unum_to_general_test
