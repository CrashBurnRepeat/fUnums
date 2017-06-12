program unum_type_conversion_test
  use ISO_FORTRAN_ENV, only : REAL64
  use unum_env_mod
  use unum_to_float_mod
  use real_to_unum_mod
  use display_unum_as_real_mod
  use display_masks_mod
  use unum_t_mod
  use display_env_mod, only : Display_unum => Display_env,&
                              Display_posinfu,&
                              Display_neginfu

  implicit none
  real (REAL64) :: real_temp
  real (REAL64) :: temp_result
  type (unum_t) :: unum_temp

  !integer (INT64) :: unum_pi     = b'011001001000011111101111' !(1,4) env

  call Set_unum_env (4,5)

  !print *, 'esize mask'
  !call Display_esizemask ()

  !print *, 'fsize mask'
  !call Display_fsizemask ()

  !print *, 'efsize mask'
  !call Display_efsizemask ()

  !print *, 'ubit mask'
  !call Display_ubitmask ()

  !print *, 'utag mask'
  !call Display_utagmask ()

  print *, 'Max ubits = ', Get_maxubits ()
  print *, 'Maxreal = ', Get_maxreal ()

  print *, 'smallnormal'
  print *, U2f (Get_smallnormalu ())

  !print *, 'Approximate Pi'
  !print *, '(', u2f (unum_pi), ',', u2f (unum_pi + Get_ulpu ()), ')'

  print *, 'Positive infinity'
  call Display_posinfu ()
  print *, U2f (Get_posinfu ())

  !print *, 'Signbigu'
  !call Display_unum (Get_signbigu ())

  print *, 'Negative infinity'
  call Display_neginfu ()
  print *, U2f (Get_neginfu ())

  real_temp = 16.12
  print *, 'Value in double precision is: ', real_temp
  print *, 'Convert to unum'
  unum_temp = X2u (real_temp)
  call Display_unum (unum_temp)

  print *, 'Convert back to real'
  call Display_unum_as_real (unum_temp)

end program unum_type_conversion_test
