program setup_env_test
  use unum_env_mod
  use display_masks_mod
  use display_env_mod
  implicit none

  ! Test bound checking
  call Set_unum_env (-1, 1)
  call Set_unum_env (1, -1)
  call Set_unum_env (-1, -1)
  call Set_unum_env (20, 1)
  call Set_unum_env (1, 20)
  call Set_unum_env (20, 20)
  ! Set with real bounds
  call Set_unum_env (2,3)
  ! Display environmental parameters
  print *, 'esizesize'
  print *, Get_esizesize ()
  print *, 'fsizesize'
  print *, Get_fsizesize ()
  print *, 'utagsize'
  print *, Get_utagsize ()
  print *, 'maxubits'
  print *, Get_maxubits ()

  print *, 'Masks'
  call Display_fsizemask ()
  call Display_esizemask ()
  call Display_efsizemask ()
  call Display_ubitmask ()
  call Display_utagmask ()

  print *, 'posinfu'
  call Display_posinfu ()
  print *, 'neginfu'
  call Display_neginfu ()
  print *, 'qNaNu'
  call Display_qNaNu ()
  print *, 'sNaNu'
  call Display_sNaNu ()
  !print *, 'A great color is '//achar(27)//'[95m pink '//achar(27)//'[0m.'
  print *, 'maxrealu'
  call Display_maxrealu ()
  print *, 'negbigu'
  call Display_negbigu ()
  print *, 'maxreal'
  print *, Get_maxreal ()
  print *, 'smallsubnormalu'
  call Display_smallsubnormalu ()
  print *, 'smallsubnormal'
  print *, Get_smallsubnormal ()
end program setup_env_test
