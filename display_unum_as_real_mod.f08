module display_unum_as_real_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use unum_to_float_mod
  use unum_env_mod,    only : Get_ulpu, Get_ubitmask
  use unum_t_mod
  implicit none

contains
  subroutine Display_unum_as_real (u)
    implicit none
    type (unum_t) :: u
    type (unum_t) :: u_temp, ulpu_local, ubitmask_local

    ulpu_local = Get_ulpu ()
    ubitmask_local = Get_ubitmask ()
    u_temp%u = u%u + ulpu_local%u

    if (IsExQ (u)) then
      print *, u2f (u)
    else
      write (unit=*, fmt=10) u2f (u), u2f (u_temp)
    end if
10 format ('(' , f15.5, ', ', f15.5, ')')
  end subroutine Display_unum_as_real
end module display_unum_as_real_mod
