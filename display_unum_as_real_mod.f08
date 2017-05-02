module display_unum_as_real_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use unum_to_float_mod
  use unum_env_mod,    only : Get_ulpu, Get_ubitmask
  implicit none

contains
  subroutine Display_unum_as_real (u)
    implicit none
    integer (INT64) :: u

    if (IsExQ (u)) then
      print *, u2f (u)
    else
      write (unit=*, fmt=10) u2f (u), u2f(u + Get_ulpu () - Get_ubitmask ())
    end if
10 format ('(' , f15.5, ', ', f15.5, ')')
  end subroutine Display_unum_as_real
end module display_unum_as_real_mod
