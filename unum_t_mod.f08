module unum_t_mod
  use ISO_FORTRAN_ENV, only : INT64
  implicit none

  type :: unum_t
    integer (INT64) :: u
  end type unum_t

end module unum_t_mod
