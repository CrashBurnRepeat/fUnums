module general_interval_t_mod
  use ISO_FORTRAN_ENV, only : REAL64

  type :: general_interval_t
    real (REAL64), dimension (2) :: endpoints
    logical,       dimension (2) :: are_closed
  end type general_interval_t

end module general_interval_t_mod
