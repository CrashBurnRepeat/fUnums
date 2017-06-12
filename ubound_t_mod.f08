module ubound_t_mod
  use unum_t_mod
  implicit none

  type :: ubound_t
    type (unum_t), dimension (2) :: ub
  end type ubound_t
  
end module ubound_t_mod
