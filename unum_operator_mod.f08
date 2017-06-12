module unum_operator_mod
  use unum_t_mod
  implicit none

  interface operator (==)
    module procedure Is_unum_equivalent
  end interface operator (==)

  interface iand
    module procedure Unum_bitwise_and
  end interface iand

  interface ieor
    module procedure Unum_bitwise_eor
  end interface ieor

contains
  function Is_unum_equivalent (a, b)
    implicit none
    type (unum_t), intent (in) :: a, b
    logical                    :: Is_unum_equivalent
    Is_unum_equivalent = (a%u == b%u)
  end function Is_unum_equivalent

  function Unum_bitwise_and (a, b)
    implicit none
    type (unum_t), intent (in) :: a, b
    type (unum_t)              :: Unum_bitwise_and
    Unum_bitwise_and%u = iand (a%u, b%u)
  end function Unum_bitwise_and

  function Unum_bitwise_eor (a, b)
    implicit none
    type (unum_t), intent (in) :: a, b
    type (unum_t)              :: Unum_bitwise_eor
    Unum_bitwise_eor%u = ieor (a%u, b%u)
  end function Unum_bitwise_eor
end module unum_operator_mod
