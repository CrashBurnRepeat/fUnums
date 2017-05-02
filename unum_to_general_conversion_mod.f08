module unum_to_general_conversion_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use IEEE_ARITHMETIC
  use unum_env_mod
  use unum_to_float_mod

  private
  real (REAL64), parameter, public :: CLOSED_END  = 1.0
  real (REAL64), parameter, public :: OPEN_END = 0.0
  public :: F2g
  public :: U2g

  interface U2g
    module procedure Unum2g
    module procedure Ubound2g
  end interface U2g

contains
  function F2g (x)
    implicit none
    real (REAL64), dimension (2, 2) :: F2g
    real (REAL64)                   :: x
    real (REAL64)                   :: NaN
    NaN = ieee_value (NaN, ieee_quiet_nan)

    if (ieee_is_nan (x)) then
      F2g = reshape ([NaN, NaN, OPEN_END, OPEN_END], shape (F2g))
    else
      F2g = reshape ([x, x, CLOSED_END, CLOSED_END], shape (F2g))
    end if
  end function

  function Bigu (u)
    implicit none
    integer (INT64) :: Bigu
    integer (INT64), intent (in) :: u

    Bigu = Expomask (u) + Fracmask (u) + &
           iand (Get_efsizemask (), u) - Get_ulpu () * &
           Boole (iand (u, Get_efsizemask ()) == Get_efsizemask ())
  end function Bigu

  function Big (u)
    implicit none
    real (REAL64) :: Big
    integer (INT64), intent (in) :: u

    Big = u2f (Bigu (u))
  end function Big

  function Unum2g (u)
    implicit none
    real (REAL64),   dimension (2, 2) :: Unum2g
    integer (INT64), intent (in)      :: u
    real (REAL64)                     :: NaN, negative_inf, positive_inf
    real (REAL64)                     :: temp_x, temp_y
    NaN = ieee_value (NaN, ieee_quiet_nan)
    negative_inf = ieee_value (negative_inf, IEEE_NEGATIVE_INF)
    positive_inf = ieee_value (positive_inf, IEEE_POSITIVE_INF)

    if (u == Get_qNaNu () .or. u == Get_sNaNu ()) then
      Unum2g = reshape ([NaN, NaN, OPEN_END, OPEN_END], shape (Unum2g))
      return
    else
      temp_x = u2f (Exact (u))
      print *, 'temp_x = ', temp_x
      temp_y = u2f (Exact (u + Get_ulpu ()))
      print *, 'temp_y = ', temp_y
    end if

    if (IsExQ (u)) then
      Unum2g = reshape ([temp_x, temp_x, CLOSED_END, CLOSED_END], shape (Unum2g))
    else if (u == (Bigu (u) + Get_ubitmask ())) then
      Unum2g = reshape ([Big (u), positive_inf, OPEN_END, OPEN_END], &
               shape (Unum2g))
    else if (u == (Signmask (u) + Bigu (u) + Get_ubitmask ())) then
      Unum2g = reshape ([negative_inf, -Big (u), OPEN_END, OPEN_END], &
               shape (Unum2g))
    else if (Signu (u) == 1) then
      Unum2g = reshape ([temp_y, temp_x, OPEN_END, OPEN_END], shape (Unum2g))
    else
      Unum2g = reshape ([temp_x, temp_y, OPEN_END, OPEN_END], shape (Unum2g))
    end if
  end function Unum2g

  function Ubound2g (ubound_in)
    implicit none
    real (REAL64),   dimension (2, 2)           :: Ubound2g
    integer (INT64), dimension (2), intent (in) :: ubound_in
    real (REAL64),   dimension (2, 2)           :: gL, gR
    real (REAL64)                               :: NaN
    NaN = ieee_value (NaN, ieee_quiet_nan)

    if (ubound_in(1) == Get_qNaNu () .or. ubound_in(1) == Get_sNaNu () .or. &
        ubound_in(2) == Get_qNaNu () .or. ubound_in(2) == Get_sNaNu ()) then
      Ubound2g = reshape ([NaN, NaN, OPEN_END, OPEN_END], shape (Ubound2g))
    else
      gL = Unum2g (ubound_in(1))
      gR = Unum2g (ubound_in(2))
      Ubound2g = reshape ([gL(1, 1), gR(2, 1), gL(1, 2), gR(2, 2)], &
                 shape (Ubound2g))
    end if
  end function Ubound2g

end module unum_to_general_conversion_mod
