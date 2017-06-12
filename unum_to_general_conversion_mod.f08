module unum_to_general_conversion_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use IEEE_ARITHMETIC
  use unum_t_mod
  use general_interval_t_mod
  use ubound_t_mod
  use unum_operator_mod
  use unum_env_mod
  use unum_to_float_mod

  private
  logical, parameter, public :: CLOSED_END  = .true.
  logical, parameter, public :: OPEN_END = .false.
  public :: F2g
  public :: U2g

  interface U2g
    module procedure Unum2g
    module procedure Ubound2g
  end interface U2g

contains
  function F2g (x)
    implicit none
    real (REAL64), intent (in) :: x
    type (general_interval_t)  :: F2g
    real (REAL64)              :: NaN
    NaN = ieee_value (NaN, ieee_quiet_nan)

    if (ieee_is_nan (x)) then
      F2g%endpoints = [NaN, NaN]
      F2g%are_closed = [OPEN_END, OPEN_END]
    else
      F2g%endpoints = [x, x]
      F2g%are_closed = [CLOSED_END, CLOSED_END]
    end if
  end function

  function Bigu (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: expomask_local, fracmask_local
    type (unum_t)              :: efsizemask_local, ulpu_local
    type (unum_t)              :: u_efsizeu
    type (unum_t)              :: Bigu

    expomask_local = Expomask (u)
    fracmask_local = Fracmask (u)
    efsizemask_local = Get_efsizemask ()
    ulpu_local = Get_ulpu ()
    u_efsizeu = iand (efsizemask_local, u)

    Bigu%u = expomask_local%u + &
             fracmask_local%u + &
             u_efsizeu%u      - &
             ulpu_local%u * Boole (u_efsizeu == efsizemask_local)
  end function Bigu

  function Big (u)
    implicit none
    type (unum_t), intent (in) :: u
    real (REAL64)              :: Big

    Big = U2f (Bigu (u))
  end function Big

  function Unum2g (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (general_interval_t)  :: Unum2g
    real (REAL64)              :: NaN, negative_inf, positive_inf
    real (REAL64)              :: temp_x, temp_y
    type (unum_t)              :: signmask_local, ubitmask_local, ulpu_local
    type (unum_t)              :: bigu_temp
    type (unum_t)              :: u_plus_ulpu
    NaN = ieee_value (NaN, ieee_quiet_nan)
    negative_inf = ieee_value (negative_inf, IEEE_NEGATIVE_INF)
    positive_inf = ieee_value (positive_inf, IEEE_POSITIVE_INF)

    if (u == Get_qNaNu () .or. u == Get_sNaNu ()) then
      Unum2g%endpoints = [NaN, NaN]
      Unum2g%are_closed = [OPEN_END, OPEN_END]
      return
    else
      temp_x = U2f (Exact (u))
      !print *, 'temp_x = ', temp_x
      ulpu_local = Get_ulpu ()
      u_plus_ulpu%u = u%u + ulpu_local%u
      temp_y = U2f (Exact (u_plus_ulpu))
      !print *, 'temp_y = ', temp_y
    end if

    if (IsExQ (u)) then
      Unum2g%endpoints = [temp_x, temp_x]
      Unum2g%are_closed = [CLOSED_END, CLOSED_END]
    else
      signmask_local = Signmask (u)
      ubitmask_local = Get_ubitmask ()
      bigu_temp = Bigu (u)
      if (u%u == (bigu_temp%u + ubitmask_local%u)) then
        Unum2g%endpoints = [Big(u), positive_inf]
        Unum2g%are_closed = [OPEN_END, OPEN_END]
      else if (u%u == (signmask_local%u + bigu_temp%u + ubitmask_local%u)) then
        Unum2g%endpoints = [negative_inf, -Big (u)]
        Unum2g%are_closed = [OPEN_END, OPEN_END]
      else if (Signi (u) == 1) then
        Unum2g%endpoints = [temp_y, temp_x]
        Unum2g%are_closed = [OPEN_END, OPEN_END]
      else
        Unum2g%endpoints = [temp_x, temp_y]
        Unum2g%are_closed = [OPEN_END, OPEN_END]
      end if
    end if
  end function Unum2g

  function Ubound2g (ubound_in)
    implicit none
    type (ubound_t), intent (in) :: ubound_in
    type (general_interval_t)    :: Ubound2g
    type (general_interval_t)    :: gL, gR
    real (REAL64)                :: NaN
    NaN = ieee_value (NaN, ieee_quiet_nan)

    if (ubound_in%ub(1) == Get_qNaNu () .or. &
        ubound_in%ub(1) == Get_sNaNu () .or. &
        ubound_in%ub(2) == Get_qNaNu () .or. &
        ubound_in%ub(2) == Get_sNaNu ()) then
      Ubound2g%endpoints = [NaN, NaN]
      Ubound2g%are_closed = [OPEN_END, OPEN_END]
    else
      gL = Unum2g (ubound_in%ub(1))
      gR = Unum2g (ubound_in%ub(2))
      Ubound2g%endpoints = [gL%endpoints(1), gR%endpoints(2)]
      Ubound2g%are_closed = [gL%are_closed(1), gR%are_closed(2)]
    end if
  end function Ubound2g

end module unum_to_general_conversion_mod
