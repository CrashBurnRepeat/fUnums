module unum_to_float_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use IEEE_ARITHMETIC, only : IEEE_NEGATIVE_INF,&
                              IEEE_POSITIVE_INF,&
                              ieee_value
  use unum_env_mod,    only : Get_esizemask,&
                              Get_esizemax,&
                              Get_fsizesize,&
                              Get_fsizemask,&
                              Get_fsizemax,&
                              Get_utagsize,&
                              Get_ubitmask,&
                              Get_posinfu,&
                              Get_neginfu
  implicit none
  private
  public :: u2f
  public :: Boole
  public :: Signmask
  public :: Fracmask
  public :: Expomask
  public :: Signu
  public :: Exact
  public :: IsExQ

contains
  ! Helper functions for disassembling unums
  ! Not dependant on the values contained in the utag
  function Esizeminus1 (u)
    implicit none
    integer (INT64)              :: Esizeminus1
    integer (INT64), intent (in) :: u
    esizeminus1 = ishft (iand (u, Get_esizemask ()), -Get_fsizesize ())
  end function Esizeminus1

  function Esize (u)
    implicit none
    integer (INT64)              :: Esize
    integer (INT64), intent (in) :: u
    esize = 1 + esizeminus1 (u)
  end function Esize

  function Fsizeminus1 (u)
    implicit none
    integer (INT64)              :: Fsizeminus1
    integer (INT64), intent (in) :: u
    fsizeminus1 = iand (u, Get_fsizemask ())
  end function Fsizeminus1

  function Fsize (u)
    implicit none
    integer (INT64)              :: Fsize
    integer (INT64), intent (in) :: u
    Fsize = 1 + Fsizeminus1 (u)
  end function Fsize

  function Utag (i_esize, i_fsize)
    implicit none
    integer (INT64)              :: Utag
    integer (INT64), intent (in) :: i_esize, i_fsize
    utag = ior (i_fsize - 1 , ishft (i_esize - 1_INT64, Get_fsizesize ()))
  end function Utag

  function Numbits (u)
    implicit none
    integer (INT64)              :: Numbits
    integer (INT64), intent (in) :: u
    Numbits = 1 + Esize (u) + Fsize (u) + Get_utagsize ()
  end function Numbits

  function Signmask (u)
    implicit none
    integer (INT64)              :: Signmask
    integer (INT64), intent (in) :: u
    Signmask = ishft (1_INT64, Numbits (u) - 1)
  end function Signmask

  function Hiddenmask (u)
    implicit none
    integer (INT64)              :: Hiddenmask
    integer (INT64), intent (in) :: u
    Hiddenmask = ishft (1_INT64, Fsize (u) + Get_utagsize ())
  end function Hiddenmask

  function Fracmask (u)
    implicit none
    integer (INT64)              :: Fracmask
    integer (INT64), intent (in) :: u
    Fracmask = ishft (ishft (1_INT64, Fsize (u)) - 1_INT64, Get_utagsize ())
  end function Fracmask

  function Expomask (u)
    implicit none
    integer (INT64)              :: Expomask
    integer (INT64), intent (in) :: u
    Expomask = ishft (ishft (1_INT64, Esize (u)) - 1_INT64, &
               Fsize (u) + Get_utagsize ())
  end function Expomask

  function Floatmask (u)
    implicit none
    integer (INT64)              :: Floatmask
    integer (INT64), intent (in) :: u
    Floatmask = Signmask (u) + Expomask (u) + Fracmask (u)
  end function Floatmask

  ! Helper functions dependant on values contained in the utag
  function Bias (u)
    implicit none
    integer (INT64) :: Bias
    integer (INT64), intent (in) :: u
    Bias = 2_INT64**(Esizeminus1 (u)) - 1
  end function Bias

  function Signu (u)
    implicit none
    integer (INT64)              :: Signu
    integer (INT64), intent (in) :: u
    Signu = Boole (iand (u, Signmask (u)) > 0)
  end function Signu

  function Expo (u)
    implicit none
    integer (INT64)              :: Expo
    integer (INT64), intent (in) :: u
    Expo = ishft (iand (u, Expomask (u)), -(Get_utagsize () + Fsize (u)))
  end function Expo

  function Hidden (u)
    implicit none
    integer (INT64)              :: Hidden
    integer (INT64), intent (in) :: u
    Hidden = Boole (Expo (u) > 0)
  end function Hidden

  function Frac (u)
    implicit none
    integer (INT64)              :: Frac
    integer (INT64), intent (in) :: u
    Frac = ishft (iand (u, Fracmask (u)), - Get_utagsize ())
  end function Frac

  function IsInexQ (u)
    implicit none
    logical                      :: IsInexQ
    integer (INT64), intent (in) :: u
    IsInexQ = iand (Get_ubitmask (), u) > 0
  end function IsInexQ

  function IsExQ (u)
    implicit none
    logical                      :: IsExQ
    integer (INT64), intent (in) :: u
    IsExQ = iand (Get_ubitmask (), u) == 0
  end function IsExQ

  function Exact (u)
    implicit none
    integer (INT64)              :: Exact
    integer (INT64), intent (in) :: u
    if (IsInexQ (u)) then
      Exact = ieor (u, Get_ubitmask ())
    else
      Exact = u
    end if
  end function Exact

  function Boole (log_in)
    implicit none
    logical, intent (in) :: log_in
    integer              :: Boole
    Boole = 0
    if (log_in) Boole = 1
  end function Boole

  ! Numerical value meant by exponent bits; helper function for u2f
  function Expovalue (u)
    implicit none
    integer (INT64)              :: Expovalue
    integer (INT64), intent (in) :: u
    Expovalue = Expo (u) - Bias (u) + 1 - Hidden (u)
  end function Expovalue

  ! Convert an exact unum to float; requires NaN cases?
  function u2f (u)
    implicit none
    real (REAL64) :: u2f
    integer (INT64) :: u
    if (u == Get_posinfu ()) then
      u2f = ieee_value (u2f, IEEE_POSITIVE_INF)
    else if (u == Get_neginfu ()) then
      u2f = ieee_value (u2f, IEEE_NEGATIVE_INF)
    else
      u2f = (-1.0_REAL64)**(Signu (u)) * &
            2.0_REAL64**(Expovalue (u)) * &
            (Hidden (u) + (Frac (u))/ 2.0_REAL64**(Fsize (u)))
    end if
  end function u2f

end module unum_to_float_mod
