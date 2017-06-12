module unum_to_float_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use unum_t_mod
  use unum_operator_mod
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
  public :: U2f
  public :: Boole
  public :: Fsize
  public :: Signmask
  public :: Fracmask
  public :: Expomask
  public :: Floatmask
  public :: Signi
  public :: Exact
  public :: IsExQ

contains
  ! Helper functions for disassembling unums
  ! Not dependant on the values contained in the utag
  function Esizeminus1 (u)
    implicit none
    type (unum_t), intent (in) :: u
    integer                    :: Esizeminus1
    type (unum_t)              :: esizemask_local
    esizemask_local = Get_esizemask ()
    Esizeminus1 = ishft (iand (u%u, esizemask_local%u), -Get_fsizesize ())
  end function Esizeminus1

  function Esize (u)
    implicit none
    type (unum_t), intent (in) :: u
    integer                    :: Esize
    Esize = 1 + Esizeminus1 (u)
  end function Esize

  function Fsizeminus1 (u)
    implicit none
    type (unum_t), intent (in) :: u
    integer                    :: Fsizeminus1
    type (unum_t)              :: fsizemask_local
    fsizemask_local = Get_fsizemask ()
    Fsizeminus1 = iand (u%u, fsizemask_local%u)
  end function Fsizeminus1

  function Fsize (u)
    implicit none
    type (unum_t), intent (in) :: u
    integer                    :: Fsize
    Fsize = 1 + Fsizeminus1 (u)
  end function Fsize

!  Function "Utag" does not seem to be referenced anywhere. Commented out
!  for the time being unless a need becomes obvious.
!
!  function Utag (i_esize, i_fsize)
!    implicit none
!    integer (INT64)              :: Utag
!    integer (INT64), intent (in) :: i_esize, i_fsize
!    Utag = ior (i_fsize - 1 , ishft (i_esize - 1_INT64, Get_fsizesize ()))
!  end function Utag

  function Numbits (u)
    implicit none
    type (unum_t), intent (in) :: u
    integer                    :: Numbits
    Numbits = 1 + Esize (u) + Fsize (u) + Get_utagsize ()
  end function Numbits

  function Signmask (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: Signmask
    Signmask%u = ishft (1_INT64, Numbits (u) - 1)
  end function Signmask

  function Hiddenmask (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: Hiddenmask
    Hiddenmask%u = ishft (1_INT64, Fsize (u) + Get_utagsize ())
  end function Hiddenmask

  function Fracmask (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: Fracmask
    Fracmask%u = ishft (ishft (1_INT64, Fsize (u)) - 1_INT64, Get_utagsize ())
  end function Fracmask

  function Expomask (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: Expomask
    Expomask%u = ishft (ishft (1_INT64, Esize (u)) - 1_INT64, &
               Fsize (u) + Get_utagsize ())
  end function Expomask

  function Floatmask (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: Floatmask
    type (unum_t)              :: signmask_local, expomask_local, fracmask_local
    signmask_local = Signmask (u)
    expomask_local = Expomask (u)
    fracmask_local = Fracmask (u)
    Floatmask%u = signmask_local%u + expomask_local%u + fracmask_local%u
  end function Floatmask

  ! Helper functions dependant on values contained in the utag
  function Bias (u)
    implicit none
    type (unum_t), intent (in) :: u
    integer (INT64)            :: Bias
    Bias = 2_INT64**(Esizeminus1 (u)) - 1
  end function Bias

  function Signi (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: signu
    integer                    :: Signi
    signu = iand (u, Signmask (u))
    Signi = Boole (signu%u > 0)
  end function Signi

  function Expo (u)
    implicit none
    type (unum_t), intent (in)   :: u
    type (unum_t)                :: expou
    integer (INT64)              :: Expo
    expou = iand (u, Expomask (u))
    Expo = ishft (expou%u, -(Get_utagsize () + Fsize (u)))
  end function Expo

  function Hidden (u)
    implicit none
    type (unum_t), intent (in) :: u
    integer (INT64)            :: Hidden
    Hidden = Boole (Expo (u) > 0)
  end function Hidden

  function Frac (u)
    implicit none
    type (unum_t), intent (in) :: u
    type (unum_t)              :: fracu
    integer (INT64)            :: Frac
    fracu = iand (u, Fracmask (u))
    Frac = ishft (fracu%u, - Get_utagsize ())
  end function Frac

  function IsInexQ (u)
    implicit none
    type (unum_t), intent (in) :: u
    logical                    :: IsInexQ
    type (unum_t)              :: ubitu
    ubitu = iand (Get_ubitmask (), u)
    IsInexQ = ubitu%u > 0
  end function IsInexQ

  function IsExQ (u)
    implicit none
    type (unum_t), intent (in) :: u
    logical                    :: IsExQ
    type (unum_t)              :: ubitu
    ubitu = iand (Get_ubitmask (), u)
    IsExQ = ubitu%u == 0
  end function IsExQ

  function Exact (u)
    implicit none
    type (unum_t)              :: Exact
    type (unum_t), intent (in) :: u
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

  ! Numerical value meant by exponent bits; helper function for U2f
  function Expovalue (u)
    implicit none
    integer (INT64)              :: Expovalue
    type (unum_t), intent (in)   :: u
    Expovalue = Expo (u) - Bias (u) + 1 - Hidden (u)
  end function Expovalue

  ! Convert an exact unum to float; requires NaN cases?
  function U2f (u)
    implicit none
    real (REAL64) :: U2f
    type (unum_t) :: u
    if (u == Get_posinfu ()) then
      U2f = ieee_value (U2f, IEEE_POSITIVE_INF)
    else if (u == Get_neginfu ()) then
      U2f = ieee_value (U2f, IEEE_NEGATIVE_INF)
    else
      U2f = (-1.0_REAL64)**(Signi (u)) * &
            2.0_REAL64**(Expovalue (u)) * &
            (Hidden (u) + (Frac (u))/ 2.0_REAL64**(Fsize (u)))
    end if
  end function U2f

end module unum_to_float_mod
