module real_to_unum_mod
  use unum_to_float_mod, only : Boole,&
                                Signmask,&
                                u2f
  use ISO_FORTRAN_ENV,   only : INT64, REAL64
  use unum_env_mod,      only : Get_qNaNu,&
                                Get_posinfu,&
                                Get_neginfu,&
                                Get_maxreal,&
                                Get_maxrealu,&
                                Get_utagmask,&
                                Get_utagsize,&
                                Get_ubitmask,&
                                Get_signbigu,&
                                Get_smallsubnormal,&
                                Get_smallnormalu,&
                                Get_efsizemask,&
                                Get_fsizemax,&
                                Get_fsizesize,&
                                Get_fsizemask,&
                                Get_ulpu
  use IEEE_ARITHMETIC           !Entire package for '==' overload

  implicit none
  private
  public :: x2u

contains
  function Scale_to_norm (x)
    implicit none
    integer (INT64)             :: Scale_to_norm
    real (REAL64), intent (in)  :: x
    if (x == 0.0) then
      Scale_to_norm = 0
    else
      Scale_to_norm = floor (log (abs (x)) / log (2.0_REAL64))
    end if
  end function Scale_to_norm

  function Min_exp_bits (x)
    implicit none
    integer (INT64)             :: Min_exp_bits
    real (REAL64), intent (in)  :: x
    if (x == 0.0 .or. Scale_to_norm (x) == 1) then
      Min_exp_bits = 1
    else
      Min_exp_bits = ceiling (&
                     log (1.0 + abs (Scale_to_norm (x) - 1)) / &
                     log (2.0), INT64) + 1
    end if
  end function Min_exp_bits

  function Subnormal2u (x)
    implicit none
    real   (REAL64), intent (in)  :: x
    integer (INT64)               :: Subnormal2u

    real   (REAL64) :: y
    integer (INT64) :: sub_temp
    integer (INT64) :: sign_adder = 0
    integer (INT64) :: ubitmask_adder = 0

    y = abs (x) / Get_smallsubnormal ()

    if (x < 0) sign_adder = Get_signbigu ()
    if (y /= floor (y)) ubitmask_adder = Get_ubitmask ()

    sub_temp = sign_adder + Get_efsizemask () + ubitmask_adder + &
               ishft (floor (y, INT64), Get_utagsize ())
    do while (iand (ishft (3, Get_utagsize () - 1), sub_temp) == 0)
      sub_temp = (sub_temp - iand (Get_efsizemask (), sub_temp)) / 2_INT64 + &
                 iand (Get_efsizemask (), sub_temp) - 1
    end do

    Subnormal2u = sub_temp
  end function Subnormal2u

  function Norm2u (x)
    implicit none
    real   (REAL64), intent (in)  :: x
    integer (INT64)               :: Norm2u

    real (REAL64)   :: y
    real (REAL64)   :: z_real
    integer (INT64) :: z_int
    integer (INT64) :: temp_norm
    integer (INT64) :: n = 0

    y = abs (x) / 2.0_REAL64**(Scale_to_norm (x))

    do while (floor (y) /= y .and. n < Get_fsizemax ())
      y = y*2.0_REAL64
      n = n + 1_INT64
    end do
    if (y == floor (y)) then
      temp_norm = n - Boole (n > 0) + &
                  ishft (Min_exp_bits (x) - 1, Get_fsizesize ())
      if (n /= 0) then
        temp_norm = temp_norm + &
                    ishft (floor (y, INT64) - &
                    2_INT64**(Scale_to_norm (y)), Get_utagsize ())
      end if
      temp_norm = temp_norm + &
                  ishft (Scale_to_norm (x) + &
                  2_INT64**(Min_exp_bits (x) - 1_INT64) - 1_INT64, &
                  Get_utagsize () + n + Boole (n == 0))
      if (x < 0) then
        temp_norm = temp_norm + &
                    ishft (1_INT64, Get_utagsize () + n + Boole (n == 0) + &
                    Min_exp_bits (x))
      end if
      z_real = log (1.0_REAL64 - log (abs (x)) / log (2.0_REAL64)) / &
               log (2.0_REAL64)
      z_int = int (z_real)
      if (z_real == z_int .and. z_int >= 0) then
        temp_norm = ishft (z_int, Get_fsizesize ()) + Get_ulpu () + &
                    Boole (x < 0) * Signmask (ishft (z_int, Get_fsizesize ()))
      end if
    else
      z_real = ceiling (abs (x) / &
              2.0_REAL64**(Scale_to_norm (x) - Get_fsizemax ())) * &
              2.0_REAL64**(Scale_to_norm (x) - Get_fsizemax ())
      !z_int = int (z_real)
      n = max (Min_exp_bits (x), Min_exp_bits (z_real))
      temp_norm = Get_fsizemask () + &
                  ishft (n - 1_INT64, Get_fsizesize ()) + &
                  Get_ubitmask () - Get_ulpu () + &
                  ishft (floor ((z_real/2.0_REAL64**Scale_to_norm (z_real) - &
                  1.0_REAL64) * 2.0_REAL64**Get_fsizemax ()), Get_utagsize ())+&
                  ishft (Scale_to_norm (z_real) + &
                  2_INT64**(n - 1_INT64) - 1_INT64, &
                  Get_utagsize () + Get_fsizemax ())
      if (x < 0) temp_norm = temp_norm + Signmask (temp_norm)
    end if
    Norm2u = temp_norm
  end function Norm2u

  function x2u (x)
    implicit none
    real (REAL64),   intent (in)  :: x
    integer (INT64)               :: x2u ! unums are stored in int64s
    if (ieee_is_nan (x)) then
      x2u = Get_qNaNu ()
    else if (ieee_class (x) == IEEE_POSITIVE_INF) then
      x2u = Get_posinfu ()
    else if (ieee_class (x) == IEEE_NEGATIVE_INF) then
      x2u = Get_neginfu ()
    else if (abs (x) > Get_maxreal ()) then
      if (x < 0) then
        x2u = Get_maxrealu () + Get_ubitmask () + Get_signbigu ()
      else
        x2u = Get_maxrealu () + Get_ubitmask ()
      end if
    else if (x == 0.0) then
      x2u = 0
    else if (abs (x) < Get_smallsubnormal ()) then
      if (x < 0) then
        x2u = Get_utagmask () + Get_signbigu ()
      else
        x2u = Get_utagmask ()
      end if
    else if (abs (x) < u2f (Get_smallnormalu ())) then
      x2u = Subnormal2u (x)
    else
      x2u = Norm2u (x)
    end if
  end function x2u

end module real_to_unum_mod
