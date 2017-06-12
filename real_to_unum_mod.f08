module real_to_unum_mod
  use unum_to_float_mod, only : Boole,&
                                Signmask,&
                                U2f
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
  use unum_t_mod

  implicit none
  private
  public :: X2u

contains
  function Scale_to_norm (x)
    implicit none
    real (REAL64), intent (in)  :: x
    integer (INT64)             :: Scale_to_norm
    if (x == 0.0) then
      Scale_to_norm = 0
    else
      Scale_to_norm = floor (log (abs (x)) / log (2.0_REAL64))
    end if
  end function Scale_to_norm

  function Min_exp_bits (x)
    implicit none
    real (REAL64), intent (in)  :: x
    integer (INT64)             :: Min_exp_bits
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
    real (REAL64), intent (in)    :: x
    type (unum_t)                 :: Subnormal2u
    real (REAL64)                 :: y
    type (unum_t)                 :: sub_temp
    type (unum_t)                 :: sign_adder
    type (unum_t)                 :: ubitmask_adder
    type (unum_t)                 :: efsizemask_local

    sign_adder%u = 0
    ubitmask_adder%u = 0
    efsizemask_local = Get_efsizemask ()

    y = abs (x) / Get_smallsubnormal ()

    if (x < 0) sign_adder = Get_signbigu ()
    if (y /= floor (y)) ubitmask_adder = Get_ubitmask ()

    sub_temp%u = sign_adder%u + efsizemask_local%u + ubitmask_adder%u + &
               ishft (floor (y, INT64), Get_utagsize ())
    do while (iand (ishft (3, Get_utagsize () - 1), sub_temp%u) == 0)
      sub_temp%u = (sub_temp%u - &
                 iand (efsizemask_local%u, sub_temp%u)) / 2_INT64 + &
                 iand (efsizemask_local%u, sub_temp%u) - 1
    end do

    Subnormal2u = sub_temp
  end function Subnormal2u

  function Norm2u (x)
    implicit none
    real   (REAL64), intent (in)  :: x
    type (unum_t)                 :: Norm2u
    real (REAL64)                 :: y
    real (REAL64)                 :: z_real
    integer (INT64)               :: z_int
    type (unum_t)                 :: z_unum
    type (unum_t)                 :: temp_norm
    type (unum_t)                 :: ulpu_local, signmask_temp
    type (unum_t)                 :: fsizemask_local, ubitmask_local
    integer (INT64)               :: n

    n = 0
    ulpu_local = Get_ulpu ()

    y = abs (x) / 2.0_REAL64**(Scale_to_norm (x))

    do while (floor (y) /= y .and. n < Get_fsizemax ())
      y = y * 2.0_REAL64
      n = n + 1_INT64
    end do
    if (y == floor (y)) then
      temp_norm%u = n - Boole (n > 0) + &
                  ishft (Min_exp_bits (x) - 1, Get_fsizesize ())
      if (n /= 0) then
        temp_norm%u = temp_norm%u + &
                    ishft (floor (y, INT64) - &
                    2_INT64**(Scale_to_norm (y)), Get_utagsize ())
      end if
      temp_norm%u = temp_norm%u + &
                  ishft (Scale_to_norm (x) + &
                  2_INT64**(Min_exp_bits (x) - 1_INT64) - 1_INT64, &
                  Get_utagsize () + n + Boole (n == 0))
      if (x < 0) then
        temp_norm%u = temp_norm%u + &
                    ishft (1_INT64, Get_utagsize () + n + Boole (n == 0) + &
                    Min_exp_bits (x))
      end if
      z_real = log (1.0_REAL64 - log (abs (x)) / log (2.0_REAL64)) / &
               log (2.0_REAL64)
      z_int = int (z_real)
      if (z_real == z_int .and. z_int >= 0) then
        z_unum%u = ishft (z_int, Get_fsizesize ())
        signmask_temp = Signmask (z_unum)
        temp_norm%u = ishft (z_int, Get_fsizesize ()) + ulpu_local%u + &
                    Boole (x < 0) * signmask_temp%u
      end if
    else
      z_real = ceiling (abs (x) / &
              2.0_REAL64**(Scale_to_norm (x) - Get_fsizemax ())) * &
              2.0_REAL64**(Scale_to_norm (x) - Get_fsizemax ())
      !z_int = int (z_real)
      n = max (Min_exp_bits (x), Min_exp_bits (z_real))
      fsizemask_local = Get_fsizemask ()
      ubitmask_local = Get_ubitmask ()
      temp_norm%u = fsizemask_local%u + &
                    ishft (n - 1_INT64, Get_fsizesize ()) + &
                    ubitmask_local%u - ulpu_local%u + &
                    ishft (floor ((z_real/2.0_REAL64**Scale_to_norm (z_real) - &
                    1.0_REAL64) * 2.0_REAL64**Get_fsizemax ()), &
                    Get_utagsize ())+&
                    ishft (Scale_to_norm (z_real) + &
                    2_INT64**(n - 1_INT64) - 1_INT64, &
                    Get_utagsize () + Get_fsizemax ())
      signmask_temp = Signmask (temp_norm)
      if (x < 0) temp_norm%u = temp_norm%u + signmask_temp%u
    end if
    Norm2u = temp_norm
  end function Norm2u

  function X2u (x)
    implicit none
    real (REAL64),   intent (in)  :: x
    type (unum_t)                 :: X2u
    type (unum_t)                 :: maxrealu_local, ubitmask_local
    type (unum_t)                 :: signbigu_local, utagmask_local
    if (ieee_is_nan (x)) then
      X2u = Get_qNaNu ()
    else if (ieee_class (x) == IEEE_POSITIVE_INF) then
      X2u = Get_posinfu ()
    else if (ieee_class (x) == IEEE_NEGATIVE_INF) then
      X2u = Get_neginfu ()
    else if (abs (x) > Get_maxreal ()) then
      maxrealu_local = Get_maxrealu ()
      ubitmask_local = Get_ubitmask ()
      if (x < 0) then
        signbigu_local = Get_signbigu ()
        X2u%u = maxrealu_local%u + ubitmask_local%u + signbigu_local%u
      else
        X2u%u = maxrealu_local%u + ubitmask_local%u
      end if
    else if (x == 0.0) then
      X2u%u = 0
    else if (abs (x) < Get_smallsubnormal ()) then
      utagmask_local = Get_utagmask ()
      if (x < 0) then
        signbigu_local = Get_signbigu ()
        X2u%u = utagmask_local%u + signbigu_local%u
      else
        X2u = utagmask_local
      end if
    else if (abs (x) < U2f (Get_smallnormalu ())) then
      X2u = Subnormal2u (x)
    else
      X2u = Norm2u (x)
    end if
  end function X2u

end module real_to_unum_mod
