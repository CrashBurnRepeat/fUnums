module unum_env_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use unum_size_limit_mod
  use unum_t_mod

  implicit none

  private
  integer       :: esizesize
  integer       :: fsizesize
  integer       :: utagsize
  integer       :: maxubits
  integer       :: esizemax
  integer       :: fsizemax
  type (unum_t) :: ubitmask
  type (unum_t) :: fsizemask
  type (unum_t) :: esizemask
  type (unum_t) :: efsizemask
  type (unum_t) :: utagmask
  type (unum_t) :: ulpu
  type (unum_t) :: smallsubnormalu
  type (unum_t) :: smallnormalu
  type (unum_t) :: signbigu
  type (unum_t) :: posinfu
  type (unum_t) :: maxrealu
  type (unum_t) :: minrealu
  type (unum_t) :: neginfu
  type (unum_t) :: negbigu
  type (unum_t) :: qNaNu
  type (unum_t) :: sNaNu
  type (unum_t) :: negopeninfu
  type (unum_t) :: posopeninfu
  type (unum_t) :: negopenzerou
  type (unum_t) :: posbig
  real (REAL64) :: maxreal
  real (REAL64) :: smallsubnormal

  public        :: Set_unum_env
  public        :: Get_esizesize
  public        :: Get_fsizesize
  public        :: Get_utagsize
  public        :: Get_maxubits
  public        :: Get_esizemax
  public        :: Get_fsizemax
  public        :: Get_esizemask
  public        :: Get_fsizemask
  public        :: Get_efsizemask
  public        :: Get_ubitmask
  public        :: Get_utagmask
  public        :: Get_ulpu
  public        :: Get_signbigu
  public        :: Get_posinfu
  public        :: Get_neginfu
  public        :: Get_negbigu
  public        :: Get_qNaNu
  public        :: Get_sNaNu
  public        :: Get_negopeninfu
  public        :: Get_posopeninfu
  public        :: Get_negopenzerou
  public        :: Get_posbig
  public        :: Get_maxreal
  public        :: Get_maxrealu
  public        :: Get_smallsubnormal
  public        :: Get_smallsubnormalu
  public        :: Get_smallnormalu

contains
  subroutine Set_unum_env (esizesize_in, fsizesize_in)
    implicit none

    integer, intent (in) :: esizesize_in
    integer, intent (in) :: fsizesize_in
    logical              :: is_bounded

    is_bounded = Check_size_bounds (esizesize_in, fsizesize_in)

    if (is_bounded) call Calc_env_params (esizesize_in, fsizesize_in)

  end subroutine Set_unum_env

  logical function Check_size_bounds (esizesize_in, fsizesize_in) &
                                                            result(exit_flag)
    implicit none

    integer, intent (in) :: esizesize_in
    integer, intent (in) :: fsizesize_in

    exit_flag = .true.

    if (esizesize_in < MIN_ESIZESIZE) then
      print *, 'Value of esizesize is too small'
      exit_flag = .false.
    end if

    if (esizesize_in > MAX_ESIZESIZE) then
      print *, 'Value of esizesize is too large'
      exit_flag = .false.
    end if

    if (fsizesize_in < MIN_FSIZESIZE) then
      print *, 'Value of fsizesize is too small'
      exit_flag = .false.
    end if

    if (fsizesize_in > MAX_FSIZESIZE) then
      print *, 'Value of fsizesize is too large'
      exit_flag = .false.
    end if

    if (.not.exit_flag) print *, 'Environment not set'

  end function Check_size_bounds

  subroutine Calc_env_params (esizesize_in, fsizesize_in)
    implicit none

    integer, intent (in) :: esizesize_in
    integer, intent (in) :: fsizesize_in

    ! e parameters
    esizesize = esizesize_in
    esizemax = 2**esizesize
    ! f parameters
    fsizesize = fsizesize_in
    fsizemax = 2**fsizesize
    ! unum parameters
    utagsize = 1 + esizesize + fsizesize
    maxubits = 1 + esizemax + fsizemax + utagsize
    ! Masks
    ubitmask%u = ishft (1_INT64, utagsize - 1)
    fsizemask%u = ishft (1_INT64, fsizesize) - 1
    esizemask%u = (ubitmask%u - 1) - fsizemask%u
    efsizemask%u = ior (fsizemask%u, esizemask%u)
    utagmask%u = ior (ubitmask%u, efsizemask%u)
    ! environment parameters
    ulpu%u = ishft (1_INT64, utagsize)
    smallsubnormalu%u = efsizemask%u + ulpu%u
    smallnormalu%u = efsizemask%u + ishft (1_INT64, maxubits - 1 - esizemax)
    signbigu%u = ishft (1_INT64, maxubits - 1)
    posinfu%u = signbigu%u - 1 - ubitmask%u
    maxrealu%u = posinfu%u - ulpu%u
    minrealu%u = maxrealu%u + signbigu%u
    neginfu%u = posinfu%u + signbigu%u
    negbigu%u = neginfu%u - ulpu%u
    qNaNu%u = posinfu%u + ubitmask%u
    sNaNu%u = neginfu%u + ubitmask%u
    if (utagsize == 1) then
      negopeninfu%u = b'1101'
      posopeninfu%u = b'0101'
    else
      negopeninfu%u = ishft (b'1111', utagsize - 1) !cast binary literal as INT64?
      posopeninfu%u = ishft (b'0111', utagsize - 1) !cast binary literal as INT64?
    end if
    negopenzerou%u = ishft (b'1001', utagsize - 1) !cast binary literal as INT64?
    maxreal = 2.0_REAL64**(2.0_REAL64**(esizemax - 1.0_REAL64)) * &
             (2.0_REAL64**(fsizemax)-1.0_REAL64) / &
             (2.0_REAL64**(fsizemax - 1.0_REAL64))
    smallsubnormal = 2.0_REAL64**&
             (2.0_REAL64-2.0_REAL64**(esizemax-1.0_REAL64)-fsizemax)
    print *, 'Environment set'
  end subroutine Calc_env_params

  function Get_esizesize ()
    implicit none
    integer :: Get_esizesize
    Get_esizesize = esizesize
  end function Get_esizesize

  function Get_fsizesize ()
    implicit none
    integer :: Get_fsizesize
    Get_fsizesize = fsizesize
  end function Get_fsizesize

  function Get_utagsize ()
    implicit none
    integer :: Get_utagsize
    Get_utagsize = utagsize
  end function Get_utagsize

  pure function Get_maxubits ()
    implicit none
    integer :: Get_maxubits
    Get_maxubits = maxubits
  end function Get_maxubits

  function Get_esizemax ()
    implicit none
    integer :: Get_esizemax
    Get_esizemax = esizemax
  end function Get_esizemax

  function Get_fsizemax ()
    implicit none
    integer :: Get_fsizemax
    Get_fsizemax = fsizemax
  end function Get_fsizemax

  function Get_esizemask ()
    implicit none
    type (unum_t) :: Get_esizemask
    Get_esizemask = esizemask
  end function Get_esizemask

  function Get_fsizemask ()
    implicit none
    type (unum_t) :: Get_fsizemask
    Get_fsizemask = fsizemask
  end function Get_fsizemask

  function Get_efsizemask ()
    implicit none
    type (unum_t) :: Get_efsizemask
    Get_efsizemask = efsizemask
  end function Get_efsizemask

  function Get_ubitmask ()
    implicit none
    type (unum_t) :: Get_ubitmask
    Get_ubitmask = ubitmask
  end function Get_ubitmask

  function Get_utagmask ()
    implicit none
    type (unum_t) :: Get_utagmask
    Get_utagmask = utagmask
  end function Get_utagmask

  function Get_ulpu ()
    implicit none
    type (unum_t) :: Get_ulpu
    Get_ulpu = ulpu
  end function Get_ulpu

  function Get_signbigu ()
    implicit none
    type (unum_t) :: Get_signbigu
    Get_signbigu = signbigu
  end function Get_signbigu

  function Get_posinfu ()
    implicit none
    type (unum_t) :: Get_posinfu
    Get_posinfu = posinfu
  end function Get_posinfu

  function Get_neginfu ()
    implicit none
    type (unum_t) :: Get_neginfu
    Get_neginfu = neginfu
  end function Get_neginfu

  function Get_negbigu ()
    implicit none
    type (unum_t) :: Get_negbigu
    Get_negbigu = negbigu
  end function Get_negbigu

  function Get_qNaNu ()
    implicit none
    type (unum_t) :: Get_qNaNu
    Get_qNaNu = qNaNu
  end function Get_qNaNu

  function Get_sNaNu ()
    implicit none
    type (unum_t) :: Get_sNaNu
    Get_sNaNu = sNaNu
  end function Get_sNaNu

  function Get_negopeninfu ()
    implicit none
    type (unum_t) :: Get_negopeninfu
    Get_negopeninfu = negopeninfu
  end function Get_negopeninfu

  function Get_posopeninfu ()
    implicit none
    type (unum_t) :: Get_posopeninfu
    Get_posopeninfu = posopeninfu
  end function Get_posopeninfu

  function Get_negopenzerou ()
    implicit none
    type (unum_t) :: Get_negopenzerou
    Get_negopenzerou = negopenzerou
  end function

  function Get_posbig ()
    implicit none
    type (unum_t) :: Get_posbig
    Get_posbig = posbig
  end function Get_posbig

  function Get_maxreal ()
    implicit none
    real (REAL64) :: Get_maxreal
    Get_maxreal = maxreal
  end function Get_maxreal

  function Get_maxrealu ()
    implicit none
    type (unum_t)   :: Get_maxrealu
    Get_maxrealu = maxrealu
  end function Get_maxrealu

  function Get_smallsubnormal ()
    implicit none
    real (REAL64) :: Get_smallsubnormal
    Get_smallsubnormal = smallsubnormal
  end function Get_smallsubnormal

  function Get_smallsubnormalu ()
    implicit none
    type (unum_t)   :: Get_smallsubnormalu
    Get_smallsubnormalu = smallsubnormalu
  end function Get_smallsubnormalu

  function Get_smallnormalu ()
    implicit none
    type (unum_t)   :: Get_smallnormalu
    Get_smallnormalu = smallnormalu
  end function Get_smallnormalu

end module unum_env_mod
