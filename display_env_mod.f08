module display_env_mod
  use ISO_FORTRAN_ENV, only   : INT64, REAL64
  use display_masks_mod, only : Display_env => Display_mask
  use unum_env_mod, only      : Get_qNaNu,&
                                Get_sNaNu,&
                                Get_posinfu,&
                                Get_neginfu,&
                                Get_maxrealu,&
                                Get_negbigu,&
                                Get_smallsubnormalu
  implicit none

contains
  subroutine Display_qNaNu ()
    implicit none
    integer (INT64) :: qNaNu
    qNaNu = Get_qNaNu ()
    call Display_env (qNaNu)
  end subroutine Display_qNaNu

  subroutine Display_sNaNu ()
    implicit none
    integer (INT64) :: sNaNu
    sNaNu = Get_sNaNu ()
    call Display_env (sNaNu)
  end subroutine Display_sNaNu

  subroutine Display_posinfu ()
    implicit none
    integer (INT64) :: posinfu
    posinfu = Get_posinfu ()
    call Display_env (posinfu)
  end subroutine Display_posinfu

  subroutine Display_neginfu ()
    implicit none
    integer (INT64) :: neginfu
    neginfu = Get_neginfu ()
    call Display_env (neginfu)
  end subroutine Display_neginfu

  subroutine Display_maxrealu ()
    implicit none
    integer (INT64) :: maxrealu
    maxrealu = Get_maxrealu ()
    call Display_env (maxrealu)
  end subroutine Display_maxrealu

  subroutine Display_negbigu ()
    implicit none
    integer (INT64) :: negbigu
    negbigu = Get_negbigu ()
    call Display_env (negbigu)
  end subroutine Display_negbigu

  subroutine Display_smallsubnormalu ()
    implicit none
    integer (INT64) :: smallsubnormalu
    smallsubnormalu = Get_smallsubnormalu ()
    call Display_env (smallsubnormalu)
  end subroutine Display_smallsubnormalu

end module display_env_mod
