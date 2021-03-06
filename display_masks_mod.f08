module display_masks_mod
  use ISO_FORTRAN_ENV, only : INT64, REAL64
  use unum_t_mod
  use unum_env_mod, only : Get_fsizemask,&
                           Get_esizemask,&
                           Get_efsizemask,&
                           Get_ubitmask,&
                           Get_utagmask,&
                           Get_maxubits
  implicit none

  interface Display_mask
    module procedure Display_mask_int32
    module procedure Display_mask_int64
    module procedure Display_mask_unum_t
  end interface Display_mask

contains
  subroutine Display_fsizemask ()
    implicit none
    type (unum_t) :: fsizemask
    fsizemask = Get_fsizemask ()
    call Display_mask (fsizemask)
  end subroutine Display_fsizemask

  subroutine Display_esizemask ()
    implicit none
    type (unum_t) :: esizemask
    esizemask = Get_esizemask ()
    call Display_mask (esizemask)
  end subroutine Display_esizemask

  subroutine Display_efsizemask ()
    implicit none
    type (unum_t) :: efsizemask
    efsizemask = Get_efsizemask ()
    call Display_mask (efsizemask)
  end subroutine Display_efsizemask

  subroutine Display_ubitmask ()
    implicit none
    type (unum_t) :: ubitmask
    ubitmask = Get_ubitmask ()
    call Display_mask (ubitmask)
  end subroutine Display_ubitmask

  subroutine Display_utagmask ()
    implicit none
    type (unum_t) :: utagmask
    utagmask = Get_utagmask ()
    call Display_mask (utagmask)
  end subroutine Display_utagmask

  subroutine Display_mask_int64 (maskvar)
    implicit none

    integer                               :: i
    integer (INT64), intent (in)          :: maskvar
    character (len = Get_maxubits ())     :: display_str
    character (len = 1)                   :: temp_str

    do i = 0, Get_maxubits () -1 !Bit indeces are zero based
      if (btest (maskvar, i)) then
        temp_str = '1'
      else
        temp_str = '0'
      end if
      display_str = temp_str(1:1) // display_str
    end do
    print *, display_str
  end subroutine Display_mask_int64

  subroutine Display_mask_int32 (maskvar)
    implicit none

    integer                               :: i
    integer, intent (in)                  :: maskvar
    character (len = Get_maxubits ())     :: display_str
    character (len = 1)                   :: temp_str

    do i = 0, Get_maxubits () -1 !Bit indeces are zero based
      if (btest (maskvar, i)) then
        temp_str = '1'
      else
        temp_str = '0'
      end if
      display_str = temp_str(1:1) // display_str
    end do
    print *, display_str
  end subroutine Display_mask_int32

  subroutine Display_mask_unum_t (maskvar)
    implicit none
    type (unum_t) :: maskvar
    call Display_mask (maskvar%u)
  end subroutine Display_mask_unum_t

end module display_masks_mod
