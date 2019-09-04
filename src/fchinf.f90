!-------------------------------------------------------------------------------------------------!
!> Display channel information
!--
program fchinf

  use iso_fortran_env
  use m_win
  use m_winch
  use m_getopt 
  use m_util
  implicit none
  !--
  type(winch__hdr), allocatable :: ch(:)
  character(256) :: fn_chtbl, chbuf, stbuf, cmpbuf
  character(4), allocatable :: chid(:)
  character(16), allocatable :: stnm(:)
  character(16), allocatable :: cmpnm(:)
  logical :: is_opt, is_opt_ch, is_opt_st, is_opt_cmp
  integer :: nch, nst, ncmp
  integer :: i, j, k
  character(256) :: fmt
  logical :: chid_mode
  integer :: ichid
  logical :: is_all_chid, is_all_st, is_all_cmp
  character(4) :: chid_tmp
  !----

  call getopt('f', is_opt,     fn_chtbl)
  if(.not. is_opt) call usage_stop()
  
  call getopt('c', is_opt_ch,  chbuf)
  call getopt('s', is_opt_st,  stbuf)
  call getopt('p', is_opt_cmp, cmpbuf)
  
  ! priority is on specified channel ID than station&components
  chid_mode = is_opt_ch

  if( (.not. is_opt_ch) .and. ( (.not. is_opt_st) .or. (.not. is_opt_cmp) ) ) call usage_stop()

  call winch__read_tbl(fn_chtbl, ch)

  if( chid_mode ) then
    call util__read_arglst(chbuf, nch, is_all_chid, chid)
    if( is_all_chid ) call winch__get_all_chid(ch, chid)
  else
    call util__read_arglst(stbuf, nst, is_all_st, stnm)
    if( is_all_st ) call winch__get_all_stnm(ch, stnm)
    call util__read_arglst(cmpbuf, ncmp, is_all_cmp, cmpnm)
    if( is_all_cmp ) call winch__get_all_cmpnm(ch, cmpnm)
  end if    

  fmt = '(A, ":  ", A, " (", A, ")", "  unit: ", A, "  T0=", F6.3, ' &
  // '"  h=", F6.3, "  location: ", F9.5, " E,", F9.5, " N,", F9.2, " m")'

  if( chid_mode ) then
    do j=1, size(ch)
      do i=1, size(chid)
        ichid = win__ach2ich(chid(i))
        if( ichid == ch(j)%ichid ) then
          !! export channel information
          write(output_unit, fmt) ch(i)%achid, trim(ch(i)%stnm), trim(ch(i)%cmpnm), &
            trim(ch(i)%unit), ch(i)%period, ch(i)%damp, ch(i)%lon, ch(i)%lat, ch(i)%elev
          exit
        end if
      end do
    end do
  else
    do j=1, size(stnm)
      do k=1, size(cmpnm)
        call winch__st2chid(ch, stnm(j), cmpnm(k), chid_tmp, i)
        if( i > 0 ) then
          write(output_unit, fmt) ch(i)%achid, trim(ch(i)%stnm), trim(ch(i)%cmpnm), &
            trim(ch(i)%unit), ch(i)%period, ch(i)%damp, ch(i)%lon, ch(i)%lat, ch(i)%elev
        end if
      end do
    end do
  end if

  contains
  subroutine usage_stop()

    write(error_unit,'(A)') 'usage:  fchinf.x  <-f chtbl> [-c chid] [-s stnm] [-p cmpnm]'
    stop

  end subroutine usage_stop

end program fchinf
!-------------------------------------------------------------------------------------------------!
