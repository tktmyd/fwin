!-------------------------------------------------------------------------------------------------!
!> win2sac fortran version
!--
program fwin2sac

  use iso_fortran_env
  use m_win
  use m_winch
  use m_getopt 
  use m_wsac
  use m_util
  implicit none
  !--

  character(4), allocatable :: chid(:)
  integer :: nch
  character(256), allocatable :: fn_win(:)
  integer :: nwin
  logical :: is_chtbl
  type(winch__hdr), allocatable :: ch(:), ch_tbl(:) 
  !----

  !-----------------------------------------------------------------------------------------------!
  ! win files
  !--
  block
    integer :: io, ierr, i
    character(256) ::  fn_winlst
    logical :: is_opt
    call getopt('wl', is_opt,     fn_winlst)

    if(.not. is_opt) call usage_stop()
    open(newunit=io, file=fn_winlst, iostat=ierr, action='read', status='old')
    if( ierr /= 0 ) error stop 'file not found: ' // trim(fn_winlst)
    call util__countline(io, nwin)
    allocate( fn_win(nwin) )
    do i=1, nwin
      read(io, '(A)') fn_win(i)
    end do
    close(io)
  end block

  !-----------------------------------------------------------------------------------------------!
  ! channel table
  !--
  block
    character(256) :: fn_chtbl
    call getopt('ct', is_chtbl, fn_chtbl)
    call winch__read_tbl(fn_chtbl, ch_tbl)
  end block

  !-----------------------------------------------------------------------------------------------!
  ! select channel IDs from command-line arguments
  !--
  block
    integer :: i, j, k
    integer :: nst, ncmp
    character(4) :: chid0
    character(16), allocatable :: stnm(:)
    character(16), allocatable :: cmpnm(:)
    character(256) ::  chbuf, stbuf, cmpbuf
    logical :: is_opt_ch, is_opt_st, is_opt_cmp
    logical :: is_all_chid, is_all_st, is_all_cmp
    integer :: ichid
    !--

    call getopt('chid',  is_opt_ch,  chbuf)
    call getopt('st',  is_opt_st,  stbuf)
    call getopt('cmp', is_opt_cmp, cmpbuf)
  
    if( (.not. is_opt_ch) .and. ( (.not. is_opt_st) .or. (.not. is_opt_cmp) ) ) call usage_stop()

    ! priority is on specified channel ID than station & components
    if( is_opt_ch ) then
      call util__read_arglst(chbuf, nch, is_all_chid, chid)
        if( is_all_chid ) call winch__get_all_chid(ch_tbl, chid)
    else
      call util__read_arglst(stbuf, nst, is_all_st, stnm)
        if( is_all_st ) call winch__get_all_stnm(ch_tbl, stnm)
      call util__read_arglst(cmpbuf, ncmp, is_all_cmp, cmpnm)
        if( is_all_cmp ) call winch__get_all_cmpnm(ch_tbl, cmpnm)

      allocate(chid(0))
      do i=1, nst
        do j=1, ncmp
          !try
          call winch__st2chid(ch_tbl, stnm(i), cmpnm(j), chid0, k)
          if( k > 0 ) chid = [chid, chid0]
        end do
      end do
    end if        

    !! prepare channel type data
    allocate(ch(0))
    do i=1, nch
      ichid = win__ach2ich(chid(i))
      do j=1, size(ch_tbl)
        if( ichid == ch_tbl(j)%ichid ) then
          ch = [ch, ch_tbl(j)]
          exit
        end if
      end do
    end do

  end block

  !-----------------------------------------------------------------------------------------------!
  ! Read win files and export to SAC file
  !--    
  block
    integer, allocatable :: dat(:,:), npts(:), sfreq(:)
    character(256) :: fn_sac
    integer :: tim, nsec
    integer :: i
    type(sac__hdr) :: sh
    character(8) :: ymd
    character(6) :: hms
    character(6) :: clen
  
    call win__read_files(fn_win, ch(:)%achid, npts, sfreq, dat, tim, nsec)
    call util__localtime(tim, &
      sh%nzyear, sh%nzmonth, sh%nzday, sh%nzhour, sh%nzmin, sh%nzsec, sh%nzjday)
    
    call sac__init(sh)
 
    sh%nzmsec = 0
    sh%b = 0
    
    write(ymd,'(I4.4,I2.2,I2.2)') sh%nzyear, sh%nzmonth, sh%nzday
    write(hms,'(3I2.2)') sh%nzhour, sh%nzmin, sh%nzsec
    write(clen,'(I6.6)') nsec


    do i=1, nch
      call ch2sh(ch(i), sh)
      sh%npts = npts(i) * nwin
      sh%delta = 1/dble(sfreq(i))
      sh%e = (sh%npts - 1) * sh%delta

      fn_sac = ymd // '__' // hms // '__' // clen // '__' // &
               trim(sh%kstnm) // '__' // trim(adjustl(sh%kcmpnm)) // '__.sac'
      
      call sac__write(fn_sac, sh, dat(:,i)*ch%conv)
    end do

  end block

  contains

  subroutine usage_stop()
    stop
  end subroutine usage_stop

  subroutine ch2sh(ch, sh)

    type(winch__hdr), intent(in)  :: ch
    type(sac__hdr), intent(out) :: sh
    !--
    integer :: iscan
    !----

    
    sh%stla  =   ch%lat 
    sh%stlo  =   ch%lon 
    sh%stdp  = - ch%elev 
    sh%stel  =   ch%elev
    if( len_trim( ch%stnm ) <= 8 ) then
      sh%kstnm = trim(ch%stnm)
    else
      sh%kstnm = ch%stnm(1:8)
    end if
    sh%kcmpnm = trim(ch%cmpnm)

    !! cmpaz, cmpinc
    !! n
    iscan = scan( ch%cmpnm, 'NX' )
    if( iscan > 0 ) then
      sh%cmpaz = 0
      sh%cmpinc = 90
    end if
    
    !! e
    iscan = scan( ch%cmpnm, 'EY' )
    if( iscan > 0 ) then
      sh%cmpaz = 90
      sh%cmpinc = 90
    end if
    
    !! v
    iscan = scan( ch%cmpnm, 'UZ' )
    if( iscan > 0 ) then
      sh%cmpaz = 0
      sh%cmpinc = 0
    end if
    
    !! unit
    select case ( trim(ch%unit) )
      case( 'm' )
        sh%idep = 6
      case( 'm/s' )
        sh%idep = 7
      case( 'm/s/s' )
        sh%idep = 8
      case default
        sh%idep = 5
    end select
  
  end subroutine ch2sh
  
end program fwin2sac
!-------------------------------------------------------------------------------------------------!
