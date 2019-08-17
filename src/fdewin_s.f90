!-------------------------------------------------------------------------------------------------!
!> Read Win/Win32-formatted seismograph data and export to ascii files: synchronous version
!!
!! @copyright
!! Copyright (c) 2019 Takuto Maeda. All rights reserved. 
!!
!! @license 
!! This software is released under the MIT license. See LICENSE for details. 
!--
program fdewin_s

  use iso_fortran_env
  use m_win
  use m_util
  use m_getopt
  
  implicit none 
  !--

  integer                     :: nch, nw
  character(256), allocatable :: fn_win(:)
  character(4),   allocatable :: chid(:)
  integer,        allocatable :: dat(:,:), dat0(:,:)
  integer,        allocatable :: npts(:), sfreq(:), npts_all(:)
  character(80)               :: d_out
  integer, parameter          :: fsmax = 200
  logical                     :: is_test_mode
  !----

  !-----------------------------------------------------------------------------------------------!
  !> command-line option processing
  !--
  block

    character(80) :: fn_winlst
    character(80) :: fn_chlst
    logical :: is_opt

    call getopt('wl', is_opt, fn_winlst, '' )

    if( is_opt ) then
      call util__readlst( fn_winlst, nw, fn_win )
    else
      nw = 1
      allocate( fn_win(1) )
      call getopt('w', is_opt, fn_win(1), '') 
      if( .not. is_opt ) error stop 'No winfile given'
    end if
    
    call getopt('cl', is_opt, fn_chlst, '' )

    if( is_opt ) then
      call util__readlst( fn_chlst, nch, chid )
    else
      nch = 1
      allocate(chid(1))
      call getopt('c', is_opt, chid(1), '')
      if( .not. is_opt ) error stop 'No channel given'
    end if

    call getopt('d', is_opt, d_out, '.' ) 

    call getopt('t', is_test_mode)

  end block
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  !> Read the data
  !--
  block
    integer :: i, j
    integer :: tim, nsec
    !----

    allocate( npts(nch), npts_all(nch), sfreq(nch) )
    allocate( dat(fsmax*60*nw,nch) ) !! initial size
    dat(:,:) = 0

    do i=1, nw
      call win__read_file(fn_win(i), chid, npts, sfreq, dat0, tim, nsec)
      do j=1, nch
        if( npts(j) > 0 ) then
          npts_all(j) = max(npts_all(j), npts(j))
          dat((i-1)*npts_all(j)+1:(i-1)*npts_all(j)+npts(j), j) = dat0(1:npts(j),j)
        end if
      end do   

    end do
  end block
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  !> Export
  !--  
  block
    integer :: i, j, io
    character(80) :: fn_asc
    !----

    do i=1, nch
      if( npts(i) > 0 ) then
        fn_asc = trim(d_out) //'/'//trim(chid(i))//'.dat'
        open(newunit=io, file=fn_asc, action='write', status='unknown')
        do j=1, npts_all(i)*nw
          write(io,'(I0)') dat(j,i)
        end do
        close(io)
        if( is_test_mode ) exit
      else
        write(error_unit,'(A)') 'Channel ' // chid(i) // ' : no data in the file'      
      end if
    end do
  end block
  !-----------------------------------------------------------------------------------------------!
      
end program fdewin_s
!-------------------------------------------------------------------------------------------------!
