!-------------------------------------------------------------------------------------------------!
!> Read Win/Win32-formatted seismograph data and export to ascii files: asynchronous version
!!
!! @copyright
!! Copyright (c) 2019-2025 Takuto Maeda. All rights reserved. 
!!
!! @license 
!! This software is released under the MIT license. See LICENSE for details. 
!--
program fdewin_a

  use iso_fortran_env
  use m_win
  use m_util
  use m_getopt

  implicit none 

  integer, parameter :: fsmax = 200
  character(256), allocatable :: fn_win(:)
  integer :: nch, nw, nsec
  character(4), allocatable :: chid(:)
  integer, allocatable :: dat(:,:)
  integer, allocatable :: npts(:,:), sfreq(:)
  character(80) :: d_out
  logical :: is_test_mode
  !! ----

  !-----------------------------------------------------------------------------------------------!
  !> command-line option processing
  !--
  block

    character(80) :: fn_winlst
    character(80) :: fn_chlst
    logical :: is_opt, is_all

    call getopt('l', is_opt, fn_winlst, '' )

    if( is_opt ) then
      call util__readlst( fn_winlst, nw, fn_win )
    else
      nw = 1
      allocate( fn_win(1) )
      call getopt('w', is_opt, fn_win(1), '') 
      if(.not. is_opt) call usage_stop() 
    end if
    
    call getopt('c', is_opt, fn_chlst, '' )

    if( is_opt ) then
      call util__read_arglst( fn_chlst, nch, is_all, chid )
    else
      call usage_stop() 
    end if

    call getopt('d', is_opt, d_out, '.' ) 

    call getopt('Z', is_test_mode) 

  end block
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  !> Read the data
  !--
  block
    integer :: tim
    !----
    
    allocate( sfreq(nch) )
    allocate( dat(fsmax*60*nw,nch) ) !! initial size
    call win__read_files(fn_win, chid, sfreq, nsec, tim, dat, npts)
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
      if( sfreq(i) > 0 ) then
        fn_asc = trim(d_out) //'/'//trim(chid(i))//'.dat'
        open(newunit=io, file=fn_asc, action='write', status='unknown')
        do j=1, sfreq(i) * nsec
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

contains

!-----------------------------------------------------------------------------------------------!
  subroutine usage_stop()

    write(error_unit,'(A)') 'usage:  fdewin_a.x <-l winlst|-w winfile> <-c chid|lst> [-d dir]'
    stop
  end subroutine usage_stop

end program fdewin_a
!-------------------------------------------------------------------------------------------------!
