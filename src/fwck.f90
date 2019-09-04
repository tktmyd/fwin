!-------------------------------------------------------------------------------------------------!
!> Display win/win32 file information
!!
!! @copyright
!! Copyright (c) 2019 Takuto Maeda. All rights reserved. 
!!
!! @license 
!! This software is released under the MIT license. See LICENSE for details. 
!--
program fwck

  use iso_fortran_env
  use m_win
  implicit none
  !--
  character(256) :: fn
  logical :: is_exist
  character, allocatable :: buf(:)
  character(4), allocatable :: chid(:)
  integer :: nch
  integer :: i
  type(win__hdr)  :: wh
  integer :: io
  !----

  if( command_argument_count() /= 1 ) error stop 'usage: fwck.x winfile'
  
  call get_command_argument(1, fn)
  inquire( file=fn, exist=is_exist )
  if( .not. is_exist ) error stop 'File not found: ' // trim(fn)
  
  call win__init()
  call win__start_file_buf( fn, wh, buf, io )
  call win__finish_file_buf(io)
  call win__scan_buf(wh, buf)
  call win__get_chid( wh, buf, 1, nch, chid )

  write(output_unit,'(A)')      " FILE:      " // trim( fn )
  write(output_unit,'(A,I8,A)') " SIZE:      ", wh%sz, " bytes"
  write(output_unit,'(A,I3)')   " TYPE:      ", wh%fmt
  write(output_unit,*)

  do i=1, nch
    write(output_unit,'(A,I5,A,A)') "  Channel #", i, ": ", chid(i)
  end do
  
  do i=1, wh%nb
    call win__get_chid(wh, buf, i, nch, chid )
    write(*,'(A,I3,A,I4,2("-",I2.2)," ",2(I2.2,":"),I2.2)') &
    "Block", i, ": ", wh%yr(i), wh%mo(i), wh%dy(i), wh%hr(i), wh%mi(i), wh%sc(i)
  end do
  
end program fwck
!-------------------------------------------------------------------------------------------------!
