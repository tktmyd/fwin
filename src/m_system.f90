!-------------------------------------------------------------------------------------------------!
!> Processig command line argument, environment variables, and system call
!!
!! @copyright
!! Copyright (c) 2019 Takuto Maeda. All rights reserved. 
!!
!! @license 
!! This software is released under the MIT license. See LICENSE for details. 
!<
!--
module m_system

  use iso_fortran_env
  implicit none
  private

  public :: system__getarg
  public :: system__getenv
  public :: system__iargc
  public :: system__expenv

  !!----------------------------------------------------------------------------------------------!
  !> Obtain command line arguments for character, integer, single and double precision data
  !--
  interface system__getarg
     
     module procedure getarg_a, getarg_i, getarg_f, getarg_d
     
  end interface
  !-----------------------------------------------------------------------------------------------!
  
contains

  !-----------------------------------------------------------------------------------------------!
  !> Returns a number of arguments. Fortran2003 wrapper function
  !--
  integer function system__iargc()
    
    system__iargc  = command_argument_count()
    
  end function system__iargc
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  !> Obtain environmental variable
  !--
  subroutine system__getenv( name, value )

    character(*), intent(in)  :: name
    character(*), intent(out) :: value
    !----
    
    call get_environment_variable( name, value )
    
  end subroutine system__getenv
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  !> get i-th command line argument as a character
  !--
  subroutine getarg_a (i, arg)

    integer,      intent(in)  :: i   ! order of the arguments
    character(*), intent(out) :: arg ! argument    
    !----

    call get_command_argument( i, arg )
    
  end subroutine getarg_a
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  !> get i-th command line argument as an integer
  !--
  subroutine getarg_i (i, arg)

    integer, intent(in) :: i
    integer, intent(out) :: arg
    !--
    character(256) :: carg
    !----

    call getarg_a( i, carg )
    read(carg,*) arg
    
  end subroutine getarg_i
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  !> get i-th command line argument as a floating variable
  !--
  subroutine getarg_f (i, arg)

    integer,  intent(in) :: i
    real, intent(out) :: arg
    !--    
    character(256) :: carg
    !----

    call getarg_a( i, carg )
    read(carg,*) arg
    
  end subroutine getarg_f
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  !> get i-th command line argument as a double precision variable
  !--
  subroutine getarg_d (i, arg)

    integer,  intent(in) :: i
    real(real64), intent(out) :: arg
    !--
    character(256) :: carg
    !----
    
    call getarg_a( i, carg )
    read(carg,*) arg
    
  end subroutine getarg_d
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  !> Expand shell environmental variables. Variables must be wrapped in '${' and '}'
  !--
  subroutine system__expenv( str )

    character(*), intent(inout) :: str
    !--
    character(256) :: str2
    integer :: ikey1, ikey2
    integer :: iptr
    character(256) :: str3
    !----

    iptr = 1
    str2 = ''
    do 
       ikey1 = scan( str(iptr:), "${" ) + iptr - 1 
       if( ikey1==iptr-1 ) exit

       ikey2 = scan( str(iptr:), "}" ) + iptr -1 
       str2=trim(str2) // str(iptr:ikey1-1)
       call system__getenv( str(ikey1+2:ikey2-1), str3 )
       str2 = trim(str2) // trim(str3)
       iptr = ikey2+1
       
    end do
    str2 = trim(str2) // trim(str(iptr:))

    str = trim(str2)
    
  end subroutine system__expenv
  !-----------------------------------------------------------------------------------------------!
  
    
end module m_system
!-------------------------------------------------------------------------------------------------!
