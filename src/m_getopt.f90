!-------------------------------------------------------------------------------------------------!
!> Processig command line option
!!
!! @copyright
!! Copyright (c) 2019-2025 Takuto Maeda. All rights reserved. 
!!
!! @license 
!! This software is released under the MIT license. See LICENSE for details. 
!<
!--
module m_getopt

  use iso_fortran_env
  implicit none
  private

  public :: getopt
  
  !-----------------------------------------------------------------------------------------------!
  !> Obtain command-line option
  !!
  !! @par Usage
  !! getopt( OPTNAME, exist, var, default_bar )
  !!   - var, default_var are optional.
  !!   - types of var can be double, single integer or character
  !--
  interface getopt

   module procedure getopt_d, getopt_s, getopt_i, getopt_c

  end interface getopt
  !-----------------------------------------------------------------------------------------------!

contains

  !-----------------------------------------------------------------------------------------------!
  !> Obtain command line as characters
  !!
  !! @par Example
  !!   For the command 
  !!   hoge -A -T 01
  !!     call getopt( 'A', isA, Aval ) -> isA=.true.,  Aval=''
  !!     call getopt( 'T', isT, Tval ) -> isT=.true.,  Tval='01'
  !!     call getopt( 'C', isC, Cval ) -> isC=.false., Cval=''
  !!     call getopt( 'A', isA )       -> isA=.true.
  !--
  subroutine getopt_c(opt, isExist, val, default)
    character(*),           intent(in)  :: opt
    logical,                intent(out) :: isExist
    character(*), optional, intent(out) :: val
    character(*), optional, intent(in)  :: default
    !--
    integer :: narg
    character(256), allocatable :: argv(:)
    integer :: i
    character(256) :: optkey
    !----

    narg = command_argument_count()
    allocate( argv(1:narg) )
    
    do i=1, narg
       call get_command_argument( i, argv(i)(:) )
    end do
    
    if( present( val ) ) then
       val = ''
    end if
    
    isExist = .false. 
    optkey = '-'//trim(adjustl( opt ) )
    
    do i=1, narg
       
       if( trim(optkey) == trim(argv(i)) ) then
          
          if( isExist ) then
             write(error_unit,*) 'getopt: ', &
                  'option '//trim(optkey)//' is multiplly defined. ' 
          end if
          
          isExist = .true.
          
          if( present( val ) ) then
             
             if( i==narg ) then
                val = ''
             else
                val = argv(i+1)
             end if
             
          end if
       end if
    end do
    
    deallocate( argv )
    
    if( .not. isExist .and. present( default ) ) then
       val = default
    end if
    
  end subroutine getopt_c
  !-----------------------------------------------------------------------------------------------!
 
  !-----------------------------------------------------------------------------------------------!
  !> Obtain command line option for double precision number
  !--
  subroutine getopt_d( opt, isExist, val, default )
 
    character(*),       intent(in)  :: opt
    logical,            intent(out) :: isExist
    real(real64),           intent(out) :: val
    real(real64), optional, intent(in)  :: default
    !--
    character(1024) :: aval
    !----

    call getopt_c( opt, isExist, aval )
    
    if( .not. isExist ) then
       if( present( default ) ) then
          val = default
       else
          val = -99999.9_real64
       end if
       return
    end if
    
    read( aval, * ) val
    
  end subroutine getopt_d
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  !> Obtain command line option for single precision number
  !--
  subroutine getopt_s( opt, isExist, val, default )

    character(*),   intent(in)  :: opt
    logical,        intent(out) :: isExist
    real,           intent(out) :: val
    real, optional, intent(in)  :: default
    !--
    character(1024) :: aval
    !----
    
    call getopt_c( opt, isExist, aval )
    
    if( .not. isExist ) then
       if( present( default ) ) then
          val = default
       else
          val = -99999.9
       end if
       return
    end if
    
    read(aval, *) val

  end subroutine getopt_s
  !-----------------------------------------------------------------------------------------------!
  

  !-----------------------------------------------------------------------------------------------!
  !> Obtain command line option for integer number
  !--  
  subroutine getopt_i( opt, isExist, val, default )

    character(*),      intent(in)  :: opt
    logical,           intent(out) :: isExist
    integer,           intent(out) :: val
    integer, optional, intent(in)  :: default
    !--
    character(1024) :: aval
    !----    
    
    call getopt_c( opt, isExist, aval )
    
    if( .not. isExist ) then
       if( present( default ) ) then
          val = default
       else
          val = -99999
       end if
       return
    end if

    read(aval, *) val
    
  end subroutine getopt_i
  !-----------------------------------------------------------------------------------------------!

end module m_getopt
!-------------------------------------------------------------------------------------------------!
