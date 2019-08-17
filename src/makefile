## Makefile for win package
##
## @copyright
## Copyright (c) 2019 Takuto Maeda. All rights reserved. 
##
## @license 
## This software is released under the MIT license. See LICENSE for details. 

FC = gfortran
#FFLAGS = -pthread -ffast-math -Ofast -march=native
FFLAGS = -fbounds-check -fbacktrace -O0
all: ../fchinf.x ../fwck.x ../fdewin_a.x ../fdewin_s.x ../fwin2sac.x

.SUFFIXES: 
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) -c $(FFLAGS) $<

fchinf.o: fchinf.f90 m_win.o m_winch.o m_getopt.o m_util.o
fdewin_a.o: fdewin_a.f90 m_win.o m_getopt.o
fdewin_s.o: fdewin_s.f90 m_win.o m_getopt.o
fwin2sac.o: fwin2sac.f90 m_win.o m_getopt.o m_system.o m_util.o m_wsac.o m_util.o m_winch.o
fwck.o: fwck.f90 m_win.o
m_getopt.o: m_getopt.f90 m_system.o
m_winch.o: m_winch.f90 m_win.o
m_wsac.o: m_wsac.f90 m_util.o
m_win.o: m_win.f90 m_util.o

../fchinf.x: fchinf.o m_win.o m_getopt.o m_system.o m_util.o m_winch.o m_util.o
	$(FC) $(FFLAGS) $^ -o $@

../fwck.x: fwck.o m_win.o m_util.o
	$(FC) $(FFLAGS) $^ -o $@

../fdewin_a.x: fdewin_a.o m_win.o m_getopt.o m_system.o m_util.o
	$(FC) $(FFLAGS) $^ -o $@

../fdewin_s.x: fdewin_s.o m_win.o m_getopt.o m_system.o m_util.o
	$(FC) $(FFLAGS) $^ -o $@

../fwin2sac.x: fwin2sac.o m_win.o m_getopt.o m_system.o m_util.o m_wsac.o m_util.o m_winch.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	/bin/rm -f *.mod *.o ../*.x
