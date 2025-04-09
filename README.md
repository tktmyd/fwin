# fwin

A module for reading seismic waveform data in WIN and WIN32 formats

## Compile

```bash
$ cd src
 (edit makefile if necessary)
$ make
```

To fully use the asynchronous I/O in Fortran2008, gfortran version 9 or later is required, while the program can be successfully compiled in more earlier versions.

## Core modules

Please read the block comments in the code for the detail of usage. Source codes of the utility programs (see below) may be useful for understanding the usage.

### module m_win (in m_win.f90)

- `subroutine win__read_files`: Asynchronously read a set of WIN/WIN32 files
- `subroutine win__read_file`: Read a win file

## module m_winch (in m_winch.f90)

- `type winch__hdr`: defines channel information. Usually use as an array
- `subroutine winch__read_tbl`: Read a channel table file
- `subroutine winch__get_all_chid`: Obtain all channel ID from the channel table data array
- `subroutine winch__get_all_stnm`: Obatin all station names contained in the channel table data array
- `subroutine winch__get_all_cmpnm`: Obtain all component names contained in the channel table data array

## Utility Programs

### fchinf.x

Display selected channel information from a given channel table file

### fdewin_s.x

Convert WIN/WIN32 files to ascii text: Synchronous version

### fdewin_a.x

Convert WIN/WIN32 files to ascii text: Asynchronous version

### fwin2sac.x

Convert WIN/WIN32 files to SAC-formatted datafiles

## License

MIT License. Please see LICENSE file for details.
