
rem ---------------------------------------------------------------
rem Template to initialize the environment before starting
rem the GNU make system for Harbour
rem
rem For further information about the GNU make system please
rem check doc/gmake.txt
rem ---------------------------------------------------------------

rem Under OS/2 you may rename this file to have a .CMD extension

rem ---------------------------------------------------------------
rem The following HB_ARCHITECTURE values are currently supported:
rem  - dos
rem  - win32
rem  - linux
rem  - os2

set HB_ARCHITECTURE=dos

rem ---------------------------------------------------------------
rem The following HB_COMPILER values are currently supported:
rem  - When HB_ARCHITECTURE=dos
rem    - bcc31
rem    - djgpp
rem    - watcom
rem  - When HB_ARCHITECTURE=win32
rem    - bcc32
rem    - gcc
rem    - icc
rem    - msvc
rem  - When HB_ARCHITECTURE=linux
rem    - gcc
rem  - When HB_ARCHITECTURE=os2
rem    - gcc
rem    - icc

set HB_COMPILER=djgpp

rem ---------------------------------------------------------------
rem Start the GNU make system

make %1 %2 %3 %4 %5 %6 %7 %8 %9
