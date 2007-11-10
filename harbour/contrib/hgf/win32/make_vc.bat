@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------

rem Save the user value, force silent file overwrite with COPY
rem (not all Windows versions support the COPY /Y flag)
set HB_ORGENV_COPYCMD=%COPYCMD%
set COPYCMD=/Y

SET _LIB=%LIB%
SET _PATH=%PATH%
SET _INCLUDE=%INCLUDE%
SET LIB=C:\COMPILER\VC\LIB;%PATH%
SET PATH=C:\COMPILER\VC\BIN;%PATH%
SET INCLUDE=..\..\..\INCLUDE;C:\COMPILER\VC\INCLUDE;%_INCLUDE%
nmake /f make_vc.mak %1 %2 %3
SET LIB=%_LIB%
SET PATH=%_PATH%
SET INCLUDE=%_INCLUDE%
SET _LIB=
SET _PATH=
SET _INCLUDE=

rem ---------------------------------------------------------------

rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%
