@echo off
REM
REM $Id$
REM
REM

rem ---------------------------------------------------------------

rem Save the user value, force silent file overwrite with COPY
rem (not all Windows versions support the COPY /Y flag)
set HB_ORGENV_COPYCMD=%COPYCMD%
set COPYCMD=/Y

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist obj\dll md obj\dll
if not exist obj\dll\b32 md obj\dll\b32

:BUILD

   make -fmake_b32_dll.mak %1 %2 %3 > make_b32_dll.log
   if errorlevel 1 goto notepad make_b32_dll.log
 
:BUILD_OK

   if exist hdll.tmp del hdll.tmp
   if exist ..\..\lib\b32\hbziparchdll.dll implib ..\..\lib\b32\hbziparchdll.lib ..\..\lib\b32\hbziparchdll.dll > nul
   if exist ..\..\lib\b32\hbziparchdll.dll copy ..\..\lib\b32\hbziparchdll.dll ..\..\lib > nul
   if exist ..\..\lib\b32\hbziparchdll.lib copy ..\..\lib\b32\hbziparchdll.lib ..\..\lib > nul

   goto EXIT

:CLEAN

   if exist dll_b32.log del dll_b32.log

:EXIT

rem ---------------------------------------------------------------

rem Restore user value
set COPYCMD=%HB_ORGENV_COPYCMD%
