@echo off
rem
rem $Id$
rem

rem "echo." intentionally used instead of "echo", to avoid conflicts
rem with external commands named echo.
rem using macros for ECHO and DEL to allow overiding such as:
rem
rem    SET ECHO=cmd /c echo
rem    SET DEL=cmd /c del
rem
rem The above might be needed on Windows 2000 and XP.
rem The macros are referenced in hb_slex.bc

if "%ECHO%"=="" SET ECHO=echo.
if "%DEL%"=="" SET DEL=del

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fhb_slex.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR
   cd contrib\libmisc
   make -fmakefile.bc %1 %2 %3 >> make_b32.log
   if errorlevel 1 goto BUILD_ERR
    cd..\..

:BUILD_OK

   copy bin\b32\*.exe bin\*.* > nul
   copy lib\b32\*.lib lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN

   if exist bin\b32\*.exe del bin\b32\*.exe
   if exist bin\b32\*.tds del bin\b32\*.tds
   if exist bin\b32\*.map del bin\b32\*.map
   if exist lib\b32\*.lib del lib\b32\*.lib
   if exist lib\b32\*.bak del lib\b32\*.bak
   if exist obj\b32\*.obj del obj\b32\*.obj
   if exist obj\b32\*.c   del obj\b32\*.c
   if exist obj\b32\*.h   del obj\b32\*.h
   if exist make_b32.log  del make_b32.log
   goto EXIT

:EXIT


