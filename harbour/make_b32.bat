@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs 
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to 
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

set HB_LEX=FLEX

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

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

