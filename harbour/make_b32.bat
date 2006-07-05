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
rem
rem Set any of the below settings to customize your build process:
rem    set HB_BUILD_MODE=C
rem    set HB_BUILD_DLL=yes
rem    set HB_BUILD_DEBUG=yes
rem    set HB_BUILD_VERBOSE=yes
rem    set HB_MAKE_PROGRAM=
rem    set HB_MAKE_FLAGS=
rem ---------------------------------------------------------------

if "%HB_MAKE_PROGRAM%" == "" set HB_MAKE_PROGRAM=make.exe

rem ---------------------------------------------------------------

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

rem ---------------------------------------------------------------

:BUILD

   if not exist obj\nul         md obj
   if not exist obj\b32\nul     md obj\b32
   if not exist obj\dll\nul     md obj\dll
   if not exist obj\dll\b32\nul md obj\dll\b32
   if not exist lib\nul         md lib
   if not exist lib\b32\nul     md lib\b32
   if not exist bin\nul         md bin
   if not exist bin\b32\nul     md bin\b32

   %HB_MAKE_PROGRAM% %HB_MAKE_FLAGS% -r -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

rem ---------------------------------------------------------------

:BUILD_OK

   copy bin\b32\*.exe bin\*.* > nul
   copy lib\b32\*.lib lib\*.* > nul
   goto EXIT

rem ---------------------------------------------------------------

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

rem ---------------------------------------------------------------

:CLEAN

   if exist bin\b32\*.exe     del bin\b32\*.exe
   if exist bin\b32\*.dll     del bin\b32\*.dll
   if exist bin\b32\*.lib     del bin\b32\*.lib
   if exist bin\b32\*.tds     del bin\b32\*.tds
   if exist bin\b32\*.map     del bin\b32\*.map
                              
   if exist lib\b32\*.lib     del lib\b32\*.lib
   if exist lib\b32\*.bak     del lib\b32\*.bak

   if exist obj\dll\b32\*.obj del obj\dll\b32\*.obj
   if exist obj\dll\b32\*.c   del obj\dll\b32\*.c
   if exist obj\dll\b32\*.h   del obj\dll\b32\*.h

   if exist obj\b32\*.obj     del obj\b32\*.obj
   if exist obj\b32\*.c       del obj\b32\*.c
   if exist obj\b32\*.h       del obj\b32\*.h

   if exist lib\*.lib         del lib\*.lib

   if exist make_b32.log del make_b32.log
   goto EXIT

rem ---------------------------------------------------------------

:EXIT
