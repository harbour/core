@echo off
rem 
rem $Id$
rem 

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc -DB40 %1 %2 %3 > make_b40.log
   notepad make_b40.log
   goto EXIT

:CLEAN

   echo Y | del bin\*.exe > NUL
   echo Y | del bin\*.tds > NUL
   echo Y | del obj\b32\*.* > NUL
   echo Y | del lib\b32\*.* > NUL
   goto EXIT

:EXIT

