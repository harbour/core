@echo off
rem 
rem $Id$
rem 

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc -DB16 %1 %2 %3 > make_b16.log
   edit make_b16.log
   goto EXIT

:CLEAN

   echo Y | del bin\*.exe > NUL
   echo Y | del obj\b16\*.* > NUL
   echo Y | del lib\b16\*.* > NUL
   goto EXIT

:EXIT

