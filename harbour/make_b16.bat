@echo off
rem 
rem $Id$
rem 

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc -DB16 %1 %2 %3 > make_b16.log
   if errorlevel 1 edit make_b16.log
   goto EXIT

:CLEAN

   echo Y | del bin\*.exe > nul
   echo Y | del obj\b16\*.* > nul
   echo Y | del lib\b16\*.* > nul
   echo Y | del make_b16.log > nul
   goto EXIT

:EXIT

