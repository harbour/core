@echo off
rem 
rem $Id$
rem 

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\rddads.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:EXIT

