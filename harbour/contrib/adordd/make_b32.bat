@echo off
rem
rem $Id$
rem

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

   make -fmakefile.bc %1 %2 %3 > make_b32.log
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy ..\..\lib\b32\adordd.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\adordd.bak del ..\..\lib\b32\adordd.bak
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\adordd.lib   del ..\..\lib\b32\adordd.lib
   if exist ..\..\lib\b32\adordd.bak  del ..\..\lib\b32\adordd.bak

   goto EXIT

:EXIT