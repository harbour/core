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

   copy ..\..\lib\b32\apollo.lib ..\..\lib\*.* > nul
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\apollo.lib   del ..\..\lib\b32\apollo.lib
   if exist ..\..\lib\b32\apollo.bak   del ..\..\lib\b32\apollo.bak
   if exist ..\..\obj\b32\apollo.obj   del ..\..\obj\b32\apollo.obj
   if exist ..\..\obj\b32\apollo1.c    del ..\..\obj\b32\apollo1.c
   if exist ..\..\obj\b32\apollo1.bak  del ..\..\obj\b32\apollo1.bak
   if exist ..\..\obj\b32\apollo1.obj  del ..\..\obj\b32\apollo1.obj

   goto EXIT

:EXIT

