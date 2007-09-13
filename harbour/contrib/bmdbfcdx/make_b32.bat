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

   copy ..\..\lib\b32\bmdbfcdx.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\bmdbfcdx.bak del ..\..\lib\b32\bmdbfcdx.bak
   copy ..\..\lib\b32\bmsixcdx.lib ..\..\lib\*.* > nul
   if exist ..\..\lib\b32\bmsixcdx.bak del ..\..\lib\b32\bmsixcdx.bak
   goto EXIT

:BUILD_ERR

   notepad make_b32.log
   goto EXIT

:CLEAN
   if exist ..\..\lib\b32\bmdbfcdx.lib   del ..\..\lib\b32\bmdbfcdx.lib
   if exist ..\..\lib\b32\bmdbfcdx.bak  del ..\..\lib\b32\bmdbfcdx.bak
   if exist ..\..\lib\b32\bmsixcdx.lib   del ..\..\lib\b32\bmsixcdx.lib
   if exist ..\..\lib\b32\bmsixcdx.bak  del ..\..\lib\b32\bmsixcdx.bak

   goto EXIT

:EXIT