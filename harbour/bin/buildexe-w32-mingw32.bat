@echo off
rem 
rem $Id$
rem 

rem Harbour executable builder batch file
rem
rem Compiler: Cygwin GCC compiler / MingW32
rem Platform: Windows 32 bit (console mode)
rem
rem Maintainer: Victor Szel <info@szelvesz.hu>

if not "%1" == "" goto compile

echo.
echo Usage: buildexe.bat name
echo.
echo - 'name' is the .prg filename *without* extension.
echo - Don't forget to make a MAIN function for you application.
echo - This batch file assumes you are in some directory off the main harbour dir
exit

:compile

harbour %1.prg /n /i..\include
gcc %1.c -mno-cygwin -I..\include -L..\lib -lrtl -lrdd -lvm -lrdd -lrtl -ldbfntx -ldbfcdx
del %1.c
