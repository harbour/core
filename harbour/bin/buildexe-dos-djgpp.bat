@echo off
rem 
rem $Id$
rem 

rem Harbour executable builder batch file
rem
rem Compiler: GCC (Cygnus)
rem Platform: 32-bit Windows
rem
rem Adapted from the MINGW32 version created by Victor Szel <info@szelvesz.hu>
rem by David G. Holm <dholm@jsd-llc.com>

if not "%1" == "" goto compile

echo.
echo Usage: makeexe.bat name
echo.
echo - 'name' is the .prg filename *without* extension.
echo - Don't forget to make a MAIN function for you application.
echo - This batch file assumes you are in some directory off the main harbour dir
exit

:compile

harbour %1.prg /n /i..\include
gcc %1.c -o%1.exe -I..\include -L..\lib -lrtl -lrdd -lvm -lrdd -lrtl -ldbfnt
del %1.c
