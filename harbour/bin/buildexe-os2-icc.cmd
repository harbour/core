@echo off
rem 
rem $Id$
rem 

rem Harbour executable builder batch file
rem
rem Compiler: ICC (IBM Visual Age C++ 3.0)
rem Platform: 32-bit OS/2
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
icc /Gs+ /W2 /Se /Sd+ /Ti+ -I..\include /C- /Tp %1.c ..\lib\rtl.lib ..\lib\rdd.lib ..\lib\vm.lib ..\lib\rdd.lib ..\lib\rtl.lib ..\lib\dbfntx.lib
del %1.c
