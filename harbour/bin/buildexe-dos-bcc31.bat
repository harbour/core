@echo off
rem 
rem $Id$
rem 

rem Harbour executable builder batch file
rem
rem Compiler: Borland C/C++ 3.1
rem Platform: 16-bit DOS
rem
rem Adapted from the MINGW32 version created by Victor Szel <info@szelvesz.hu>
rem by David G. Holm <dholm@jsd-llc.com>

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
bcc -O2 -mh -I..\include %1.c ..\lib\rtl.lib ..\lib\rdd.lib ..\lib\vm.lib ..\lib\rdd.lib ..\lib\rtl.lib ..\lib\dbfntx.lib ..\lib\dbfcdx.lib
del %1.c
