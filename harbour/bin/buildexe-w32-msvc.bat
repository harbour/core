@echo off
rem 
rem $Id$
rem 

rem Harbour executable builder batch file
rem
rem Compiler: MSVC v 12.00.8168
rem Platform: Windows 32 bit (console mode)
rem
rem Maintainer: Paul Tucker <ptucker@sympatico.ca>

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
cl -Fd..\bin\harbour -w -Zi -TP -GZ -GA -DHARBOUR_USE_WIN_GTAPI -I..\include %1.c /link /subsystem:CONSOLE ..\obj\symbols.obj ..\lib\harbour.lib ..\lib\terminal.lib ..\lib\hbtools.lib ..\lib\dbfntx.lib ..\lib\debug.lib
echo Ignore LNK4033 warning
del %1.c
