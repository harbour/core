@echo off
rem
rem $Id$
rem

rem Harbour executable builder batch file
rem
rem Compiler: JAVA
rem Platform: 32-bit Windows
rem

if not "%1" == "" goto COMPILE

echo.
echo Usage: bld_java.bat name
echo.
echo - 'name' is the .prg filename *without* extension.
echo - Don't forget to make a MAIN function for you application.
echo - This batch file assumes you are two level deeper than the main harbour dir
exit

:COMPILE

..\..\bin\harbour %1.prg /n /gj /i..\..\include
javac %1.java

