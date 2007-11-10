@echo off
rem 
rem $Id$
rem 

CLS

if A%1 == A GOTO :SYNTAX
if NOT EXIST %1.prg GOTO :NOEXIST

ECHO Compiling...

set hdir=..\..\..
set bcdir=C:\bcc55\bin

%hdir%\bin\harbour %1 /n /p /i..\include;%hdir%\include %2 %3 > bld_b32.log
type bld_b32.log
IF ERRORLEVEL 1 PAUSE
IF ERRORLEVEL 1 GOTO EXIT

echo -O2 -e%1.exe -I%hdir%\include %1.c > b32.bc
%BCDIR%\bcc32 -M -c @b32.bc
:ENDCOMPILE

echo c0w32.obj + > b32.bc
echo %1.obj, + >> b32.bc
echo %1.exe, + >> b32.bc
echo %1.map, + >> b32.bc
echo %hdir%\lib\rtl.lib + >> b32.bc
echo %hdir%\lib\vm.lib + >> b32.bc
echo %hdir%\lib\gtwin.lib + >> b32.bc
echo %hdir%\lib\lang.lib + >> b32.bc
echo %hdir%\lib\macro.lib + >> b32.bc
echo %hdir%\lib\rdd.lib + >> b32.bc
echo %hdir%\lib\dbfntx.lib + >> b32.bc
echo %hdir%\lib\dbfcdx.lib + >> b32.bc
echo %hdir%\lib\debug.lib + >> b32.bc
echo %hdir%\lib\common.lib + >> b32.bc
echo %hdir%\lib\pp.lib + >> b32.bc
echo %hdir%\lib\hbsix.lib + >> b32.bc 
echo %hdir%\lib\dbffpt.lib + >> b32.bc
echo %hdir%\lib\hgfwin32.lib + >> b32.bc

rem Uncomment these two lines to use Advantage RDD
rem echo %hdir%\lib\rddads.lib + >> b32.bc  
rem echo %hdir%\lib\ace32.lib + >> b32.bc

echo %BCDIR%\lib\cw32.lib + >> b32.bc
echo %BCDIR%\lib\import32.lib, >> b32.bc

ECHO * 
ECHO Linking...
rem Use these flags to avoid the console window creation
rem %BCDIR%\ilink32 -Gn -aa -Tpe -s @b32.bc
%BCDIR%\ilink32 -Gn -Tpe -s @b32.bc

rem delete temporary files
del %1.c
del b32.bc
del bld_b32.log
del %1.map
del %1.obj
del %1.tds

IF ERRORLEVEL 1 GOTO LINKERROR
ECHO * Application successfully built
%1
GOTO EXIT
ECHO

:LINKERROR
rem if exist meminfo.txt notepad meminfo.txt
rem PAUSE * Linking errors *
GOTO EXIT

:SYNTAX
ECHO    SYNTAX: Build [Program]     {-- Don't specify .prg extension
GOTO EXIT

:NOEXIST
ECHO The specified PRG %1 does not exist

:EXIT
