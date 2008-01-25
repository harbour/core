@echo off
rem 
rem $Id$
rem 

if A%1 == A GOTO :SYNTAX
if NOT EXIST %1.prg GOTO :NOEXIST

ECHO Build: Compiling...

set hdir=..\..\..

%hdir%\bin\harbour %1 /n /i..\include;%hdir%\include %2 %3 > bld_b32.log
type bld_b32.log
IF ERRORLEVEL 1 PAUSE
IF ERRORLEVEL 1 GOTO EXIT

echo -O2 -e%1.exe -I%hdir%\include %1.c > bld_b32.mak
bcc32 -M -c @bld_b32.mak
:ENDCOMPILE

echo c0w32.obj + > bld_b32.mak
echo %1.obj, + >> bld_b32.mak
echo %1.exe, + >> bld_b32.mak
echo %1.map, + >> bld_b32.mak
echo %hdir%\lib\hbrtl.lib + >> bld_b32.mak
echo %hdir%\lib\hbvm.lib + >> bld_b32.mak
echo %hdir%\lib\gtwin.lib + >> bld_b32.mak
echo %hdir%\lib\hblang.lib + >> bld_b32.mak
echo %hdir%\lib\hbmacro.lib + >> bld_b32.mak
echo %hdir%\lib\hbrdd.lib + >> bld_b32.mak
echo %hdir%\lib\hbdebug.lib + >> bld_b32.mak
echo %hdir%\lib\hbcommon.lib + >> bld_b32.mak
echo %hdir%\lib\hbpp.lib + >> bld_b32.mak
echo %hdir%\lib\hbsix.lib + >> bld_b32.mak 
echo %hdir%\lib\rddntx.lib + >> bld_b32.mak
echo %hdir%\lib\rddcdx.lib + >> bld_b32.mak
echo %hdir%\lib\rddfpt.lib + >> bld_b32.mak
echo %hdir%\lib\hbgfw32.lib + >> bld_b32.mak

rem Uncomment these two lines to use Advantage RDD
rem echo %hdir%\lib\hbrddads.lib + >> bld_b32.mak  
rem echo %hdir%\lib\ace32.lib + >> bld_b32.mak

echo cw32.lib + >> bld_b32.mak
echo import32.lib, >> bld_b32.mak

ECHO Build: Linking...
rem Use these flags to avoid the console window creation
rem ilink32 -Gn -aa -Tpe -s @bld_b32.mak
ilink32 -Gn -Tpe -s @bld_b32.mak

rem delete temporary files
del %1.c
del %1.obj
if exist %1.map del %1.map
if exist %1.tds del %1.tds
del bld_b32.mak
del bld_b32.log

IF ERRORLEVEL 1 GOTO LINKERROR
ECHO Build: Done.
%1
GOTO EXIT
ECHO

:LINKERROR
rem if exist meminfo.txt notepad meminfo.txt
rem PAUSE * Linking errors *
GOTO EXIT

:SYNTAX
ECHO SYNTAX: bld_b32 [Program]     {-- Don't specify .prg extension
GOTO EXIT

:NOEXIST
ECHO Build: The specified %1.prg does not exist

:EXIT
