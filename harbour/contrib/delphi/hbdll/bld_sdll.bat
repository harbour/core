rem Self contained Harbour DLL, original idea and research Antonio Linares
rem
rem $Id$
rem
@ECHO OFF
CLS

if A%1 == A GOTO :SINTAX
if NOT EXIST %1.prg GOTO :NOEXIST

ECHO Compiling...

set hdir=c:\hb-dev\harbour
set bcdir=c:\borland\bcc55\bin

%bcdir%\bcc32 -c -D__EXPORT__ -I%hdir%\include -L%bcdir%\..\lib %hdir%\source\vm\maindll.c

rem Files to integrate harbour DLL to Delphi
%bcdir%\bcc32 -c -D__EXPORT__ -I%hdir%\include -L%bcdir%\..\lib macrcall.c
%hdir%\bin\harbour -n -w errorsys
%bcdir%\bcc32 -c -D__EXPORT__ -I%hdir%\include errorsys.c

%hdir%\bin\harbour %1 /n /i%hdir%\include /w /p %2 %3 > clip.log
@type clip.log
IF ERRORLEVEL 1 PAUSE
IF ERRORLEVEL 1 GOTO EXIT

echo -O2 -I%hdir%\include %1.c > b32.bc
%bcdir%\bcc32 -M -c @b32.bc
:ENDCOMPILE

IF EXIST %1.rc %bcdir%\brc32 -r %1

echo c0d32.obj + > b32.bc
echo %1.obj+errorsys.obj+maindll.obj+macrcall.obj, + >> b32.bc
echo %1.dll, + >> b32.bc
echo %1.map, + >> b32.bc
rem echo ..\lib\FiveH.lib ..\lib\FiveHC.lib + >> b32.bc
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

rem Uncomment these two lines to use Advantage RDD
rem echo %hdir%\lib\rddads.lib + >> b32.bc
rem echo ..\lib\Ace32.lib + >> b32.bc

echo %bcdir%\lib\import32.lib + >> b32.bc
echo %bcdir%\lib\cw32.lib + >> b32.bc
echo %bcdir%\lib\psdk\odbc32.lib, >> b32.bc
IF EXIST %1.res echo %1.res >> b32.bc
%bcdir%\ilink32 -Tpd -aa -L%bcdir%\..\lib -L%bcdir%\..\lib\PSDK @b32.bc

rem delete temporary files
@del %1.c
@del %1.il?

IF ERRORLEVEL 1 GOTO LINKERROR
ECHO * self contained DLL successfully built
GOTO EXIT
ECHO

:LINKERROR
rem if exist meminfo.txt notepad meminfo.txt
rem PAUSE * Linking errors *
GOTO EXIT

:SINTAX
ECHO    SYNTAX: Build [Program]     {-- No especifiques la extensi½n PRG
ECHO                                {-- Don't specify .PRG extension
GOTO EXIT

:NOEXIST
ECHO The specified PRG %1 does not exist

:EXIT

