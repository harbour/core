@echo off
rem
rem $Id$
rem

IF A%1 == A GOTO :SINTAX
IF A%2 == A GOTO :NOOUTPUT

echo -O2 -e%2.exe -I..\include %1.c > b32.bc
echo ..\lib\b32\harbour.lib ..\lib\b32\terminal.lib >> b32.bc
echo ..\lib\b32\hbgt.lib ..\lib\b32\hbpp.lib >> b32.bc
echo ..\lib\b32\rdd.lib >> b32.bc
bcc32 -v @b32.bc
del b32.bc
GOTO :END

:NOOUTPUT
echo -O2 -e%1.exe -I..\include %1.c > b32.bc
echo ..\lib\b32\harbour.lib ..\lib\b32\terminal.lib >> b32.bc
echo ..\lib\b32\hbgt.lib ..\lib\b32\hbpp.lib >> b32.bc
echo ..\lib\b32\rdd.lib >> b32.bc
bcc32 -v @b32.bc
del b32.bc
GOTO :END

:SINTAX
ECHO syntax: BuildExe Harbour_Output_Filename [Exe_Output_Filename]
ECHO Use Harbour_Output_Filename and Exe_Output_Filename without extensions
ECHO\

:END
