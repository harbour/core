@echo off
rem
rem $Id$
rem

IF A%1 == A GOTO :SINTAX
IF A%2 == A GOTO :NOOUTPUT

echo -P -mh -O2 -Fm -e%2.exe -I..\..\include ..\..\source\vm\hvm.c %1.c > b16.bc
echo ..\..\lib\b16\harbour.lib ..\..\lib\b16\hbtools.lib ..\..\lib\b16\terminal.lib >> b16.bc
bcc @b16.bc
del b16.bc
GOTO :END

:NOOUTPUT
echo -P -mh -O2 -Fm -e%1.exe -I..\..\include ..\..\source\vm\hvm.c %1.c > b16.bc
echo ..\..\lib\b16\harbour.lib ..\..\lib\b16\hbtools.lib ..\..\lib\b16\terminal.lib >> b16.bc
bcc @b16.bc
del b16.bc
GOTO :END

:SINTAX
ECHO syntax: BuildExe Harbour_Output_Filename [Exe_Output_Filename]
ECHO Use Harbour_Output_Filename and Exe_Output_Filename without extensions
ECHO\

:END
