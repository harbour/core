@echo off

IF A%1 == A GOTO :SINTAX
IF A%2 == A GOTO :NOOUTPUT

bcc32 -DNO_OBJ -O2 -M -e%2.exe -I...\include -L...\libs\b32 ...\source\vm\hvm.c %1.c harbour.lib terminal.lib
GOTO :END

:NOOUTPUT
bcc32 -DNO_OBJ -O2 -M -e%1.exe -I...\include -L...\libs\b32 ...\source\vm\hvm.c %1.c harbour.lib terminal.lib
GOTO :END

:SINTAX
ECHO syntax: BuildExe Harbour_Output_Filename [Exe_Output_Filename]
ECHO Use Harbour_Output_Filename and Exe_Output_Filename without extensions
ECHO\

:END
