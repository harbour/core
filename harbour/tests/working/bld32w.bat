@echo off

IF A%1 == A GOTO :SINTAX
IF A%2 == A GOTO :NOOUTPUT

bcc32 -O2 -e%2.exe -DWINDOWS -tW -I..\..\include ..\..\source\vm\hvm.c %1.c ..\..\libs\b32\harbour.lib ..\..\libs\win32\terminal.lib
GOTO :END

:NOOUTPUT
bcc32 -O2 -e%1.exe -DWINDOWS -tW -I..\..\include ..\..\source\vm\hvm.c %1.c ..\..\libs\b32\harbour.lib ..\..\libs\win32\terminal.lib
GOTO :END

:SINTAX
ECHO syntax: BuildExe Harbour_Output_Filename [Exe_Output_Filename]
ECHO Use Harbour_Output_Filename and Exe_Output_Filename without extensions
ECHO\

:END
