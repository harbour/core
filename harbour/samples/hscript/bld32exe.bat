@echo off

IF A%1 == A GOTO :SINTAX
IF A%2 == A GOTO :NOOUTPUT

echo -O2 -e%2.exe -I..\..\include ..\..\source\vm\hvm.c %1.c > b32.bc
echo ..\..\libs\b32\harbour.lib ..\..\libs\b32\terminal.lib >> b32.bc
echo ..\..\libs\b32\hbgt.lib >> b32.bc
bcc32 @b32.bc
del b32.bc
GOTO :END

:NOOUTPUT
echo -O2 -e%1.exe -I..\..\include ..\..\source\vm\hvm.c %1.c > b32.bc
echo ..\..\libs\b32\harbour.lib ..\..\libs\b32\terminal.lib >> b32.bc
echo ..\..\libs\b32\hbgt.lib >> b32.bc
echo ..\runner.obj >> b32.bc
bcc32 @b32.bc
del b32.bc
GOTO :END

:SINTAX
ECHO syntax: BuildExe Harbour_Output_Filename [Exe_Output_Filename]
ECHO Use Harbour_Output_Filename and Exe_Output_Filename without extensions
ECHO\

:END
