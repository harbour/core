@echo off

IF A%1 == A GOTO :SINTAX
IF A%2 == A GOTO :NOOUTPUT

icc /Ti+ /W2 /Se /Sd+ /I..\..\include /Fe%2.exe /fohvm.obj /Tp..\..\source\vm\hvm.c /fo%1.obj /Tp%1.c ..\..\libs\icc\harbour.lib ..\..\libs\icc\hbtools.lib
GOTO :END

:NOOUTPUT
icc /Ti+ /W2 /Se /Sd+ /I..\..\include /Fe%1.exe /fohvm.obj /Tp..\..\source\vm\hvm.c /fo%1.obj /Tp%1.c ..\..\libs\icc\harbour.lib ..\..\libs\icc\hbtools.lib
GOTO :END

:SINTAX
ECHO syntax: BiccExe Harbour_Output_Filename [Exe_Output_Filename]
ECHO Use Harbour_Output_Filename and Exe_Output_Filename without extensions
ECHO\

:END
