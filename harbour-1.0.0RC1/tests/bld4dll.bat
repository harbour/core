@echo off
rem 
rem $Id$
rem 

rem ---------------------------------------------------------------
rem This file is intended to build a program, which uses harbour.dll
rem The main function must be called _AppMain.
rem To run the program, you need to have harbour.dll in your path.
rem ---------------------------------------------------------------

set HB_PATH=..\

%HB_PATH%\bin\harbour %1.prg -n -i%HB_PATH%\include %2 %3

bcc32 -c -O2 -d -I%HB_PATH%\include %1.c %HB_PATH%\source\vm\mainwin.c

@echo c0w32.obj + > _b32.bc
@echo %1.obj + >> _b32.bc
@echo mainwin.obj,+ >> _b32.bc
@echo %1.exe, + >> _b32.bc
@echo , + >> _b32.bc
@echo %HB_PATH%\lib\harbour.lib + >> _b32.bc
@echo cw32.lib + >> _b32.bc
@echo import32.lib, >> _b32.bc

ilink32 -Tpe -Gn @_b32.bc

del %1.obj
del mainwin.obj
del %1.c
del *.tds
del _b32.bc
