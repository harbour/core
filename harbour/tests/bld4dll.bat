@echo off
rem ---------------------------------------------------------------
rem This file is intended to build a program, which uses harbour.dll
rem The main function must be called _AppMain.
rem To run the program, you need to have harbour.dll in your path.
rem ---------------------------------------------------------------

set HB_PATH=..\

  %HB_PATH%\BIN\harbour %1.prg -n -i%HB_PATH%\INCLUDE %2 %3

  bcc32 -c -O2 -d -I%HB_PATH%\INCLUDE %1.c %HB_PATH%\source\vm\mainwin.c

  @echo c0w32.obj + > b32.bc
  @echo %1.obj + >> b32.bc
  @echo mainwin.obj,+ >> b32.bc
  @echo %1.exe, + >> b32.bc
  @echo , + >> b32.bc
  @echo %HB_PATH%\LIB\harbour.lib + >> b32.bc
  @echo cw32.lib + >> b32.bc
  @echo import32.lib, >> b32.bc

  ilink32 -Tpe -Gn @b32.bc

del %1.obj
del mainwin.obj
del %1.c
del *.tds
del b32.bc
