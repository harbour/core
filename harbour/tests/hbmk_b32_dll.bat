@echo off
rem 
rem $Id$
rem 

rem ---------------------------------------------------------------
rem This file is intended to build a program, which uses harbour-b32.dll
rem The main function must be called _AppMain.
rem To run the program, you need to have harbour-b32.dll in your path.
rem ---------------------------------------------------------------

..\bin\harbour %1.prg -n -i..\include %2 %3

bcc32 -c -O2 -d -I..\include %1.c ..\source\vm\mainwin.c

@echo c0w32.obj +                 >  _hbmk_b32_dll.tmp
@echo %1.obj +                    >> _hbmk_b32_dll.tmp
@echo mainwin.obj, +              >> _hbmk_b32_dll.tmp
@echo %1.exe, +                   >> _hbmk_b32_dll.tmp
@echo , +                         >> _hbmk_b32_dll.tmp
@echo ..\lib\harbour-11-b32.lib + >> _hbmk_b32_dll.tmp
@echo cw32.lib +                  >> _hbmk_b32_dll.tmp
@echo import32.lib,               >> _hbmk_b32_dll.tmp

ilink32 -Tpe -Gn @_hbmk_b32_dll.tmp

del %1.obj
del mainwin.obj
del %1.c
del *.tds

del _hbmk_b32_dll.tmp
