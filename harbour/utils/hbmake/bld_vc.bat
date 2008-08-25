@echo off
rem
rem $Id$
rem

..\..\bin\harbour /w3 /q /n /km /i..\..\include hbmake hbmutils pickarry

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\include hbmake.c hbmutils.c pickarry.c hbmfrdln.c hbmgauge.c hbmlang.c  /link /subsystem:CONSOLE /LIBPATH:..\..\lib hbvm.lib hbrtl.lib gtwin.lib hbnulrdd.lib hbmacro.lib hbcommon.lib user32.lib winspool.lib

del *.obj
del hbmake.c hbmutils.c pickarry.c
