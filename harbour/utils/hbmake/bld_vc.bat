@echo off
rem
rem $Id$
rem

..\..\bin\harbour /q /n /gc0 /km /i..\..\include hbmake ft_funcs hbmutils pickarry tmake

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\include hbmake.c ft_funcs.c hbmutils.c pickarry.c tmake.c hbmfrdln.c hbmgauge.c hbmlang.c  /link /subsystem:CONSOLE /LIBPATH:..\..\lib hbvm.lib hbrtl.lib gtwin.lib hbnulrdd.lib hbmacro.lib hbcommon.lib user32.lib winspool.lib

del *.obj
del hbmake.c ft_funcs.c hbmutils.c pickarry.c tmake.c
