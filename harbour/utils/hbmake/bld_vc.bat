@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n hbmake   /i..\..\include
..\..\bin\harbour /n ft_funcs /i..\..\include
..\..\bin\harbour /n hbmutils /i..\..\include
..\..\bin\harbour /n pickarry /i..\..\include
..\..\bin\harbour /n tmake    /i..\..\include

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\include hbmake.c ft_funcs.c hbmutils.c pickarry.c tmake.c hbmfrdln.c hbmgauge.c hbmlang.c  /link /subsystem:CONSOLE /LIBPATH:..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib winspool.lib

del *.obj

del hbmake.c
del ft_funcs.c
del hbmutils.c
del pickarry.c
del tmake.c
