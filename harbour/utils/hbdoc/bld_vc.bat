@echo off
rem
rem $Id$
rem

..\..\bin\harbour /w3 /q /n /km /i..\..\include hbdoc ft_funcs genasc genchm genhpc genhtm genng genos2 genpdf1 genrtf gentrf

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\include hbdoc.c ft_funcs.c genasc.c genchm.c genhpc.c genhtm.c genng.c genos2.c genpdf1.c genrtf.c gentrf.c hbdfrdln.c /link /subsystem:CONSOLE /LIBPATH:..\..\lib hbvm.lib hbrtl.lib gtwin.lib hbnulrdd.lib hbmacro.lib hbcommon.lib user32.lib

del *.obj
del hbdoc.c ft_funcs.c genasc.c genchm.c genhpc.c genhtm.c genng.c genos2.c genpdf1.c genrtf.c gentrf.c
