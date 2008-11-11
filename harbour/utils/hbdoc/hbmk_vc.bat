@echo off
rem
rem $Id$
rem

..\..\bin\harbour -w3 -q -n -km -l -i..\..\include hbdoc ft_funcs genasc genchm genhpc genhtm genng genos2 genpdf1 genrtf gentrf

cl -nologo -O2 -W3 -I..\..\include hbdoc.c ft_funcs.c genasc.c genchm.c genhpc.c genhtm.c genng.c genos2.c genpdf1.c genrtf.c gentrf.c hbdfrdln.c /link /libpath:..\..\lib hbvm.lib hbrtl.lib gtwin.lib gtwvt.lib hbnulrdd.lib hbmacro.lib hbcommon.lib user32.lib

del *.obj
del hbdoc.c ft_funcs.c genasc.c genchm.c genhpc.c genhtm.c genng.c genos2.c genpdf1.c genrtf.c gentrf.c
