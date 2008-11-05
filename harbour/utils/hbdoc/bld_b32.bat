@echo off
rem
rem $Id$
rem

..\..\bin\harbour -w3 -q -n -km -l -i..\..\include hbdoc ft_funcs genasc genchm genhpc genhtm genng genos2 genpdf1 genrtf gentrf

bcc32 -O2 -I..\..\include -L..\..\lib hbdoc.c ft_funcs.c genasc.c genchm.c genhpc.c genhtm.c genng.c genos2.c genpdf1.c genrtf.c gentrf.c hbdfrdln.c hbvm.lib hbrtl.lib gtwin.lib hbnulrdd.lib hbmacro.lib hbcommon.lib

del *.obj
del hbdoc.c ft_funcs.c genasc.c genchm.c genhpc.c genhtm.c genng.c genos2.c genpdf1.c genrtf.c gentrf.c
