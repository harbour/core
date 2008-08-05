@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n hbmake   /i..\..\include
..\..\bin\harbour /n checks   /i..\..\include
..\..\bin\harbour /n fclass1  /i..\..\include
..\..\bin\harbour /n ffile1   /i..\..\include
..\..\bin\harbour /n ft_funcs /i..\..\include
..\..\bin\harbour /n hbmutils /i..\..\include
..\..\bin\harbour /n pickarry /i..\..\include
..\..\bin\harbour /n pickfile /i..\..\include
..\..\bin\harbour /n prb_stak /i..\..\include
..\..\bin\harbour /n radios   /i..\..\include
..\..\bin\harbour /n tmake    /i..\..\include

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\include hbmake.c checks.c fclass1.c ffile1.c ft_funcs.c hbmutils.c pickarry.c pickfile.c prb_stak.c radios.c tmake.c hbmlang.c readline.c /link /subsystem:CONSOLE /LIBPATH:..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib winspool.lib

del *.obj

del hbmake.c
del checks.c
del fclass1.c
del ffile1.c
del ft_funcs.c
del hbmutils.c
del pickarry.c
del pickfile.c
del prb_stak.c
del radios.c
del tmake.c
