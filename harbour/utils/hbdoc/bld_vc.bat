@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n hbdoc    /i..\..\include
..\..\bin\harbour /n fclass1  /i..\..\include
..\..\bin\harbour /n ffile1   /i..\..\include
..\..\bin\harbour /n ft_funcs /i..\..\include
..\..\bin\harbour /n genasc   /i..\..\include
..\..\bin\harbour /n genchm   /i..\..\include
..\..\bin\harbour /n genhpc   /i..\..\include
..\..\bin\harbour /n genhtm   /i..\..\include
..\..\bin\harbour /n genng    /i..\..\include
..\..\bin\harbour /n genos2   /i..\..\include
..\..\bin\harbour /n genpdf1  /i..\..\include
..\..\bin\harbour /n genrtf   /i..\..\include
..\..\bin\harbour /n gentrf   /i..\..\include
..\..\bin\harbour /n html     /i..\..\include
..\..\bin\harbour /n ng       /i..\..\include
..\..\bin\harbour /n os2      /i..\..\include
..\..\bin\harbour /n rtf      /i..\..\include
..\..\bin\harbour /n teeasc   /i..\..\include
..\..\bin\harbour /n troff    /i..\..\include

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\include hbdoc.c fclass1.c ffile1.c ft_funcs.c genasc.c genchm.c genhpc.c genhtm.c genng.c genos2.c genpdf1.c genrtf.c gentrf.c html.c ng.c os2.c rtf.c teeasc.c troff.c /link /subsystem:CONSOLE /LIBPATH:..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib winspool.lib
