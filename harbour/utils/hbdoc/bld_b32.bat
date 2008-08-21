@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n hbdoc    /i..\..\include
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

echo -O2 -I..\..\include -L..\..\lib > build.tmp

echo hbdoc.c      >> build.tmp
echo ft_funcs.c   >> build.tmp
echo genasc.c     >> build.tmp
echo genchm.c     >> build.tmp
echo genhpc.c     >> build.tmp
echo genhtm.c     >> build.tmp
echo genng.c      >> build.tmp
echo genos2.c     >> build.tmp
echo genpdf1.c    >> build.tmp
echo genrtf.c     >> build.tmp
echo gentrf.c     >> build.tmp
echo html.c       >> build.tmp
echo ng.c         >> build.tmp
echo os2.c        >> build.tmp
echo rtf.c        >> build.tmp
echo teeasc.c     >> build.tmp
echo troff.c      >> build.tmp

echo hbdebug.lib  >> build.tmp
echo hbvm.lib     >> build.tmp
echo hbrtl.lib    >> build.tmp
echo gtwin.lib    >> build.tmp
echo hblang.lib   >> build.tmp
echo hbrdd.lib    >> build.tmp
echo hbmacro.lib  >> build.tmp
echo hbpp.lib     >> build.tmp
echo rddntx.lib   >> build.tmp
echo rddcdx.lib   >> build.tmp
echo rddfpt.lib   >> build.tmp
echo hbsix.lib    >> build.tmp
echo hbcommon.lib >> build.tmp

bcc32 @build.tmp
del build.tmp

del *.obj

del hbdoc.c
del ft_funcs.c
del genasc.c
del genchm.c
del genhpc.c
del genhtm.c
del genng.c
del genos2.c
del genpdf1.c
del genrtf.c
del gentrf.c
del html.c
del ng.c
del os2.c
del rtf.c
del teeasc.c
del troff.c
