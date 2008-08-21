@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n hbmake   /i..\..\include
..\..\bin\harbour /n fclass1  /i..\..\include
..\..\bin\harbour /n ffile1   /i..\..\include
..\..\bin\harbour /n ft_funcs /i..\..\include
..\..\bin\harbour /n hbmutils /i..\..\include
..\..\bin\harbour /n pickarry /i..\..\include
..\..\bin\harbour /n pickfile /i..\..\include
..\..\bin\harbour /n prb_stak /i..\..\include
..\..\bin\harbour /n tmake    /i..\..\include

echo -O2 -I..\..\include -L..\..\lib > build.tmp

echo hbmake.c     >> build.tmp
echo fclass1.c    >> build.tmp
echo ffile1.c     >> build.tmp
echo ft_funcs.c   >> build.tmp
echo hbmutils.c   >> build.tmp
echo pickarry.c   >> build.tmp
echo pickfile.c   >> build.tmp
echo prb_stak.c   >> build.tmp
echo tmake.c      >> build.tmp

echo hbmfrdln.c   >> build.tmp
echo hbmgauge.c   >> build.tmp
echo hbmlang.c    >> build.tmp

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

del hbmake.c
del fclass1.c
del ffile1.c
del ft_funcs.c
del hbmutils.c
del pickarry.c
del pickfile.c
del prb_stak.c
del tmake.c
