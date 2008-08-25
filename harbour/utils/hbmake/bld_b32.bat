@echo off
rem
rem $Id$
rem

..\..\bin\harbour /q /n /gc0 /km /i..\..\include hbmake hbmutils pickarry

echo -O2 -I..\..\include -L..\..\lib > build.tmp

echo hbmake.c     >> build.tmp
echo hbmutils.c   >> build.tmp
echo pickarry.c   >> build.tmp

echo hbmfrdln.c   >> build.tmp
echo hbmgauge.c   >> build.tmp
echo hbmlang.c    >> build.tmp

echo hbvm.lib     >> build.tmp
echo hbrtl.lib    >> build.tmp
echo gtwin.lib    >> build.tmp
echo hbnulrdd.lib >> build.tmp
echo hbmacro.lib  >> build.tmp
echo hbcommon.lib >> build.tmp

bcc32 @build.tmp
del build.tmp

del *.obj
del hbmake.c hbmutils.c pickarry.c
