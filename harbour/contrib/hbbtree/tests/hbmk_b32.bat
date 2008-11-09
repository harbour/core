@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /w3 /q /n /km /i..\..\..\include test ttest

echo -O2 -I..\..\..\include -L..\..\..\lib > build.tmp

echo test.c       >> build.tmp
echo ttest.c      >> build.tmp
echo ctest.c      >> build.tmp
echo hbvm.lib     >> build.tmp
echo hbrtl.lib    >> build.tmp
echo gtwin.lib    >> build.tmp
echo hbnulrdd.lib >> build.tmp
echo hbmacro.lib  >> build.tmp
echo hbcommon.lib >> build.tmp
echo hbbtree.lib  >> build.tmp
echo hbct.lib     >> build.tmp

bcc32 @build.tmp
del build.tmp

del *.obj
del test.c ttest.c
