@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n guestbk
..\..\bin\harbour /n ..\..\tests\inifiles
..\..\bin\harbour /n ..\..\tests\testcgi

echo -O2 -I..\..\include -L..\..\lib > build.tmp
echo -eguestbk.exe >> build.tmp
echo guestbk.c inifiles.c testcgi.c >> build.tmp
echo tools.lib >> build.tmp
echo debug.lib >> build.tmp
echo vm.lib >> build.tmp
echo rtl.lib >> build.tmp
echo rdd.lib >> build.tmp
echo macro.lib >> build.tmp
echo pp.lib >> build.tmp
echo dbfntx.lib >> build.tmp
echo dbfcdx.lib >> build.tmp
echo common.lib >> build.tmp
bcc32 @build.tmp
del build.tmp

guestbk
