@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include
echo -O2 -mh -I..\include -L..\lib > build.tmp
echo -e%1.exe %1.c >> build.tmp
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
bcc @build.tmp
del build.tmp
rem del %1.c

