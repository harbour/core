@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include
echo -O2 -I..\include -mh -L..\lib\b16 > build.tmp
echo %1.c >> build.tmp
echo harbour.lib >> build.tmp
echo pp.lib >> build.tmp
echo common.lib >> build.tmp
echo terminal.lib >> build.tmp
echo macro.lib >> build.tmp
echo tools.lib >> build.tmp
echo dbfntx.lib >> build.tmp
echo dbfcdx.lib >> build.tmp
echo debug.lib >> build.tmp
echo runner.lib >> build.tmp
bcc @build.tmp
del build.tmp
del %1.c

