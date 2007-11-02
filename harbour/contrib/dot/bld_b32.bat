@echo off
rem
rem $Id$
rem

..\..\bin\harbour /l /gc0 /n pp       /i..\..\include\

echo -O2 -I..\..\include -L..\..\lib > build.tmp
echo -edot.exe >> build.tmp
echo pp.c >> build.tmp
echo debug.lib >> build.tmp
echo vm.lib >> build.tmp
echo rtl.lib >> build.tmp
echo gtwin.lib >> build.tmp
echo lang.lib >> build.tmp
echo rdd.lib >> build.tmp
echo macro.lib >> build.tmp
echo pp.lib >> build.tmp
echo dbfntx.lib >> build.tmp
echo dbfcdx.lib >> build.tmp
echo dbffpt.lib >> build.tmp
echo hbsix.lib >> build.tmp
echo common.lib >> build.tmp
echo codepage.lib >> build.tmp
echo hbpcre.lib >> build.tmp
echo hsx.lib >> build.tmp
bcc32 @build.tmp
del build.tmp

