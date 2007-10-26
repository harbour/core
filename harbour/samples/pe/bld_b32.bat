@echo off
rem
rem $Id$
rem

..\..\bin\harbour /n pe       /i..\..\include\
..\..\bin\harbour /n editorhi /i..\..\include\

echo -O2 -I..\..\include -L..\..\lib > build.tmp
echo -epe.exe >> build.tmp
echo pe.c editorhi.c editorlo.c >> build.tmp
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
bcc32 @build.tmp
del build.tmp

pe
