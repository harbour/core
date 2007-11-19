@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n dbu.prg     /i..\..\..\include\
..\..\..\bin\harbour /n dbucopy.prg /i..\..\..\include\
..\..\..\bin\harbour /n dbuedit.prg /i..\..\..\include\
..\..\..\bin\harbour /n dbuhelp.prg /i..\..\..\include\
..\..\..\bin\harbour /n dbuindx.prg /i..\..\..\include\
..\..\..\bin\harbour /n dbunet.prg  /i..\..\..\include\
..\..\..\bin\harbour /n dbustru.prg /i..\..\..\include\
..\..\..\bin\harbour /n dbuutil.prg /i..\..\..\include\
..\..\..\bin\harbour /n dbuview.prg /i..\..\..\include\

echo -O2 -I..\..\..\include -L..\..\..\lib > build.tmp
echo -edbu.exe >> build.tmp
echo dbu.c     >> build.tmp
echo dbucopy.c >> build.tmp
echo dbuedit.c >> build.tmp
echo dbuhelp.c >> build.tmp
echo dbuindx.c >> build.tmp
echo dbunet.c  >> build.tmp
echo dbustru.c >> build.tmp
echo dbuutil.c >> build.tmp
echo dbuview.c >> build.tmp
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

dbu
