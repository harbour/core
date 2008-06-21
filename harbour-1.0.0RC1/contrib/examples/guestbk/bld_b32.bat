@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n guestbk  /i..\..\..\include\
..\..\..\bin\harbour /n inifiles /i..\..\..\include\
..\..\..\bin\harbour /n testcgi  /i..\..\..\include\

echo -O2 -I..\..\..\include -L..\..\..\lib > build.tmp
echo -eguestbk.exe >> build.tmp
echo guestbk.c inifiles.c testcgi.c >> build.tmp
echo hbdebug.lib >> build.tmp
echo hbvm.lib >> build.tmp
echo hbrtl.lib >> build.tmp
echo gtwin.lib >> build.tmp
echo hblang.lib >> build.tmp
echo hbrdd.lib >> build.tmp
echo hbmacro.lib >> build.tmp
echo hbpp.lib >> build.tmp
echo rddntx.lib >> build.tmp
echo rddcdx.lib >> build.tmp
echo rddfpt.lib >> build.tmp
echo hbsix.lib >> build.tmp
echo hbcommon.lib >> build.tmp
bcc32 @build.tmp
del build.tmp

guestbk
