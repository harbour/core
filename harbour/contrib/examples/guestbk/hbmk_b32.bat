@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n /i..\..\..\include\ guestbk inifiles testcgi

bcc32 -O2 -I..\..\..\include -L..\..\..\lib guestbk.c inifiles.c testcgi.c hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib

del *.obj
del guestbk.c inifiles.c testcgi.c

guestbk
