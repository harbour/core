@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n /i..\..\..\include\ guestbk inifiles testcgi

cl -nologo -O2 -W3 -I..\..\..\include guestbk.c inifiles.c testcgi.c /link /subsystem:console /libpath:..\..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib winspool.lib

del *.obj
del guestbk.c inifiles.c testcgi.c

guestbk
