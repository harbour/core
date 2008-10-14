@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n guestbk  /i..\..\..\include\
..\..\..\bin\harbour /n inifiles /i..\..\..\include\
..\..\..\bin\harbour /n testcgi  /i..\..\..\include\

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -W3 -I..\..\..\include guestbk.c inifiles.c testcgi.c /link /subsystem:CONSOLE /LIBPATH:..\..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib winspool.lib

del *.obj

del guestbk.c
del inifiles.c
del testcgi.c

guestbk
