@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n /i..\..\..\include\ trm_appn terminal

cl -nologo -O2 -W3 -I..\..\..\include trm_appn.c terminal.c /link /subsystem:windows /libpath:..\..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib gdi32.lib hbzlib.lib gtwvg.lib hbwin.lib xhb.lib

del *.obj
del trm_appn.c terminal.c

guestbk
