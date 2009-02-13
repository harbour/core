@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n /i..\..\..\include\ trm_appn terminal

bcc32 -O2 -tW -I..\..\..\include -L..\..\..\lib trm_appn.c terminal.c hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbzlib.lib gtwvg.lib hbwin.lib xhb.lib

del *.obj
del trm_appn.c terminal.c
