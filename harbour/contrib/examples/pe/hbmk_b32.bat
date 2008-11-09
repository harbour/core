@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /n /i..\..\..\include\ pe editorhi

bcc32 -O2 -I..\..\..\include -L..\..\..\lib pe.c editorhi.c editorlo.c hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib

del *.obj
del pe.c editorhi.c

pe
