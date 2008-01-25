@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour %1 /n /i..\..\include /i..\ %2
bcc32 -e%1.exe -O2 -d -I..\..\..\include -L..\..\..\lib\b32 %1.c hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbcommon.lib hbsix.lib hbole.lib

rem del %1.c
