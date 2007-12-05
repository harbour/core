@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour %1 /n /i..\..\include /i..\ %2
bcc32 -e%1.exe -O2 -d -I..\..\..\include -L..\..\..\lib\b32 %1.c debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib dbffpt.lib common.lib hbsix.lib hbole.lib

rem del %1.c
