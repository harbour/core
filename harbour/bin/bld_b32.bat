@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include
bcc32 -O2 -I..\include -L..\lib -e%1.exe %1.c tools.lib debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
rem del %1.c

