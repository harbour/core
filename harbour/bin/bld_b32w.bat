@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include
bcc32 -O2 -tW -I..\include -L..\lib\b32 -e%1.exe %1.c tools.lib debug.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
rem del %1.c

