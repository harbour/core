@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include
bcc32 %1.c -e%1.exe -O2 -I..\include -L..\lib\b32 -5 -tW common.lib vm.lib rtl.lib rdd.lib macro.lib debug.lib tools.lib pp.lib dbfntx.lib dbfcdx.lib
rem del %1.c
