@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include /gf
tlink32 -L..\lib\b32 c0x32.obj %1.obj hvm.obj,%1.exe,, common.lib vm.lib rtl.lib rdd.lib macro.lib debug.lib tools.lib pp.lib dbfntx.lib dbfcdx.lib import32.lib cw32mt.lib
del %1.obj

