@echo off
rem
rem $Id$
rem

rem NOTE: Experimental

..\bin\harbour %1 %2 %3 /n /i..\include /gf
tlink32 -L..\lib c0x32.obj %1.obj hvm.obj,%1.exe,, import32.lib cw32mt.lib tools.lib debug.lib vm.lib rtl.lib gtwin.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
del %1.obj

