@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include
cl -Fd..\bin\harbour -w -Zi -TP -GZ -GA -I..\include %1.c /link /subsystem:CONSOLE ..\lib\tools.lib ..\lib\debug.lib ..\lib\vm.lib ..\lib\rtl.lib ..\lib\gtwin.lib ..\lib\lang.lib ..\lib\rdd.lib ..\lib\macro.lib ..\lib\pp.lib ..\lib\dbfntx.lib ..\lib\dbfcdx.lib ..\lib\common.lib user32.lib
rem del %1.c

