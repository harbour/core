@echo off
rem
rem $Id$
rem

..\bin\harbour %1 %2 %3 /n /i..\include
cl -Fd..\bin\harbour -w -Zi -TP -GZ -GA -I..\include %1.c /link /subsystem:CONSOLE ..\lib\vc\rtl.lib ..\lib\vc\common.lib ..\lib\vc\tools.lib ..\lib\vc\rdd.lib ..\lib\vc\nulsys.lib ..\lib\vc\dbfntx.lib ..\lib\vc\dbfcdx.lib ..\lib\vc\debug.lib ..\lib\vc\macro.lib ..\lib\vc\vm.lib
rem del %1.c

