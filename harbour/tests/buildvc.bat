@echo off
rem
rem $Id$
rem

..\bin\harbour %1 /n /i..\include
cl -Fd..\bin\harbour -w -Zi -TP -GZ -GA -DDEBUG -DHARBOUR_USE_WIN_GTAPI -I..\include %1.c /link /subsystem:CONSOLE ..\lib\vc\harbour.lib ..\lib\vc\terminal.lib ..\lib\vc\hbtools.lib ..\lib\vc\dbfntx.lib ..\lib\vc\debug.lib
