rem 
rem $Id$
rem 

@echo off

..\..\bin\harbour %1 /n /i..\..\include
cl -Fd..\..\bin\harbour -w -Zi -TP -GZ -GA -DDEBUG -DHARBOUR_USE_GTAPI -DHARBOUR_USE_WIN_GTAPI -I..\..\include %1.c /link /subsystem:CONSOLE ..\..\obj\symbols.obj ..\..\libs\vc\harbour.lib ..\..\libs\vc\terminal.lib ..\..\libs\vc\hbtools.lib ..\..\libs\vc\dbfntx.lib ..\..\libs\vc\debug.lib
