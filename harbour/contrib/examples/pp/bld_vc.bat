@echo off
rem
rem $Id$
rem

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\..\include pp.c hbppcomp.c hbppcore.c hbpptbl.c hbpragma.c /link /subsystem:CONSOLE /LIBPATH:..\..\..\lib hbcommon.lib
del *.obj

pp
