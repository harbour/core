@echo off
rem
rem $Id$
rem

cl -nologo -O2 -W3 -I..\..\..\include pp.c hbppcomp.c hbppcore.c hbpptbl.c hbpragma.c /link /subsystem:console /libpath:..\..\..\lib hbcommon.lib

del *.obj

pp
