@echo off
rem
rem $Id$
rem

bcc32 -O2 -I..\..\..\include -L..\..\..\lib pp.c hbppcomp.c hbppcore.c hbpptbl.c hbpragma.c hbcommon.lib

del *.obj

pp
