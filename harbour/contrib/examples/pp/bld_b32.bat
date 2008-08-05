@echo off
rem
rem $Id$
rem

echo -O2 -I..\..\..\include -L..\..\..\lib > build.tmp
echo pp.c hbppcomp.c hbppcore.c hbpptbl.c hbpragma.c >> build.tmp
echo hbcommon.lib >> build.tmp
bcc32 @build.tmp
del build.tmp

pp
