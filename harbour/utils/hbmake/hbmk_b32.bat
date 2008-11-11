@echo off
rem
rem $Id$
rem

..\..\bin\harbour -w3 -q -n -km -l -i..\..\include hbmake

bcc32 -O2 -I..\..\include -L..\..\lib hbmake.c hbmfrdln.c hbmgauge.c hbmlang.c hbvm.lib hbrtl.lib gtwin.lib gtwvt.lib hbnulrdd.lib hbmacro.lib hbcommon.lib

del *.obj
del hbmake.c
