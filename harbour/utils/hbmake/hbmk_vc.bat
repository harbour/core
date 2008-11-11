@echo off
rem
rem $Id$
rem

..\..\bin\harbour -w3 -q -n -km -l -i..\..\include hbmake

cl -nologo -O2 -W3 -I..\..\include hbmake.c hbmfrdln.c hbmgauge.c hbmlang.c /link /libpath:..\..\lib hbvm.lib hbrtl.lib gtwin.lib gtwvt.lib hbnulrdd.lib hbmacro.lib hbcommon.lib user32.lib

del *.obj
del hbmake.c
