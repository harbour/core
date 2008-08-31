@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /w3 /q /n /km /i..\..\..\include test ttest

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\..\include test.c ttest.c ctest.c /link /subsystem:CONSOLE /LIBPATH:..\..\lib hbvm.lib hbrtl.lib gtwin.lib hbnulrdd.lib hbmacro.lib hbcommon.lib user32.lib hbct.lib hbbtree.lib

del *.obj
del test.c ttest.c
