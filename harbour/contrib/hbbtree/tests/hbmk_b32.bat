@echo off
rem
rem $Id$
rem

..\..\..\bin\harbour /w3 /q /n /km /i..\..\..\include test ttest

bcc32 -O2 -I..\..\..\include -L..\..\..\lib test.c ttest.c ctest.c hbvm.lib hbrtl.lib gtwin.lib hbnulrdd.lib hbmacro.lib hbcommon.lib hbbtree.lib hbct.lib

del *.obj
del test.c ttest.c
