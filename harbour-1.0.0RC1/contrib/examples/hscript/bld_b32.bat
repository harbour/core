@echo off
rem
rem $Id$
rem

rem NOTE: This sample program needs hbnf.lib from contrib/hbnf

..\..\..\bin\harbour hscript /n /i..\..\..\include
bcc32 -O2 -I..\..\..\include -L..\..\..\lib -ehscript.exe hscript.c hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbcpage.lib hbsix.lib hbcommon.lib hbpcre.lib hbhsx.lib hbnf.lib hbzlib.lib
del hscript.c

if not exist hscript.exe goto :EXIT

hscript dir.hs      > dir.htm
hscript hello.hs    > hello.htm
hscript multiply.hs > multiply.htm
hscript ugly.hs     > ugly.htm

echo Ready to go!
echo.
echo Try:
echo.
echo start dir.htm
echo start hello.htm
echo start multiply.htm
echo start ugly.htm

:EXIT
