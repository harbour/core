@echo off
rem
rem $Id$
rem

rem NOTE: This sample program needs hbnf.lib from contrib/hbnf

..\..\..\bin\harbour hscript /n /i..\..\..\include
cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -TP -W3 -I..\..\..\include hscript.c /link /subsystem:CONSOLE /LIBPATH:..\..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib hbnf.lib user32.lib winspool.lib
del *.obj
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
