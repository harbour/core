@echo off
rem
rem $Id: hbmk_b32.bat 9884 2008-11-09 19:37:16Z vszakats $
rem

rem NOTE: This sample program needs hbgd.lib from contrib/hbgd

..\..\..\bin\harbour uhttpd /n /i..\..\..\include
bcc32 -O2 -tW -d -a8 -I..\..\..\include -L..\..\..\lib uhttpd.c socket.c hbdebug.lib hbvmmt.lib hbrtl.lib gtwvt.lib gtwin.lib gtgui.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbcpage.lib hbsix.lib hbcommon.lib hbpcre.lib hbhsx.lib hbzlib.lib hbgd.lib bgd.lib xhb.lib hbct.lib cw32mt.lib

:CLEAN
del *.obj
del *.tds
del uhttpd.c

if not exist uhttpd.exe goto :EXIT
if not exist bgd.dll echo.ATTENTION! This program needs bgd.dll
echo.Build complete.
:EXIT
