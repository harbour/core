@echo off
cls
rem
rem $Id: hbmk_b32.bat 9884 2008-11-09 19:37:16Z vszakats $
rem

SET UHTTP_GD_SUPPORT=yes
SET UHTTP_GD_DEF=
SET UHTTP_GD_LIBS=

if "%1". == "--without-gd". SET UHTTP_GD_SUPPORT=no

echo.
echo.Building uHTTPD server
echo.
if %UHTTP_GD_SUPPORT%.==yes. echo.Lib GD support enabled
if %UHTTP_GD_SUPPORT%.==no.  echo.Lib GD support disabled
echo.
if %UHTTP_GD_SUPPORT%.==yes. SET UHTTP_GD_DEF=-DGD_SUPPORT
if %UHTTP_GD_SUPPORT%.==yes. SET UHTTP_GD_LIBS=hbgd.lib bgd.lib

..\..\..\bin\harbour uhttpd /n /es2 /w3 /i..\..\..\include %UHTTP_GD_DEF%
if errorlevel 1 goto DOERROR
bcc32 -O2 -tW -d -a8 -I..\..\..\include -L..\..\..\lib uhttpd.c socket.c hbdebug.lib hbvmmt.lib hbrtl.lib gtwvt.lib gtwin.lib gtgui.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbcpage.lib hbsix.lib hbcommon.lib hbpcre.lib hbhsx.lib hbzlib.lib xhb.lib hbct.lib cw32mt.lib %UHTTP_GD_LIBS%
if errorlevel 1 goto DOERROR

:CLEAN
del *.obj
del *.tds
del uhttpd.c

if not exist uhttpd.exe goto :EXIT
if %UHTTP_GD_SUPPORT%.==no. goto BUILD_OK
if not exist bgd.dll goto NOBGD

:BUILD_OK
echo.Build complete.
goto EXIT

:DOERROR
echo.Build error
goto EXIT

:NOBGD
echo.ATTENTION! This program needs bgd.dll
echo.Please download it from:
echo.http://www.libgd.org/releases/gd-latest-win32.zip
:EXIT

SET UHTTP_GD_SUPPORT=
SET UHTTP_GD_DEF=
SET UHTTP_GD_LIBS=
