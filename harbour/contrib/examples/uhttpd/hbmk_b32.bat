@echo off
cls
rem
rem $Id: hbmk_b32.bat 9884 2008-11-09 19:37:16Z vszakats $
rem

SET UHTTP_INET_SUPPORT=no
SET UHTTP_INET_DEF=
SET UHTTP_INET_SOCKET=
SET UHTTP_GD_SUPPORT=yes
SET UHTTP_GD_DEF=
SET UHTTP_GD_LIBS=

:PARAM_CHECK
if "%1". == "--without-gd". goto PARAM_GD
if "%1". == "--with-inet".  goto PARAM_INET

GOTO GO_ON
:PARAM_GD
SET UHTTP_GD_SUPPORT=no
shift
goto PARAM_CHECK

:PARAM_INET
SET UHTTP_INET_SUPPORT=yes
shift
goto PARAM_CHECK

:GO_ON
echo.
echo.Building uHTTPD server
echo.
if %UHTTP_GD_SUPPORT%.==yes. echo.Lib GD support enabled
if %UHTTP_GD_SUPPORT%.==no.  echo.Lib GD support disabled
echo.
if %UHTTP_INET_SUPPORT%.==yes. echo.HB_INET socket
if %UHTTP_INET_SUPPORT%.==no.  echo.Mindaugas socket
echo.

if %UHTTP_GD_SUPPORT%.==yes. SET UHTTP_GD_DEF=-DGD_SUPPORT
if %UHTTP_GD_SUPPORT%.==yes. SET UHTTP_GD_LIBS=hbgd.lib bgd.lib

if %UHTTP_INET_SUPPORT%.==yes. SET UHTTP_INET_DEF=-DUSE_HB_INET
if %UHTTP_INET_SUPPORT%.==no.  SET UHTTP_INET_SOCKET=socket.c

..\..\..\bin\harbour uhttpd /n /es2 /w3 /i..\..\..\include %UHTTP_GD_DEF% %UHTTP_INET_DEF%
if errorlevel 1 goto DOERROR
bcc32 -O2 -tW -d -a8 -I..\..\..\include -L..\..\..\lib uhttpd.c %UHTTP_INET_SOCKET% hbdebug.lib hbvmmt.lib hbrtl.lib gtwvt.lib gtwin.lib gtgui.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbcpage.lib hbsix.lib hbcommon.lib hbpcre.lib hbhsx.lib hbzlib.lib xhb.lib hbct.lib cw32mt.lib %UHTTP_GD_LIBS%
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

SET UHTTP_INET_SUPPORT=
SET UHTTP_INET_DEF=
SET UHTTP_INET_SOCKET=
SET UHTTP_GD_SUPPORT=
SET UHTTP_GD_DEF=
SET UHTTP_GD_LIBS=
