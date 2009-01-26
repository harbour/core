@echo off
cls
rem
rem $Id$
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
cl -nologo -O2 -W3 -I..\..\..\include uhttpd.c uhttpdc.c %UHTTP_INET_SOCKET% /link /subsystem:windows /libpath:..\..\..\lib hbcpage.lib hbdebug.lib hbvmmt.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib wsock32.lib advapi32.lib hbct.lib gdi32.lib hbwin.lib hbhsx.lib gtwvt.lib %UHTTP_GD_LIBS% xhb.lib
if errorlevel 1 goto DOERROR

:CLEAN
del *.obj
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
