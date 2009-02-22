@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem NOTE: .prg files have to be built with -n1
rem NOTE: .c files have to be built with -DHB_DYNLIB

if not "%OS%" == "Windows_NT" ( echo This script needs Windows NT or newer. && goto END )
if "%HB_ARCHITECTURE%" == "" ( echo HB_ARCHITECTURE needs to be set. && goto END )
if "%HB_COMPILER%" == "" ( echo HB_COMPILER needs to be set. && goto END )
if not "%HB_ARCHITECTURE%" == "win" goto END

set HB_DLL_VERSION=11

if not "%HB_COMPILER%" == "msvc" goto NOT_MSVC

rem Generating Harbour .dll

md _dll
cd _dll

set HB_DLL_LIBS=hbcommon,hbpp,hbrtl,hbmacro,hblang,hbcpage,hbpcre,hbzlib,hbextern,hbrdd,rddntx,rddnsx,rddcdx,rddfpt,hbsix,hbhsx,hbusrrdd,gtcgi,gtpca,gtstd,gtwin,gtwvt,gtgui
rem ; Extract core objects
echo.> _hbocore.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo %%f
   lib %HB_LIB_INSTALL%\%%f.lib /nologo /list > _hboraw.txt
   for /F %%p in (_hboraw.txt) do (
      lib %HB_LIB_INSTALL%\%%f.lib /nologo /extract:%%p
      echo %%p>> _hbocore.txt
   )
   del _hboraw.txt
)

md vm
cd vm
set HB_DLL_LIBS=hbvm
rem ; Extract VM objects
echo.> ..\_hbovm.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo %%f
   lib %HB_LIB_INSTALL%\%%f.lib /nologo /list > _hboraw.txt
   for /F %%p in (_hboraw.txt) do (
      if not "%%p" == "maindll.obj" (
      if not "%%p" == "maindllp.obj" (
         lib %HB_LIB_INSTALL%\%%f.lib /nologo /extract:%%p
         echo vm\%%p>> ..\_hbovm.txt
      )
      )
   )
   del _hboraw.txt
)
cd ..

md vmmt
cd vmmt
set HB_DLL_LIBS=hbvmmt
rem ; Extract VM (MT) objects
echo.> ..\_hbovmmt.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo %%f
   lib %HB_LIB_INSTALL%\%%f.lib /nologo /list > _hboraw.txt
   for /F %%p in (_hboraw.txt) do (
      if not "%%p" == "maindll.obj" (
      if not "%%p" == "maindllp.obj" (
         lib %HB_LIB_INSTALL%\%%f.lib /nologo /extract:%%p
         echo vmmt\%%p>> ..\_hbovmmt.txt
      )
      )
   )
   del _hboraw.txt
)
cd ..

link /dll /out:%HB_BIN_INSTALL%\harbour-%HB_DLL_VERSION%-vc.dll   @_hbocore.txt @_hbovm.txt   user32.lib wsock32.lib advapi32.lib gdi32.lib
link /dll /out:%HB_BIN_INSTALL%\harbourmt-%HB_DLL_VERSION%-vc.dll @_hbocore.txt @_hbovmmt.txt user32.lib wsock32.lib advapi32.lib gdi32.lib

for /F %%o in (_hbovm.txt) do (
   del %%o
)
del _hbovm.txt
rmdir vm

rem ; Cleanup
for /F %%o in (_hbovmmt.txt) do (
   del %%o
)
del _hbovmmt.txt
rmdir vmmt

for /F %%o in (_hbocore.txt) do (
   del %%o
)
del _hbocore.txt
cd ..
rmdir _dll

goto END

:NOT_MSVC

if not "%HB_COMPILER%" == "bcc32" goto NOT_BCC32

echo bcc32 not yet supported
rem ilink32 -Gn -C -aa -Tpd -Gi -L$(LIB_DIR) $(HB_USER_LDFLAGS) @&&! c0d32.obj $**, $@,, cw32mt$(RTLIBSUFFIX).lib import32.lib

goto END

:NOT_BCC32

:END
