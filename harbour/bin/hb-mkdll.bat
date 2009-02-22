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
rem NOTE: Borland tlib doesn't work if the source library path
rem       contains '.' char. (as of version 5.8.2)

if not "%OS%" == "Windows_NT" ( echo This script needs Windows NT or newer. && goto END )
if "%HB_ARCHITECTURE%" == "" ( echo HB_ARCHITECTURE needs to be set. && goto END )
if "%HB_COMPILER%" == "" ( echo HB_COMPILER needs to be set. && goto END )
if not "%HB_ARCHITECTURE%" == "win" goto END

set HB_DLL_VERSION=11
set HB_DLL_LIBS=hbcommon,hbpp,hbrtl,hbmacro,hblang,hbcpage,hbpcre,hbzlib,hbextern,hbrdd,rddntx,rddnsx,rddcdx,rddfpt,hbsix,hbhsx,hbusrrdd,gtcgi,gtpca,gtstd,gtwin,gtwvt,gtgui

if not "%HB_COMPILER%" == "msvc" goto NOT_MSVC

rem Generating Harbour .dll for MSVC

md _dll
cd _dll

rem ; Extract core objects
echo.> _hbocore.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo Processing library: %%f
   lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
   for /F %%p in (_hboraw.txt) do (
      lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p
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
   echo Processing library: %%f
   lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
   for /F %%p in (_hboraw.txt) do (
      if not "%%p" == "maindll.obj" (
      if not "%%p" == "maindllp.obj" (
         lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p
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
   echo Processing library: %%f
   lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
   for /F %%p in (_hboraw.txt) do (
      if not "%%p" == "maindll.obj" (
      if not "%%p" == "maindllp.obj" (
         lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p
         echo vmmt\%%p>> ..\_hbovmmt.txt
      )
      )
   )
   del _hboraw.txt
)
cd ..

link /dll /out:"%HB_BIN_INSTALL%\harbour-%HB_DLL_VERSION%-vc.dll"   @_hbocore.txt @_hbovm.txt   user32.lib wsock32.lib advapi32.lib gdi32.lib > nul
link /dll /out:"%HB_BIN_INSTALL%\harbourmt-%HB_DLL_VERSION%-vc.dll" @_hbocore.txt @_hbovmmt.txt user32.lib wsock32.lib advapi32.lib gdi32.lib > nul

rem ; Cleanup
for /F %%o in (_hbovm.txt) do (
   del %%o
)
del _hbovm.txt
rmdir vm

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

rem Generating Harbour .dll for Borland C/C++

md _dll
cd _dll

echo. c0d32.obj +> _hball.txt
echo. c0d32.obj +> _hballvm.txt

rem ; Extract core objects
echo.> _hbocore.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo Processing library: %%f
   tlib "%HB_LIB_INSTALL%\%%f.lib", _hboraw.txt > nul
   echo.> _hboraw2.txt
   for /F "tokens=1,2" %%f in (_hboraw.txt) do (
      if "%%g" == "size" (
         echo %%f.obj >> _hboraw2.txt
      )
   )
   del _hboraw.txt
   for /F %%p in (_hboraw2.txt) do (
      tlib "%HB_LIB_INSTALL%\%%f.lib" * %%p > nul
      echo %%p +>> _hball.txt
      echo %%p +>> _hballvm.txt
      echo %%p>> _hbocore.txt
   )
   del _hboraw2.txt
)

md vm
cd vm
set HB_DLL_LIBS=hbvm
rem ; Extract VM objects
echo.> ..\_hbovm.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo Processing library: %%f
   tlib "%HB_LIB_INSTALL%\%%f.lib", _hboraw.txt > nul
   echo.> _hboraw2.txt
   for /F "tokens=1,2" %%f in (_hboraw.txt) do (
      if "%%g" == "size" (
         echo %%f.obj >> _hboraw2.txt
      )
   )
   del _hboraw.txt
   for /F %%p in (_hboraw2.txt) do (
      tlib "%HB_LIB_INSTALL%\%%f.lib" * %%p > nul
      echo vm\%%p +>> ..\_hball.txt
      echo vm\%%p>> ..\_hbovm.txt
   )
   del _hboraw2.txt
)
cd ..

md vmmt
cd vmmt
set HB_DLL_LIBS=hbvmmt
rem ; Extract VM (MT) objects
echo.> ..\_hbovmmt.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo Processing library: %%f
   tlib "%HB_LIB_INSTALL%\%%f.lib", _hboraw.txt > nul
   echo.> _hboraw2.txt
   for /F "tokens=1,2" %%f in (_hboraw.txt) do (
      if "%%g" == "size" (
         echo %%f.obj >> _hboraw2.txt
      )
   )
   del _hboraw.txt
   for /F %%p in (_hboraw2.txt) do (
      tlib "%HB_LIB_INSTALL%\%%f.lib" * %%p > nul
      echo vmmt\%%p +>> ..\_hballvm.txt
      echo vmmt\%%p>> ..\_hbovmmt.txt
   )
   del _hboraw2.txt
)
cd ..

echo. , "%HB_BIN_INSTALL%\harbour-%HB_DLL_VERSION%-b32.dll"  ,, cw32mt.lib import32.lib >> _hball.txt
echo. , "%HB_BIN_INSTALL%\harbourvm-%HB_DLL_VERSION%-b32.dll",, cw32mt.lib import32.lib >> _hballvm.txt

ilink32 -Gn -C -aa -Tpd -Gi -x c0d32.obj @_hball.txt > nul
ilink32 -Gn -C -aa -Tpd -Gi -x c0d32.obj @_hballvm.txt > nul

del _hball.txt
del _hballvm.txt

rem ; Cleanup
for /F %%o in (_hbovm.txt) do (
   del %%o
)
del _hbovm.txt
rmdir vm

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

:NOT_BCC32

:END
