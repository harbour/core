@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem NOTE: .prg files have to be compiled with -n1
rem NOTE: .c   files have to be compiled with -DHB_DYNLIB

if not "%OS%" == "Windows_NT" ( echo This script needs Windows NT or newer. && goto END )
if "%HB_ARCHITECTURE%" == "" ( echo HB_ARCHITECTURE needs to be set. && goto END )
if "%HB_COMPILER%" == "" ( echo HB_COMPILER needs to be set. && goto END )
if not "%HB_ARCHITECTURE%" == "win" goto END

set HB_DLL_VERSION=11
set HB_DLL_LIBS=hbcommon hbpp hbrtl hbmacro hblang hbcpage hbpcre hbzlib hbextern hbrdd rddntx rddnsx rddcdx rddfpt hbsix hbhsx hbusrrdd gtcgi gtpca gtstd gtwin gtwvt gtgui
set HB_DLL_LIBS_ST=hbvm
set HB_DLL_LIBS_MT=hbvmmt

if not "%HB_COMPILER%" == "msvc" goto NOT_MSVC

echo Making .dlls for %HB_COMPILER%...

md _dll
cd _dll

rem ; Extract neutral objects
echo.> _hboneut.txt
for %%f in (%HB_DLL_LIBS%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
      echo Processing library: %%f
      lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p
         echo %%p>> _hboneut.txt
      )
      del _hboraw.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)

md _st
cd _st
rem ; Extract ST objects
echo.> ..\_hbost.txt
for %%f in (%HB_DLL_LIBS_ST%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
      echo Processing library: %%f
      lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
            lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p
            echo _st\%%p>> ..\_hbost.txt
         )
         )
      )
      del _hboraw.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)
cd ..

md _mt
cd _mt
rem ; Extract MT objects
echo.> ..\_hbomt.txt
for %%f in (%HB_DLL_LIBS_MT%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
      echo Processing library: %%f
      lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
            lib "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p
            echo _mt\%%p>> ..\_hbomt.txt
         )
         )
      )
      del _hboraw.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)
cd ..

set _DST_NAME_ST=%HB_BIN_INSTALL%\harbour-%HB_DLL_VERSION%-vc.dll
set _DST_NAME_MT=%HB_BIN_INSTALL%\harbourmt-%HB_DLL_VERSION%-vc.dll

echo Making %_DST_NAME_ST%... && link /dll /out:"%_DST_NAME_ST%" @_hboneut.txt @_hbost.txt user32.lib wsock32.lib advapi32.lib gdi32.lib > nul
echo Making %_DST_NAME_MT%... && link /dll /out:"%_DST_NAME_MT%" @_hboneut.txt @_hbomt.txt user32.lib wsock32.lib advapi32.lib gdi32.lib > nul

rem ; Cleanup
for /F %%o in (_hbost.txt) do ( del %%o )
del _hbost.txt
rmdir _st

for /F %%o in (_hbomt.txt) do ( del %%o )
del _hbomt.txt
rmdir _mt

for /F %%o in (_hboneut.txt) do ( del %%o )
del _hboneut.txt
cd ..
rmdir _dll

goto END

:NOT_MSVC

if not "%HB_COMPILER%" == "bcc32" goto NOT_BCC32

echo Making .dlls for %HB_COMPILER%...

md _dll
cd _dll

echo. c0d32.obj +> _hballst.txt
echo. c0d32.obj +> _hballmt.txt

rem ; Extract neutral objects
echo.> _hboneut.txt
for %%f in (%HB_DLL_LIBS%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
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
         echo %%p +>> _hballst.txt
         echo %%p +>> _hballmt.txt
         echo %%p>> _hboneut.txt
      )
      del _hboraw2.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)

md _st
cd _st
rem ; Extract ST objects
echo.> ..\_hbost.txt
for %%f in (%HB_DLL_LIBS_ST%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
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
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
            tlib "%HB_LIB_INSTALL%\%%f.lib" * %%p > nul
            echo _st\%%p +>> ..\_hballst.txt
            echo _st\%%p>> ..\_hbost.txt
         )
         )
      )
      del _hboraw2.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)
cd ..

md _mt
cd _mt
rem ; Extract MT objects
echo.> ..\_hbomt.txt
for %%f in (%HB_DLL_LIBS_MT%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
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
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
            tlib "%HB_LIB_INSTALL%\%%f.lib" * %%p > nul
            echo _mt\%%p +>> ..\_hballmt.txt
            echo _mt\%%p>> ..\_hbomt.txt
         )
         )
      )
      del _hboraw2.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)
cd ..

set _DST_NAME_ST=%HB_BIN_INSTALL%\harbour-%HB_DLL_VERSION%-b32.dll
set _DST_NAME_MT=%HB_BIN_INSTALL%\harbourmt-%HB_DLL_VERSION%-b32.dll

echo. , "%_DST_NAME_ST%",, cw32mt.lib import32.lib >> _hballst.txt
echo. , "%_DST_NAME_ST%",, cw32mt.lib import32.lib >> _hballmt.txt

echo Making %_DST_NAME_ST%... && ilink32 -Gn -C -aa -Tpd -Gi -x c0d32.obj @_hballst.txt > nul
echo Making %_DST_NAME_MT%... && ilink32 -Gn -C -aa -Tpd -Gi -x c0d32.obj @_hballmt.txt > nul

del _hballst.txt
del _hballmt.txt

rem ; Cleanup
for /F %%o in (_hbost.txt) do ( del %%o )
del _hbost.txt
rmdir _st

for /F %%o in (_hbomt.txt) do ( del %%o )
del _hbomt.txt
rmdir _mt

for /F %%o in (_hboneut.txt) do ( del %%o )
del _hboneut.txt
cd ..
rmdir _dll

goto END

:NOT_BCC32

:END
