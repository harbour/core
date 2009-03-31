@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem ---------------------------------------------------------------

rem NOTE: .prg files have to be compiled with -n1
rem NOTE: .c   files have to be compiled with -DHB_DYNLIB

if not "%OS%" == "Windows_NT" ( echo This script needs Windows NT or newer. && goto END )
if "%HB_ARCHITECTURE%" == "" ( echo HB_ARCHITECTURE needs to be set. && goto END )
if "%HB_COMPILER%" == "" ( echo HB_COMPILER needs to be set. && goto END )

set HB_DLL_VERSION=11
set HB_DLL_LIBS=hbcommon hbpp hbrtl hbmacro hblang hbcpage hbpcre hbzlib hbextern hbrdd rddntx rddnsx rddcdx rddfpt hbsix hbhsx hbusrrdd gtcgi gtpca gtstd gtwin gtwvt gtgui hbmaindllh
set HB_DLL_LIBS_ST=hbvm
set HB_DLL_LIBS_MT=hbvmmt

if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"      goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64"  goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc"     goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc64"   goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcia64" goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcce"   goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_bcc"      goto DO_BCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_owatcom"  goto DO_OWATCOM
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc"     goto DO_POCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_poccce"   goto DO_POCC

echo Platform %HB_ARCHITECTURE% / %HB_COMPILER% isn't supported.
goto END

:DO_MSVC

echo Making .dlls for %HB_ARCHITECTURE% / %HB_COMPILER%...

md _dll
cd _dll

set _BIN_LIB=lib
set _BIN_LINK=link
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"     set _BIN_LIB=xilib
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"     set _BIN_LINK=xilink
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64" set _BIN_LIB=xilib
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64" set _BIN_LINK=xilink

rem ; Extract neutral objects
echo.> _hboneut.txt
for %%f in (%HB_DLL_LIBS%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
      echo Processing library: %%f
      %_BIN_LIB% "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         %_BIN_LIB% "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p /out:%%p
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
      %_BIN_LIB% "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
            %_BIN_LIB% "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p /out:%%p
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
      %_BIN_LIB% "%HB_LIB_INSTALL%\%%f.lib" /nologo /list > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
            %_BIN_LIB% "%HB_LIB_INSTALL%\%%f.lib" /nologo /extract:%%p /out:%%p
            echo _mt\%%p>> ..\_hbomt.txt
         )
         )
      )
      del _hboraw.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)
cd ..

if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"      set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"      set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64"  set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64"  set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc"     set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc"     set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcce"   set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcce"   set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc64"   set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc64"   set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcia64" set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcia64" set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-ia64

if not "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcce" set _SYSLIBS=user32.lib ws2_32.lib advapi32.lib gdi32.lib
if     "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcce" set _SYSLIBS=wininet.lib ws2.lib

echo Making %_DST_NAME_ST%.dll... && %_BIN_LINK% /nologo /dll /out:"%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" @_hboneut.txt @_hbost.txt %_SYSLIBS%
echo Making %_DST_NAME_MT%.dll... && %_BIN_LINK% /nologo /dll /out:"%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" @_hboneut.txt @_hbomt.txt %_SYSLIBS%

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.exp"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.exp"

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

:DO_BCC

echo Making .dlls for %HB_ARCHITECTURE% / %HB_COMPILER%...

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

set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-bcc
set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-bcc

echo. , "%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll",, cw32mt.lib import32.lib >> _hballst.txt
echo. , "%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll",, cw32mt.lib import32.lib >> _hballmt.txt

echo Making %_DST_NAME_ST%.dll... && ilink32 -q -Gn -C -aa -Tpd -Gi -x c0d32.obj @_hballst.txt
echo Making %_DST_NAME_MT%.dll... && ilink32 -q -Gn -C -aa -Tpd -Gi -x c0d32.obj @_hballmt.txt

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

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

:DO_OWATCOM

echo Making .dlls for %HB_ARCHITECTURE% / %HB_COMPILER%...

md _dll
cd _dll

echo.> _hbsst.txt
echo.> _hbsmt.txt
for %%f in (%HB_DLL_LIBS%) do (
   echo FILE '%HB_LIB_INSTALL%\%%f.lib'>> _hbsst.txt
   echo FILE '%HB_LIB_INSTALL%\%%f.lib'>> _hbsmt.txt
)

copy /b /y "%HB_LIB_INSTALL%\%HB_DLL_LIBS_ST%.lib" . > nul && wlib -q -b "%HB_DLL_LIBS_ST%.lib" - mainwin.obj
copy /b /y "%HB_LIB_INSTALL%\%HB_DLL_LIBS_MT%.lib" . > nul && wlib -q -b "%HB_DLL_LIBS_MT%.lib" - mainwin.obj

echo FILE '%HB_DLL_LIBS_ST%.lib'>> _hbsst.txt
echo FILE '%HB_DLL_LIBS_MT%.lib'>> _hbsmt.txt

set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%

echo Making %_DST_NAME_ST%.dll... && wlink OP QUIET SYS NT_DLL OP IMPLIB NAME '%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll' @_hbsst.txt LIB user32.lib, ws2_32.lib, advapi32.lib, gdi32.lib
echo Making %_DST_NAME_MT%.dll... && wlink OP QUIET SYS NT_DLL OP IMPLIB NAME '%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll' @_hbsmt.txt LIB user32.lib, ws2_32.lib, advapi32.lib, gdi32.lib

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

del %HB_DLL_LIBS_ST%.lib
del %HB_DLL_LIBS_MT%.lib

del _hbsst.txt
del _hbsmt.txt
cd ..
rmdir _dll

goto END

:DO_POCC

echo Making .dlls for %HB_ARCHITECTURE% / %HB_COMPILER%...

md _dll
cd _dll

rem ; Extract neutral objects
echo.> _hboneut.txt
for %%f in (%HB_DLL_LIBS%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
      echo Processing library: %%f
      polib "%HB_LIB_INSTALL%\%%f.lib" /list /explode >> _hboneut.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)

md _st
cd _st
rem ; Extract ST objects
echo.> ..\_hbost.txt
for %%f in (%HB_DLL_LIBS_ST%) do (
   if exist "%HB_LIB_INSTALL%\%%f.lib" (
      echo Processing library: %%f
      polib "%HB_LIB_INSTALL%\%%f.lib" /list /explode > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
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
      polib "%HB_LIB_INSTALL%\%%f.lib" /list /explode > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll.obj" (
         if not "%%p" == "maindllp.obj" (
            echo _mt\%%p>> ..\_hbomt.txt
         )
         )
      )
      del _hboraw.txt
   ) else ( echo Library not found: %HB_LIB_INSTALL%\%%f.lib )
)
cd ..

if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc"     set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc"     set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_poccce"   set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_poccce"   set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-arm

if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc"     set _SYSLIBS=user32.lib ws2_32.lib advapi32.lib gdi32.lib
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_poccce"   set _SYSLIBS=wininet.lib ws2.lib

echo Making %_DST_NAME_ST%.dll... && polink /nologo /dll /out:"%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" @_hboneut.txt @_hbost.txt %_SYSLIBS%
echo Making %_DST_NAME_MT%.dll... && polink /nologo /dll /out:"%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" @_hboneut.txt @_hbomt.txt %_SYSLIBS%

polib "%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" /out:"%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
polib "%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" /out:"%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

rem ; Cleanup
for /F %%o in (_hbost.txt) do ( del %%o )
if exist _st\maindll.obj  del _st\maindll.obj
if exist _st\maindllp.obj del _st\maindllp.obj
del _hbost.txt
rmdir _st

for /F %%o in (_hbomt.txt) do ( del %%o )
if exist _mt\maindll.obj  del _mt\maindll.obj
if exist _mt\maindllp.obj del _mt\maindllp.obj
del _hbomt.txt
rmdir _mt

for /F %%o in (_hboneut.txt) do ( del %%o )
del _hboneut.txt
cd ..
rmdir _dll

goto END

:END

endlocal
