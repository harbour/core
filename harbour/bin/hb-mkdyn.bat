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

if not "%OS%" == "Windows_NT" echo This Harbour build script requires Windows NT or upper.
if not "%OS%" == "Windows_NT" goto :EOF

setlocal

if "%HB_ARCHITECTURE%" == "" ( echo HB_ARCHITECTURE needs to be set. && goto END )
if "%HB_COMPILER%" == "" ( echo HB_COMPILER needs to be set. && goto END )
if "%HB_BIN_INSTALL%" == "" ( echo HB_BIN_INSTALL needs to be set. && goto END )
if "%HB_LIB_INSTALL%" == "" ( echo HB_LIB_INSTALL needs to be set. && goto END )

set HB_DLL_VERSION=20
set HB_DLL_LIBS=source\common source\pp source\rtl source\macro source\lang source\codepage source\hbpcre source\hbzlib source\hbextern source\rdd source\rdd\dbfntx source\rdd\dbfnsx source\rdd\dbfcdx source\rdd\dbffpt source\rdd\hbsix source\rdd\hsx source\rdd\usrrdd source\rtl\gtcgi source\rtl\gtpca source\rtl\gtstd source\rtl\gtwvt source\rtl\gtgui
set HB_DLL_LIBS_WIN=source\rtl\gtwin
set HB_DLL_LIBS_ST=source\vm
set HB_DLL_LIBS_MT=source\vm\vmmt
set HB_DLL_LIBS_WATCOM=source\vm\maindllh
set HB_OBJ_EXT=.obj
set HB_OBJ_DIR=obj\%HB_ARCHITECTURE%\%HB_COMPILER%%HB_BUILD_NAME%
set HB_OBJ_PREF=
set HB_OBJ_POST=

set _DST_NAME_ST=
set _DST_NAME_MT=
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"      set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"      set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64"  set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64"  set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc"     set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc"     set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_msvcarm"  set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_msvcarm"  set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc64"   set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc64"   set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcia64" set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcia64" set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_mingw"    set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_mingw"    set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_mingw64"  set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_mingw64"  set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_mingwarm" set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_mingwarm" set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_cygwin"   set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_cygwin"   set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_bcc"      set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-bcc
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_bcc"      set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-bcc
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_watcom"   set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_watcom"   set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc"     set _DST_NAME_ST=harbour-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc"     set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc64"   set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc64"   set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_poccarm"  set _DST_NAME_ST=harbour-%HB_DLL_VERSION%-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_poccarm"  set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%-arm

if "%_DST_NAME_ST%" == "" ( echo Harbour .dll creation isn't supported for this platform. && goto END )

if "%HB_ARCHITECTURE%" == "wce"      set HB_DLL_LIBS_WIN=
if not "%HB_COMPILER%" == "watcom"   set HB_DLL_LIBS_WATCOM=
if "%HB_COMPILER%"     == "mingw"    set HB_OBJ_EXT=.o
if "%HB_COMPILER%"     == "mingw64"  set HB_OBJ_EXT=.o
if "%HB_COMPILER%"     == "mingwarm" set HB_OBJ_EXT=.o
if "%HB_COMPILER%"     == "cygwin"   set HB_OBJ_EXT=.o

echo.> _hballst.txt
echo.> _hballmt.txt

if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"      goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64"  goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc"     goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc64"   goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcia64" goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_msvcarm"  goto DO_MSVC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_mingw"    goto DO_GCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_mingw64"  goto DO_GCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_mingwarm" goto DO_GCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_cygwin"   goto DO_GCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_bcc"      goto DO_BCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_watcom"   goto DO_WATCOM
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc"     goto DO_POCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc64"   goto DO_POCC
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_poccarm"  goto DO_POCC

echo Platform %HB_ARCHITECTURE% / %HB_COMPILER% isn't supported.
goto END

:DO_MSVC

call :MAKE_LISTS

set _BIN_LINK=link
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"     set _BIN_LINK=xilink
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64" set _BIN_LINK=xilink

if "%HB_ARCHITECTURE%" == "win" set _SYSLIBS=user32.lib ws2_32.lib advapi32.lib gdi32.lib
if "%HB_ARCHITECTURE%" == "wce" set _SYSLIBS=wininet.lib ws2.lib

echo Making %_DST_NAME_ST%.dll... && %_BIN_LINK% /nologo /dll /subsystem:console /out:"%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" @_hballst.txt %_SYSLIBS% %HB_DLLLIBS%
echo Making %_DST_NAME_MT%.dll... && %_BIN_LINK% /nologo /dll /subsystem:console /out:"%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" @_hballmt.txt %_SYSLIBS% %HB_DLLLIBS%

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.exp"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.exp"

goto END

:DO_GCC

call :MAKE_LISTS

setlocal enabledelayedexpansion
set _HBOST=
for /f %%f in (_hballst.txt) do set _HBOST=!_HBOST! %%f
set _HBOMT=
for /f %%f in (_hballmt.txt) do set _HBOMT=!_HBOMT! %%f

if "%HB_ARCHITECTURE%" == "win" set _SYSLIBS=-luser32 -lws2_32 -ladvapi32 -lgdi32
if "%HB_ARCHITECTURE%" == "wce" set _SYSLIBS=-lwininet -lws2

echo Making %_DST_NAME_ST%.dll... && %HB_CCPREFIX%gcc -shared -o "%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" %_HBOST% %HB_USER_LDFLAGS% %_SYSLIBS% %HB_DLLLIBS% -Wl,--output-def,"%HB_BIN_INSTALL%\%_DST_NAME_ST%.def"
echo Making %_DST_NAME_MT%.dll... && %HB_CCPREFIX%gcc -shared -o "%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" %_HBOMT% %HB_USER_LDFLAGS% %_SYSLIBS% %HB_DLLLIBS% -Wl,--output-def,"%HB_BIN_INSTALL%\%_DST_NAME_MT%.def"

rem ,--out-implib,"%HB_LIB_INSTALL%\lib%_DST_NAME_ST%.a"
rem ,--out-implib,"%HB_LIB_INSTALL%\lib%_DST_NAME_MT%.a"

goto END

:DO_BCC

echo. c0d32%HB_OBJ_EXT% +> _hballst.txt
echo. c0d32%HB_OBJ_EXT% +> _hballmt.txt

set HB_OBJ_PREF=
set HB_OBJ_POST= +
call :MAKE_LISTS

echo Making %_DST_NAME_ST%.dll... && ilink32 -q -Gn -C -aa -Tpd -Gi -x @_hballst.txt, "%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll",, cw32mt.lib import32.lib %HB_DLLLIBS%
echo Making %_DST_NAME_MT%.dll... && ilink32 -q -Gn -C -aa -Tpd -Gi -x @_hballmt.txt, "%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll",, cw32mt.lib import32.lib %HB_DLLLIBS%

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

goto END

:DO_WATCOM

set HB_OBJ_PREF=FILE '
set HB_OBJ_POST='
call :MAKE_LISTS

echo.> _hbsst.txt
for %%f in (_hballst.txt) do echo FILE '%%f'>> _hbsst.txt
echo.> _hbsmt.txt
for %%f in (_hballst.txt) do echo FILE '%%f'>> _hbsmt.txt

copy /b /y "%HB_LIB_INSTALL%\%HB_DLL_LIBS_ST%.lib" . > nul && wlib -q -b "%HB_DLL_LIBS_ST%.lib" - mainwin%HB_OBJ_EXT% - mainstd%HB_OBJ_EXT%
copy /b /y "%HB_LIB_INSTALL%\%HB_DLL_LIBS_MT%.lib" . > nul && wlib -q -b "%HB_DLL_LIBS_MT%.lib" - mainwin%HB_OBJ_EXT% - mainstd%HB_OBJ_EXT%

echo FILE '%HB_DLL_LIBS_ST%.lib'>> _hbsst.txt
echo FILE '%HB_DLL_LIBS_MT%.lib'>> _hbsmt.txt

echo Making %_DST_NAME_ST%.dll... && wlink OP QUIET SYS NT_DLL OP IMPLIB NAME '%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll' @_hballst.txt LIB user32.lib, ws2_32.lib, advapi32.lib, gdi32.lib
echo Making %_DST_NAME_MT%.dll... && wlink OP QUIET SYS NT_DLL OP IMPLIB NAME '%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll' @_hballmt.txt LIB user32.lib, ws2_32.lib, advapi32.lib, gdi32.lib

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

del %HB_DLL_LIBS_ST%.lib
del %HB_DLL_LIBS_MT%.lib

del _hbsst.txt
del _hbsmt.txt

goto END

:DO_POCC

call :MAKE_LISTS

if "%HB_ARCHITECTURE%" == "win" set _SYSLIBS=user32.lib ws2_32.lib advapi32.lib gdi32.lib
if "%HB_ARCHITECTURE%" == "wce" set _SYSLIBS=wininet.lib ws2.lib

echo Making %_DST_NAME_ST%.dll... && polink /nologo /dll /out:"%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" @_hballst.txt %_SYSLIBS% %HB_DLLLIBS%
echo Making %_DST_NAME_MT%.dll... && polink /nologo /dll /out:"%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" @_hballmt.txt %_SYSLIBS% %HB_DLLLIBS%

polib "%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" /out:"%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
polib "%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" /out:"%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

goto END

:MAKE_LISTS

echo Making .dlls for %HB_ARCHITECTURE% / %HB_COMPILER%...

rem ; Extract neutral objects
for %%f in (%HB_DLL_LIBS% %HB_DLL_LIBS_WIN%) do (
   if exist "%%f\%HB_OBJ_DIR%" (
      echo Processing directory: %%f\%HB_OBJ_DIR%
      dir /b "%%f\%HB_OBJ_DIR%\*_dyn%HB_OBJ_EXT%" > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "hbpp_dyn%HB_OBJ_EXT%" (
            echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> _hballst.txt
            echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> _hballmt.txt
         )
      )
      del _hboraw.txt
   ) else ( echo Directory not found: %%f\%HB_OBJ_DIR% )
)

rem ; Extract ST objects
for %%f in (%HB_DLL_LIBS_ST%) do (
   if exist "%%f\%HB_OBJ_DIR%" (
      echo Processing directory: %%f\%HB_OBJ_DIR%
      dir /b "%%f\%HB_OBJ_DIR%\*_dyn%HB_OBJ_EXT%" > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll_dyn%HB_OBJ_EXT%" (
         if not "%%p" == "maindllp_dyn%HB_OBJ_EXT%" (
            echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> _hballst.txt
         )
         )
      )
      del _hboraw.txt
   ) else ( echo Directory not found: %%f\%HB_OBJ_DIR% )
)

rem ; Extract MT objects
for %%f in (%HB_DLL_LIBS_MT%) do (
   if exist "%%f\%HB_OBJ_DIR%" (
      echo Processing directory: %%f\%HB_OBJ_DIR%
      dir /b "%%f\%HB_OBJ_DIR%\*_dyn%HB_OBJ_EXT%" > _hboraw.txt
      for /F %%p in (_hboraw.txt) do (
         if not "%%p" == "maindll_dyn%HB_OBJ_EXT%" (
         if not "%%p" == "maindllp_dyn%HB_OBJ_EXT%" (
            echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> _hballmt.txt
         )
         )
      )
      del _hboraw.txt
   ) else ( echo Directory not found: %%f\%HB_OBJ_DIR% )
)

goto :EOF

:END

rem del _hballst.txt
rem del _hballmt.txt

endlocal
