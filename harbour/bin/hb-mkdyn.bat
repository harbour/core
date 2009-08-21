@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" echo ! hb-mkdyn.bat Harbour build script requires Windows NT or upper.
if not "%OS%" == "Windows_NT" goto :EOF

setlocal

rem NOTE: .prg files have to be compiled with -n1
rem NOTE: .c   files have to be compiled with -DHB_DYNLIB

if "%HB_ARCHITECTURE%" == "" ( echo ! HB_ARCHITECTURE needs to be set. && goto END )
if "%HB_COMPILER%"     == "" ( echo ! HB_COMPILER needs to be set. && goto END )
if "%HB_BIN_INSTALL%"  == "" ( echo ! HB_BIN_INSTALL needs to be set. && goto END )
if "%HB_LIB_INSTALL%"  == "" ( echo ! HB_LIB_INSTALL needs to be set. && goto END )

set HB_DLL_VERSION=20
set HB_DLL_LIBS=source\common source\pp source\rtl source\macro source\lang source\codepage source\hbpcre source\hbzlib source\hbextern source\rdd source\rdd\dbfntx source\rdd\dbfnsx source\rdd\dbfcdx source\rdd\dbffpt source\rdd\hbsix source\rdd\hsx source\rdd\usrrdd source\rtl\gtcgi source\rtl\gtpca source\rtl\gtstd source\rtl\gtwvt source\rtl\gtgui
set HB_DLL_LIBS_WIN=source\rtl\gtwin
set HB_DLL_LIBS_ST=source\vm
set HB_DLL_LIBS_MT=source\vm\vmmt
set HB_DLL_LIBS_WATCOM=source\vm\maindllh
set HB_OBJ_EXT=_dyn.obj
set HB_OBJ_PREF=
set HB_OBJ_POST=

set _DST_NAME_POST=
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_bcc"      set _DST_NAME_POST=-bcc
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvc64"   set _DST_NAME_POST=-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_mingw64"  set _DST_NAME_POST=-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_pocc64"   set _DST_NAME_POST=-x64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64"  set _DST_NAME_POST=-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_msvcia64" set _DST_NAME_POST=-ia64
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_msvcarm"  set _DST_NAME_POST=-wce-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_mingwarm" set _DST_NAME_POST=-wce-arm
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "wce_poccarm"  set _DST_NAME_POST=-wce-arm

set _DST_NAME_ST=harbour-%HB_DLL_VERSION%%_DST_NAME_POST%
set _DST_NAME_MT=harbourmt-%HB_DLL_VERSION%%_DST_NAME_POST%

set _LIST_ST=%HB_BIN_INSTALL%\_hballst.txt
set _LIST_MT=%HB_BIN_INSTALL%\_hballmt.txt

if "%HB_ARCHITECTURE%" == "wce"      set HB_DLL_LIBS_WIN=
if not "%HB_COMPILER%" == "watcom"   set HB_DLL_LIBS_WATCOM=
if "%HB_COMPILER%"     == "mingw"    set HB_OBJ_EXT=.o
if "%HB_COMPILER%"     == "mingw64"  set HB_OBJ_EXT=.o
if "%HB_COMPILER%"     == "mingwarm" set HB_OBJ_EXT=.o
if "%HB_COMPILER%"     == "cygwin"   set HB_OBJ_EXT=.o

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

echo ! Platform %HB_ARCHITECTURE% / %HB_COMPILER% isn't supported.
goto END

:DO_MSVC

echo.> "%_LIST_ST%"
echo.> "%_LIST_MT%"

call :MAKE_LISTS

set _BIN_LINK=link
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_icc"     set _BIN_LINK=xilink
if "%HB_ARCHITECTURE%_%HB_COMPILER%" == "win_iccia64" set _BIN_LINK=xilink

if "%HB_ARCHITECTURE%" == "win" set _SYSLIBS=user32.lib ws2_32.lib advapi32.lib gdi32.lib
if "%HB_ARCHITECTURE%" == "wce" set _SYSLIBS=coredll.lib ws2.lib

echo ! Making %_DST_NAME_ST%.dll... && %_BIN_LINK% /nologo /dll /subsystem:console %HB_USER_DFLAGS% /out:"%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" @"%_LIST_ST%" %_SYSLIBS% %HB_DLLIBS%
echo ! Making %_DST_NAME_MT%.dll... && %_BIN_LINK% /nologo /dll /subsystem:console %HB_USER_DFLAGS% /out:"%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" @"%_LIST_MT%" %_SYSLIBS% %HB_DLLIBS%

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.exp"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.exp"

del "%_LIST_ST%"
del "%_LIST_MT%"

goto END

:DO_GCC

echo.> "%_LIST_ST%"
echo.> "%_LIST_MT%"

call :MAKE_LISTS

rem Can't do this with HB_OBJ_PREF/POST because cmd processor
rem gets confused about the closing paranthesis. [vszakats]
echo.> "%_LIST_ST%_"
for /f %%f in (%_LIST_ST%) do echo INPUT( %%f )>> "%_LIST_ST%_"
echo.> "%_LIST_MT%_"
for /f %%f in (%_LIST_MT%) do echo INPUT( %%f )>> "%_LIST_MT%_"

del "%_LIST_ST%"
del "%_LIST_MT%"

if "%HB_ARCHITECTURE%" == "win" set _SYSLIBS=-luser32 -lws2_32 -ladvapi32 -lgdi32
if "%HB_ARCHITECTURE%" == "wce" set _SYSLIBS=-lcoredll -lws2

echo ! Making %_DST_NAME_ST%.dll... && %HB_CCPREFIX%gcc -shared %HB_USER_DFLAGS% -o "%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" "%_LIST_ST%_" %HB_USER_LDFLAGS% %_SYSLIBS% %HB_DLLIBS% -Wl,--output-def,"%HB_BIN_INSTALL%\%_DST_NAME_ST%.def"
echo ! Making %_DST_NAME_MT%.dll... && %HB_CCPREFIX%gcc -shared %HB_USER_DFLAGS% -o "%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" "%_LIST_MT%_" %HB_USER_LDFLAGS% %_SYSLIBS% %HB_DLLIBS% -Wl,--output-def,"%HB_BIN_INSTALL%\%_DST_NAME_MT%.def"

rem ,--out-implib,"%HB_LIB_INSTALL%\lib%_DST_NAME_ST%.a"
rem ,--out-implib,"%HB_LIB_INSTALL%\lib%_DST_NAME_MT%.a"

del "%_LIST_ST%_"
del "%_LIST_MT%_"

goto END

:DO_BCC

if exist "%_LIST_ST%" del "%_LIST_ST%"
if exist "%_LIST_MT%" del "%_LIST_MT%"

set HB_OBJ_PREF=
set HB_OBJ_POST= +
call :MAKE_LISTS

echo. , "%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll",, cw32mt.lib import32.lib %HB_DLLIBS% >> "%_LIST_ST%"
echo. , "%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll",, cw32mt.lib import32.lib %HB_DLLIBS% >> "%_LIST_MT%"

echo ! Making %_DST_NAME_ST%.dll... && ilink32 -q -Gn -C -aa -Tpd -Gi -x %HB_USER_DFLAGS% c0d32.obj @%_LIST_ST%
echo ! Making %_DST_NAME_MT%.dll... && ilink32 -q -Gn -C -aa -Tpd -Gi -x %HB_USER_DFLAGS% c0d32.obj @%_LIST_MT%

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

del "%_LIST_ST%"
del "%_LIST_MT%"

goto END

:DO_WATCOM

echo.> "%_LIST_ST%"
echo.> "%_LIST_MT%"

set HB_OBJ_PREF=FILE '
set HB_OBJ_POST='
call :MAKE_LISTS

echo ! Making %_DST_NAME_ST%.dll... && wlink OP QUIET SYS NT_DLL %HB_USER_DFLAGS% OP IMPLIB NAME '%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll' @'%_LIST_ST%' LIB user32.lib, ws2_32.lib, advapi32.lib, gdi32.lib
echo ! Making %_DST_NAME_MT%.dll... && wlink OP QUIET SYS NT_DLL %HB_USER_DFLAGS% OP IMPLIB NAME '%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll' @'%_LIST_MT%' LIB user32.lib, ws2_32.lib, advapi32.lib, gdi32.lib

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"

del "%_LIST_ST%"
del "%_LIST_MT%"

goto END

:DO_POCC

echo.> "%_LIST_ST%"
echo.> "%_LIST_MT%"

call :MAKE_LISTS

if "%HB_ARCHITECTURE%" == "win" set _SYSLIBS=user32.lib ws2_32.lib advapi32.lib gdi32.lib
if "%HB_ARCHITECTURE%" == "wce" set _SYSLIBS=coredll.lib ws2.lib

echo ! Making %_DST_NAME_ST%.dll... && polink /nologo /dll %HB_USER_DFLAGS% /out:"%HB_BIN_INSTALL%\%_DST_NAME_ST%.dll" @%_LIST_ST% %_SYSLIBS% %HB_DLLIBS%
echo ! Making %_DST_NAME_MT%.dll... && polink /nologo /dll %HB_USER_DFLAGS% /out:"%HB_BIN_INSTALL%\%_DST_NAME_MT%.dll" @%_LIST_MT% %_SYSLIBS% %HB_DLLIBS%

if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_ST%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_MT%.lib" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.lib"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_ST%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_ST%.exp"
if exist "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" move /Y "%HB_BIN_INSTALL%\%_DST_NAME_MT%.exp" "%HB_LIB_INSTALL%\%_DST_NAME_MT%.exp"

del "%_LIST_ST%"
del "%_LIST_MT%"

goto END

:MAKE_LISTS

rem echo ! Making .dlls for %HB_ARCHITECTURE% / %HB_COMPILER%...

rem ; Extract neutral objects
for %%f in (%HB_DLL_LIBS% %HB_DLL_LIBS_WIN% %HB_DLL_LIBS_WATCOM%) do (
   if exist "%%f\%HB_OBJ_DIR%" (
      rem echo ! Processing directory: %%f\%HB_OBJ_DIR%
      dir /b "%%f\%HB_OBJ_DIR%\*%HB_OBJ_EXT%" > "%HB_BIN_INSTALL%\_hboraw.txt"
      for /F %%p in (%HB_BIN_INSTALL%\_hboraw.txt) do (
            echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> "%_LIST_ST%"
            echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> "%_LIST_MT%"
      )
      del "%HB_BIN_INSTALL%\_hboraw.txt"
   ) else ( echo ! Directory not found: %%f\%HB_OBJ_DIR% )
)

rem ; Extract ST objects
for %%f in (%HB_DLL_LIBS_ST%) do (
   if exist "%%f\%HB_OBJ_DIR%" (
      rem echo ! Processing directory: %%f\%HB_OBJ_DIR%
      dir /b "%%f\%HB_OBJ_DIR%\*%HB_OBJ_EXT%" > "%HB_BIN_INSTALL%\_hboraw.txt"
      for /F %%p in (%HB_BIN_INSTALL%\_hboraw.txt) do echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> "%_LIST_ST%"
      )
      del "%HB_BIN_INSTALL%\_hboraw.txt"
   ) else ( echo ! Directory not found: %%f\%HB_OBJ_DIR% )
)

rem ; Extract MT objects
for %%f in (%HB_DLL_LIBS_MT%) do (
   if exist "%%f\%HB_OBJ_DIR%" (
      rem echo ! Processing directory: %%f\%HB_OBJ_DIR%
      dir /b "%%f\%HB_OBJ_DIR%\*%HB_OBJ_EXT%" > "%HB_BIN_INSTALL%\_hboraw.txt"
      for /F %%p in (%HB_BIN_INSTALL%\_hboraw.txt) do echo %HB_OBJ_PREF%%%f\%HB_OBJ_DIR%\%%p%HB_OBJ_POST%>> "%_LIST_MT%"
      del "%HB_BIN_INSTALL%\_hboraw.txt"
   ) else ( echo ! Directory not found: %%f\%HB_OBJ_DIR% )
)

goto :EOF

:END

endlocal
