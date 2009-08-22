@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
rem See COPYING for licensing terms.
rem
rem script to create import libs for various compilers
rem
rem This script requires:
rem    - Windows NT or upper
rem    - C compiler in PATH
rem    - HB_COMPILER and HB_LIB_INSTALL envvars set
rem    - HB_DIR_* envvars pointing to installed 3rd party packages
rem    - GNU sed (sed.exe) for some msvc and msvc64 targets
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" echo ! hb-mkimp.bat Harbour build script requires Windows NT or upper.
if not "%OS%" == "Windows_NT" goto END

if not "%HB_LIB_INSTALL%" == "" (

   echo ! Making import libs...

   if "%HB_COMPILER%" == "bcc" (

      if exist "%HB_DIR_ADS%\Redistribute\ace32.dll"    implib    "%HB_LIB_INSTALL%\ace32.lib"              "%HB_DIR_ADS%\Redistribute\ace32.dll"
      if exist "%HB_DIR_ADS%\ace32.dll"                 implib    "%HB_LIB_INSTALL%\ace32.lib"              "%HB_DIR_ADS%\ace32.dll"
      if exist "%HB_DIR_ADS%\32bit\ace32.dll"           implib    "%HB_LIB_INSTALL%\ace32.lib"              "%HB_DIR_ADS%\32bit\ace32.dll"
      if exist "%HB_DIR_ALLEGRO%\bin\alleg42.dll"       implib -a "%HB_LIB_INSTALL%\alleg.lib"              "%HB_DIR_ALLEGRO%\bin\alleg42.dll"
      if exist "%HB_DIR_APOLLO%\sde61.dll"              implib    "%HB_LIB_INSTALL%\sde61.lib"              "%HB_DIR_APOLLO%\sde61.dll"
      if exist "%HB_DIR_BLAT%\full\blat.dll"            implib -a "%HB_LIB_INSTALL%\blat.lib"               "%HB_DIR_BLAT%\full\blat.dll"
      if exist "%HB_DIR_CURL%\libcurl.dll"              implib -a "%HB_LIB_INSTALL%\libcurl.lib"            "%HB_DIR_CURL%\libcurl.dll"
      if exist "%HB_DIR_CURL%\bin\libcurl.dll"          implib -a "%HB_LIB_INSTALL%\libcurl.lib"            "%HB_DIR_CURL%\bin\libcurl.dll"
      if exist "%HB_DIR_FIREBIRD%\lib\fbclient_bor.lib" copy /b /y "%HB_DIR_FIREBIRD%\lib\fbclient_bor.lib" "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_DIR_FREEIMAGE%\Dist\FreeImage.dll"  implib    "%HB_LIB_INSTALL%\FreeImage.lib"          "%HB_DIR_FREEIMAGE%\Dist\FreeImage.dll"
      if exist "%HB_DIR_GD%\bin\bgd.dll"                implib    "%HB_LIB_INSTALL%\bgd.lib"                "%HB_DIR_GD%\bin\bgd.dll"
      if exist "%HB_DIR_LIBHARU%\libhpdf.dll"           implib    "%HB_LIB_INSTALL%\libhpdf.lib"            "%HB_DIR_LIBHARU%\libhpdf.dll"
      if exist "%HB_DIR_LIBHARU%\lib_dll\libhpdf.dll"   implib    "%HB_LIB_INSTALL%\libhpdf.lib"            "%HB_DIR_LIBHARU%\lib_dll\libhpdf.dll"
      if exist "%HB_DIR_MYSQL%\bin\libmySQL.dll"        implib    "%HB_LIB_INSTALL%\libmysql.lib"           "%HB_DIR_MYSQL%\bin\libmySQL.dll"
      if exist "%HB_DIR_OPENSSL%\out32dll\libeay32.dll" implib -a "%HB_LIB_INSTALL%\libeay32.lib"           "%HB_DIR_OPENSSL%\out32dll\libeay32.dll"
      if exist "%HB_DIR_OPENSSL%\dll\libeay32.dll"      implib -a "%HB_LIB_INSTALL%\libeay32.lib"           "%HB_DIR_OPENSSL%\dll\libeay32.dll"
      if exist "%HB_DIR_OPENSSL%\libeay32.dll"          implib -a "%HB_LIB_INSTALL%\libeay32.lib"           "%HB_DIR_OPENSSL%\libeay32.dll"
      if exist "%HB_DIR_OPENSSL%\out32dll\ssleay32.dll" implib -a "%HB_LIB_INSTALL%\ssleay32.lib"           "%HB_DIR_OPENSSL%\out32dll\ssleay32.dll"
      if exist "%HB_DIR_OPENSSL%\dll\ssleay32.dll"      implib -a "%HB_LIB_INSTALL%\ssleay32.lib"           "%HB_DIR_OPENSSL%\dll\ssleay32.dll"
      if exist "%HB_DIR_OPENSSL%\ssleay32.dll"          implib -a "%HB_LIB_INSTALL%\ssleay32.lib"           "%HB_DIR_OPENSSL%\ssleay32.dll"
      if exist "%HB_DIR_PGSQL%\lib\libpq.dll"           implib -a "%HB_LIB_INSTALL%\libpq.lib"              "%HB_DIR_PGSQL%\lib\libpq.dll"

      goto END
   )

   if "%HB_COMPILER%" == "msvc" (

      if exist "%HB_DIR_ADS%\Redistribute\ace32.lib"    copy /b /y "%HB_DIR_ADS%\Redistribute\ace32.lib"     "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_DIR_ADS%\ace32.lib"                 copy /b /y "%HB_DIR_ADS%\ace32.lib"                  "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_DIR_ADS%\32bit\ace32.lib"           copy /b /y "%HB_DIR_ADS%\32bit\ace32.lib"            "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_DIR_ALLEGRO%\lib\alleg.lib"         copy /b /y "%HB_DIR_ALLEGRO%\lib\alleg.lib"          "%HB_LIB_INSTALL%\alleg.lib"
      if exist "%HB_DIR_APOLLO%\sde61.dll"              call :P_MSVC_IMPLIB x86 "%HB_DIR_APOLLO%\sde61.dll"      "%HB_LIB_INSTALL%\sde61.lib"
      if exist "%HB_DIR_BLAT%\full\blat.lib"            copy /b /y "%HB_DIR_BLAT%\full\blat.lib"             "%HB_LIB_INSTALL%\blat.lib"
      if exist "%HB_DIR_CURL%\libcurl.dll"              call :P_MSVC_IMPLIB x86 "%HB_DIR_CURL%\libcurl.dll"      "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_DIR_CURL%\bin\libcurl.dll"          call :P_MSVC_IMPLIB x86 "%HB_DIR_CURL%\bin\libcurl.dll"  "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  copy /b /y "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"   "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_DIR_FREEIMAGE%\Dist\FreeImage.lib"  copy /b /y "%HB_DIR_FREEIMAGE%\Dist\FreeImage.lib"   "%HB_LIB_INSTALL%\FreeImage.lib"
      if exist "%HB_DIR_GD%\lib\bgd.lib"                copy /b /y "%HB_DIR_GD%\lib\bgd.lib"                 "%HB_LIB_INSTALL%\bgd.lib"
      if exist "%HB_DIR_LIBHARU%\libhpdf.lib"           copy /b /y "%HB_DIR_LIBHARU%\libhpdf.lib"            "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_DIR_LIBHARU%\lib_dll\libhpdf.lib"   copy /b /y "%HB_DIR_LIBHARU%\lib_dll\libhpdf.lib"    "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    copy /b /y "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"     "%HB_LIB_INSTALL%\libmySQL.lib"
      if exist "%HB_DIR_OPENSSL%\out32dll\libeay32.lib" copy /b /y "%HB_DIR_OPENSSL%\out32dll\libeay32.lib"  "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\dll\libeay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\dll\libeay32.lib"       "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\lib\libeay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\lib\libeay32.lib"       "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\libeay32.lib"          copy /b /y "%HB_DIR_OPENSSL%\libeay32.lib"           "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\out32dll\ssleay32.lib" copy /b /y "%HB_DIR_OPENSSL%\out32dll\ssleay32.lib"  "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_OPENSSL%\dll\ssleay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\dll\ssleay32.lib"       "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_OPENSSL%\lib\ssleay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\lib\ssleay32.lib"       "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_OPENSSL%\ssleay32.lib"          copy /b /y "%HB_DIR_OPENSSL%\ssleay32.lib"           "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_PGSQL%\bin\libpq.dll"           call :P_MSVC_IMPLIB x86 "%HB_DIR_PGSQL%\bin\libpq.dll"   "%HB_LIB_INSTALL%\libpq.lib"
      if exist "%HB_DIR_PGSQL%\lib\libpq.lib"           copy /b /y "%HB_DIR_PGSQL%\lib\libpq.lib"            "%HB_LIB_INSTALL%\libpq.lib"
      if exist "%HB_DIR_QT%\bin\QtCore4.dll"            call :P_MSVC_IMPLIB x86 "%HB_DIR_QT%\bin\QtCore4.dll"    "%HB_LIB_INSTALL%\QtCore4.lib"
      if exist "%HB_DIR_QT%\bin\QtGui4.dll"             call :P_MSVC_IMPLIB x86 "%HB_DIR_QT%\bin\QtGui4.dll"     "%HB_LIB_INSTALL%\QtGui4.lib"
      if exist "%HB_DIR_QT%\bin\QtNetwork4.dll"         call :P_MSVC_IMPLIB x86 "%HB_DIR_QT%\bin\QtNetwork4.dll" "%HB_LIB_INSTALL%\QtNetwork4.lib"
      if exist "%HB_DIR_QT%\bin\QtWebKit4.dll"          call :P_MSVC_IMPLIB x86 "%HB_DIR_QT%\bin\QtWebKit4.dll"  "%HB_LIB_INSTALL%\QtWebKit4.lib"

      goto END
   )

   if "%HB_COMPILER%" == "msvc64" (

      if exist "%HB_DIR_CURL%\libcurl.dll"              call :P_MSVC_IMPLIB x64 "%HB_DIR_CURL%\libcurl.dll"      "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_DIR_CURL%\bin\libcurl.dll"          call :P_MSVC_IMPLIB x64 "%HB_DIR_CURL%\bin\libcurl.dll"  "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  copy /b /y "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"   "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    copy /b /y "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"     "%HB_LIB_INSTALL%\libmySQL.lib"

      goto END
   )

   if "%HB_COMPILER%" == "mingw" (

      if exist "%HB_DIR_ADS%\Redistribute\ace32.lib"    copy /b /y "%HB_DIR_ADS%\Redistribute\ace32.lib"    "%HB_LIB_INSTALL%\libace32.a"
      if exist "%HB_DIR_ADS%\ace32.lib"                 copy /b /y "%HB_DIR_ADS%\ace32.lib"                 "%HB_LIB_INSTALL%\libace32.a"
      if exist "%HB_DIR_ADS%\32bit\ace32.lib"           copy /b /y "%HB_DIR_ADS%\32bit\ace32.lib"           "%HB_LIB_INSTALL%\libace32.a"
      if exist "%HB_DIR_ALLEGRO%\lib\alleg.lib"         copy /b /y "%HB_DIR_ALLEGRO%\lib\alleg.lib"         "%HB_LIB_INSTALL%\liballeg.a"
      if exist "%HB_DIR_BLAT%\full\blat.lib"            copy /b /y "%HB_DIR_BLAT%\full\blat.lib"            "%HB_LIB_INSTALL%\libblat.a"
      if exist "%HB_DIR_CURL%\lib\libcurl.a"            copy /b /y "%HB_DIR_CURL%\lib\libcurl.a"            "%HB_LIB_INSTALL%\libcurl.a"
      if exist "%HB_DIR_CURL%\lib\libcurldll.a"         copy /b /y "%HB_DIR_CURL%\lib\libcurldll.a"         "%HB_LIB_INSTALL%\libcurldll.a"
      if exist "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  copy /b /y "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  "%HB_LIB_INSTALL%\libfbclient.a"
      if exist "%HB_DIR_FREEIMAGE%\Dist\FreeImage.lib"  copy /b /y "%HB_DIR_FREEIMAGE%\Dist\FreeImage.lib"  "%HB_LIB_INSTALL%\libFreeImage.a"
      if exist "%HB_DIR_GD%\lib\bgd.lib"                copy /b /y "%HB_DIR_GD%\lib\bgd.lib"                "%HB_LIB_INSTALL%\libbgd.a"
      if exist "%HB_DIR_LIBHARU%\libhpdf.lib"           copy /b /y "%HB_DIR_LIBHARU%\libhpdf.lib"           "%HB_LIB_INSTALL%\liblibhpdf.a"
      if exist "%HB_DIR_LIBHARU%\lib_dll\libhpdf.lib"   copy /b /y "%HB_DIR_LIBHARU%\lib_dll\libhpdf.lib"   "%HB_LIB_INSTALL%\liblibhpdf.a"
      if exist "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    copy /b /y "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    "%HB_LIB_INSTALL%\liblibmysql.a"
      if exist "%HB_DIR_OPENSSL%\out32dll\libeay32.lib" copy /b /y "%HB_DIR_OPENSSL%\out32dll\libeay32.lib" "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_DIR_OPENSSL%\dll\libeay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\dll\libeay32.lib"      "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_DIR_OPENSSL%\lib\libeay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\lib\libeay32.lib"      "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_DIR_OPENSSL%\libeay32.lib"          copy /b /y "%HB_DIR_OPENSSL%\libeay32.lib"          "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_DIR_OPENSSL%\out32dll\ssleay32.lib" copy /b /y "%HB_DIR_OPENSSL%\out32dll\ssleay32.lib" "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_DIR_OPENSSL%\dll\ssleay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\dll\ssleay32.lib"      "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_DIR_OPENSSL%\lib\ssleay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\lib\ssleay32.lib"      "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_DIR_OPENSSL%\ssleay32.lib"          copy /b /y "%HB_DIR_OPENSSL%\ssleay32.lib"          "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_DIR_PGSQL%\lib\libpq.lib"           copy /b /y "%HB_DIR_PGSQL%\lib\libpq.lib"           "%HB_LIB_INSTALL%\liblibpq.a"
      if exist "%HB_DIR_QT%\lib\libQtCore4.a"           copy /b /y "%HB_DIR_QT%\lib\libQtCore4.a"           "%HB_LIB_INSTALL%\libQtCore4.a"
      if exist "%HB_DIR_QT%\lib\libQtGui4.a"            copy /b /y "%HB_DIR_QT%\lib\libQtGui4.a"            "%HB_LIB_INSTALL%\libQtGui4.a"
      if exist "%HB_DIR_QT%\lib\libQtNetwork4.a"        copy /b /y "%HB_DIR_QT%\lib\libQtNetwork4.a"        "%HB_LIB_INSTALL%\libQtNetwork4.a"
      if exist "%HB_DIR_QT%\lib\libQtWebKit4.a"         copy /b /y "%HB_DIR_QT%\lib\libQtWebKit4.a"         "%HB_LIB_INSTALL%\libQtWebKit4.a"

      goto END
   )

   if "%HB_COMPILER%" == "mingw64" (

      if exist "%HB_DIR_CURL%\lib\libcurl.a"            copy /b /y "%HB_DIR_CURL%\lib\libcurl.a"            "%HB_LIB_INSTALL%\libcurl.a"
      if exist "%HB_DIR_CURL%\lib\libcurldll.a"         copy /b /y "%HB_DIR_CURL%\lib\libcurldll.a"         "%HB_LIB_INSTALL%\libcurldll.a"
      if exist "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  copy /b /y "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  "%HB_LIB_INSTALL%\libfbclient.a"
      if exist "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    copy /b /y "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    "%HB_LIB_INSTALL%\liblibmysql.a"

      goto END
   )

   if "%HB_COMPILER%" == "pocc" (

      if exist "%HB_DIR_ADS%\Redistribute\ace32.lib"    copy /b /y "%HB_DIR_ADS%\Redistribute\ace32.lib"    "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_DIR_ADS%\ace32.lib"                 copy /b /y "%HB_DIR_ADS%\ace32.lib"                 "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_DIR_ADS%\32bit\ace32.lib"           copy /b /y "%HB_DIR_ADS%\32bit\ace32.lib"           "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_DIR_ALLEGRO%\lib\alleg.lib"         copy /b /y "%HB_DIR_ALLEGRO%\lib\alleg.lib"         "%HB_LIB_INSTALL%\alleg.lib"
      if exist "%HB_DIR_APOLLO%\sde61.dll"              polib "%HB_DIR_APOLLO%\sde61.dll"              /out:"%HB_LIB_INSTALL%\sde61.lib"
      if exist "%HB_DIR_BLAT%\full\blat.lib"            copy /b /y "%HB_DIR_BLAT%\full\blat.lib"            "%HB_LIB_INSTALL%\blat.lib"
      if exist "%HB_DIR_CURL%\libcurl.dll"              polib "%HB_DIR_CURL%\libcurl.dll"              /out:"%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_DIR_CURL%\bin\libcurl.dll"          polib "%HB_DIR_CURL%\bin\libcurl.dll"          /out:"%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  copy /b /y "%HB_DIR_FIREBIRD%\lib\fbclient_ms.lib"  "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_DIR_FREEIMAGE%\Dist\FreeImage.lib"  copy /b /y "%HB_DIR_FREEIMAGE%\Dist\FreeImage.lib"  "%HB_LIB_INSTALL%\FreeImage.lib"
      if exist "%HB_DIR_GD%\lib\bgd.lib"                copy /b /y "%HB_DIR_GD%\lib\bgd.lib"                "%HB_LIB_INSTALL%\bgd.lib"
      if exist "%HB_DIR_LIBHARU%\libhpdf.lib"           copy /b /y "%HB_DIR_LIBHARU%\libhpdf.lib"           "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_DIR_LIBHARU%\lib_dll\libhpdf.lib"   copy /b /y "%HB_DIR_LIBHARU%\lib_dll\libhpdf.lib"   "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    copy /b /y "%HB_DIR_MYSQL%\lib\opt\libmySQL.lib"    "%HB_LIB_INSTALL%\libmySQL.lib"
      if exist "%HB_DIR_OPENSSL%\out32dll\libeay32.lib" copy /b /y "%HB_DIR_OPENSSL%\out32dll\libeay32.lib" "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\dll\libeay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\dll\libeay32.lib"      "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\lib\libeay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\lib\libeay32.lib"      "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\libeay32.lib"          copy /b /y "%HB_DIR_OPENSSL%\libeay32.lib"          "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_DIR_OPENSSL%\out32dll\ssleay32.lib" copy /b /y "%HB_DIR_OPENSSL%\out32dll\ssleay32.lib" "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_OPENSSL%\dll\ssleay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\dll\ssleay32.lib"      "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_OPENSSL%\lib\ssleay32.lib"      copy /b /y "%HB_DIR_OPENSSL%\lib\ssleay32.lib"      "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_OPENSSL%\ssleay32.lib"          copy /b /y "%HB_DIR_OPENSSL%\ssleay32.lib"          "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_DIR_PGSQL%\lib\libpq.lib"           copy /b /y "%HB_DIR_PGSQL%\lib\libpq.lib"           "%HB_LIB_INSTALL%\libpq.lib"

      goto END
   )

   if "%HB_COMPILER%" == "watcom" (

      if exist "%HB_DIR_ADS%\Redistribute\ace32.dll"    wlib.exe -q -o="%HB_LIB_INSTALL%\ace32.lib"     "%HB_DIR_ADS%\Redistribute\ace32.dll"
      if exist "%HB_DIR_ADS%\ace32.dll"                 wlib.exe -q -o="%HB_LIB_INSTALL%\ace32.lib"     "%HB_DIR_ADS%\ace32.dll"
      if exist "%HB_DIR_ADS%\32bit\ace32.dll"           wlib.exe -q -o="%HB_LIB_INSTALL%\ace32.lib"     "%HB_DIR_ADS%\32bit\ace32.dll"
      if exist "%HB_DIR_ALLEGRO%\bin\alleg42.dll"       wlib.exe -q -o="%HB_LIB_INSTALL%\alleg.lib"     "%HB_DIR_ALLEGRO%\bin\alleg42.dll"
      if exist "%HB_DIR_APOLLO%\sde61.dll"              wlib.exe -q -o="%HB_LIB_INSTALL%\sde61.lib"     "%HB_DIR_APOLLO%\sde61.dll"
      if exist "%HB_DIR_BLAT%\full\blat.dll"            wlib.exe -q -o="%HB_LIB_INSTALL%\blat.lib"      "%HB_DIR_BLAT%\full\blat.dll"
      if exist "%HB_DIR_CURL%\libcurl.dll"              wlib.exe -q -o="%HB_LIB_INSTALL%\libcurl.lib"   "%HB_DIR_CURL%\libcurl.dll"
      if exist "%HB_DIR_CURL%\bin\libcurl.dll"          wlib.exe -q -o="%HB_LIB_INSTALL%\libcurl.lib"   "%HB_DIR_CURL%\bin\libcurl.dll"
      if exist "%HB_DIR_FIREBIRD%\bin\fbclient.dll"     wlib.exe -q -o="%HB_LIB_INSTALL%\fbclient.lib"  "%HB_DIR_FIREBIRD%\bin\fbclient.dll"
      if exist "%HB_DIR_FREEIMAGE%\Dist\FreeImage.dll"  wlib.exe -q -o="%HB_LIB_INSTALL%\FreeImage.lib" "%HB_DIR_FREEIMAGE%\Dist\FreeImage.dll"
      if exist "%HB_DIR_GD%\bin\bgd.dll"                wlib.exe -q -o="%HB_LIB_INSTALL%\bgd.lib"       "%HB_DIR_GD%\bin\bgd.dll"
      if exist "%HB_DIR_LIBHARU%\libhpdf.dll"           wlib.exe -q -o="%HB_LIB_INSTALL%\libhpdf.lib"   "%HB_DIR_LIBHARU%\libhpdf.dll"
      if exist "%HB_DIR_LIBHARU%\lib_dll\libhpdf.dll"   wlib.exe -q -o="%HB_LIB_INSTALL%\libhpdf.lib"   "%HB_DIR_LIBHARU%\lib_dll\libhpdf.dll"
      if exist "%HB_DIR_MYSQL%\bin\libmySQL.dll"        wlib.exe -q -o="%HB_LIB_INSTALL%\libmySQL.lib"  "%HB_DIR_MYSQL%\bin\libmySQL.dll"
      if exist "%HB_DIR_OPENSSL%\out32dll\libeay32.dll" wlib.exe -q -o="%HB_LIB_INSTALL%\libeay32.lib"  "%HB_DIR_OPENSSL%\out32dll\libeay32.dll"
      if exist "%HB_DIR_OPENSSL%\dll\libeay32.dll"      wlib.exe -q -o="%HB_LIB_INSTALL%\libeay32.lib"  "%HB_DIR_OPENSSL%\dll\libeay32.dll"
      if exist "%HB_DIR_OPENSSL%\libeay32.dll"          wlib.exe -q -o="%HB_LIB_INSTALL%\libeay32.lib"  "%HB_DIR_OPENSSL%\libeay32.dll"
      if exist "%HB_DIR_OPENSSL%\out32dll\ssleay32.dll" wlib.exe -q -o="%HB_LIB_INSTALL%\ssleay32.lib"  "%HB_DIR_OPENSSL%\out32dll\ssleay32.dll"
      if exist "%HB_DIR_OPENSSL%\dll\ssleay32.dll"      wlib.exe -q -o="%HB_LIB_INSTALL%\ssleay32.lib"  "%HB_DIR_OPENSSL%\dll\ssleay32.dll"
      if exist "%HB_DIR_OPENSSL%\ssleay32.dll"          wlib.exe -q -o="%HB_LIB_INSTALL%\ssleay32.lib"  "%HB_DIR_OPENSSL%\ssleay32.dll"
      if exist "%HB_DIR_PGSQL%\lib\libpq.dll"           wlib.exe -q -o="%HB_LIB_INSTALL%\libpq.lib"     "%HB_DIR_PGSQL%\lib\libpq.dll"

      goto END
   )
)

goto END

rem ---------------------------------------------------------------
rem These .dll to .lib conversions need GNU sed.exe in the path
rem ---------------------------------------------------------------

:P_MSVC_IMPLIB

   echo /[ \t]*ordinal hint/,/^^[ \t]*Summary/{> _hbtemp.sed
   echo  /^^[ \t]\+[0-9]\+/{>> _hbtemp.sed
   echo    s/^^[ \t]\+[0-9]\+[ \t]\+[0-9A-Fa-f]\+[ \t]\+[0-9A-Fa-f]\+[ \t]\+\(.*\)/\1/p>> _hbtemp.sed
   echo  }>> _hbtemp.sed
   echo }>> _hbtemp.sed
   dumpbin /exports "%2" > _dump.tmp
   echo LIBRARY "%~n2" > _temp.def
   echo EXPORTS >> _temp.def
   sed -nf _hbtemp.sed < _dump.tmp >> _temp.def
   lib /nologo /machine:%1 /def:_temp.def /out:"%3"
   del _dump.tmp
   del _temp.def
   del _hbtemp.sed
   rem ---------------------------------------------------------------

   goto END

:END
