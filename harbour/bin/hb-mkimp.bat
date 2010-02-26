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
rem    - HB_WITH_* envvars pointing to installed 3rd party _headers_
rem    - GNU Sed (sed.exe) for some msvc and msvc64 targets
rem ---------------------------------------------------------------

if not "%OS%" == "Windows_NT" echo ! hb-mkimp.bat Harbour build script requires Windows NT or upper.
if not "%OS%" == "Windows_NT" goto END

if not "%HB_LIB_INSTALL%" == "" (

   echo ! Making import libs...

   if "%HB_COMPILER%" == "bcc" (

      if exist "%HB_WITH_ADS%\Redistribute\ace32.dll"       implib    "%HB_LIB_INSTALL%\ace32.lib"                  "%HB_WITH_ADS%\Redistribute\ace32.dll"
      if exist "%HB_WITH_ADS%\ace32.dll"                    implib    "%HB_LIB_INSTALL%\ace32.lib"                  "%HB_WITH_ADS%\ace32.dll"
      if exist "%HB_WITH_ADS%\32bit\ace32.dll"              implib    "%HB_LIB_INSTALL%\ace32.lib"                  "%HB_WITH_ADS%\32bit\ace32.dll"
      if exist "%HB_WITH_ALLEGRO%\..\bin\alleg42.dll"       implib -a "%HB_LIB_INSTALL%\alleg.lib"                  "%HB_WITH_ALLEGRO%\..\bin\alleg42.dll"
      if exist "%HB_WITH_APOLLO%\..\sde61.dll"              implib    "%HB_LIB_INSTALL%\sde61.lib"                  "%HB_WITH_APOLLO%\..\sde61.dll"
      if exist "%HB_WITH_APOLLO%\..\sde7.dll"               implib    "%HB_LIB_INSTALL%\sde7.lib"                   "%HB_WITH_APOLLO%\..\sde7.dll"
      if exist "%HB_WITH_BLAT%\..\blat.dll"                 implib -a "%HB_LIB_INSTALL%\blat.lib"                   "%HB_WITH_BLAT%\..\blat.dll"
      if exist "%HB_WITH_CAIRO%\..\..\bin\libcairo-2.dll"   implib -a "%HB_LIB_INSTALL%\cairo.lib"                  "%HB_WITH_CAIRO%\..\..\bin\libcairo-2.dll"
      if exist "%HB_WITH_CURL%\..\libcurl.dll"              implib -a "%HB_LIB_INSTALL%\libcurl.lib"                "%HB_WITH_CURL%\..\libcurl.dll"
      if exist "%HB_WITH_CURL%\..\bin\libcurl.dll"          implib -a "%HB_LIB_INSTALL%\libcurl.lib"                "%HB_WITH_CURL%\..\bin\libcurl.dll"
      if exist "%HB_WITH_FIREBIRD%\..\lib\fbclient_bor.lib" copy /b /y "%HB_WITH_FIREBIRD%\..\lib\fbclient_bor.lib" "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.dll"  implib    "%HB_LIB_INSTALL%\FreeImage.lib"              "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.dll"
      if exist "%HB_WITH_GD%\..\bin\bgd.dll"                implib    "%HB_LIB_INSTALL%\bgd.lib"                    "%HB_WITH_GD%\..\bin\bgd.dll"
      if exist "%HB_WITH_LIBHARU%\..\libhpdf.dll"           implib    "%HB_LIB_INSTALL%\libhpdf.lib"                "%HB_WITH_LIBHARU%\..\libhpdf.dll"
      if exist "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.dll"   implib    "%HB_LIB_INSTALL%\libhpdf.lib"                "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.dll"
      if exist "%HB_WITH_MYSQL%\..\bin\libmySQL.dll"        implib    "%HB_LIB_INSTALL%\libmysql.lib"               "%HB_WITH_MYSQL%\..\bin\libmySQL.dll"
      if exist "%HB_WITH_OCILIB%\..\lib32\ociliba.dll"      implib    "%HB_LIB_INSTALL%\ociliba.lib"                "%HB_WITH_OCILIB%\..\lib32\ociliba.dll"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibm.dll"      implib    "%HB_LIB_INSTALL%\ocilibm.lib"                "%HB_WITH_OCILIB%\..\lib32\ocilibm.dll"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibw.dll"      implib    "%HB_LIB_INSTALL%\ocilibw.lib"                "%HB_WITH_OCILIB%\..\lib32\ocilibw.dll"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\libeay32.dll" implib -a "%HB_LIB_INSTALL%\libeay32.lib"               "%HB_WITH_OPENSSL%\..\out32dll\libeay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.dll" implib -a "%HB_LIB_INSTALL%\ssleay32.lib"               "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\dll\libeay32.dll"      implib -a "%HB_LIB_INSTALL%\libeay32.lib"               "%HB_WITH_OPENSSL%\..\dll\libeay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\dll\ssleay32.dll"      implib -a "%HB_LIB_INSTALL%\ssleay32.lib"               "%HB_WITH_OPENSSL%\..\dll\ssleay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\libeay32.dll"          implib -a "%HB_LIB_INSTALL%\libeay32.lib"               "%HB_WITH_OPENSSL%\..\libeay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\ssleay32.dll"          implib -a "%HB_LIB_INSTALL%\ssleay32.lib"               "%HB_WITH_OPENSSL%\..\ssleay32.dll"
      if exist "%HB_WITH_PGSQL%\..\lib\libpq.dll"           implib -a "%HB_LIB_INSTALL%\libpq.lib"                  "%HB_WITH_PGSQL%\..\lib\libpq.dll"

      goto END
   )

   if "%HB_COMPILER%" == "msvc" (

      if exist "%HB_WITH_ADS%\Redistribute\ace32.lib"       copy /b /y "%HB_WITH_ADS%\Redistribute\ace32.lib"           "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_WITH_ADS%\ace32.lib"                    copy /b /y "%HB_WITH_ADS%\ace32.lib"                        "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_WITH_ADS%\32bit\ace32.lib"              copy /b /y "%HB_WITH_ADS%\32bit\ace32.lib"                  "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_WITH_ALLEGRO%\..\lib\alleg.lib"         copy /b /y "%HB_WITH_ALLEGRO%\..\lib\alleg.lib"             "%HB_LIB_INSTALL%\alleg.lib"
      if exist "%HB_WITH_APOLLO%\..\sde61.dll"              call :P_MSVC_IMPLIB x86 "%HB_WITH_APOLLO%\..\sde61.dll"     "%HB_LIB_INSTALL%\sde61.lib"
      if exist "%HB_WITH_APOLLO%\..\sde7.dll"               call :P_MSVC_IMPLIB x86 "%HB_WITH_APOLLO%\..\sde7.dll"      "%HB_LIB_INSTALL%\sde7.lib"
      if exist "%HB_WITH_BLAT%\..\blat.lib"                 copy /b /y "%HB_WITH_BLAT%\..\blat.lib"                     "%HB_LIB_INSTALL%\blat.lib"
      if exist "%HB_WITH_CAIRO%\..\..\lib\cairo.lib"        copy /b /y "%HB_WITH_CAIRO%\..\..\lib\cairo.lib"            "%HB_LIB_INSTALL%\cairo.lib"
      if exist "%HB_WITH_CURL%\..\libcurl.dll"              call :P_MSVC_IMPLIB x86 "%HB_WITH_CURL%\..\libcurl.dll"     "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_WITH_CURL%\..\bin\libcurl.dll"          call :P_MSVC_IMPLIB x86 "%HB_WITH_CURL%\..\bin\libcurl.dll" "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"  copy /b /y "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"      "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.lib"  copy /b /y "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.lib"      "%HB_LIB_INSTALL%\FreeImage.lib"
      if exist "%HB_WITH_GD%\..\lib\bgd.lib"                copy /b /y "%HB_WITH_GD%\..\lib\bgd.lib"                    "%HB_LIB_INSTALL%\bgd.lib"
      if exist "%HB_WITH_LIBHARU%\..\libhpdf.lib"           copy /b /y "%HB_WITH_LIBHARU%\..\libhpdf.lib"               "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.lib"   copy /b /y "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.lib"       "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"    copy /b /y "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"        "%HB_LIB_INSTALL%\libmySQL.lib"
      if exist "%HB_WITH_OCILIB%\..\lib32\ociliba.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib32\ociliba.lib"          "%HB_LIB_INSTALL%\ociliba.lib"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibm.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib32\ocilibm.lib"          "%HB_LIB_INSTALL%\ocilibm.lib"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibw.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib32\ocilibw.lib"          "%HB_LIB_INSTALL%\ocilibw.lib"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\libeay32.lib" copy /b /y "%HB_WITH_OPENSSL%\..\out32dll\libeay32.lib"     "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.lib" copy /b /y "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.lib"     "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\dll\libeay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\dll\libeay32.lib"          "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\dll\ssleay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\dll\ssleay32.lib"          "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\lib\libeay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\lib\libeay32.lib"          "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\lib\ssleay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\lib\ssleay32.lib"          "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\libeay32.lib"          copy /b /y "%HB_WITH_OPENSSL%\..\libeay32.lib"              "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\ssleay32.lib"          copy /b /y "%HB_WITH_OPENSSL%\..\ssleay32.lib"              "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_PGSQL%\..\bin\libpq.dll"           call :P_MSVC_IMPLIB x86 "%HB_WITH_PGSQL%\..\bin\libpq.dll"  "%HB_LIB_INSTALL%\libpq.lib"
      if exist "%HB_WITH_PGSQL%\..\lib\libpq.lib"           copy /b /y "%HB_WITH_PGSQL%\..\lib\libpq.lib"               "%HB_LIB_INSTALL%\libpq.lib"

      goto END
   )

   if "%HB_COMPILER%" == "msvc64" (

      if exist "%HB_WITH_CURL%\..\libcurl.dll"              call :P_MSVC_IMPLIB x64 "%HB_WITH_CURL%\..\libcurl.dll"     "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_WITH_CURL%\..\bin\libcurl.dll"          call :P_MSVC_IMPLIB x64 "%HB_WITH_CURL%\..\bin\libcurl.dll" "%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"  copy /b /y "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"      "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"    copy /b /y "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"        "%HB_LIB_INSTALL%\libmySQL.lib"
      if exist "%HB_WITH_OCILIB%\..\lib64\ociliba.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib64\ociliba.lib"          "%HB_LIB_INSTALL%\ociliba.lib"
      if exist "%HB_WITH_OCILIB%\..\lib64\ocilibm.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib64\ocilibm.lib"          "%HB_LIB_INSTALL%\ocilibm.lib"
      if exist "%HB_WITH_OCILIB%\..\lib64\ocilibw.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib64\ocilibw.lib"          "%HB_LIB_INSTALL%\ocilibw.lib"

      goto END
   )

   if "%HB_COMPILER%" == "mingw" (

      if exist "%HB_WITH_ADS%\Redistribute\ace32.lib"       copy /b /y "%HB_WITH_ADS%\Redistribute\ace32.lib"       "%HB_LIB_INSTALL%\libace32.a"
      if exist "%HB_WITH_ADS%\ace32.lib"                    copy /b /y "%HB_WITH_ADS%\ace32.lib"                    "%HB_LIB_INSTALL%\libace32.a"
      if exist "%HB_WITH_ADS%\32bit\ace32.lib"              copy /b /y "%HB_WITH_ADS%\32bit\ace32.lib"              "%HB_LIB_INSTALL%\libace32.a"
      if exist "%HB_WITH_ALLEGRO%\..\lib\alleg.lib"         copy /b /y "%HB_WITH_ALLEGRO%\..\lib\alleg.lib"         "%HB_LIB_INSTALL%\liballeg.a"
      if exist "%HB_WITH_BLAT%\..\blat.lib"                 copy /b /y "%HB_WITH_BLAT%\..\blat.lib"                 "%HB_LIB_INSTALL%\libblat.a"
      if exist "%HB_WITH_CAIRO%\..\..\lib\libcairo.dll.a"   copy /b /y "%HB_WITH_CAIRO%\..\..\lib\libcairo.dll.a"   "%HB_LIB_INSTALL%\libcairo.a"
      if exist "%HB_WITH_CURL%\..\lib\libcurl.a"            copy /b /y "%HB_WITH_CURL%\..\lib\libcurl.a"            "%HB_LIB_INSTALL%\libcurl.a"
      if exist "%HB_WITH_CURL%\..\lib\libcurldll.a"         copy /b /y "%HB_WITH_CURL%\..\lib\libcurldll.a"         "%HB_LIB_INSTALL%\libcurldll.a"
      if exist "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"  copy /b /y "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"  "%HB_LIB_INSTALL%\libfbclient.a"
      if exist "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.lib"  copy /b /y "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.lib"  "%HB_LIB_INSTALL%\libFreeImage.a"
      if exist "%HB_WITH_GD%\..\lib\bgd.lib"                copy /b /y "%HB_WITH_GD%\..\lib\bgd.lib"                "%HB_LIB_INSTALL%\libbgd.a"
      if exist "%HB_WITH_LIBHARU%\..\libhpdf.lib"           copy /b /y "%HB_WITH_LIBHARU%\..\libhpdf.lib"           "%HB_LIB_INSTALL%\liblibhpdf.a"
      if exist "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.lib"   copy /b /y "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.lib"   "%HB_LIB_INSTALL%\liblibhpdf.a"
      if exist "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"    copy /b /y "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"    "%HB_LIB_INSTALL%\liblibmysql.a"
      if exist "%HB_WITH_OCILIB%\..\lib32\libociliba.a"     copy /b /y "%HB_WITH_OCILIB%\..\lib32\libociliba.a"     "%HB_LIB_INSTALL%\libociliba.a"
      if exist "%HB_WITH_OCILIB%\..\lib32\libocilibm.a"     copy /b /y "%HB_WITH_OCILIB%\..\lib32\libocilibm.a"     "%HB_LIB_INSTALL%\libocilibm.a"
      if exist "%HB_WITH_OCILIB%\..\lib32\libocilibw.a"     copy /b /y "%HB_WITH_OCILIB%\..\lib32\libocilibw.a"     "%HB_LIB_INSTALL%\libocilibw.a"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\libeay32.lib" copy /b /y "%HB_WITH_OPENSSL%\..\out32dll\libeay32.lib" "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.lib" copy /b /y "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.lib" "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_WITH_OPENSSL%\..\dll\libeay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\dll\libeay32.lib"      "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_WITH_OPENSSL%\..\dll\ssleay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\dll\ssleay32.lib"      "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_WITH_OPENSSL%\..\lib\libeay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\lib\libeay32.lib"      "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_WITH_OPENSSL%\..\lib\ssleay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\lib\ssleay32.lib"      "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_WITH_OPENSSL%\..\libeay32.lib"          copy /b /y "%HB_WITH_OPENSSL%\..\libeay32.lib"          "%HB_LIB_INSTALL%\liblibeay32.a"
      if exist "%HB_WITH_OPENSSL%\..\ssleay32.lib"          copy /b /y "%HB_WITH_OPENSSL%\..\ssleay32.lib"          "%HB_LIB_INSTALL%\libssleay32.a"
      if exist "%HB_WITH_PGSQL%\..\lib\libpq.lib"           copy /b /y "%HB_WITH_PGSQL%\..\lib\libpq.lib"           "%HB_LIB_INSTALL%\liblibpq.a"

      goto END
   )

   if "%HB_COMPILER%" == "mingw64" (

      if exist "%HB_WITH_CURL%\..\lib\libcurl.a"            copy /b /y "%HB_WITH_CURL%\..\lib\libcurl.a"            "%HB_LIB_INSTALL%\libcurl.a"
      if exist "%HB_WITH_CURL%\..\lib\libcurldll.a"         copy /b /y "%HB_WITH_CURL%\..\lib\libcurldll.a"         "%HB_LIB_INSTALL%\libcurldll.a"
      if exist "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"  copy /b /y "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"  "%HB_LIB_INSTALL%\libfbclient.a"
      if exist "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"    copy /b /y "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"    "%HB_LIB_INSTALL%\liblibmysql.a"
      if exist "%HB_WITH_OCILIB%\..\lib64\ociliba.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib64\ociliba.lib"      "%HB_LIB_INSTALL%\libociliba.a"
      if exist "%HB_WITH_OCILIB%\..\lib64\ocilibm.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib64\ocilibm.lib"      "%HB_LIB_INSTALL%\libocilibm.a"
      if exist "%HB_WITH_OCILIB%\..\lib64\ocilibw.lib"      copy /b /y "%HB_WITH_OCILIB%\..\lib64\ocilibw.lib"      "%HB_LIB_INSTALL%\libocilibw.a"

      goto END
   )

   if "%HB_COMPILER%" == "pocc" (

      if exist "%HB_WITH_ADS%\Redistribute\ace32.lib"       copy /b /y "%HB_WITH_ADS%\Redistribute\ace32.lib"           "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_WITH_ADS%\ace32.lib"                    copy /b /y "%HB_WITH_ADS%\ace32.lib"                        "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_WITH_ADS%\32bit\ace32.lib"              copy /b /y "%HB_WITH_ADS%\32bit\ace32.lib"                  "%HB_LIB_INSTALL%\ace32.lib"
      if exist "%HB_WITH_ALLEGRO%\..\lib\alleg.lib"         copy /b /y "%HB_WITH_ALLEGRO%\..\lib\alleg.lib"             "%HB_LIB_INSTALL%\alleg.lib"
      if exist "%HB_WITH_APOLLO%\..\sde61.dll"              polib "%HB_WITH_APOLLO%\..\sde61.dll"                  /out:"%HB_LIB_INSTALL%\sde61.lib"
      if exist "%HB_WITH_APOLLO%\..\sde7.dll"               polib "%HB_WITH_APOLLO%\..\sde7.dll"                   /out:"%HB_LIB_INSTALL%\sde7.lib"
      if exist "%HB_WITH_BLAT%\..\blat.lib"                 copy /b /y "%HB_WITH_BLAT%\..\blat.lib"                     "%HB_LIB_INSTALL%\blat.lib"
      if exist "%HB_WITH_CAIRO%\..\..\lib\cairo.lib"        copy /b /y "%HB_WITH_CAIRO%\..\..\lib\cairo.lib"            "%HB_LIB_INSTALL%\cairo.lib"
      if exist "%HB_WITH_CURL%\..\libcurl.dll"              polib "%HB_WITH_CURL%\..\libcurl.dll"                  /out:"%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_WITH_CURL%\..\bin\libcurl.dll"          polib "%HB_WITH_CURL%\..\bin\libcurl.dll"              /out:"%HB_LIB_INSTALL%\libcurl.lib"
      if exist "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"  copy /b /y "%HB_WITH_FIREBIRD%\..\lib\fbclient_ms.lib"      "%HB_LIB_INSTALL%\fbclient.lib"
      if exist "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.lib"  copy /b /y "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.lib"      "%HB_LIB_INSTALL%\FreeImage.lib"
      if exist "%HB_WITH_GD%\..\lib\bgd.lib"                copy /b /y "%HB_WITH_GD%\..\lib\bgd.lib"                    "%HB_LIB_INSTALL%\bgd.lib"
      if exist "%HB_WITH_LIBHARU%\..\libhpdf.lib"           copy /b /y "%HB_WITH_LIBHARU%\..\libhpdf.lib"               "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.lib"   copy /b /y "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.lib"       "%HB_LIB_INSTALL%\libhpdf.lib"
      if exist "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"    copy /b /y "%HB_WITH_MYSQL%\..\lib\opt\libmySQL.lib"        "%HB_LIB_INSTALL%\libmySQL.lib"
      if exist "%HB_WITH_OCILIB%\..\lib32\ociliba.lib"      copy /b /y "%HB_WITH_OCILIB%\ociliba.lib"                   "%HB_LIB_INSTALL%\ociliba.lib"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibm.lib"      copy /b /y "%HB_WITH_OCILIB%\ocilibm.lib"                   "%HB_LIB_INSTALL%\ocilibm.lib"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibw.lib"      copy /b /y "%HB_WITH_OCILIB%\ocilibw.lib"                   "%HB_LIB_INSTALL%\ocilibw.lib"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\libeay32.lib" copy /b /y "%HB_WITH_OPENSSL%\..\out32dll\libeay32.lib"     "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.lib" copy /b /y "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.lib"     "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\dll\libeay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\dll\libeay32.lib"          "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\dll\ssleay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\dll\ssleay32.lib"          "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\lib\libeay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\lib\libeay32.lib"          "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\lib\ssleay32.lib"      copy /b /y "%HB_WITH_OPENSSL%\..\lib\ssleay32.lib"          "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\libeay32.lib"          copy /b /y "%HB_WITH_OPENSSL%\..\libeay32.lib"              "%HB_LIB_INSTALL%\libeay32.lib"
      if exist "%HB_WITH_OPENSSL%\..\ssleay32.lib"          copy /b /y "%HB_WITH_OPENSSL%\..\ssleay32.lib"              "%HB_LIB_INSTALL%\ssleay32.lib"
      if exist "%HB_WITH_PGSQL%\..\lib\libpq.lib"           copy /b /y "%HB_WITH_PGSQL%\..\lib\libpq.lib"               "%HB_LIB_INSTALL%\libpq.lib"

      goto END
   )

   if "%HB_COMPILER%" == "watcom" (

      if exist "%HB_WITH_ADS%\Redistribute\ace32.dll"       wlib.exe -q -o="%HB_LIB_INSTALL%\ace32.lib"     "%HB_WITH_ADS%\Redistribute\ace32.dll"
      if exist "%HB_WITH_ADS%\ace32.dll"                    wlib.exe -q -o="%HB_LIB_INSTALL%\ace32.lib"     "%HB_WITH_ADS%\ace32.dll"
      if exist "%HB_WITH_ADS%\32bit\ace32.dll"              wlib.exe -q -o="%HB_LIB_INSTALL%\ace32.lib"     "%HB_WITH_ADS%\32bit\ace32.dll"
      if exist "%HB_WITH_ALLEGRO%\..\bin\alleg42.dll"       wlib.exe -q -o="%HB_LIB_INSTALL%\alleg.lib"     "%HB_WITH_ALLEGRO%\..\bin\alleg42.dll"
      if exist "%HB_WITH_APOLLO%\..\sde61.dll"              wlib.exe -q -o="%HB_LIB_INSTALL%\sde61.lib"     "%HB_WITH_APOLLO%\..\sde61.dll"
      if exist "%HB_WITH_APOLLO%\..\sde7.dll"               wlib.exe -q -o="%HB_LIB_INSTALL%\sde7.lib"      "%HB_WITH_APOLLO%\..\sde7.dll"
      if exist "%HB_WITH_BLAT%\..\blat.dll"                 wlib.exe -q -o="%HB_LIB_INSTALL%\blat.lib"      "%HB_WITH_BLAT%\..\blat.dll"
      if exist "%HB_WITH_CAIRO%\..\..\bin\libcairo-2.dll"   wlib.exe -q -o="%HB_LIB_INSTALL%\cairo.lib"     "%HB_WITH_CAIRO%\..\..\bin\libcairo-2.dll"
      if exist "%HB_WITH_CURL%\..\libcurl.dll"              wlib.exe -q -o="%HB_LIB_INSTALL%\libcurl.lib"   "%HB_WITH_CURL%\..\libcurl.dll"
      if exist "%HB_WITH_CURL%\..\bin\libcurl.dll"          wlib.exe -q -o="%HB_LIB_INSTALL%\libcurl.lib"   "%HB_WITH_CURL%\..\bin\libcurl.dll"
      if exist "%HB_WITH_FIREBIRD%\..\bin\fbclient.dll"     wlib.exe -q -o="%HB_LIB_INSTALL%\fbclient.lib"  "%HB_WITH_FIREBIRD%\..\bin\fbclient.dll"
      if exist "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.dll"  wlib.exe -q -o="%HB_LIB_INSTALL%\FreeImage.lib" "%HB_WITH_FREEIMAGE%\..\Dist\FreeImage.dll"
      if exist "%HB_WITH_GD%\..\bin\bgd.dll"                wlib.exe -q -o="%HB_LIB_INSTALL%\bgd.lib"       "%HB_WITH_GD%\..\bin\bgd.dll"
      if exist "%HB_WITH_LIBHARU%\..\libhpdf.dll"           wlib.exe -q -o="%HB_LIB_INSTALL%\libhpdf.lib"   "%HB_WITH_LIBHARU%\..\libhpdf.dll"
      if exist "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.dll"   wlib.exe -q -o="%HB_LIB_INSTALL%\libhpdf.lib"   "%HB_WITH_LIBHARU%\..\lib_dll\libhpdf.dll"
      if exist "%HB_WITH_MYSQL%\..\bin\libmySQL.dll"        wlib.exe -q -o="%HB_LIB_INSTALL%\libmySQL.lib"  "%HB_WITH_MYSQL%\..\bin\libmySQL.dll"
      if exist "%HB_WITH_OCILIB%\..\lib32\ociliba.dll"      wlib.exe -q -o="%HB_LIB_INSTALL%\ociliba.lib"   "%HB_WITH_OCILIB%\..\lib32\ociliba.dll"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibm.dll"      wlib.exe -q -o="%HB_LIB_INSTALL%\ocilibm.lib"   "%HB_WITH_OCILIB%\..\lib32\ocilibm.dll"
      if exist "%HB_WITH_OCILIB%\..\lib32\ocilibw.dll"      wlib.exe -q -o="%HB_LIB_INSTALL%\ocilibw.lib"   "%HB_WITH_OCILIB%\..\lib32\ocilibw.dll"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\libeay32.dll" wlib.exe -q -o="%HB_LIB_INSTALL%\libeay32.lib"  "%HB_WITH_OPENSSL%\..\out32dll\libeay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.dll" wlib.exe -q -o="%HB_LIB_INSTALL%\ssleay32.lib"  "%HB_WITH_OPENSSL%\..\out32dll\ssleay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\dll\libeay32.dll"      wlib.exe -q -o="%HB_LIB_INSTALL%\libeay32.lib"  "%HB_WITH_OPENSSL%\..\dll\libeay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\dll\ssleay32.dll"      wlib.exe -q -o="%HB_LIB_INSTALL%\ssleay32.lib"  "%HB_WITH_OPENSSL%\..\dll\ssleay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\libeay32.dll"          wlib.exe -q -o="%HB_LIB_INSTALL%\libeay32.lib"  "%HB_WITH_OPENSSL%\..\libeay32.dll"
      if exist "%HB_WITH_OPENSSL%\..\ssleay32.dll"          wlib.exe -q -o="%HB_LIB_INSTALL%\ssleay32.lib"  "%HB_WITH_OPENSSL%\..\ssleay32.dll"
      if exist "%HB_WITH_PGSQL%\..\lib\libpq.dll"           wlib.exe -q -o="%HB_LIB_INSTALL%\libpq.lib"     "%HB_WITH_PGSQL%\..\lib\libpq.dll"

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
   dumpbin -exports "%2" > _dump.tmp
   echo LIBRARY "%~n2" > _temp.def
   echo EXPORTS >> _temp.def
   sed -nf _hbtemp.sed < _dump.tmp >> _temp.def
   lib -nologo -machine:%1 -def:_temp.def -out:"%3"
   del _dump.tmp
   del _temp.def
   del _hbtemp.sed
   rem ---------------------------------------------------------------

   goto END

:END
