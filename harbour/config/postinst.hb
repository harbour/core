/*
 * $Id$
 */

/*
 * This Harbour script is part of the GNU Make-based build system.
 * WARNING: Running it separately is not supported.
 *
 * Copyright 2009-2010 Viktor Szakats (harbour syenar.net)
 * Copyright 2003 Przemyslaw Czerpak (druzus/at/priv.onet.pl) (embedded autoinstall bash script)
 * See COPYING for licensing terms.
 */

#pragma warninglevel=3
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "fileio.ch"

PROCEDURE Main( ... )
   LOCAL nErrorLevel := 0
   LOCAL aFile

   LOCAL tmp, tmp1
   LOCAL cOldDir

   LOCAL cTar_Name
   LOCAL cTar_NameExt
   LOCAL cTar_Path
   LOCAL cBin_Tar
   LOCAL lGNU_Tar
   LOCAL cOwner
   LOCAL cGroup
   LOCAL cSH_Script
   LOCAL nAttr

   LOCAL cDynVersionFull
   LOCAL cDynVersionComp
   LOCAL cDynVersionless

   IF Empty( GetEnvC( "HB_PLATFORM" ) ) .OR. ;
      Empty( GetEnvC( "HB_COMPILER" ) ) .OR. ;
      Empty( GetEnvC( "HB_HOST_BIN_DIR" ) )

      OutStd( "! Error: This script has to be called from the GNU Make process." + hb_eol() )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   /* Detect install build phase */

   IF AScan( hb_AParams(), {| tmp | Lower( tmp ) == "install" } ) > 0

      /* Installing some misc files */
      tmp := GetEnvC( "HB_INSTALL_DOC" )
      IF !( tmp == "no" )
         IF GetEnvC( "HB_PLATFORM" ) $ "win|wce|os2|dos"
            tmp := GetEnvC( "HB_INSTALL_PREFIX" )
         ENDIF
         IF ! Empty( tmp )

            OutStd( "! Copying root documents..." + hb_eol() )

            IF hb_DirBuild( PathSepToSelf( tmp ) )
               FOR EACH aFile IN Directory( "Change*" )
                  mk_hb_FCopy( aFile[ F_NAME ], tmp + hb_ps() + iif( GetEnvC( "HB_PLATFORM" ) == "dos", "CHANGES", "" ) )
               NEXT

               mk_hb_FCopy( "COPYING", tmp + hb_ps() )
               mk_hb_FCopy( "INSTALL", tmp + hb_ps() )
               mk_hb_FCopy( "NEWS"   , tmp + hb_ps() )
               mk_hb_FCopy( "TODO"   , tmp + hb_ps() )
            ELSE
               OutStd( hb_StrFormat( "! Error: Cannot create directory '%1$s'", tmp ) + hb_eol() )
            ENDIF
         ENDIF
      ENDIF

      IF ! Empty( GetEnvC( "HB_INSTALL_BIN" ) ) .AND. ;
         ! GetEnvC( "HB_BUILD_PARTS" ) == "lib"

         OutStd( "! Copying Harbour script files..." + hb_eol() )

         /* public Harbour scripts */
         FOR EACH tmp IN { "bin/3rdpatch.hb" }
            mk_hb_FCopy( tmp, GetEnvC( "HB_INSTALL_BIN" ) + hb_ps() )
         NEXT
      ENDIF

      IF ! Empty( GetEnvC( "HB_INSTALL_ETC" ) )

         OutStd( "! Copying *nix config files..." + hb_eol() )

         IF hb_DirBuild( PathSepToSelf( GetEnvC( "HB_INSTALL_ETC" ) ) )
            mk_hb_FCopy( "src/rtl/gtcrs/hb-charmap.def", GetEnvC( "HB_INSTALL_ETC" ) + hb_ps(), .T. )
         ELSE
            OutStd( hb_StrFormat( "! Error: Cannot create directory '%1$s'", GetEnvC( "HB_INSTALL_ETC" ) ) + hb_eol() )
         ENDIF

         IF GetEnvC( "HB_PLATFORM" ) $ "linux" .AND. ;
            ! Empty( GetEnvC( "HB_INSTALL_DYN" ) )

            OutStd( "! Creating Linux ld config file..." + hb_eol() )

            tmp := GetEnvC( "HB_INSTALL_ETC" ) + hb_ps() + ".." + hb_ps() + "ld.so.conf.d"
            IF hb_DirBuild( PathSepToSelf( tmp ) )
               tmp1 := GetEnvC( "HB_INSTALL_DYN" )
               IF Left( tmp1, Len( GetEnvC( "HB_INSTALL_PKG_ROOT" ) ) ) == GetEnvC( "HB_INSTALL_PKG_ROOT" )
                  tmp1 := SubStr( tmp1, Len( GetEnvC( "HB_INSTALL_PKG_ROOT" ) ) + 1 )
               ENDIF
               hb_MemoWrit( tmp + hb_ps() + "harbour.conf", tmp1 + hb_eol() )
            ELSE
               OutStd( hb_StrFormat( "! Error: Cannot create directory '%1$s'", tmp ) + hb_eol() )
            ENDIF
         ENDIF
      ENDIF

      IF ! Empty( GetEnvC( "HB_INSTALL_MAN" ) )

         OutStd( "! Copying *nix man files..." + hb_eol() )

         IF hb_DirBuild( PathSepToSelf( GetEnvC( "HB_INSTALL_MAN" ) ) + hb_ps() + "man1" )
            FOR EACH tmp IN { "src/main/harbour.1"    ,;
                              "src/pp/hbpp.1"         ,;
                              "utils/hbmk2/hbmk2.1"   ,;
                              "utils/hbtest/hbtest.1" ,;
                              "contrib/hbrun/hbrun.1" }
               mk_hb_FCopy( tmp, GetEnvC( "HB_INSTALL_MAN" ) + hb_ps() + "man1" + hb_ps(), .T. )
            NEXT
         ELSE
            OutStd( hb_StrFormat( "! Error: Cannot create directory '%1$s'", GetEnvC( "HB_INSTALL_MAN" ) ) + hb_eol() )
         ENDIF
      ENDIF

      IF !( GetEnvC( "HB_PLATFORM" ) $ "win|wce|os2|dos|cygwin" ) .AND. ;
         ! Empty( GetEnvC( "HB_INSTALL_DYN" ) ) .AND. ;
         hb_FileExists( PathSepToSelf( GetEnvC( "HB_DYNLIB_DIR" ) ) + hb_ps() + GetEnvC( "HB_DYNLIB_PREF" ) + GetEnvC( "HB_DYNLIB_BASE" ) + GetEnvC( "HB_DYNLIB_POST" ) + GetEnvC( "HB_DYNLIB_EXT" ) + GetEnvC( "HB_DYNLIB_PEXT" ) )

         OutStd( "! Creating dynamic lib symlinks..." + hb_eol() )

         cDynVersionFull := GetEnvC( "HB_DYNLIB_PREF" ) + GetEnvC( "HB_DYNLIB_BASE" ) + GetEnvC( "HB_DYNLIB_POST" ) + GetEnvC( "HB_DYNLIB_EXT" ) + GetEnvC( "HB_DYNLIB_PEXT" )
         cDynVersionComp := GetEnvC( "HB_DYNLIB_PREF" ) + GetEnvC( "HB_DYNLIB_BASE" ) + GetEnvC( "HB_DYNLIB_POSC" ) + GetEnvC( "HB_DYNLIB_EXT" ) + GetEnvC( "HB_DYNLIB_PEXC" )
         cDynVersionless := GetEnvC( "HB_DYNLIB_PREF" ) + GetEnvC( "HB_DYNLIB_BASE" )                               + GetEnvC( "HB_DYNLIB_EXT" )

         mk_hb_FLinkSym( cDynVersionFull, PathSepToSelf( GetEnvC( "HB_INSTALL_DYN" ) ) + hb_ps() + cDynVersionComp )
         mk_hb_FLinkSym( cDynVersionFull, PathSepToSelf( GetEnvC( "HB_INSTALL_DYN" ) ) + hb_ps() + cDynVersionless )

         DO CASE
         CASE EndsWith( GetEnvC( "HB_INSTALL_DYN" ), "/usr/lib/harbour" ) .OR. ;
              EndsWith( GetEnvC( "HB_INSTALL_DYN" ), "/usr/lib64/harbour" ) .OR. ;
              EndsWith( GetEnvC( "HB_INSTALL_DYN" ), "/usr/local/lib/harbour" ) .OR. ;
              EndsWith( GetEnvC( "HB_INSTALL_DYN" ), "/usr/local/lib64/harbour" )

            mk_hb_FLinkSym( "harbour" + hb_ps() + cDynVersionFull, PathSepToSelf( GetEnvC( "HB_INSTALL_DYN" ) ) + hb_ps() + ".." + hb_ps() + cDynVersionless )
            mk_hb_FLinkSym( "harbour" + hb_ps() + cDynVersionFull, PathSepToSelf( GetEnvC( "HB_INSTALL_DYN" ) ) + hb_ps() + ".." + hb_ps() + cDynVersionComp )
            mk_hb_FLinkSym( "harbour" + hb_ps() + cDynVersionFull, PathSepToSelf( GetEnvC( "HB_INSTALL_DYN" ) ) + hb_ps() + ".." + hb_ps() + cDynVersionFull )

         CASE GetEnvC( "HB_INSTALL_DYN" ) == "/usr/local/harbour/lib"
            /* TOFIX: Rewrite this in .prg */
            #if 0
               ld="/usr/lib"
               if [ -n "${HB_INST_PKGPREF}" ] || [ -w $ld ]
                  then
                     mkdir -p ${HB_INST_PKGPREF}$ld
                     ln -sf ../local/harbour/lib/$l ${HB_INST_PKGPREF}$ld/$ll
                     ln -sf ../local/harbour/lib/$l ${HB_INST_PKGPREF}$ld/$l
                  fi
            #endif
         ENDCASE
      ENDIF

      /* Creating language files */

      IF ! Empty( GetEnvC( "HB_INSTALL_BIN" ) ) .AND. ;
         ! GetEnvC( "HB_BUILD_PARTS" ) == "lib"

         OutStd( "! Making core translation (.hbl) files..." + hb_eol() )

         FOR EACH tmp IN Directory( "utils" + hb_ps() + hb_osFileMask(), "D" )
            IF "D" $ tmp[ F_ATTR ] .AND. !( tmp[ F_NAME ] == "." ) .AND. !( tmp[ F_NAME ] == ".." )
               FOR EACH aFile IN Directory( "utils" + hb_ps() + tmp[ F_NAME ] + hb_ps() + "*.po" )
                  mk_hbl( "utils" + hb_ps() + tmp[ F_NAME ] + hb_ps() + aFile[ F_NAME ],;
                          PathSepToSelf( GetEnvC( "HB_INSTALL_BIN" ) ) + hb_ps() + hb_FNameExtSet( aFile[ F_NAME ], ".hbl" ) )
               NEXT
            ENDIF
         NEXT
      ENDIF

      /* Creating docs for core */

      IF ! Empty( tmp := GetEnvC( "HB_INSTALL_DOC" ) ) .AND. ! tmp == "no"

         OutStd( "! Compiling core documentation (.hbd)..." + hb_eol() )

         mk_hbd_core( "." + hb_ps(), tmp )
      ENDIF

      /* Creating install packages */

      IF GetEnvC( "HB_BUILD_PKG" ) == "yes" .AND. ;
         ! Empty( GetEnvC( "HB_TOP" ) )

         IF GetEnvC( "HB_PLATFORM" ) $ "win|wce|os2|dos"

            tmp := GetEnvC( "HB_TOP" ) + hb_ps() + GetEnvC( "HB_PKGNAME" ) + ".zip"

            OutStd( "! Making Harbour .zip install package: '" + tmp + "'" + hb_eol() )

            FErase( tmp )

            /* NOTE: Believe it or not this is the official method to zip a different dir with subdirs
                     without including the whole root path in filenames; you have to 'cd' into it.
                     Even with zip 3.0. For this reason we need absolute path in HB_TOP. There is also
                     no zip 2.x compatible way to force creation of a new .zip, so we have to delete it
                     first to avoid mixing in an existing .zip file. [vszakats] */

            cOldDir := hb_cwd( GetEnvC( "HB_INSTALL_PKG_ROOT" ) )

            mk_hb_processRun( PathSepToSelf( GetEnvC( "HB_DIR_ZIP" ) ) + "zip" +;
                              " -q -9 -X -r -o" +;
                              " " + FNameEscape( tmp ) +;
                              " . -i " + FNameEscape( GetEnvC( "HB_PKGNAME" ) + hb_ps() + "*" ) +;
                              " -x *.tds -x *.exp" )

            hb_cwd( cOldDir )

            IF GetEnvC( "HB_PLATFORM" ) $ "win|wce"

               tmp := GetEnvC( "HB_TOP" ) + hb_ps() + GetEnvC( "HB_PKGNAME" ) + ".exe"

               OutStd( "! Making Harbour .exe install package: '" + tmp + "'" + hb_eol() )

               mk_hb_processRun( PathSepToSelf( GetEnvC( "HB_DIR_NSIS" ) ) + "makensis.exe" +;
                                 " -V2" +;
                                 " " + FNameEscape( StrTran( "package/mpkg_win.nsi", "/", hb_ps() ) ) )
            ENDIF
         ELSE
            cBin_Tar := "tar"
            lGNU_Tar := .T.
            IF ! Empty( query_stdout( "gtar --version" ) )
               cBin_Tar := "gtar"
            ELSEIF Empty( query_stdout( "tar --version" ) )
               cBin_Tar := ""
            ELSEIF "bsdtar" $ query_stdout( "tar --version" )
               /* tar is mapped to bsdtar starting OS X 10.6 */
               lGNU_Tar := .F.
            ENDIF

            IF ! Empty( cBin_Tar )

               cTar_Name := GetEnvC( "HB_PKGNAME" )
               IF ! Empty( tmp := unix_name() )
                  cTar_Name += "-" + tmp
               ENDIF
               cTar_NameExt := cTar_Name + iif( GetEnvC( "HB_PLATFORM" ) == "dos", ".tgz", ".bin.tar.gz" )
               cTar_Path := GetEnvC( "HB_TOP" ) + hb_ps() + cTar_NameExt

               OutStd( "! Making Harbour tar install package: '" + cTar_Path + "'" + hb_eol() )

               FErase( cTar_Path )

               cOwner := "root"
               cGroup := iif( GetEnvC( "HB_PLATFORM" ) == "darwin" .OR. ;
                              GetEnvC( "HB_PLATFORM" ) == "bsd", "wheel", "root" )

               cOldDir := hb_cwd( GetEnvC( "HB_INSTALL_PKG_ROOT" ) )

               /* TODO: Add support for non-GNU non-BSD tar (which gets the data from stdio) */

               mk_hb_processRun( cBin_Tar +;
                                 " czvf" +;
                                 " " + FNameEscape( cTar_Path ) +;
                                 iif( lGNU_Tar, " --owner=" + cOwner + " --group=" + cGroup, "" ) +;
                                 " ." )

               hb_cwd( cOldDir )

               IF !( GetEnvC( "HB_PLATFORM" ) == "dos" )

                  tmp := GetEnvC( "HB_TOP" ) + hb_ps() + cTar_Name + ".inst.sh"

                  OutStd( "! Making Harbour tar installer package: '" + tmp + "'" + hb_eol() )

                  /* In the generated script always use tar because we can't be sure
                     if cBin_Tar exists in the installation environment */
                  cSH_Script := '#!/bin/sh' + Chr( 10 )
                  cSH_Script += 'if [ "\$1" = "--extract" ]; then' + Chr( 10 )
                  cSH_Script += '   tail -c ' + hb_ntos( hb_FSize( cTar_Path ) ) + ' "\$0" > "' + cTar_NameExt + '"' + Chr( 10 )
                  cSH_Script += '   exit' + Chr( 10 )
                  cSH_Script += 'fi' + Chr( 10 )
                  cSH_Script += 'if [ \`id -u\` != 0 ]; then' + Chr( 10 )
                  cSH_Script += '   echo "This package has to be installed from root account."' + Chr( 10 )
                  cSH_Script += '   exit 1' + Chr( 10 )
                  cSH_Script += 'fi' + Chr( 10 )
                  cSH_Script += 'echo "Do you want to install Harbour (y/n)"' + Chr( 10 )
                  cSH_Script += 'read ASK' + Chr( 10 )
                  cSH_Script += 'if [ "\${ASK}" != "y" ] && [ "\${ASK}" != "Y" ]; then' + Chr( 10 )
                  cSH_Script += '   exit 1' + Chr( 10 )
                  cSH_Script += 'fi' + Chr( 10 )
                  cSH_Script += '(tail -c ' + hb_ntos( hb_FSize( cTar_Path ) ) + ' "\$0" | gzip -cd | (cd /;tar xvpf -)) ' + iif( GetEnvC( "HB_PLATFORM" ) == "linux", "&& ldconfig", "" ) + Chr( 10 )
                  cSH_Script += 'exit \$?' + Chr( 10 )
                  cSH_Script += 'HB_INST_EOF' + Chr( 10 )

                  hb_MemoWrit( tmp, cSH_Script + hb_MemoRead( cTar_Path ) )

                  hb_FGetAttr( tmp, @nAttr )
                  hb_FSetAttr( tmp, hb_bitOr( nAttr, HB_FA_XOTH ) )
               ENDIF
            ELSE
               OutStd( "! Error: Cannot find 'tar' tool" + hb_eol() )
            ENDIF
         ENDIF
      ENDIF

      /* Executing user postinst configuration script */

      IF ! Empty( tmp := GetEnvC( "HB_INSTALL_SCRIPT" ) )

         mk_hb_processRun( tmp )
      ENDIF

   ELSE
      /* Regenerating extern headers */

      mk_extern_core()
   ENDIF

   OutStd( "! postinst script finished" + ;
      iif( nErrorLevel == 0, "", " with with errorlevel=" + hb_ntos( nErrorLevel ) ) + hb_eol() )

   ErrorLevel( nErrorLevel )

   RETURN

STATIC FUNCTION mk_hbl( cIn, cOut )
   LOCAL cErrorMsg
   LOCAL aTrans

   aTrans := __i18n_potArrayLoad( cIn, @cErrorMsg )
   IF aTrans != NIL
      IF hb_MemoWrit( cOut, hb_i18n_SaveTable( __i18n_hashTable( __i18n_potArrayToHash( aTrans, .F. ) ) ) )
         OutStd( "! Created " + cOut + " <= " + cIn + hb_eol() )
         RETURN .T.
      ELSE
         OutErr( "! Error: Cannot create file: " + cOut + hb_eol() )
      ENDIF
   ELSE
      OutErr( "! Error: Loading translation: " + cIn + " (" + cErrorMsg + ")" + hb_eol() )
   ENDIF

   RETURN .F.

STATIC FUNCTION mk_hbd_core( cDirSource, cDirDest )
   LOCAL cName := "harbour"
   LOCAL tmp

   LOCAL aErrMsg := {}
   LOCAL aEntry := __hbdoc_LoadDir( cDirSource, cName, aErrMsg )

   FOR EACH tmp IN aErrMsg
      OutErr( hb_StrFormat( "! %1$s", tmp ) + hb_eol() )
   NEXT

   IF ! Empty( aEntry )
      cName := hb_DirSepAdd( PathSepToSelf( cDirDest ) ) + cName + ".hbd"
      IF __hbdoc_SaveHBD( cName, aEntry )
         OutStd( "! Created " + cName + " <= " + cDirSource + hb_eol() )
         RETURN .T.
      ELSE
         OutErr( hb_StrFormat( "! Error: Saving '%1$s'", cName ) + hb_eol() )
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION mk_hb_processRun( cCommand, ... )

   OutStd( cCommand + hb_eol() )

   RETURN hb_processRun( cCommand, ... )

STATIC FUNCTION FNameEscape( cFN )
   RETURN Chr( 34 ) + cFN + Chr( 34 )

STATIC FUNCTION PathSepToSelf( cFileName )
   RETURN StrTran( cFileName, iif( hb_ps() == "\", "/", "\" ), hb_ps() )

/* Like hb_FCopy(), but accepts dir as target and can set attributes */
STATIC PROCEDURE mk_hb_FCopy( cSrc, cDst, l644 )
   LOCAL cDir, cName, cExt

   IF ! hb_isLogical( l644 )
      l644 := .F.
   ENDIF

   cSrc := PathSepToSelf( cSrc )
   cDst := PathSepToSelf( cDst )

   hb_FNameSplit( cDst, @cDir, @cName, @cExt )
   IF Empty( cName ) .AND. Empty( cExt )
      hb_FNameSplit( cSrc,, @cName, @cExt )
   ENDIF
   cDst := hb_FNameMerge( cDir, cName, cExt )

   IF hb_FCopy( cSrc, cDst ) == 0
#if 0
      OutStd( hb_StrFormat( "! Copied: %1$s <= %2$s", cDst, cSrc ) + hb_eol() )
#endif
      IF l644
         hb_FSetAttr( cDst, hb_bitOr( HB_FA_RUSR, HB_FA_WUSR, HB_FA_RGRP, HB_FA_ROTH ) )
      ENDIF
#if 0
   ELSE
      OutStd( hb_StrFormat( "! Error: Copying %1$s <= %2$s", cDst, cSrc ) + hb_eol() )
#endif
   ENDIF

   RETURN

/* Like hb_FLinkSym(), but with feedback */
STATIC PROCEDURE mk_hb_FLinkSym( cDst, cSrc )

   FErase( cSrc ) /* remove old links if any */
   IF hb_FLinkSym( cDst, cSrc ) == 0
      OutStd( hb_StrFormat( "! Symlink: %1$s <= %2$s", cDst, cSrc ) + hb_eol() )
   ELSE
      OutStd( hb_StrFormat( "! Error: Creating symlink %1$s <= %2$s", cDst, cSrc ) + hb_eol() )
   ENDIF

   RETURN

STATIC FUNCTION EndsWith( cString, cEnd )
   RETURN Right( cString, Len( cEnd ) ) == cEnd

STATIC FUNCTION query_stdout( cName )
   LOCAL cStdOut
   LOCAL cStdErr
   LOCAL nRetVal

   nRetVal := hb_processRun( cName,, @cStdOut, @cStdErr )

   RETURN iif( nRetVal == 0, AllTrim( StrTran( cStdOut, Chr( 10 ), " " ) ), "" )

STATIC FUNCTION query_rpm( cName, cID )
   LOCAL cResult := query_stdout( "rpm -q --queryformat='.%{VERSION}' " + cName )

   RETURN iif( Empty( cResult ), "", cID + AllTrim( StrTran( StrTran( cResult, Chr( 10 ), " " ), "." ) ) )

/* Please add your distro suffix if it not belong to the one recognized below
   and remember that order checking can be important */
STATIC FUNCTION unix_name()
   LOCAL tmp

   DO CASE
   CASE GetEnvC( "HB_PLATFORM" ) == "dos" ; RETURN "djgpp"
   CASE GetEnvC( "HB_PLATFORM" ) == "win" ; RETURN GetEnvC( "HB_COMPILER" )
   CASE ! Empty( tmp := query_rpm( "mandriva-release-One", "mdv" ) ) ; RETURN tmp
   CASE ! Empty( tmp := query_rpm( "mandriva-release"    , "mdv" ) ) ; RETURN tmp
   CASE ! Empty( tmp := query_rpm( "redhat-release"      , "rh"  ) ) ; RETURN tmp
   CASE ! Empty( tmp := query_rpm( "fedora-release"      , "fc"  ) ) ; RETURN tmp
   CASE ! Empty( tmp := query_rpm( "suse-release"        , "sus" ) ) ; RETURN tmp
   CASE ! Empty( tmp := query_rpm( "openSUSE-release"    , "sus" ) ) ; RETURN tmp
   /* TODO: Rewrite this in Harbour */
   CASE hb_FileExists( "/etc/pld-release" )
      RETURN "" /* cat /etc/pld-release|sed -e '/1/ !d' -e 's/[^0-9]//g' -e 's/^/pld/'` */
   ENDCASE

   RETURN StrTran( Lower( query_stdout( "uname -s" ) ), " ", "_" )

STATIC FUNCTION mk_extern_core()
   LOCAL aExtern

   IF GetEnvC( "HB_REBUILD_EXTERN" ) == "yes" .AND. ;
      ! Empty( GetEnvC( "HB_DYNLIB_BASE" ) )

      /* TOFIX: Use list of libs instead of dynamic lib */
      IF ( aExtern := __hb_extern_get_list( PathSepToSelf( GetEnvC( "HB_DYNLIB_DIR" ) ) + hb_ps() + GetEnvC( "HB_DYNLIB_PREF" ) + GetEnvC( "HB_DYNLIB_BASE" ) + GetEnvC( "HB_DYNLIB_POST" ) + GetEnvC( "HB_DYNLIB_EXT" ) + GetEnvC( "HB_DYNLIB_PEXT" ) ) ) != NIL

         OutStd( "! Generating core extern headers..." + hb_eol() )

         __hb_extern_gen( aExtern, "include" + hb_ps() + "hbscalar.hbx" )
         __hb_extern_gen( aExtern, "include" + hb_ps() + "hbcpage.hbx"  )
         __hb_extern_gen( aExtern, "include" + hb_ps() + "hblang.hbx"   )
         __hb_extern_gen( aExtern, "include" + hb_ps() + "hbusrrdd.hbx" )
         __hb_extern_gen( aExtern, "include" + hb_ps() + "harbour.hbx"  )

         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION GetEnvC( cEnvVar )
   STATIC s_hEnvCache := { => }

   IF cEnvVar $ s_hEnvCache
      RETURN s_hEnvCache[ cEnvVar ]
   ENDIF

   RETURN s_hEnvCache[ cEnvVar ] := GetEnv( cEnvVar )

#define _HB_FUNC_INCLUDE_ "HB_FUNC_INCLUDE"
#define _HB_FUNC_EXCLUDE_ "HB_FUNC_EXCLUDE"

#define _HB_SELF_PREFIX   "__HBEXTERN__"
#define _HB_SELF_SUFFIX   "__"

STATIC FUNCTION __hb_extern_get_list( cInputName )
   LOCAL aExtern := NIL
   LOCAL hExtern

   LOCAL cStdOut, cStdErr
   LOCAL cTempFile
   LOCAL pRegex
   LOCAL aResult
   LOCAL tmp

   LOCAL cCommand
   LOCAL cRegex := "[[:space:]]_?HB_FUN_([A-Z0-9_]*)[[:space:]]"

   /* NOTE: non-gcc extractor configs don't support dynamic libs as input. */
   DO CASE
   CASE "|" + GetEnv( "HB_COMPILER" ) + "|" $ "|gcc|mingw|mingw64|djgpp|"
      cCommand := "nm -g" + iif( GetEnv( "HB_PLATFORM" ) == "darwin", "", " --defined-only -C" ) + " {I}"
   CASE "|" + GetEnv( "HB_COMPILER" ) + "|" $ "|msvc|msvc64|pocc|pocc64|"
      IF "|" + GetEnv( "HB_COMPILER" ) + "|" $ "|msvc|msvc64|"
         cCommand := "dumpbin -symbols {I}"
      ELSE
         cCommand := "podump -symbols {I}"
      ENDIF
      cRegex := "SECT[0-9A-Z][0-9A-Z ].*[Ee]xternal.*_?HB_FUN_([A-Z0-9_]*)[[:space:]]"
   CASE GetEnv( "HB_COMPILER" ) == "watcom"
      cCommand := "wlib {I}"
   CASE GetEnv( "HB_COMPILER" ) == "bcc"
      cCommand := "tlib {I}, {T}"
   ENDCASE

   IF ! Empty( cCommand ) .AND. ;
      ! Empty( cRegex )
      IF hb_FileExists( cInputName )
         cCommand := StrTran( cCommand, "{I}", cInputName )
         IF "{T}" $ cCommand
            FClose( hb_FTempCreateEx( @cTempFile,,, ".tmp" ) )
            cCommand := StrTran( cCommand, "{T}", cTempFile )
         ENDIF
         IF hb_processRun( cCommand,, @cStdOut, @cStdErr ) == 0
            IF ! Empty( cTempFile )
               cStdOut := MemoRead( cTempFile )
            ENDIF
            IF ! Empty( pRegex := hb_regexComp( cRegex, .T., .T. ) )
               aResult := hb_regexAll( pRegex, StrTran( cStdOut, Chr( 13 ) ),,,,, .T. )
               aExtern := {}
               hExtern := { => }
               FOR EACH tmp IN aResult
                  tmp[ 2 ] := hb_asciiUpper( tmp[ 2 ] )
                  IF !( tmp[ 2 ] $ hExtern )
                     AAdd( aExtern, tmp[ 2 ] )
                     hExtern[ tmp[ 2 ] ] := NIL
                  ENDIF
               NEXT
               tmp := hb_cdpSelect( "EN" )
               ASort( aExtern,,, {| tmp, tmp1 | tmp < tmp1 } )
               hb_cdpSelect( tmp )
            ENDIF
         ENDIF
         IF ! Empty( cTempFile )
            FErase( cTempFile )
         ENDIF
      ENDIF
   ENDIF

   RETURN aExtern

STATIC PROCEDURE __hb_extern_get_exception_list( cInputName, /* @ */ aInclude, /* @ */ aExclude, /* @ */ hDynamic )
   LOCAL cFile
   LOCAL pRegex
   LOCAL tmp

   aInclude := {}
   aExclude := {}
   hDynamic := { => }

   IF ! Empty( cFile := MemoRead( cInputName ) )
      IF ! Empty( pRegex := hb_regexComp( "[[:space:]]" + _HB_FUNC_INCLUDE_ + "[[:space:]]([a-zA-Z0-9_].[^ \t\n\r]*)", .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            AAdd( aInclude, Upper( tmp[ 2 ] ) )
         NEXT
      ENDIF
      IF ! Empty( pRegex := hb_regexComp( "[[:space:]]" + _HB_FUNC_EXCLUDE_ + "[[:space:]]([a-zA-Z0-9_].[^ \t\n\r]*)", .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            AAdd( aExclude, Upper( tmp[ 2 ] ) )
         NEXT
      ENDIF
      IF ! Empty( pRegex := hb_regexComp( "^DYNAMIC ([a-zA-Z0-9_]*)$", .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            hDynamic[ Upper( tmp[ 2 ] ) ] := tmp[ 2 ]
         NEXT
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION __hb_extern_gen( aFuncList, cOutputName )
   LOCAL aExtern
   LOCAL cExtern
   LOCAL tmp

   LOCAL aInclude
   LOCAL aExclude
   LOCAL hDynamic

   LOCAL cSelfName := _HB_SELF_PREFIX + Upper( hb_FNameName( cOutputName ) ) + _HB_SELF_SUFFIX

   LOCAL cLine := "/* " + Replicate( "-", 68 ) + hb_eol()
   LOCAL cHelp := " *          Syntax: // HB_FUNC_INCLUDE <func>" + hb_eol() +;
                  " *                  // HB_FUNC_EXCLUDE <func>" + hb_eol() +;
                  " */" + hb_eol()

   __hb_extern_get_exception_list( cOutputName, @aInclude, @aExclude, @hDynamic )

   cExtern := "/*" + hb_eol()
   cExtern += " * $" + "Id" + "$" + hb_eol()
   cExtern += " */" + hb_eol()
   IF Empty( aInclude ) .AND. ;
      Empty( aExclude )
      cExtern += hb_eol()
      cExtern += cLine
      cExtern += " * NOTE: You can add manual override which functions to include or" + hb_eol()
      cExtern += " *       exclude from automatically generated EXTERNAL/DYNAMIC list." + hb_eol()
      cExtern += cHelp
   ELSE
      cExtern += hb_eol()
      cExtern += cLine
      cExtern += " * NOTE: Following comments are control commands for the generator." + hb_eol()
      cExtern += " *       Do not edit them unless you know what you are doing." + hb_eol()
      cExtern += cHelp
      IF ! Empty( aInclude )
         cExtern += hb_eol()
         FOR EACH tmp IN aInclude
            cExtern += "// " + _HB_FUNC_INCLUDE_ + " " + tmp + hb_eol()
         NEXT
      ENDIF
      IF ! Empty( aExclude )
         cExtern += hb_eol()
         FOR EACH tmp IN aExclude
            cExtern += "// " + _HB_FUNC_EXCLUDE_ + " " + tmp + hb_eol()
         NEXT
      ENDIF
   ENDIF
   cExtern += hb_eol()
   cExtern += cLine
   cExtern += " * WARNING: Automatically generated code below. DO NOT EDIT!" + hb_eol()
   cExtern += " *          Regenerate with HB_REBUILD_EXTERN=yes build option." + hb_eol()
   cExtern += " */" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#ifndef " + "__HBEXTERN_CH__" + Upper( hb_FNameName( cOutputName ) ) + "__" + hb_eol()
   cExtern += "#define " + "__HBEXTERN_CH__" + Upper( hb_FNameName( cOutputName ) ) + "__" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#if defined( __HBEXTREQ__ ) .OR. defined( " + cSelfName + "ANNOUNCE" + " )" + hb_eol()
   cExtern += "   ANNOUNCE " + cSelfName + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#if defined( __HBEXTREQ__ ) .OR. defined( " + cSelfName + "REQUEST" + " )" + hb_eol()
   cExtern += "   #command DYNAMIC <fncs,...> => EXTERNAL <fncs>" + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   IF Empty( aInclude )
      aExtern := aFuncList
   ELSE
      aExtern := {}
      FOR EACH tmp IN aFuncList
         IF AScan( aInclude, {| flt | hb_WildMatch( flt, tmp, .T. ) } ) > 0
            AAdd( aExtern, tmp )
         ENDIF
      NEXT
   ENDIF
   FOR EACH tmp IN aExtern
      IF ! hb_WildMatch( "HB_GT_*_DEFAULT", tmp, .T. ) .AND. ;
         ! hb_WildMatch( _HB_SELF_PREFIX + "*" + _HB_SELF_SUFFIX, tmp, .T. ) .AND. ;
         AScan( aExclude, {| flt | hb_WildMatch( flt, tmp, .T. ) } ) == 0
         cExtern += "DYNAMIC " + hb_HGetDef( hDynamic, tmp, tmp ) + hb_eol()
      ENDIF
   NEXT
   cExtern += hb_eol()
   cExtern += "#if defined( __HBEXTREQ__ ) .OR. defined( " + cSelfName + "REQUEST" + " )" + hb_eol()
   cExtern += "   #uncommand DYNAMIC <fncs,...> => EXTERNAL <fncs>" + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#endif" + hb_eol()

   RETURN hb_MemoWrit( cOutputName, cExtern )
