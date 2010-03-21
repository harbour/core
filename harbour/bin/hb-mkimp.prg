/*
 * $Id$
 */

/*
 * Copyright 2009-2010 Viktor Szakats (harbour.01 syenar.hu)
 * See COPYING for licensing terms.
 *
 * Create import libs for various Windows compilers
 *
 * This script requires:
 *    - C compiler in PATH
 *    - HB_COMPILER and HB_LIB_INSTALL envvars set
 *    - HB_WITH_* envvars pointing to installed 3rd party _headers_
 */

/* TOFIX: Ugly hack to avoid #include "fileio.ch" */
#define F_ERROR ( -1 )

PROCEDURE Main( cCompiler, cLibDir )
   LOCAL aLibs
   LOCAL hComps

   LOCAL lib
   LOCAL comp

   LOCAL cBase
   LOCAL cSource
   LOCAL cTarget
   LOCAL lDone

   IF ! hb_isString( cCompiler )
      cCompiler := GetEnv( "HB_COMPILER" )
   ENDIF
   IF ! hb_isString( cLibDir )
      cLibDir := GetEnv( "HB_LIB_INSTALL" )
   ENDIF

   IF ! Empty( cLibDir )

      OutStd( "! Making import libs...", hb_osNewLine() )

      #define _L_NAME     1
      #define _L_BASE     2
      #define _L_DLL      3
      #define _L_DLLMS    4 /* Controls BCC's iditoic -a implib flag. */
      #define _L_LIBMS    5
      #define _L_LIBMS64  6
      #define _L_LIBA     7

      aLibs := {;
         { "ace32"     , "HB_WITH_ADS"       , "Redistribute\ace32.dll"   , .F. , "Redistribute\ace32.lib"   ,                        ,                            }, ;
         { "ace32"     , "HB_WITH_ADS"       , "ace32.dll"                , .F. , "ace32.lib"                ,                        ,                            }, ;
         { "ace32"     , "HB_WITH_ADS"       , "32bit\ace32.dll"          , .F. , "32bit\ace32.lib"          ,                        ,                            }, ;
         { "alleg"     , "HB_WITH_ALLEGRO"   , "..\bin\alleg42.dll"       , .T. , "..\lib\alleg.lib"         ,                        ,                            }, ;
         { "sde61"     , "HB_WITH_APOLLO"    , "..\sde61.dll"             , .F. ,                            ,                        ,                            }, ;
         { "sde7"      , "HB_WITH_APOLLO"    , "..\sde7.dll"              , .F. ,                            ,                        ,                            }, ;
         { "blat"      , "HB_WITH_BLAT"      , "..\blat.dll"              , .T. , "..\blat.lib"              ,                        ,                            }, ;
         { "cairo"     , "HB_WITH_CAIRO"     , "..\..\bin\libcairo-2.dll" , .T. , "..\..\lib\cairo.lib"      ,                        , "..\..\lib\libcairo.dll.a" }, ;
         { "libcurl"   , "HB_WITH_CURL"      , "..\libcurl.dll"           , .T. ,                            ,                        , "..\lib\libcurl.a"         }, ;
         { "libcurl"   , "HB_WITH_CURL"      , "..\bin\libcurl.dll"       , .T. ,                            ,                        , "..\lib\libcurldll.a"      }, ;
         { "fbclient"  , "HB_WITH_FIREBIRD"  , "..\bin\fbclient.dll"      , .F. , "..\lib\fbclient_ms.lib"   ,                        ,                            }, ;
         { "FreeImage" , "HB_WITH_FREEIMAGE" , "..\Dist\FreeImage.dll"    , .F. , "..\Dist\FreeImage.lib"    ,                        ,                            }, ;
         { "bgd"       , "HB_WITH_GD"        , "..\bin\bgd.dll"           , .F. , "..\lib\bgd.lib"           ,                        ,                            }, ;
         { "libhpdf"   , "HB_WITH_LIBHARU"   , "..\libhpdf.dll"           , .F. , "..\libhpdf.lib"           ,                        ,                            }, ;
         { "libhpdf"   , "HB_WITH_LIBHARU"   , "..\lib_dll\libhpdf.dll"   , .F. , "..\lib_dll\libhpdf.lib"   ,                        ,                            }, ;
         { "libmysql"  , "HB_WITH_MYSQL"     , "..\bin\libmySQL.dll"      , .F. , "..\lib\opt\libmySQL.lib"  ,                        ,                            }, ;
         { "ociliba"   , "HB_WITH_OCILIB"    , "..\lib32\ociliba.dll"     , .F. , "..\lib32\ociliba.lib"     , "..\lib64\ociliba.lib" , "..\lib32\libociliba.a"    }, ;
         { "ocilibm"   , "HB_WITH_OCILIB"    , "..\lib32\ocilibm.dll"     , .F. , "..\lib32\ocilibm.lib"     , "..\lib64\ocilibm.lib" , "..\lib32\libocilibm.a"    }, ;
         { "ocilibw"   , "HB_WITH_OCILIB"    , "..\lib32\ocilibw.dll"     , .F. , "..\lib32\ocilibw.lib"     , "..\lib64\ocilibw.lib" , "..\lib32\libocilibw.a"    }, ;
         { "libeay32"  , "HB_WITH_OPENSSL"   , "..\out32dll\libeay32.dll" , .T. , "..\out32dll\libeay32.lib" ,                        ,                            }, ;
         { "ssleay32"  , "HB_WITH_OPENSSL"   , "..\out32dll\ssleay32.dll" , .T. , "..\out32dll\ssleay32.lib" ,                        ,                            }, ;
         { "libeay32"  , "HB_WITH_OPENSSL"   , "..\dll\libeay32.dll"      , .T. , "..\dll\libeay32.lib"      ,                        ,                            }, ;
         { "ssleay32"  , "HB_WITH_OPENSSL"   , "..\dll\ssleay32.dll"      , .T. , "..\dll\ssleay32.lib"      ,                        ,                            }, ;
         { "libeay32"  , "HB_WITH_OPENSSL"   , "..\libeay32.dll"          , .T. , "..\libeay32.lib"          ,                        ,                            }, ;
         { "ssleay32"  , "HB_WITH_OPENSSL"   , "..\ssleay32.dll"          , .T. , "..\ssleay32.lib"          ,                        ,                            }, ;
         { "libpq"     , "HB_WITH_PGSQL"     , "..\lib\libpq.dll"         , .T. , "..\lib\libpq.lib"         ,                        ,                            } }

      #define _C_LIBPREFIX    1
      #define _C_LIBEXT       2
      #define _C_PROC_DLL     3
      #define _C_PROC_LIBMS   4
      #define _C_PROC_LIBMS64 5
      #define _C_PROC_LIBA    6

      hComps := {;
         "mingw"    => { "lib", ".a"  , {| s, t | hb_FCopy( s, t ) != F_ERROR }, {| s, t | hb_FCopy( s, t ) != F_ERROR }, NIL, {| s, t | hb_FCopy( s, t ) != F_ERROR } }, ;
         "mingw64"  => { "lib", ".a"  , {| s, t | hb_FCopy( s, t ) != F_ERROR }, NIL, {| s, t | hb_FCopy( s, t ) != F_ERROR }, {| s, t | hb_FCopy( s, t ) != F_ERROR } }, ;
         "msvc"     => { ""   , ".lib", {| s, t | MSVC_implib( s, t, "x86" ) }, {| s, t | hb_FCopy( s, t ) != F_ERROR }, NIL, NIL }, ;
         "msvc64"   => { ""   , ".lib", {| s, t | MSVC_implib( s, t, "x64" ) }, NIL, {| s, t | hb_FCopy( s, t ) != F_ERROR }, NIL }, ;
         "msvcia64" => { ""   , ".lib", {| s, t | MSVC_implib( s, t, "ia86" ) }, NIL, NIL, NIL }, ;
         "msvcarm"  => { ""   , ".lib", {| s, t | MSVC_implib( s, t, "arm" ) }, NIL, NIL, NIL }, ;
         "xcc"      => { ""   , ".lib", {| s, t | hb_processRun( "xLib " + FN_Escape( s ) + " /out:" + FN_Escape( t ) ) == 0 }, {| s, t | hb_FCopy( s, t ) != F_ERROR }, NIL, NIL }, ;
         "pocc"     => { ""   , ".lib", {| s, t | hb_processRun( "polib " + FN_Escape( s ) + " /out:" + FN_Escape( t ) ) == 0 }, {| s, t | hb_FCopy( s, t ) != F_ERROR }, NIL, NIL }, ;
         "pocc64"   => { ""   , ".lib", {| s, t | hb_processRun( "polib " + FN_Escape( s ) + " /out:" + FN_Escape( t ) ) == 0 }, NIL, {| s, t | hb_FCopy( s, t ) != F_ERROR }, NIL }, ;
         "watcom"   => { ""   , ".lib", {| s, t | hb_processRun( "wlib -q -o=" + FN_Escape( t ) + " " + FN_Escape( s ) ) == 0 }, NIL, NIL, NIL }, ;
         "bcc"      => { ""   , ".lib", {| s, t, lib | hb_processRun( "implib " + iif( lib[ _L_DLLMS ], "-a", "" ) + " " + FN_Escape( t ) + " " + FN_Escape( s ) ) == 0 }, NIL, NIL, NIL } }

      IF Lower( cCompiler ) $ hComps

         comp := hb_HGet( hComps, Lower( cCompiler ) )

         FOR EACH lib IN aLibs
            lDone := .F.
            cSource := ""
            IF ! Empty( cBase := GetEnv( lib[ _L_BASE ] ) )
               cTarget := DirAddPathSep( cLibDir ) + comp[ _C_LIBPREFIX ] + lib[ _L_NAME ] + comp[ _C_LIBEXT ]
               IF ! lDone .AND. ! Empty( comp[ _C_PROC_LIBA ] ) .AND. ! Empty( lib[ _L_LIBA ] )
                  cSource := DirAddPathSep( cBase ) + lib[ _L_LIBA ]
                  IF hb_FileExists( cSource )
                     lDone := Eval( comp[ _C_PROC_LIBA ], cSource, cTarget, lib )
                  ENDIF
               ENDIF
               IF ! lDone .AND. ! Empty( comp[ _C_PROC_LIBMS64 ] ) .AND. ! Empty( lib[ _L_LIBMS64 ] )
                  cSource := DirAddPathSep( cBase ) + lib[ _L_LIBMS64 ]
                  IF hb_FileExists( cSource )
                     lDone := Eval( comp[ _C_PROC_LIBMS64 ], cSource, cTarget, lib )
                  ENDIF
               ENDIF
               IF ! lDone .AND. ! Empty( comp[ _C_PROC_LIBMS ] ) .AND. ! Empty( lib[ _L_LIBMS ] )
                  cSource := DirAddPathSep( cBase ) + lib[ _L_LIBMS ]
                  IF hb_FileExists( cSource )
                     lDone := Eval( comp[ _C_PROC_LIBMS ], cSource, cTarget, lib )
                  ENDIF
               ENDIF
               IF ! lDone .AND. ! Empty( comp[ _C_PROC_DLL ] ) .AND. ! Empty( lib[ _L_DLL ] )
                  cSource := DirAddPathSep( cBase ) + lib[ _L_DLL ]
                  IF hb_FileExists( cSource )
                     lDone := Eval( comp[ _C_PROC_DLL ], cSource, cTarget, lib )
                  ENDIF
               ENDIF
            ENDIF
            IF lDone
               OutStd( "! Import library created: " + cTarget )
               OutStd( " <= " + cSource )
               OutStd( hb_osNewLine() )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION MSVC_implib( s, t, cMode )
   LOCAL lSuccess := .F.

   LOCAL cExports
   LOCAL fhnd
   LOCAL cDef
   LOCAL cLine
   LOCAL tmp
   LOCAL aCols

   LOCAL cFuncList

   IF hb_processRun( "dumpbin -exports " + FN_Escape( s ),, @cExports ) == 0

      cFuncList := "LIBRARY " + Chr( 34 ) + FN_NameExtGet( s ) + Chr( 34 ) + hb_osNewLine() +;
                   "EXPORTS" + hb_osNewLine()

      cExports := StrTran( cExports, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )

      tmp := At( "ordinal hint", cExports )
      IF tmp > 0
         cExports := SubStr( cExports, tmp + Len( "ordinal hint" ) )
      ENDIF

      FOR EACH cLine IN hb_ATokens( cExports, Chr( 10 ) )
         IF ! Empty( cLine )
            aCols := hb_ATokens( cLine )
            IF Len( aCols ) >= 4
               cFuncList += aCols[ 4 ] + hb_osNewLine()
            ENDIF
         ENDIF
      NEXT

      fhnd := hb_FTempCreateEx( @cDef )
      IF fhnd != F_ERROR
         FWrite( fhnd, cFuncList )
         FClose( fhnd )

         lSuccess := ( hb_processRun( "lib -nologo -machine:" + cMode + " -def:" + FN_Escape( cDef ) + " -out:" + FN_Escape( t ) ) == 0 )

         FErase( cDef )
      ENDIF
   ENDIF

   RETURN lSuccess

STATIC FUNCTION FN_Escape( cFileName )
   RETURN Chr( 34 ) + cFileName + Chr( 34 )

STATIC FUNCTION FN_NameExtGet( cFileName )
   LOCAL cName, cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )

   RETURN hb_FNameMerge( NIL, cName, cExt )

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_osPathSeparator() )
      cDir += hb_osPathSeparator()
   ENDIF

   RETURN cDir
