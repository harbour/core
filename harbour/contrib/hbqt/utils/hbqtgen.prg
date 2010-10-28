/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour-Qt wrapper generator.
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "fileio.ch"
#include "hbtrace.ch"

#define _EOL   chr( 10 )

/*----------------------------------------------------------------------*/

#define PRT_L_CONST                                1
#define PRT_L_FAR                                  2
#define PRT_L_AND                                  3
#define PRT_L_VIRT                                 4
#define PRT_NAME                                   5
#define PRT_CAST                                   6
#define PRT_DEFAULT                                7
#define PRT_RAW                                    8
#define PRT_BODY                                   9
#define PRT_DOC                                   10
#define PRT_BODY_PRE                              11
#define PRT_L_CONST_LAST                          12

#define PRT_ATTRB_MAX                             12

#define THIS_PROPER( s )   ( upper( left( s,1 ) ) + substr( s,2 ) )

#define  QT_VER  "4.5"
#define  QT_WEB  "http://doc.trolltech.com/"

/*----------------------------------------------------------------------*/

FUNCTION Main( ... )
   LOCAL oGen

   oGen := HbqtGenerator():new( ... )
   IF oGen:lValid
      oGen:process()
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

CREATE CLASS HbQtGenerator

   DATA   lValid                                  INIT .f.
   DATA   lCheckArgs                              INIT .f.
   DATA   isObject                                INIT .f.

   DATA   aPrjFiles                               INIT {}
   DATA   aProFiles                               INIT {}

   DATA   cTrMode                                 INIT "HB_TR_DEBUG"
   DATA   cPathIn
   DATA   cPathOut
   DATA   cPathDoc

   DATA   aWidgetList                             INIT {}
   DATA   cpp_                                    INIT {}
   DATA   prg_                                    INIT {}

   METHOD new( ... )
   METHOD process()
   METHOD manageProject( cProFile )
   METHOD genSource( cProFile, cPathIn, cPathOut, cPathDoc, cProject )
   METHOD buildMakeFile( cPathOut, cProFile )
   METHOD buildHeaderFile( cpp_, cPathOut, cProFile )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbQtGenerator:new( ... )
   LOCAL cLParam, cParam, x, cPath, cFile, cExt, aDir

   IF !empty( GetEnv( "HBQT_BUILD_TR_LEVEL" ) )
      ::cTrMode := upper( GetEnv( "HBQT_BUILD_TR_LEVEL" ) )
      IF ! ( ::cTrMode $ "HB_TR_ALWAYS,HB_TR_WARNING,HB_TR_ERROR" )
         ::cTrMode := "HB_TR_DEBUG"
      ENDIF
   ENDIF

   DispLogo()

   FOR EACH cParam IN hb_AParams()
      cLParam := lower( cParam )

      DO CASE
      CASE left( cParam,1 ) == "@"
         x := substr( cParam,2 )
         hb_fNameSplit( x, @cPath, @cFile, @cExt )
         IF empty( cExt ) .or. !( lower( cExt ) == ".qtp" )
            cExt := ".qtp"
         ENDIF
         x := iif( empty( cPath ), "", cPath + hb_ps() )+ cFile + cExt
         aadd( ::aPrjFiles, x )

      CASE right( cLParam,4 ) == ".qtp"
         aadd( ::aPrjFiles, cParam )

      CASE right( cLParam,4 ) == ".qth"
         aadd( ::aProFiles, cParam )

      CASE left( cLParam,2 ) == "-o"
         ::cPathOut := substr( cParam, 3 )

      CASE lower( left( cParam,2 ) ) == "-i"
         ::cPathIn := substr( cParam, 3 )

      CASE lower( left( cParam,2 ) ) == "-d"
         ::cPathDoc := substr( cParam, 3 )

      CASE cLParam == "-c"

      CASE cLParam == "-help"
         DispHelp()
         RETURN Self

      ENDCASE
   NEXT

   IF empty( ::aPrjFiles ) .AND. empty( ::aProFiles )
      FOR EACH aDir IN directory( "*.qtp" )
         aadd( ::aPrjFiles, aDir[ 1 ] )
      NEXT
   ENDIF

   IF empty( ::aPrjFiles ) .AND. empty( ::aProFiles )
      DispHelp()
      RETURN Self
   ENDIF

   IF empty( ::cPathOut )
      ::cPathOut := hb_dirBase()
   ENDIF
   IF empty( ::cPathIn )
      ::cPathIn  := hb_dirBase()
   ENDIF
   IF empty( ::cPathDoc )
      ::cPathDoc := hb_dirBase()
   ENDIF
   IF Right( ::cPathOut, 1 ) == hb_ps()
      ::cPathOut := hb_StrShrink( ::cPathOut, 1 )
   ENDIF
   IF Right( ::cPathIn, 1 ) == hb_ps()
      ::cPathIn := hb_StrShrink( ::cPathIn, 1 )
   ENDIF
   IF Right( ::cPathDoc, 1 ) == hb_ps()
      ::cPathDoc := hb_StrShrink( ::cPathDoc, 1 )
   ENDIF

   ::lValid := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtGenerator:process()
   LOCAL cProFile

   /* Manage Project File - make files are buit only for projects */
   FOR EACH cProFile IN ::aPrjFiles
      ::ManageProject( cProFile )
   NEXT

   /* Individual file - Directly Generate .cpp Sources */
   FOR EACH cProFile IN ::aProFiles
      ::GenSource( cProFile, ::cPathIn, ::cPathOut, ::cPathDoc, cProFile )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtGenerator:manageProject( cProFile )
   LOCAL cFile, cPrj, cTkn, cVal, cPIn, cPOut, cPDoc, n, nn
   LOCAL prj_, a_

   cFile := cProFile
   IF ! hb_fileExists( cFile )
      RETURN Self
   ENDIF

   cPIn  := ::cPathIn
   cPOut := ::cPathOut
   cPDoc := ::cPathDoc

   ::aWidgetList := {}     /* Per Sub Library */
   ::cpp_:= {}
   ::prg_:= {}

   OutStd( "Processing: " + cFile + hb_eol() )

   cPrj  := memoread( cFile )

   /* Pullout all ANSI C style comments */
   DO WHILE .t.
      IF ( n := at( "/*", cPrj ) ) == 0
         EXIT
      ENDIF
      /* We must have a matching pair */
      nn := at( "*/", cPrj )
      IF nn == 0
         OutStd( "Project file has unbalanced comment section..." + hb_eol() )
         RETURN nil
      ENDIF
      cPrj := substr( cPrj, 1, n-1 ) + substr( cPrj, nn+2 )
   ENDDO

   /* Prepare to be parsed properly */
   cPrj := strtran( cPrj, hb_eol(), _EOL )
   cPrj := strtran( cPrj, chr( 13 )+chr( 10 ), _EOL )
   cPrj := strtran( cPrj, chr( 13 ), _EOL )

   prj_:= hb_ATokens( cPrj, _EOL )

   FOR EACH cPrj IN prj_
      cPrj := alltrim( cPrj )

      IF left( cPrj,1 ) $ ":#;/*"
         LOOP
      ENDIF

      IF ( n := at( "=", cPrj ) ) > 0
         cTkn := alltrim( substr( cPrj,1,n-1 ) )
         cVal := alltrim( substr( cPrj,n+1   ) )

         DO CASE
         CASE cTkn == "-I"
            cPIn := cVal

         CASE cTkn == "-O"
            cPOut := cVal

         CASE cTkn == "-D"
            cPDoc := cVal
         ENDCASE

      ELSEIF lower( right( cPrj,4 ) ) == ".qth"
         a_:= ::genSource( cPrj, cPIn, cPOut, cPDoc, cProFile )
         IF !empty( a_[ 1 ] )
            aadd( ::cpp_, a_[ 1 ] )
            IF !empty( a_[ 2 ] )
               aadd( ::prg_, a_[ 2 ] )
            ENDIF
         ENDIF

      ENDIF
   NEXT

   IF !empty( ::cpp_ )
      ::buildMakefile( cPOut, cProFile )
      ::buildHeaderFile( ::cpp_, cPOut, cProFile )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtGenerator:genSource( cProFile, cPathIn, cPathOut, cPathDoc, cProject )
   LOCAL cFile, cWidget, cPath, oSrc

   hb_fNameSplit( cProFile, @cPath, @cWidget )

   IF empty( cPath )
      cFile := cPathIn + hb_ps() + cProFile
   ELSE
      cFile := cProFile
   ENDIF
   IF ! hb_fileExists( cFile )
      OutStd( "Cannot find: " + cFile + hb_eol() )
      RETURN { nil }
   ENDIF

   IF empty( memoread( cFile ) )
      OutStd( "Cannot read: " + cFile + hb_eol() )
      RETURN { nil }
   ENDIF

   OutStd( "Processing: " + cFile + hb_eol() )

   /* Mark to which sub library class belongs to */
   aadd( ::aWidgetList, cWidget )

   oSrc := HbQtSource():new( Self, cFile, cPathOut, cPathDoc, cProject ):build()

   RETURN { oSrc:cCPP, oSrc:cPRG }

/*----------------------------------------------------------------------*/

METHOD HbQtGenerator:buildHeaderFile( cpp_, cPathOut, cProFile )
   LOCAL txt_, s, tmp
   LOCAL cFile := iif( empty( cPathOut ), "", cPathOut + hb_ps() )
   LOCAL hdr_  := {}
   LOCAL cName := FNameGetName( cProFile )

   aadd( hdr_, "/*"                                                                            )
   aadd( hdr_, " * $" + "Id" + "$"                                                             )
   aadd( hdr_, " */"                                                                           )
   aadd( hdr_, ""                                                                              )
   aadd( hdr_, "/* -------------------------------------------------------------------- */"    )
   aadd( hdr_, "/* WARNING: Automatically generated source file. DO NOT EDIT!           */"    )
   aadd( hdr_, "/*          Instead, edit corresponding .qth file,                      */"    )
   aadd( hdr_, "/*          or the generator tool itself, and run regenarate.           */"    )
   aadd( hdr_, "/* -------------------------------------------------------------------- */"    )
   aadd( hdr_, "" )

   txt_ := {}
   AEval( hdr_, {| tmp | AAdd( txt_, tmp ) } )

   aadd( txt_, "#ifndef __HB" + Upper( cName ) + "_H" )
   aadd( txt_, "#define __HB" + Upper( cName ) + "_H" )
   aadd( txt_, "" )
   aadd( txt_, '#include "hbqt.h"' )
   aadd( txt_, "" )
   aadd( txt_, "HB_EXTERN_BEGIN" )
   aadd( txt_, "" )

   FOR EACH s IN cpp_
      aadd( txt_, "extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_" + s + " );" )
   NEXT
   aadd( txt_, "" )

   FOR EACH s IN cpp_
      aadd( txt_, "extern HB_EXPORT void * hbqt_gcAllocate_" + s + "( void * pObj, bool bNew );" )
   NEXT
   aadd( txt_, "" )

   aadd( txt_, "HB_EXTERN_END" )
   aadd( txt_, "" )

   FOR EACH s IN cpp_
      IF s == "QList" /* TOFIX: Ugly hack */
         tmp := s + "< void * >"
      ELSE
         tmp := s
      ENDIF
      aadd( txt_, PadR( "#define hbqt_par_" + s + "( n )", 64 ) + PadR( "( ( " + tmp, 48 ) + "* ) hbqt_gcpointer( n ) )" )
   NEXT
   aadd( txt_, "" )

   FOR EACH s IN cpp_
      aadd( txt_, PadR( "#define HBQT_TYPE_" + s, 64 ) + "0x" + hb_NumToHex( hb_crc32( "HBQT_TYPE_" + hb_asciiUpper( cName ) + s ), 8 ) )
   NEXT
   aadd( txt_, "" )

   aadd( txt_, "#endif /* __HB" + Upper( cName ) + "_H */" )

   CreateTarget( cFile + "hb" + cName + ".h", txt_ )

   /* Create extern puller file */

   txt_ := {}
   AEval( hdr_, {| tmp | AAdd( txt_, tmp ) } )

   aadd( txt_, "#ifndef __HB" + Upper( cName ) + "_EXTERN_CH" )
   aadd( txt_, "#define __HB" + Upper( cName ) + "_EXTERN_CH" )
   aadd( txt_, "" )
   aadd( txt_, "#define __HBEXTERN__HB" + Upper( cName ) + "__ANNOUNCE" )
   aadd( txt_, "#define __HBEXTERN__HB" + Upper( cName ) + "__REQUEST" )
   aadd( txt_, '#include "hb' + cName + '.hbx"' )
   aadd( txt_, "" )

   aadd( txt_, "#endif /* __HB" + Upper( cName ) + "_EXTERN_CH */" )

   CreateTarget( cFile + "hb" + cName + "_extern.prg", txt_ )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbQtGenerator:buildMakeFile( cPathOut, cProFile )
   LOCAL cFile := iif( empty( cPathOut ), "", cPathOut + hb_ps() )
   LOCAL s
   LOCAL hdr_:= {}
   LOCAL hbm_:= {}

   aadd( hdr_, "#" )
   aadd( hdr_, "# $" + "Id" + "$" )
   aadd( hdr_, "#" )
   aadd( hdr_, "" )
   aadd( hdr_, "# --------------------------------------------------------------------" )
   aadd( hdr_, "# WARNING: Automatically generated source file. DO NOT EDIT!          " )
   aadd( hdr_, "#          Instead, edit corresponding .qth file,                     " )
   aadd( hdr_, "#          or the generator tool itself, and run regenarate.          " )
   aadd( hdr_, "# --------------------------------------------------------------------" )
   aadd( hdr_, "" )

   hbm_ := {}
   aeval( hdr_, {|e| aadd( hbm_, e ) } )
   //
   aadd( hbm_, "hb" + FNameGetName( cProFile ) + "_extern.prg" )
   FOR EACH s IN ::aWidgetList
      aadd( hbm_, + s + ".cpp" )
   NEXT
   aadd( hbm_, "" )
   FOR EACH s IN ::aWidgetList
      aadd( hbm_, + "T" + s + ".prg" )
   NEXT
   //
   CreateTarget( cFile + "filelist.hbm", hbm_ )

   RETURN NIL

/*----------------------------------------------------------------------*/

CREATE CLASS HbQtSource

   DATA   oGen
   DATA   cFileQth
   DATA   cPathOut
   DATA   cPathDoc
   DATA   cProject
   DATA   cProFile
   DATA   cWidget

   DATA   aMethods                                INIT {}

   DATA   isList                                  INIT .f.
   DATA   isDestructor                            INIT .t.
   DATA   isConstructor                           INIT .f.
   DATA   isObject                                INIT .t.
   DATA   isDetached                              INIT .f.
   DATA   areMethodsClubbed                       INIT .t.

   DATA   class_                                  INIT {}
   DATA   subCls_                                 INIT {}
   DATA   docum_                                  INIT {}
   DATA   code_                                   INIT {}
   DATA   cls_                                    INIT {}
   DATA   new_                                    INIT {}
   DATA   old_                                    INIT {}
   DATA   enums_                                  INIT {}
   DATA   enum_                                   INIT {}
   DATA   protos_                                 INIT {}
   DATA   varbls_                                 INIT {}
   DATA   slots_                                  INIT {}

   DATA   dummy_                                  INIT {}
   DATA   func_                                   INIT { { "", 0 } }
   DATA   txt_                                    INIT {}
   DATA   cpp_                                    INIT {}
   DATA   cmntd_                                  INIT {}
   DATA   doc_                                    INIT {}
   DATA   constructors_                           INIT {}

   DATA   nFuncs                                  INIT 0
   DATA   nCnvrtd                                 INIT 0

   DATA   cFunc
   DATA   cTrMode

   DATA   cPrg
   DATA   cCpp

   DATA   cInt                                    INIT "int,qint16,quint16,short,ushort,unsigned"
   DATA   cIntLong                                INIT "qint32,quint32,QRgb"
   DATA   cIntLongLong                            INIT "qint64,quint64,qlonglong,qulonglong,ulong"

   METHOD new( oGen, cFileQth, cPathOut, cPathDoc, cProject )
   METHOD parseProto( cProto, fBody_ )
   METHOD parseVariables( cProto )
   METHOD build()
   METHOD buildCppCode( oMtd )
   METHOD buildMethodBody( oMtd )
   METHOD buildDocument()
   METHOD buildClass()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbQtSource:new( oGen, cFileQth, cPathOut, cPathDoc, cProject )
   LOCAL cQth, s, n, i, n1, b_, tmp, cOrg, fBody_

   ::oGen     := oGen
   ::cFileQth := cFileQth
   ::cPathOut := cPathOut
   ::cPathDoc := cPathDoc
   ::cProject := cProject
   ::cProFile := cProject

   ::cTrMode  := oGen:cTrMode

   hb_fNameSplit( cFileQth, , @tmp )
   ::cWidget := tmp

   cQth := hb_memoread( cFileQth )

   /* Prepare to be parsed properly */
   IF ! hb_eol() == _EOL
      cQth := StrTran( cQth, hb_eol(), _EOL )
   ENDIF
   IF ! hb_eol() == Chr( 13 ) + Chr( 10 )
      cQth := StrTran( cQth, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF

   IF !empty( ::class_:= PullOutSection( @cQth, "CLASS" ) )
      FOR EACH s IN ::class_
         IF ( n := at( "=", s ) ) > 0
            aadd( ::cls_, { alltrim( substr( s, 1, n-1 ) ), alltrim( substr( s, n+1 ) ) } )
         ENDIF
      NEXT
   ENDIF

   /* Pull out SUBCLASS section */
   ::subCls_ := PullOutSection( @cQth, "SUBCLASS" )

   /* Pull out Doc Section */
   ::docum_  := PullOutSection( @cQth, "DOC"   )

   /* Pull out Code Section */
   ::code_   := PullOutSection( @cQth, "CODE"   )

   /* Separate constructor function */
   ::new_:= {}
   ::cFunc := "HB_FUNC( QT_" + upper( ::cWidget ) + " )"

   n := ascan( ::code_, {|e| ::cFunc $ e } )
   FOR i := n TO len( ::code_ )
      aadd( ::new_, ::code_[ i ] )
      IF trim( ::code_[ i ] ) == "}"
         n1 := i
         EXIT
      ENDIF
   NEXT
   ::old_:={}
   FOR i := 1 TO len( ::code_ )
      IF i < n .or. i > n1
         aadd( ::old_, ::code_[ i ] )
      ENDIF
   NEXT
   ::code_:= ::old_

   /* Pullout constructor methods */
   #if 0
   tmp := ::cWidget + " ("
   FOR EACH s IN ::code_
      IF ( n := at( tmp, s ) ) > 0 .AND. ! ( "~" $ s )
         aadd( ::constructors_, substr( s, n ) )
      ENDIF
   NEXT
   #endif

   /* Pull out Enumerators  */
   ::enums_:= PullOutSection( @cQth, "ENUMS"  )
   ::enum_:= {}
   FOR EACH s IN ::enums_
      IF ( "enum " $ s .or. "flags " $ s )
         b_:= hb_ATokens( alltrim( s ), " " )
         aadd( ::enum_, b_[ 2 ] )
      ENDIF
   NEXT

   /* Pull out Prototypes   */
//   ::protos_ := PullOutSection( @cQth, "PROTOS" )
   tmp := PullOutSection( @cQth, "PROTOS" )
   aeval( ::constructors_, {|e| aadd( ::protos_, e ) } )
   aeval( tmp, {|e| aadd( ::protos_, e ) } )

   /* Pull out Variables */
   ::varbls_ := PullOutSection( @cQth, "VARIABLES" )

   /* Pull Out Signals      */
   ::slots_  := PullOutSection( @cQth, "SLOTS"  )

   /* Combine signals and protos : same nature */
   aeval( ::slots_, {|e| aadd( ::protos_, e ) } )

   ::isList            := ascan( ::cls_, {|e_| lower( e_[ 1 ] ) == "list"        .AND. lower( e_[ 2 ] ) == "yes" } ) > 0
   ::isDetached        := ascan( ::cls_, {|e_| lower( e_[ 1 ] ) == "detached"    .AND. lower( e_[ 2 ] ) == "yes" } ) > 0
   ::isConstructor     := ascan( ::cls_, {|e_| lower( e_[ 1 ] ) == "constructor" .AND. lower( e_[ 2 ] ) == "no"  } ) == 0
   ::isDestructor      := ascan( ::cls_, {|e_| lower( e_[ 1 ] ) == "destructor"  .AND. lower( e_[ 2 ] ) == "no"  } ) == 0
   ::isObject          := ascan( ::cls_, {|e_| lower( e_[ 1 ] ) == "qobject"     .AND. lower( e_[ 2 ] ) == "no"  } ) == 0
   ::areMethodsClubbed := ascan( ::cls_, {|e_| lower( e_[ 1 ] ) == "clubmethods" .AND. lower( e_[ 2 ] ) == "no"  } ) == 0
   /* Determine Constructor - but this is hacky a bit. What could be easiest ? */
   IF ! ::isConstructor
      FOR i := 3 TO len( ::new_ ) - 1
         IF !( left( ltrim( ::new_[ i ] ), 2 ) == "//" )
            IF "__HB_RETPTRGC__(" $ ::new_[ i ]
               ::isConstructor := .t.
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   FOR EACH s IN ::protos_
      cOrg := s
      IF empty( s := alltrim( s ) )
         LOOP
      ENDIF

      /* Check if proto is commented out */
      IF left( s,2 ) == "//"
         aadd( ::cmntd_, cOrg )
         LOOP
      ENDIF
      /* Check if it is not ANSI C Comment */
      IF left( alltrim( cOrg ),1 ) $ "/*"
         LOOP
      ENDIF
      /* Another comment tokens */
      IF empty( s ) .or. left( s,1 ) $ "#;}"
         LOOP
      ENDIF

      ::nFuncs++

      fBody_:= {}
      IF right( s, 1 ) == "{"
         fBody_:= PullOutFuncBody( ::protos_, s:__enumIndex() )
         s := substr( s, 1, len( s ) - 1 )
      ENDIF
      IF ::parseProto( s, fBody_ )
         ::nCnvrtd++
      ELSE
         aadd( ::dummy_, cOrg )
      ENDIF
   NEXT

   FOR EACH s IN ::varbls_
      cOrg := s

      IF empty( s := alltrim( s ) )
         LOOP
      ENDIF
      /* Check if proto is commented out */
      IF left( s,2 ) == "//"
         aadd( ::cmntd_, cOrg )
         LOOP
      ENDIF
      /* Check if it is not ANSI C Comment */
      IF left( alltrim( cOrg ),1 ) $ "/*"
         LOOP
      ENDIF
      /* Another comment tokens */
      IF empty( s ) .or. left( s,1 ) $ "#;"
         LOOP
      ENDIF

      ::nFuncs++

      IF ::parseVariables( s )
         ::nCnvrtd++
      ELSE
         aadd( ::dummy_, cOrg )
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtSource:build()
   LOCAL cFileCpp, i, s, oMtd
   //LOCAL lList, lDestructor, lObject, lConst

   /* Methods Body */
   FOR EACH oMtd IN ::aMethods
      ::buildMethodBody( oMtd )
   NEXT

   /* Pull .cpp copyright text */
   BuildCopyrightText( @::cpp_, 0, ::cProject )

   /* Place ENUM definitions into the source */
   IF !empty( ::enums_ )
      aadd( ::cpp_, "/*" )
      aeval( ::enums_, {|e| iif( !empty( e ), aadd( ::cpp_, " *  " + e ), NIL ) } )
      aadd( ::cpp_, " */ " )
      aadd( ::cpp_, "" )
   ENDIF

   /* Insert information about prototypes not converted to functions */
   aadd( ::cpp_, "/*" )
   aadd( ::cpp_, " *  Constructed[ " + hb_ntos( ::nCnvrtd ) + "/" + hb_ntos( ::nFuncs ) + " [ " + hb_ntos( ::nCnvrtd / ::nFuncs * 100 ) + "% ] ]" )
   aadd( ::cpp_, " *  " )
   IF !empty( ::dummy_ )
      aadd( ::cpp_, " *  *** Unconvered Prototypes ***" )
      aadd( ::cpp_, " *  " )
      aeval( ::dummy_, {|e| aadd( ::cpp_, " *  " + e ) } )
   ENDIF
   IF !empty( ::cmntd_ )
      aadd( ::cpp_, " *  " )
      aadd( ::cpp_, " *  " + "*** Commented out protostypes ***" )
      aadd( ::cpp_, " *  " )
      aeval( ::cmntd_, {|e| aadd( ::cpp_, " *  " + e ) } )
   ENDIF
   aadd( ::cpp_, " */ " )
   aadd( ::cpp_, "" )

   /* Insert user defined code - INCLUDEs */
   aadd( ::cpp_, "#include <QtCore/QPointer>" )
   IF !empty( ::code_ )
      aeval( ::code_, {|e| aadd( ::cpp_, strtran( e, chr( 13 ), "" ) ) } )
      aadd( ::cpp_, "" )
   ENDIF

   aadd( ::cpp_, "typedef struct"                  )
   aadd( ::cpp_, "{"                               )
   IF ::isObject
      aadd( ::cpp_, "   QPointer< "+ ::cWidget +" > ph;" )
   ELSE
      IF ::isList
          aadd( ::cpp_, "   " + ::cWidget + "< void * > * ph;"                    )
      ELSE
          aadd( ::cpp_, "   " + ::cWidget + " * ph;"                    )
      ENDIF
   ENDIF
   aadd( ::cpp_, "   bool bNew;"                    )
   aadd( ::cpp_, "   PHBQT_GC_FUNC func;"           )
   aadd( ::cpp_, "   int type;"           )
   aadd( ::cpp_, "} HBQT_GC_T_" + ::cWidget + ";"  )
   aadd( ::cpp_, " "                               )

   aadd( ::cpp_, "HBQT_GC_FUNC( hbqt_gcRelease_" + ::cWidget + " )"  )
   aadd( ::cpp_, "{"                                     )
   IF ( ::isDestructor ) .AND. ( ::isConstructor )
      IF ::isObject
         aadd( ::cpp_, "   " + ::cWidget + " " + iif( ::isList, "< void * >", "" )+" * ph = NULL ;" )
         aadd( ::cpp_, "   HBQT_GC_T_" + ::cWidget + " * p = ( HBQT_GC_T_" + ::cWidget + " * ) Cargo; " )
         aadd( ::cpp_, "   " )
         aadd( ::cpp_, "   if( p && p->bNew && p->ph )" )
         aadd( ::cpp_, "   {" )
         aadd( ::cpp_, "      ph = p->ph; " )
         aadd( ::cpp_, "      if( ph )" )
         aadd( ::cpp_, "      {" )
         aadd( ::cpp_, "         const QMetaObject * m = ( ph )->metaObject();" )
         aadd( ::cpp_, '         if( ( QString ) m->className() != ( QString ) "QObject" )' )
         aadd( ::cpp_, "         {" )
         aadd( ::cpp_, '            HB_TRACE( ' + ::cTrMode + ', ( "ph=%p %p YES_rel_' + ::cWidget + '   /.\\   ", (void*) ph, (void*) p->ph ) );' )
         aadd( ::cpp_, "            delete ( p->ph ); " )
         aadd( ::cpp_, '            HB_TRACE( ' + ::cTrMode + ', ( "ph=%p %p YES_rel_' + ::cWidget + '   \\./   ", (void*) ph, (void*) p->ph ) );' )
         aadd( ::cpp_, "            p->ph = NULL;" )
         aadd( ::cpp_, "         }" )
         aadd( ::cpp_, "         else" )
         aadd( ::cpp_, "         {" )
         aadd( ::cpp_, '            HB_TRACE( ' + ::cTrMode + ', ( "ph=%p NO__rel_' + ::cWidget + '          ", ph ) );')
         aadd( ::cpp_, "            p->ph = NULL;" )
         aadd( ::cpp_, "         }" )
         aadd( ::cpp_, "      }" )
         aadd( ::cpp_, "      else" )
         aadd( ::cpp_, "      {" )
         aadd( ::cpp_, '         HB_TRACE( ' + ::cTrMode + ', ( "ph=%p DEL_rel_' + ::cWidget + '    :     Object already deleted!", ph ) );' )
         aadd( ::cpp_, "         p->ph = NULL;" )
         aadd( ::cpp_, "      }" )
         aadd( ::cpp_, "   }" )
         aadd( ::cpp_, "   else" )
         aadd( ::cpp_, "   {" )
         aadd( ::cpp_, '      HB_TRACE( ' + ::cTrMode + ', ( "ph=%p PTR_rel_' + ::cWidget + '    :    Object not created with new=true", ph ) );' )
         aadd( ::cpp_, "      p->ph = NULL;" )
         aadd( ::cpp_, "   }" )
      ELSE
         aadd( ::cpp_, "   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;" )
         aadd( ::cpp_, "   " )
         aadd( ::cpp_, "   if( p && p->bNew )" )
         aadd( ::cpp_, "   {" )
         aadd( ::cpp_, "      if( p->ph )" )
         aadd( ::cpp_, "      {" )
         aadd( ::cpp_, '         HB_TRACE( ' + ::cTrMode + ', ( "ph=%p    _rel_' + ::cWidget + '   /.\\", p->ph ) );' )
         aadd( ::cpp_, "         delete ( ( " + ::cWidget + IF( ::isList, "< void * >", "" ) + " * ) p->ph ); " )
         aadd( ::cpp_, '         HB_TRACE( ' + ::cTrMode + ', ( "ph=%p YES_rel_' + ::cWidget + '   \\./", p->ph ) );' )
         aadd( ::cpp_, "         p->ph = NULL;" )
         aadd( ::cpp_, "      }" )
         aadd( ::cpp_, "      else" )
         aadd( ::cpp_, "      {" )
         aadd( ::cpp_, '         HB_TRACE( ' + ::cTrMode + ', ( "ph=%p DEL_rel_' + ::cWidget + '    :     Object already deleted!", p->ph ) );' )
         aadd( ::cpp_, "         p->ph = NULL;" )
         aadd( ::cpp_, "      }" )
         aadd( ::cpp_, "   }" )
         aadd( ::cpp_, "   else" )
         aadd( ::cpp_, "   {" )
         aadd( ::cpp_, '      HB_TRACE( ' + ::cTrMode + ', ( "ph=%p PTR_rel_' + ::cWidget + '    :    Object not created with new=true", p->ph ) );' )
         aadd( ::cpp_, "      p->ph = NULL;" )
         aadd( ::cpp_, "   }" )
      ENDIF
   ELSE
      aadd( ::cpp_, "   HB_SYMBOL_UNUSED( Cargo );" )
      aadd( ::cpp_, "   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;" )
      aadd( ::cpp_, "   " )
      aadd( ::cpp_, "   if( p && p->bNew )" )
      aadd( ::cpp_, "   {" )
      aadd( ::cpp_, "      p->ph = NULL;" )
      aadd( ::cpp_, "   }" )
   ENDIF
   aadd( ::cpp_, "}" )
   aadd( ::cpp_, "" )


   aadd( ::cpp_, "void * hbqt_gcAllocate_" + ::cWidget + "( void * pObj, bool bNew )" )
   aadd( ::cpp_, "{                                      " )
   IF ::isObject
      aadd( ::cpp_, "   HBQT_GC_T_" + ::cWidget + " * p = ( HBQT_GC_T_" + ::cWidget + " * ) hb_gcAllocate( sizeof( HBQT_GC_T_" + ::cWidget + " ), hbqt_gcFuncs() );" )
   ELSE
      aadd( ::cpp_, "   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );" )
   ENDIF
   aadd( ::cpp_, "" )
   IF ::isObject
      aadd( ::cpp_, "   new( & p->ph ) QPointer< "+ ::cWidget +" >( ( " + ::cWidget + " * ) pObj );" )
   ELSE
      aadd( ::cpp_, "   p->ph = ( " + ::cWidget + iif( ::isList, "< void * >", "" ) + " * ) pObj;" )
   ENDIF
   aadd( ::cpp_, "   p->bNew = bNew;" )
   aadd( ::cpp_, "   p->func = hbqt_gcRelease_" + ::cWidget + ";" )
   aadd( ::cpp_, "   p->type = HBQT_TYPE_" + ::cWidget + ";" )
   aadd( ::cpp_, "" )
   aadd( ::cpp_, "   if( bNew )" )
   aadd( ::cpp_, "   {" )
   aadd( ::cpp_, '      HB_TRACE( ' + ::cTrMode + ', ( "ph=%p    _new_' + ::cWidget + iif( ::isObject, '  under p->pq', '' ) + '", pObj ) );' )
   aadd( ::cpp_, "   }" )
   aadd( ::cpp_, "   else" )
   aadd( ::cpp_, "   {" )
   aadd( ::cpp_, '      HB_TRACE( ' + ::cTrMode + ', ( "ph=%p NOT_new_' + ::cWidget + '", pObj ) );' )
   aadd( ::cpp_, "   }" )
   aadd( ::cpp_, "   return p;" )
   aadd( ::cpp_, "}" )
   aadd( ::cpp_, "" )

   aadd( ::cpp_, ::new_[ 1 ] )           // Func definition
   aadd( ::cpp_, ::new_[ 2 ] )           // {
   IF ::isConstructor
      if ( ::isList )
          aadd( ::cpp_, "   " + ::cWidget + "< void * > * pObj = NULL;" )
      else
          aadd( ::cpp_, "   " + ::cWidget + " * pObj = NULL;" )
      endif
      aadd( ::cpp_, " " )
      FOR i := 3 TO len( ::new_ ) - 1
          IF !( left( ltrim( ::new_[ i ] ), 2 ) == "//" )
             IF "__HB_RETPTRGC__(" $ ::new_[ i ]
                s := ::new_[ i ]
                s := trim( strtran( s, "__HB_RETPTRGC__(", "pObj =" ) )
                s := strtran( s, ");", ";" )
                s := strtran( s, "( "+ ::cWidget + "* )", "" )
                aadd( ::cpp_, s )
            ELSE
               aadd( ::cpp_, ::new_[ i ] )
            ENDIF
         ENDIF
      NEXT
      aadd( ::cpp_, " " )
      aadd( ::cpp_, "   hb_retptrGC( hbqt_gcAllocate_" + ::cWidget + "( ( void * ) pObj, " + iif( ::isDetached, "false", "true" ) + " ) );" )
   ELSE
      FOR i := 3 TO len( ::new_ ) - 1
          aadd( ::cpp_, ::new_[ i ] )
      NEXT
   ENDIF
   aadd( ::cpp_, ::new_[ len( ::new_ ) ] ) // }
   aadd( ::cpp_, "" )

   /* Insert Functions */
   aeval( ::txt_, {|e| aadd( ::cpp_, strtran( e, chr( 13 ), "" ) ) } )

   /* Footer */
   BuildFooter( @::cpp_ )

   /* Build Document File */
   ::buildDocument()

   /* Distribute in specific lib subfolder */
   cFileCpp := GetSourcePathByLib( ::cWidget, ::cPathOut, ".cpp", "" )
   CreateTarget( cFileCpp, ::cpp_ )

   /* Build CLASS */
   IF !empty( ::cls_ )
      ::buildClass()
      ::cPRG := ::cWidget
   ELSE
      ::cPRG := ""
   ENDIF
   ::cCPP := ::cWidget

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtSource:buildMethodBody( oMtd )
   LOCAL n, FP, cFunc

   oMtd:cCmd := StrTran( oMtd:cCmd, "(  )", "()" ) + ";"

   IF ( n := ascan( ::func_, {|e_| e_[ 1 ] == oMtd:cFun } ) ) > 0
      ::func_[ n,2 ]++
      oMtd:cHBFunc := oMtd:cFun + "_" + hb_ntos( ::func_[ n, 2 ] )
      oMtd:isSibling := .t.
      IF ( n := ascan( ::aMethods, {|o| o:cFun == oMtd:cFun } ) ) > 0  /* and it must be */
         ::aMethods[ n ]:nSiblings++
      ENDIF
   ELSE
      oMtd:cHBFunc := oMtd:cFun
      aadd( ::func_, { oMtd:cFun, 0, "" } )
   ENDIF

   aadd( ::txt_, "/*" )
   aadd( ::txt_, " * " + strtran( oMtd:cProto, chr(13), "" ) )
   aadd( ::txt_, " */" )
   aadd( ::txt_, "HB_FUNC( QT_" + upper( ::cWidget ) + "_" + upper( oMtd:cHBFunc ) + " )" )
   aadd( ::txt_, "{" )
   IF !empty( oMtd:fBody_ )
      aeval( oMtd:fBody_, {|e| aadd( ::txt_, e ) } )

   ELSE
      IF ! oMtd:isConstructor
         aadd( ::txt_, "   " + ::cWidget + iif( ::isList, "< void *>", "" ) + " * p = hbqt_par_" + ::cWidget + "( 1 );" )
      ENDIF

      /* Insert parameters by reference */
      IF ! empty( oMtd:aPre )
         FOR n := 1 TO len( oMtd:aPre )
            aadd( ::txt_, "   " + oMtd:aPre[ n, 1 ] )
         NEXT
         aadd( ::txt_, "" )
      ENDIF

      /* One line function body */
      IF ! oMtd:isConstructor
         FP = strtran( oMtd:cCmd, "hbqt_par_" + ::cWidget + "( 1 )", "( p )" )
         aadd( ::txt_, "   if( p )" )
         aadd( ::txt_, "   {" )
      ELSE
         FP := oMtd:cCmd
      ENDIF
      //
      /* Manage detached Argument */
      IF oMtd:nDetach > 0
         #if 0
         aadd( ::txt_, "      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), " + hb_ntos( oMtd:nDetach + 1 ) + " );" )
         aadd( ::txt_, "      if( q && q->ph )" )
         aadd( ::txt_, "      {" )
         aadd( ::txt_, "         q->bNew = false;" )
         aadd( ::txt_, "      }" )
         #endif
         aadd( ::txt_, "      hbqt_detachgcpointer( " + hb_ntos( oMtd:nDetach + 1 ) + " );" )
      ENDIF

      IF "hb_parstr_utf8(" $ oMtd:cCmd
         aadd( ::txt_, "      void * pText;" )
      ENDIF

      IF ! oMtd:isConstructor
         aadd( ::txt_, "      " + FP )
      ELSE
         aadd( ::txt_, "   " + FP )
      ENDIF

      IF "hb_parstr_utf8(" $ oMtd:cCmd
         aadd( ::txt_, "      hb_strfree( pText );" )
      ENDIF
      //
      IF ! oMtd:isConstructor
         aadd( ::txt_, "   }" )
      ENDIF

      /* Return values back to PRG */
      IF ! empty( oMtd:aPre )
         aadd( ::txt_, "" )
         FOR n := 1 TO len( oMtd:aPre )
            aadd( ::txt_, "   " + oMtd:aPre[ n, 4 ] + "( " + oMtd:aPre[ n, 3 ] + ", " + hb_ntos( oMtd:aPre[ n, 2 ] ) + " );" )
         NEXT
      ENDIF
   ENDIF
   aadd( ::txt_, "}" )
   aadd( ::txt_, ""  )

   cFunc := iif( ::areMethodsClubbed, stripLastFrom( oMtd:cHBFunc, "_" ), oMtd:cHBFunc )

   oMtd:cDoc := "Qt_" + ::cWidget + "_" + cFunc + "( p" + ::cWidget + ;
                     iif( empty( oMtd:cDocs ), "", ", " + oMtd:cDocs ) + " ) -> " + oMtd:cPrgRet

   aadd( ::doc_, oMtd:cDoc )
   aadd( ::doc_, "" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtSource:buildClass()
   LOCAL s, n, cM, cC, oMtd, oArg, cP, txt_:={}, a_, cMtd, lClub

   lClub := ascan( ::cls_, {|e_| lower( e_[ 1 ] ) == "clubmethods" .AND. lower( e_[ 2 ] ) == "no"  } ) == 0

   FOR EACH oMtd IN ::aMethods
      cM := "" ; cC := "::pPtr, "
      IF oMtd:nArgs > 0
         FOR EACH oArg IN oMtd:hArgs
            cP := strtran( strtran( oArg:cDoc, "@", "" ), "::", "_" )
            cM += cP + ", "
            cC += iif( left( cP, 1 ) == "p", "__hbqt_ptr( " + cP + " )", cP ) + ", "
         NEXT
      ENDIF
      cM := alltrim( cM ) ; cM := iif( right( cM,1 ) == ",", substr( cM, 1, len( cM ) - 1 ), cM )
      cC := allTrim( cC ) ; cC := iif( right( cC,1 ) == ",", substr( cC, 1, len( cC ) - 1 ), cC )

      cM :=                           oMtd:cHBFunc + "(" + iif( empty( cM ), "", " " ) + cM + iif( empty( cM ), "", " " ) + ")"
      cC := "Qt_" + ::cWidget + "_" + oMtd:cHBFunc + "(" + iif( empty( cC ), "", " " ) + cC + iif( empty( cC ), "", " " ) + ")"

      oMtd:cMtdDef  := cM
      oMtd:cMtdCall := cC
      oMtd:areFuncClubbed := lClub
   NEXT

   BuildCopyrightText( @txt_, 1 )

   aadd( txt_, "" )
   aadd( txt_, "REQUEST __HB" + Upper( FNameGetName( ::cProfile ) ) )
   aadd( txt_, "" )
   aadd( txt_, "" )
   aadd( txt_, "FUNCTION " + ::cWidget + "( ... )" )
   aadd( txt_, "   RETURN HB_" + ::cWidget + "():new( ... )" )
   aadd( txt_, "" )
   aadd( txt_, "FUNCTION " + ::cWidget + "FromPointer( ... )" )
   aadd( txt_, "   RETURN HB_" + ::cWidget + "():fromPointer( ... )" )
   aadd( txt_, "" )
   aadd( txt_, "" )

   n := ascan( ::cls_, {|e_| left( lower( e_[ 1 ] ), 7 ) == "inherit" .and. !empty( e_[ 2 ] ) } )
   s := "CREATE CLASS " + ::cWidget + " INHERIT HbQtObjectHandler" + iif( n > 0, ", " + strtran( ::cls_[ n, 2 ], "Q", "HB_Q" ), "" ) + " FUNCTION HB_" + ::cWidget

   aadd( txt_, s                                            )
   aadd( txt_, "   "                                        )
   aadd( txt_, "   METHOD  new( ... )"                      )
   aadd( txt_, "   "                                        )
   FOR EACH oMtd IN ::aMethods
      IF oMtd:areFuncClubbed
         cMtd := "   METHOD  " + oMtd:cHBFunc //+ "( ... )"
      ELSE
         cMtd := "   METHOD  " + oMtd:cHBFunc
      ENDIF
      cMtd := pad( cMtd, max( 40, len( cMtd ) ) ) + ;
                                  " // " + pad( "( " +  oMtd:cDocs + " )", max( 50, len( "( " +  oMtd:cDocs + " )" ) ) ) + ;
                                  " -> " + prgRetNormalize( oMtd:cPrgRet )

      IF oMtd:areFuncClubbed
         IF ! oMtd:isSibling
            aadd( txt_, cMtd )
         ELSE
            aadd( txt_, space( 40 ) + ;
                                  " // " + pad( "( " +  oMtd:cDocs + " )", max( 50, len( "( " +  oMtd:cDocs + " )" ) ) ) + ;
                                  " -> " + prgRetNormalize( oMtd:cPrgRet ) )
         ENDIF
      ELSE
         aadd( txt_, cMtd                                   )
      ENDIF
   NEXT
   aadd( txt_, "   "                                        )
   aadd( txt_, "   ENDCLASS"                                )
   aadd( txt_, "   "                                        )
   aadd( txt_, "   "                                        )
   aadd( txt_, "METHOD " + ::cWidget + ":new( ... )"        )
   aadd( txt_, "   LOCAL p"                                 )
   aadd( txt_, "   FOR EACH p IN { ... }"                   )
   aadd( txt_, "      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )" )
   aadd( txt_, "   NEXT"                                    )
   aadd( txt_, "   ::pPtr := Qt_" + ::cWidget + "( ... )"   )
   aadd( txt_, "   RETURN Self"                             )
   aadd( txt_, "   "                                        )

   FOR EACH oMtd IN ::aMethods
      IF "..." $ oMtd:cMtdDef                           /* reworked at c++ level - embed as is */
         aadd( txt_, ""                                           )
         aadd( txt_, "METHOD " + ::cWidget + ":" + oMtd:cMtdDef   )
         hbide_addReturnMethod( txt_, oMtd, ::cWidget, 3, 1, .f., .f., 0, NIL )
         aadd( txt_, ""                                           )

      ELSEIF oMtd:areFuncClubbed .AND. oMtd:nSiblings > 0             /* has more calls with same name */
         aadd( txt_, ""                                           )
         aadd( txt_, "METHOD " + ::cWidget + ":" + oMtd:cHBFunc + "( ... )"  )
         a_:= hbide_pullSameMethods( oMtd:cFun, ::aMethods, ::cWidget )
         aeval( a_, {|e| aadd( txt_, e ) } )
         aadd( txt_, "   RETURN __hbqt_error()"        )
         aadd( txt_, ""                                           )

      ELSEIF  oMtd:areFuncClubbed .AND. oMtd:isSibling                /* is another call with same name handedlled previously - do nothing */
         // Skip

      ELSE                                   /* as usual */
         IF oMtd:areFuncClubbed
            aadd( txt_, ""                                           )
            aadd( txt_, "METHOD " + ::cWidget + ":" + oMtd:cHBFunc + "( ... )"  )
            a_:= hbide_pullSameMethods( oMtd:cFun, ::aMethods, ::cWidget )
            aeval( a_, {|e| aadd( txt_, e ) } )
            aadd( txt_, "   RETURN __hbqt_error()"        )
            aadd( txt_, ""                                           )
         ELSE
            aadd( txt_, ""                                           )
            aadd( txt_, "METHOD " + ::cWidget + ":" + oMtd:cHBFunc + "( ... )"  )
            hbide_addReturnMethod( txt_, oMtd, ::cWidget, 3, 1, .f., .f., 0, NIL )
            aadd( txt_, ""                                           )
         ENDIF
      ENDIF
   NEXT

   IF !empty( ::subCls_ )
      aadd( txt_, ""                                        )
      aeval( ::subCls_, {|e| aadd( txt_, e ) } )
      aadd( txt_, ""                                        )
   ENDIF

   /* Generate .prg */
   CreateTarget( GetSourcePathByLib( ::cWidget, ::cPathOut, ".prg", "T" ), txt_ )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION prgRetNormalize( cPrgRet )

   cPrgRet := strtran( cPrgRet, "::", "_" )
   cPrgRet := strtran( cPrgRet, "<", "_" )
   cPrgRet := strtran( cPrgRet, " *>", "" )
   cPrgRet := strtran( cPrgRet, "*>", "" )

   RETURN cPrgRet

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_pullSameMethods( cFun, aMethods, cWidget )
   LOCAL i, oMtd, a_:={}, b_:={}, c_:={}, nArgs, n, txt_:={}
   LOCAL cSpc, cCrc, lFirst, nMtds, nTySame, lInIf

   FOR EACH oMtd IN aMethods
      IF oMtd:cFun == cFun
         aadd( a_, oMtd )
      ENDIF
   NEXT
   asort( a_, , , {|e,f| strzero( e:nArgs,2 ) + iif( e:nArgs == 0, "", e:hArgs[ 1 ]:cTypeHB ) > strzero( f:nArgs, 2 ) + iif( f:nArgs == 0, "", f:hArgs[ 1 ]:cTypeHB )  } )

   /* know the maximum groups by number of parameters - first CASE */
   aeval( a_, {|o| iif( ascan( b_, o:nArgs     ) == 0, aadd( b_, o:nArgs     ), NIL ) } )

   /* also take into account optional arguments if any */
   FOR EACH oMtd IN a_
      IF oMtd:nArgsReal < oMtd:nArgs
         FOR i := oMtd:nArgs - 1 TO oMtd:nArgsReal STEP -1
            IF ascan( b_, i ) == 0
               aadd( b_, i )
            ENDIF
         NEXT
      ENDIF
   NEXT

   /* Build the structure number of parameters wise */
   FOR EACH nArgs IN b_
      aadd( c_, { nArgs, {}, {}, {} } )
      n := len( c_ )
      FOR EACH oMtd IN a_
         IF oMtd:nArgs == nArgs
            aadd( c_[ n,2 ], oMtd )
         ENDIF
      NEXT
      /* Again append methods with optional arguments */
      FOR EACH oMtd IN a_
         IF oMtd:nArgsReal < oMtd:nArgs
            FOR i := oMtd:nArgs - 1 TO oMtd:nArgsReal STEP -1
               IF i == nArgs
                  aadd( c_[ n,2 ], oMtd )
               ENDIF
            NEXT
         ENDIF
      NEXT
   NEXT

   /* stack groups based on parameters descending */
   asort( c_, , , {|e,f| e[ 1 ] > f[ 1 ] } )

   /* again sort no of arguments based methods by type of arguments */
   FOR i := 10 TO 0 STEP -1  /* consider maximum 10 arguments */
      IF ( n := ascan( c_, {|e_| e_[ 1 ] == i } ) ) > 0
         a_:= c_[ n,2 ]
         asort( a_, , , {|e,f| TY( e, c_[ n,1 ] ) < TY( f, c_[ n,1 ] ) } )
      ENDIF
   NEXT

   cSpc := "   "
   aadd( txt_, cSpc + "SWITCH PCount()" )
   FOR EACH b_ IN c_
      n := b_[ 1 ]
      a_:= b_[ 2 ]
      aadd( txt_, cSpc + "CASE " + hb_ntos( n ) )      /* number of parameters */
      cCrc    := "xxx"
      nMtds   := 0
      lInIf   := .f.
      nTySame := 0
      IF n > 0
         lFirst := .t.
         aadd( txt_, cSpc + cSpc + "DO CASE" )               /* type of parameters   */
      ENDIF
      FOR EACH oMtd IN a_
         IF n > 0
            IF cCrc != TY( oMtd, n )
               cCrc    := TY( oMtd, n )
               nMtds   := 0
               nTySame := 0
               aeval( a_, {|o| iif( TY( o,n ) == cCrc, nTySame++, NIL ) } )
               lInIf   := oMtd:nArgQCast > 0 .AND. oMtd:nArgQCast <= n .AND. nTySame > 1
               IF ! lFirst
                  lFirst := .t.
                  aadd( txt_, cSpc + cSpc + "ENDCASE" )
               ENDIF
               aadd( txt_, cSpc + cSpc + "CASE " + TY_TYPES( oMtd,n ) )
            ENDIF
         ENDIF
         nMtds++
         hbide_addReturnMethod( txt_, oMtd, cWidget, iif( n == 0, 6, 9 ), nMtds, .t., lInIf, nTySame, n )
      NEXT
      IF n > 0
         aadd( txt_, cSpc + cSpc + "ENDCASE" )
         aadd( txt_, cSpc + cSpc + "EXIT" )
      ENDIF
   NEXT
   aadd( txt_, cSpc + "ENDSWITCH" )

   RETURN txt_

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_addReturnMethod( txt_, oM, cWidget, nInd, nCount, lClubbed, lInIf, nTySame, nArgToCheck )
   LOCAL cFun, sp := space( nInd )
   LOCAL cRetCast := oM:oRet:cCast
   LOCAL cPostFix := ""
   LOCAL cPreFix

   HB_SYMBOL_UNUSED( lClubbed )

   DEFAULT lInIf       TO .f.
   DEFAULT nTySame     TO 0
   DEFAULT nArgToCheck TO oM:nArgs

   cPreFix := ""

   IF    ! ( "::" $ cRetCast ) .AND. ;
         ! ( "<"  $ cRetCast ) .AND. ;
         ! ( cRetCast $ "QString,QRgb" ) .AND. ;
         ( left( cRetCast, 1 ) == "Q" .OR. left( cRetCast, 3 ) == "HBQ" )

      cFun := cRetCast + "FromPointer( " + "Qt_" + cWidget + "_" + oM:cHBFunc + "( ::pPtr, ... )" + " )"

   ELSEIF ( "<" $ cRetCast )
      cFun := "QList" + "FromPointer( " + "Qt_" + cWidget + "_" + oM:cHBFunc + "( ::pPtr, ... )" + " )"

   ELSE
      cFun := "Qt_" + cWidget + "_" + oM:cHBFunc + "( ::pPtr, ... )"

   ENDIF

   IF nTySame > 0 .AND. lInIf
      HB_TRACE( HB_TR_ALWAYS, oM:nArgQCast, oM:nArgs )

      IF oM:nArgQCast == 0
         aadd( txt_, sp + "// " + "RETURN " + cFun + cPostFix )
         HB_TRACE( HB_TR_ALWAYS, "// RETURN " + cFun + cPostFix )  /* needed to refine the engine further */
         IF nTySame > 1 .AND. nCount == nTySame
            aadd( txt_, sp + "ENDSWITCH" )
         ENDIF
         RETURN NIL
      ENDIF

      IF nTySame > 1 .AND. nCount == 1
         aadd( txt_, sp + "SWITCH " + hbide_getSwitch( oM, nArgToCheck ) )
      ENDIF
      IF nTySame > 1
         aadd( txt_, sp + "CASE " + hbide_getCase( oM, nArgToCheck ) )
         aadd( txt_, sp + "   " + cPrefix + "RETURN " + cFun + cPostFix )
      ELSE
         aadd( txt_, sp + "IF __objGetClsName( hb_pvalue( " + hb_ntos( oM:nArgQCast ) + " ) ) == " + '"' + upper( oM:hArgs[ oM:nArgQCast ]:cCast ) + '"' )
         aadd( txt_, sp + "   " + cPrefix + "RETURN " + cFun + cPostFix )
         aadd( txt_, sp + "ENDIF" )
      ENDIF
      IF nTySame > 1 .AND. nCount == nTySame
         aadd( txt_, sp + "ENDSWITCH" )
      ENDIF
   ELSE
      IF nCount > 1
         aadd( txt_, sp + "// " + "RETURN " + cFun + cPostFix )
         HB_TRACE( HB_TR_ALWAYS, "// RETURN " + cFun + cPostFix )  /* needed to refine the engine further */
      ELSE
         IF "..." $ cFun
            aadd( txt_, sp + cPrefix + "RETURN " + cFun + cPostFix )
         ELSE
            aadd( txt_, sp + "RETURN " + cFun + cPostFix )
         ENDIF
      ENDIF
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_getCase( oMtd, nArgs )
   LOCAL nFirst := oMtd:nArgQCast
   LOCAL n, oArg, nNext := 0

   FOR EACH oArg IN oMtd:hArgs
      n := oArg:__enumIndex()
      IF n > nFirst .AND. n <= nArgs
         IF oArg:cTypeHB $ "O" // "PO"
            nNext := n
            EXIT
         ENDIF
      ENDIF
   NEXT

   IF nNext > 0
      RETURN '"' + upper( oMtd:hArgs[ nFirst ]:cCast ) + upper( oMtd:hArgs[ nNext ]:cCast ) + '"'
   ELSE
      RETURN '"' + upper( oMtd:hArgs[ nFirst ]:cCast ) + '"'
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_getSwitch( oMtd, nArgs )
   LOCAL nFirst := oMtd:nArgQCast
   LOCAL n, oArg, nNext := 0

   FOR EACH oArg IN oMtd:hArgs
      n := oArg:__enumIndex()
      IF n > nFirst .AND. n <= nArgs
         IF oArg:cTypeHB $ "PO"
            nNext := n
            EXIT
         ENDIF
      ENDIF
   NEXT

   IF nNext > 0
      RETURN "__objGetClsName( hb_pvalue( " + hb_ntos( nFirst ) + " ) )" + " + " + "__objGetClsName( hb_pvalue( " + hb_ntos( nNext ) + " ) )"
   ELSE
      RETURN "__objGetClsName( hb_pvalue( " + hb_ntos( nFirst ) + " ) )"
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_methodInfo( oMtd )
   LOCAL s := "", oArg
   FOR EACH oArg IN oMtd:hArgs
      s += oArg:cTypeHB + " " + left( oArg:cDoc, 1 ) + " " + oArg:cCast + ", "
   NEXT
   s := trim( s )
   s := iif( right( s,1 ) == ",", substr( s, 1, len( s ) - 1 ), s )
   RETURN s

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_paramCheckStr( cType, nArg )

   SWITCH cType
   CASE "PB"
//      RETURN "( " + "hb_isBlock( hb_pvalue( " + hb_ntos( nArg ) + " ) )" + " .OR. " + "hb_isPointer( hb_pvalue( " + hb_ntos( nArg ) + " ) ) )"
      RETURN "( " + " hb_pvalue( " + hb_ntos( nArg ) + " )" + " != NIL )"
   CASE "P"
      RETURN "hb_isPointer( hb_pvalue( " + hb_ntos( nArg ) + " ) )"
   CASE "O"
      RETURN "hb_isObject( hb_pvalue( " + hb_ntos( nArg ) + " ) )"
   CASE "CO"
      RETURN "( hb_isObject( hb_pvalue( " + hb_ntos( nArg ) + " ) ) .OR. hb_isChar( hb_pvalue( " + hb_ntos( nArg ) + " ) ) )"
   CASE "N"
      RETURN  "hb_isNumeric( hb_pvalue( " + hb_ntos( nArg ) + " ) )"
   CASE "L"
      RETURN  "hb_isLogical( hb_pvalue( " + hb_ntos( nArg ) + " ) )"
   CASE "C"
      RETURN  "hb_isChar( hb_pvalue( " + hb_ntos( nArg ) + " ) )"
   ENDSWITCH
   RETURN ""

/*----------------------------------------------------------------------*/

STATIC FUNCTION TY_TYPES( oM, nArgs )
   LOCAL i, s := ""
   FOR i := 1 TO nArgs
      s += hbide_paramCheckStr( oM:hArgs[ i ]:cTypeHB, i ) + " .AND. "
   NEXT
   IF " .AND. " $ s
      s := substr( s, 1, len( s ) - 7 )
   ENDIF
   RETURN s

/*----------------------------------------------------------------------*/

STATIC FUNCTION TY( oM, nArgs )
   LOCAL i, s := ""
   FOR i := 1 TO nArgs
      s += pad( oM:hArgs[ i ]:cTypeHB, 3 )
   NEXT
   RETURN s

/*----------------------------------------------------------------------*/

METHOD HbQtSource:buildDocument()
   LOCAL cText, n, n1, n2, nLen, pWidget, cRet, cLib, cFile, i, cInherits, cHdr

   LOCAL hEntry := { => }

   hb_HKeepOrder( hEntry, .T. )

   n := ascan( ::cls_, {|e_| left( lower( e_[ 1 ] ), 7 ) $ "inherits" .and. !empty( e_[ 2 ] ) } )
   cInherits := iif( n > 0, ::cls_[ n, 2 ], "" )

   cLib := FNameGetName( ::cProFile )

   hEntry[ "TEMPLATE"     ] := "    " + "Class"
   hEntry[ "NAME"         ] := "    " + ::cWidget + "()"
   hEntry[ "CATEGORY"     ] := "    " + "Harbour Bindings for Qt"
   hEntry[ "SUBCATEGORY"  ] := "    " + "GUI"
   hEntry[ "EXTERNALLINK" ] := "    " + QT_WEB + QT_VER + "/" + lower( ::cWidget ) + ".html"
   hEntry[ "ONELINER"     ] := "    " + "Creates a new " + ::cWidget + " object."
   hEntry[ "INHERITS"     ] := "    " + cInherits
   hEntry[ "SYNTAX"       ] := "    " + ::cWidget + "( ... )" + hb_eol()
   hEntry[ "ARGUMENTS"    ] := ""
   hEntry[ "RETURNS"      ] := "    " + "An instance of the object of type " + ::cWidget
   IF ! Empty( ::doc_ )
      hEntry[ "METHODS"      ] := ""
      nLen    := len( ::cWidget )
      n       := at( ::cWidget, ::doc_[ 1 ] )
      pWidget := "p" + ::cWidget
      FOR i := 1 TO len( ::doc_ )
         IF !empty( cText := ::doc_[ i ] )
            cText := substr( cText, n+nLen+1 )
            cText := strtran( cText, pWidget + ", ", "" )
            cText := strtran( cText, pWidget, "" )
            cText := strtran( cText, "(  )", "()" )
            n1    := at( "->", cText )
            cRet  := prgRetNormalize( alltrim( substr( cText, n1+2 ) ) )
            cText := substr( cText, 1, n1-1 )
            n2    := max( 50, len( cText ) )
            cText := padR( cText, n2 )
            IF !empty( cRet )
               hEntry[ "METHODS" ] += "    :" + cText + " -> " + cRet + hb_eol()
            ENDIF
         ENDIF
      NEXT
   ENDIF
   hEntry[ "DESCRIPTION" ] := ""
   hEntry[ "EXAMPLES"    ] := ""
   FOR EACH cText IN ::docum_
      IF !empty( cText )
         hEntry[ "EXAMPLES" ] += "    " + cText + hb_eol()
      ENDIF
   NEXT
   hEntry[ "TESTS"      ] := ""
   hEntry[ "STATUS"     ] := "    " + "R"
   hEntry[ "COMPLIANCE" ] := "    " + "Not Clipper compatible"
   hEntry[ "PLATFORMS"  ] := "    " + "Windows, Linux, Mac OS X, OS/2"
   hEntry[ "VERSION"    ] := "    " + "4.5 or upper"
   hEntry[ "FILES"      ] := ""
   hEntry[ "FILES"      ] += "    " + "Harbour source: " + "contrib/hbqt" + iif( Empty( cLib ), "", "/" + cLib ) + "/T" + ::cWidget + ".prg" + hb_eol()
   hEntry[ "FILES"      ] += "    " + "C++ wrappers  : " + "contrib/hbqt" + iif( Empty( cLib ), "", "/" + cLib ) + "/"  + ::cWidget + ".cpp" + hb_eol()
   hEntry[ "FILES"      ] += "    " + "Library       : " + "hb" + cLib
#if 0
   hEntry[ "SEEALSO"    ] := ""
   hEntry[ "SEEALSO"    ] += "    " + iif( empty( cInherits ), "", cInherits + "()" )
#endif

   cFile := ::cPathDoc + hb_ps() + "en" + hb_ps() + "class_" + lower( ::cWidget ) + ".txt"

   cHdr := ;
      "/*" + hb_eol() +;
      " * $" + "Id" + "$" + hb_eol() +;
      " */" + hb_eol()

   RETURN hb_MemoWrit( cFile, cHdr + __hbdoc_ToSource( { hEntry } ) )

/*----------------------------------------------------------------------*/

METHOD HbQtSource:parseVariables( cProto )
   LOCAL n, oMtd, oRet

   IF ( n := at( " ", cProto ) ) == 0
      RETURN .f.
   ENDIF

   oMtd := HbqtMethod():new()
   oMtd:cProto     := cProto
   oMtd:isVariable := .t.

   oMtd:cPre := cProto

   oMtd:cRet := alltrim( substr( cProto, 1, n - 1 ) )
   oMtd:cFun := alltrim( substr( cProto, n + 1    ) )

   oRet := HbqtArgument():new( oMtd:cRet, ::cWidget, ::enum_, "const" $ oMtd:cPas, .t. )
   oMtd:oRet := oRet

   ::buildCppCode( oMtd )

   RETURN oMtd:lValid

/*----------------------------------------------------------------------*/

METHOD HbQtSource:parseProto( cProto, fBody_ )
   LOCAL aArg, n, nn, cHBIdx, nIndex, s, ss, cFirstParamCast, cArg
   LOCAL oMtd, oRet, oArg, k, cKey, cVal

   IF ( n := at( "(", cProto ) ) == 0
      RETURN .f.
   ENDIF
   IF ( nn := rat( ")", cProto ) ) == 0
      RETURN .f.
   ENDIF

   /*                    Method Parsing                    */
   oMtd := HbqtMethod():new()
   oMtd:cProto := cProto
   oMtd:fBody_ := fBody_

   oMtd:cPre := alltrim( substr( cProto,   1, n-1    ) )
   oMtd:cPar := alltrim( substr( cProto, n+1, nn-1-n ) )
   oMtd:cPas := alltrim( substr( cProto, nn+1        ) )

   IF ( n := at( "[*", oMtd:cPas ) ) > 0
      IF ( nn := at( "*]", oMtd:cPas ) ) > 0
         oMtd:cMrk := alltrim( substr( oMtd:cPas, n + 2, nn - n - 2 ) )
         oMtd:cPas := alltrim( substr( oMtd:cPas, 1, n-1 ) )
         FOR EACH k IN hb_aTokens( oMtd:cMrk, ";" )
            IF ( n := at( "=", k ) ) > 0
               cKey := alltrim( substr( k, 1, n-1 ) )
               cVal := alltrim( substr( k, n+1 ) )
               SWITCH upper( cKey )
               CASE "D"
                  oMtd:nDetach := val( cVal )
                  EXIT
               CASE "xxx"
                  EXIT
               ENDSWITCH
            ENDIF
         NEXT
      ENDIF
   ENDIF
   IF ( n := rat( " ", oMtd:cPre ) ) > 0
      oMtd:cFun := alltrim( substr( oMtd:cPre, n + 1    ) )
      oMtd:cRet := alltrim( substr( oMtd:cPre, 1, n - 1 ) )
   ELSE
      oMtd:cFun := oMtd:cPre
      oMtd:cRet := ""
   ENDIF
   IF empty( oMtd:cRet ) .AND. oMtd:cFun == ::cWidget
      oMtd:isConstructor := .t.
      oMtd:cRet := oMtd:cFun
   ENDIF

   /*                 Return Value Parsing                   */
   oRet := HbqtArgument():new( oMtd:cRet, ::cWidget, ::enum_, "const" $ oMtd:cPas, .t. )
   oMtd:oRet := oRet

   IF !empty( oMtd:cPar )
      /*                 Arguments Parsing                      */
      aArg := hb_ATokens( oMtd:cPar, "," )
      aeval( aArg, {|e,i| aArg[ i ] := alltrim( e ) } )

      FOR EACH cArg IN aArg
         nIndex := cArg:__enumIndex()

         oArg := HbqtArgument():new( cArg, ::cWidget, ::enum_, .f., .f. )
         oMtd:hArgs[ nIndex ] := oArg

         oMtd:nHBIdx := nIndex + 1 // iif( oMtd:isConstructor, 0, 1 )
         cHBIdx := hb_ntos( oMtd:nHBIdx )
         oMtd:cDocNM := THIS_PROPER( oArg:cName )

         oMtd:nArgs++
         oMtd:nArgsOpt += iif( oArg:lOptional, 1, 0 )

         IF empty( cFirstParamCast )
            cFirstParamCast := oArg:cCast
            IF "::" $ cFirstParamCast
               cFirstParamCast := substr( cFirstParamCast, at( "::", cFirstParamCast ) + 2 )
            ENDIF
         ENDIF

         DO CASE
         CASE oArg:cCast == "..."
            oArg:cBody   := "..."
            oArg:cDoc    := "..."
            oArg:cTypeHB := "..."

         CASE oArg:cCast == "PHB_ITEM"
            oArg:cBody   := "hb_param( " + cHBIdx + ", HB_IT_ANY )"
            oArg:cDoc    := "x" + oMtd:cDocNM
            oArg:cTypeHB := "PB"

         CASE oArg:cCast == "T"
            oArg:cBody   := "hb_param( " + cHBIdx + ", HB_IT_ANY )"
            oArg:cDoc    := "x" + oMtd:cDocNM
            oArg:cTypeHB := "P"

         CASE oArg:cCast $ ::cInt .and. oArg:lFar
            aadd( oMtd:aPre, { oArg:cCast + " i" + oMtd:cDocNM + " = 0;", oMtd:nHBIdx, "i" + oMtd:cDocNM, "hb_storni" } )
            oArg:cBody   := "&i" + oMtd:cDocNM
            oArg:cDoc    := "@n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ ::cIntLong .and. oArg:lFar
            aadd( oMtd:aPre, { oArg:cCast + " i" + oMtd:cDocNM + " = 0;", oMtd:nHBIdx, "i" + oMtd:cDocNM, "hb_stornl" } )
            oArg:cBody   := "&i" + oMtd:cDocNM
            oArg:cDoc    := "@n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ ::cIntLongLong .and. oArg:lFar
            aadd( oMtd:aPre, { oArg:cCast + " i" + oMtd:cDocNM + " = 0;", oMtd:nHBIdx, "i" + oMtd:cDocNM, "hb_stornint" } )
            oArg:cBody   := "&i" + oMtd:cDocNM
            oArg:cDoc    := "@n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ ::cInt
            IF !empty( oArg:cDefault ) .AND. !( oArg:cDefault == "0" )
               oArg:cBody := "hb_parnidef( " + cHBIdx + ", " + oArg:cDefault + " )"
            ELSE
               oArg:cBody := "hb_parni( " + cHBIdx + " )"
            ENDIF
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ ::cIntLong
            IF !empty( oArg:cDefault ) .AND. !( oArg:cDefault == "0" )
               oArg:cBody := "hb_parnldef( " + cHBIdx + ", " + oArg:cDefault + " )"
            ELSE
               oArg:cBody := "hb_parnl( " + cHBIdx + " )"
            ENDIF
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ "qlonglong,qulonglong"
            IF !empty( oArg:cDefault ) .AND. !( oArg:cDefault == "0" )
               oArg:cBody := "( " + oArg:cCast + " ) hb_parnintdef( " + cHBIdx + ", " + oArg:cDefault + " )"
            ELSE
               oArg:cBody := "( " + oArg:cCast + " ) hb_parnint( " + cHBIdx + " )"
            ENDIF
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ ::cIntLongLong
            IF !empty( oArg:cDefault ) .AND. !( oArg:cDefault == "0" )
               oArg:cBody := "hb_parnintdef( " + cHBIdx + ", " + oArg:cDefault + " )"
            ELSE
               oArg:cBody := "hb_parnint( " + cHBIdx + " )"
            ENDIF
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ "double,qreal" .and. oArg:lFar
            aadd( oMtd:aPre, { "qreal qr" + oMtd:cDocNM + " = 0;", oMtd:nHBIdx, "qr" + oMtd:cDocNM, "hb_stornd"  } )
            oArg:cBody   := "&qr" + oMtd:cDocNM
            oArg:cDoc    := "@n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast $ "double,qreal,float"
            oArg:cBody   := "hb_parnd( " + cHBIdx + " )"
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast == "uchar" .and. oArg:lFar .and. !( oArg:lConst )
            /* TOFIX: Such code is not valid and should never be generated (const->non-const) [vszakats] */
            oArg:cBody   := "( uchar * ) hb_parc( " + cHBIdx + " )"
            oArg:cDoc    := "c" + oMtd:cDocNM
            oArg:cTypeHB := "C"

         CASE oArg:cCast == "uchar" .and. !( oArg:lFar ) .and. !( oArg:lConst )
            oArg:cBody   := "( uchar ) hb_parni( " + cHBIdx + " )"
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast == "char" .and. oArg:lFar .and. !( oArg:lConst )
            /* TOFIX: Such code is not valid and should never be generated (const->non-const) [vszakats] */
            oArg:cBody   := "( char * ) hb_parc( " + cHBIdx + " )"
            oArg:cDoc    := "c" + oMtd:cDocNM
            oArg:cTypeHB := "C"

         CASE oArg:cCast == "char" .and. oArg:lFar .and. oArg:lConst
            oArg:cBody   := "( const char * ) hb_parc( " + cHBIdx + " )"
            oArg:cDoc    := "c" + oMtd:cDocNM
            oArg:cTypeHB := "C"

         CASE oArg:cCast == "char" .and. !( oArg:lFar ) .and. !( oArg:lConst )
            oArg:cBody   := "( char ) hb_parni( " + cHBIdx + " )"
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE ( "::" $ oArg:cCast ) .and. oArg:lFar
            aadd( oMtd:aPre, { oArg:cCast + " i" + oMtd:cDocNM + " = ( " + oArg:cCast + " ) 0;", oMtd:nHBIdx, "i" + oMtd:cDocNM, "hb_storni" } )
            oArg:cBody   := "&i" + oMtd:cDocNM
            oArg:cDoc    := "@n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE ( "::" $ oArg:cCast )
            s := "( " + oArg:cCast + " ) hb_parni( " + cHBIdx + " )"
            IF !empty( oArg:cDefault ) .AND. !( oArg:cDefault == "0" )
               IF ascan( ::enum_, oArg:cDefault ) > 0
                  ss := ::cWidget + "::" + oArg:cDefault
               ELSE
                  ss := iif( "::" $ oArg:cDefault, oArg:cDefault, ;
                     iif( isDigit( left( oArg:cDefault, 1 ) ), oArg:cDefault, ::cWidget + "::" + oArg:cDefault ) )
               ENDIF
               ss := "( " + oArg:cCast + " ) " + ss
               oArg:cBody := "( HB_ISNUM( " + cHBIdx + " ) ? " + s + " : " + ss + " )"
            ELSE
               oArg:cBody := s
            ENDIF
            oArg:cDoc    := "n" + oMtd:cDocNM
            oArg:cTypeHB := "N"

         CASE oArg:cCast == "bool" .and. oArg:lFar
            aadd( oMtd:aPre, { "bool i" + oMtd:cDocNM + " = 0;", oMtd:nHBIdx, "i" + oMtd:cDocNM, "hb_stornl" } )
            oArg:cBody   := "&i" + oMtd:cDocNM
            oArg:cDoc    := "@l" + oMtd:cDocNM
            oArg:cTypeHB := "L"

         CASE oArg:cCast == "bool"
            oArg:cBody   := "hb_parl( " + cHBIdx + " )"
            oArg:cDoc    := "l" + oMtd:cDocNM
            oArg:cTypeHB := "L"

         CASE oArg:cCast == "QString"
            oArg:cBody   := "hb_parstr_utf8( " + cHBIdx + ", &pText, NULL )"
            oArg:cDoc    := "c" + oMtd:cDocNM  // oArg:cCast - W R O N G
            oArg:cTypeHB := "C"

         CASE oArg:cCast == "FT_Face"
            oArg:cBody   := "hbqt_par_FT_Face( " + cHBIdx + " )"
            oArg:cDoc    := "c" + oMtd:cDocNM
            oArg:cTypeHB := "C"

         CASE oArg:cCast == "QIcon"
            s := "*hbqt_par_QIcon( " + cHBIdx + " )"
            oArg:cBody   := "( HB_ISCHAR( " + cHBIdx + " ) ? " + "QIcon( hbqt_par_QString( " + cHBIdx + " ) )" + " : " + s + ")"
            oArg:cDoc    := "co" + oArg:cCast //oMtd:cDocNM  // "p"
            oArg:cTypeHB := "CO" // "PCO"

         CASE oArg:lFar
            oArg:cBody   := "hbqt_par_" + oArg:cCast + "( " + cHBIdx + " )"
            oArg:cDoc    := "o" + oArg:cCast //oMtd:cDocNM
            oArg:cTypeHB := "O" // "PO"

         CASE oArg:lAnd .AND. oArg:lConst
            s := "*hbqt_par_" + oArg:cCast + "( " + cHBIdx + " )"
            IF !empty( oArg:cDefault ) .and. ( "(" $ oArg:cDefault )
               oArg:cBody := "( HB_ISOBJECT( " + cHBIdx + " ) ? " + s + " : " + oArg:cDefault + " )"
            ELSE
               oArg:cBody := s
            ENDIF
            oArg:cDoc    := "o" + oArg:cCast //oMtd:cDocNM
            oArg:cTypeHB := "O" //"PO"

         CASE oArg:lAnd
            oArg:cBody   := "*hbqt_par_" + oArg:cCast + "( " + cHBIdx + " )"
            oArg:cDoc    := "o" + oArg:cCast //oMtd:cDocNM //p
            oArg:cTypeHB := "O" //"PO"

         CASE oArg:cCast == "QChar"
            oArg:cBody   := "*hbqt_par_" + oArg:cCast + "( " + cHBIdx + " )"
            oArg:cDoc    := "o" + oArg:cCast //oMtd:cDocNM
            oArg:cTypeHB := "O" //"PO"

         OTHERWISE
            oArg:cBody   := ""   /* Just in case */
            oArg:cDoc    := ""
            oArg:cTypeHB := ""

         ENDCASE

         oMtd:cParas += oArg:cBody + ", "
         oMtd:cDocs  += oArg:cDoc + ", "
      NEXT
   ENDIF

   oMtd:nArgsReal := oMtd:nArgs - oMtd:nArgsOpt

   FOR EACH oArg IN oMtd:hArgs
      IF ( left( oArg:cCast, 1 ) == "Q" .OR. left( oArg:cCast, 3 ) == "HBQ" ) .AND. ;
                                            ! ( oArg:cCast $ "QString,QRgb" ) .AND. ;
                                            ! ( "::" $ oArg:cCast )
         oMtd:nArgQCast := oArg:__enumIndex()
         EXIT
      ENDIF
   NEXT
   FOR EACH oArg IN oMtd:hArgs
      IF oArg:cTypeHB $ "O"              // "PO"
         oMtd:nArgHBObj := oArg:__enumIndex()
         EXIT
      ENDIF
   NEXT

   IF right( oMtd:cParas, 2 ) == ", "
      oMtd:cParas := substr( oMtd:cParas, 1, len( oMtd:cParas ) - 2 )
      oMtd:cDocs  := substr( oMtd:cDocs , 1, len( oMtd:cDocs  ) - 2 )
   ENDIF

   ::buildCppCode( oMtd )

   RETURN oMtd:lValid

/*----------------------------------------------------------------------*/

METHOD HbQtSource:buildCppCode( oMtd )
   LOCAL oRet := oMtd:oRet
   LOCAL cPara := oMtd:cParas

   oMtd:cWdg      := "hbqt_par_" + ::cWidget + "( 1 )->"
   oMtd:cParas    := iif( oMtd:isVariable(), "", "( " + oMtd:cParas + " )" )
   oMtd:cCmn      := oMtd:cWdg + oMtd:cFun + oMtd:cParas
   oMtd:cDocNMRet := THIS_PROPER( oRet:cName )

   DO CASE
   CASE oMtd:isConstructor
      oMtd:cCmd := "hb_retptrGC( hbqt_gcAllocate_" + ::cWidget + "( new " + oRet:cCast + "( " + cPara + " ), true ) )"
      oMtd:cPrgRet := "o" + ::cWidget  // "p"

   CASE "<" $ oRet:cCast
      DO CASE
      CASE ! ( "QList" $ oRet:cCast )
         oMtd:cCmd := ""
         oMtd:cPrgRet := ""
      CASE "::" $ oRet:cCast
         oMtd:cCmd := ""
         oMtd:cPrgRet := ""
      CASE "QPair" $ oRet:cCast
         oMtd:cCmd := ""
         oMtd:cPrgRet := ""
      CASE "<T>" $ oRet:cCast
         oMtd:cCmd := ""
         oMtd:cPrgRet := ""
      OTHERWISE
         oMtd:cCmd := "hb_retptrGC( hbqt_gcAllocate_QList( new " + oRet:cCast + "( " + oMtd:cCmn + " ), true ) )"
         oMtd:cPrgRet := "o" + oMtd:cDocNMRet
      ENDCASE

   CASE oRet:cCast == "T"
      /* TOFIX: Such code is not valid and should never be generated [vszakats] */
      oMtd:cCmd := "hb_retptr( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "p" + oMtd:cDocNMRet

   CASE oRet:cCast == "void"
      oMtd:cCmd := oMtd:cCmn
      oMtd:cPrgRet := "NIL"

   CASE oRet:cCast $ ::cInt
      oMtd:cCmd := "hb_retni( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "n" + oMtd:cDocNMRet

   CASE oRet:cCast $ ::cIntLong
      oMtd:cCmd := "hb_retnl( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "n" + oMtd:cDocNMRet

   CASE oRet:cCast $ ::cIntLongLong
      oMtd:cCmd := "hb_retnint( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "n" + oMtd:cDocNMRet

   CASE oRet:cCast $ "double,qreal,float"
      oMtd:cCmd := "hb_retnd( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "n" + oMtd:cDocNMRet

   CASE ( "::" $ oRet:cCast )
      oMtd:cCmd := "hb_retni( ( " + oRet:cCast + " ) " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "n" + oMtd:cDocNMRet

   CASE oRet:cCast == "bool"
      oMtd:cCmd := "hb_retl( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "l" + oMtd:cDocNMRet

   CASE oRet:cCast == "char" .AND. oRet:lFar
      oMtd:cCmd := "hb_retc( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "c" + oMtd:cDocNMRet

   CASE oRet:cCast == "char"
      oMtd:cCmd := "hb_retni( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "c" + oMtd:cDocNMRet

   CASE oRet:cCast == "QString"
      oMtd:cCmd := "hb_retstr_utf8( " + oMtd:cCmn + ".toUtf8().data()" + " )"
      oMtd:cPrgRet := "c" + oMtd:cDocNMRet

   CASE oRet:cCast == "FT_Face"
      oMtd:cCmd := "hb_retc( " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "c" + oMtd:cDocNMRet

   CASE oRet:lFar .AND. ( oRet:cCast $ "uchar" )
      oMtd:cCmd := "hb_retc( ( const char * ) " + oMtd:cCmn + " )"
      oMtd:cPrgRet := "c" + oMtd:cDocNMRet //p

   CASE oRet:lFar .AND. !( oRet:lConst )
      IF ( isAqtObject( oRet:cCast ) )
         oMtd:cCmd := Get_Command( oRet:cCast, oMtd:cCmn, .F. )
      ELSE
         /* TOFIX: Such code is not valid and should never be generated [vszakats] */
         oMtd:cCmd := "hb_retptr( ( " + oRet:cCast + "* ) " + oMtd:cCmn + " )"
      ENDIF
      oMtd:cPrgRet := "o" + oMtd:cDocNMRet

   CASE ( isAqtObject( oRet:cCast ) )      .AND. ;
                             oRet:lFar     .AND. ;
                             oRet:lConst   .AND. ;
                             ( "Abstract" $ oRet:cCast )
      oMtd:cCmd := "hb_retptrGC( hbqt_gcAllocate_" + oRet:cCast + "( ( void * ) " + oMtd:cCmn + ", false ) )"
      oMtd:cPrgRet := "o" + oMtd:cDocNMRet  //p

   CASE ( isAqtObject( oRet:cCast ) )      .AND. ;
                             oRet:lFar     .AND. ;
                             oRet:lConst   .AND. ;
                             oRet:lVirt
      oMtd:cCmd := "hb_retptrGC( hbqt_gcAllocate_" + oRet:cCast + "( ( void * ) " + oMtd:cCmn + ", false ) )"
      oMtd:cPrgRet := "o" + oMtd:cDocNMRet  //p

   CASE ( isAqtObject( oRet:cCast ) )      .AND. ;
                             oRet:lFar     .AND. ;
                             oRet:lConst   .AND. ;
                             oRet:lConstL
      oMtd:cCmd := Get_Command_1( oRet:cCast, oMtd:cCmn )
      oMtd:cPrgRet := "o" + oMtd:cDocNMRet //p

   CASE oRet:lAnd .AND. oRet:lConst
      oMtd:cCmd := Get_Command( oRet:cCast, oMtd:cCmn )
      oMtd:cPrgRet := "o" + oMtd:cDocNMRet //p

   CASE oRet:lConst
      oMtd:cCmd := Get_Command( oRet:cCast, oMtd:cCmn )
      oMtd:cPrgRet := "o" + oMtd:cDocNMRet //p

   CASE oRet:lAnd
      oMtd:cCmd := Get_Command( oRet:cCast, oMtd:cCmn )
      oMtd:cPrgRet := "o" + oMtd:cDocNMRet //p

   OTHERWISE
      /* No attribute is attached to return value */
      IF ( isAqtObject( oRet:cCast ) )
         oMtd:cCmd := Get_Command( oRet:cCast, oMtd:cCmn )
         oMtd:cPrgRet := "o" + oMtd:cDocNMRet //p

      ELSE
         oMtd:cError := "<<< " + oMtd:cProto + " | " + oRet:cCast + " >>>"
         oMtd:cCmd := ""
         oMtd:cPrgRet := ""
         OutStd( oMtd:cError + hb_eol() )

      ENDIF
   ENDCASE

   /* Lists to be disabled in parameters - TODO */
   IF "<" $ oMtd:cPar
      oMtd:cCmd := ""
   ENDIF

   IF ( oMtd:lValid := ! Empty( oMtd:cCmd ) )
      aadd( ::aMethods, oMtd )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//                          Class HbqtMethod
/*----------------------------------------------------------------------*/

CLASS HbqtMethod

   DATA   name                                    INIT ""   //  widget
   DATA   isVariable                              INIT .f.
   DATA   lValid                                  INIT .t.
   DATA   nSiblings                               INIT 0    //  names post_fixed by number
   DATA   isSibling                               INIT .f.  //  is nother function with same name
   DATA   isConstructor                           INIT .f.
   DATA   areFuncClubbed                          INIT .t.

   DATA   cProto                                  INIT ""   //  QWidget * widget ( QWidget * parent, const QString & name ) const  [*D=4*]

   DATA   cPre                                    INIT ""   //  ^^^^^^^^^^^^^^^^
   DATA   cPar                                    INIT ""   //                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   DATA   cPas                                    INIT ""   //                                                              ^^^^^
   DATA   cMrk                                    INIT ""   //                                                                       ^^^

   DATA   nDetach                                 INIT 0

   DATA   cFun                                    INIT ""
   DATA   cRet                                    INIT ""

   DATA   cParas                                  INIT ""
   DATA   cDocs                                   INIT ""

   DATA   cDoc                                    INIT ""   // Qt_QWidget_setSize_1( nWidth, nHeight ) -> NIL

   DATA   cError                                  INIT ""
   DATA   cCmd                                    INIT ""
   DATA   cCmn                                    INIT ""
   DATA   cDocNM                                  INIT ""
   DATA   cDocNMRet                               INIT ""
   DATA   cPrgRet                                 INIT ""
   DATA   cWdg                                    INIT ""
   DATA   cHBFunc                                 INIT ""

   DATA   aPre                                    INIT {}
   DATA   nHBIdx
   DATA   nArgQCast                               INIT 0    //  First argument position of type Q*Class
   DATA   nArgHBObj                               INIT 0    //  First argument position of type Q*Class

   DATA   oRet
   DATA   nArgs                                   INIT 0    //  Number of arguments contained
   DATA   nArgsOpt                                INIT 0    //  Number of optional arguments contained
   DATA   nArgsReal                               INIT 0    //  Number of minimum arguments to be supplied

   DATA   hArgs                                   INIT {=>}

   DATA   fBody_                                  INIT {}
   DATA   cpp_                                    INIT {}
   DATA   prg_                                    INIT {}

   DATA   cMtdDef
   DATA   cMtdCall

   METHOD new()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbqtMethod:new()
   hb_hKeepOrder( ::hArgs, .t. )
   RETURN Self

/*----------------------------------------------------------------------*/
//                         Class HbqtArgument
/*----------------------------------------------------------------------*/

CLASS HbqtArgument

   DATA   cRaw
   DATA   cNormal
   DATA   cName
   DATA   cCast                                INIT ""
   DATA   cBody
   DATA   cDoc

   DATA   lRet                                 INIT .f.

   DATA   cTypeHb
   DATA   cTypeQt
   DATA   cObject

   DATA   lConst                               INIT .f.
   DATA   lAnd                                 INIT .f.
   DATA   lFar                                 INIT .f.
   DATA   lVirt                                INIT .f.
   DATA   lConstL                              INIT .f.

   DATA   lList                                INIT .f.

   DATA   lOptional                            INIT .f.
   DATA   cDefault

   METHOD new( cTxt, cWidget, enum_, lConstL, lIsRetArg )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbqtArgument:new( cTxt, cWidget, enum_, lConstL, lIsRetArg )
   LOCAL n

   ::cRaw    := cTxt
   ::lRet    := lIsRetArg
   ::lList   := "<" $ cTxt

   ::lConst  := "const"   $ cTxt
   ::lAnd    := "&"       $ cTxt
   ::lFar    := "*"       $ cTxt
   ::lVirt   := "virtual" $ cTxt
   ::lConstL := lConstL

   IF ( n := at( "=", cTxt ) ) > 0
      ::cDefault  := alltrim( substr( cTxt, n+1 ) )
      ::lOptional := .t.
      cTxt := substr( cTxt, 1, n-1 )
   ENDIF

   cTxt := strtran( cTxt, "virtual ", ""  )
   cTxt := strtran( cTxt, "const "  , ""  )
   cTxt := strtran( cTxt, "   "     , " " )
   cTxt := strtran( cTxt, "  "      , " " )
   IF ! ::lList
      cTxt := strtran( cTxt, "& "   , ""  )
      cTxt := strtran( cTxt, "&"    , ""  )
      cTxt := strtran( cTxt, "* "   , ""  )
      cTxt := strtran( cTxt, "*"    , ""  )
   ENDIF
   ::cNormal := cTxt := alltrim( cTxt )

   IF ::lList
      ::cCast := cTxt
      ::cName := ::cCast
   ELSE
      IF ( n := at( " ", cTxt ) ) > 0
         ::cCast := substr( cTxt, 1, n-1 )
         ::cName := substr( cTxt, n+1 )
      ELSE
         ::cCast := cTxt
         ::cName := cTxt
      ENDIF
   ENDIF

   IF ascan( enum_, {|e| iif( empty( e ), .f., e == ::cCast ) } ) > 0
      ::cCast := cWidget + "::" + ::cCast
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//                        Helper Functions
/*----------------------------------------------------------------------*/

STATIC FUNCTION Get_Command_1( cWgt, cCmn )

   RETURN "hb_retptrGC( hbqt_gcAllocate_" + cWgt + "( new " + cWgt + "( *( " + cCmn + " ) ), true ) )"

/*----------------------------------------------------------------------*/

STATIC FUNCTION Get_Command( cWgt, cCmn, lNew )

   DEFAULT lNew TO .T.
   IF lNew
      RETURN "hb_retptrGC( hbqt_gcAllocate_" + cWgt + "( new " + cWgt + "( " + cCmn + " ), true ) )"
   ELSE
      RETURN "hb_retptrGC( hbqt_gcAllocate_" + cWgt + "( " + cCmn + ", false ) )"
   ENDIF
   RETURN ""

/*----------------------------------------------------------------------*/

STATIC FUNCTION ParsePtr( cParam )
   LOCAL cPar := ""
   LOCAL s, n


   IF at( " p" , cParam ) > 0
      DO WHILE .t.

         IF ( n := at( " p" , cParam ) ) > 0
            cPar += substr( cParam, 1, n )
            cParam := substr( cParam, n+1 )

            IF ( n := at( ",", cParam ) ) > 0
               s := substr( cParam, 1, n-1 )
               cParam := substr( cParam, n )
               cPar += "__hbqt_ptr( " + s + " )"

            ELSEIF ( n := at( " ", cParam ) ) > 0
               s := substr( cParam, 1, n-1 )
               cParam := substr( cParam, n )
               cPar += "__hbqt_ptr( " + s + " )"
               cPar += cParam
               EXIT

            ENDIF

         ELSE
            cPar += cParam
            EXIT

         ENDif
      ENDDO

   ELSE
      cPar := cParam

   ENDIF

   RETURN cPar

/*----------------------------------------------------------------------*/

STATIC FUNCTION FNameGetName( cFileName )
   LOCAL cName
   hb_FNameSplit( cFileName,, @cName )
   RETURN cName

/*----------------------------------------------------------------------*/

FUNCTION GetSourcePathByLib( cWidget, cPathOut, cExt, cPre )
   RETURN cPathOut + hb_ps() + cPre + cWidget + cExt

/*----------------------------------------------------------------------*/

STATIC FUNCTION PullOutSection( cQth, cSec )
   LOCAL cTxt, n, nn, cTknB, cTknE
   LOCAL a_:={}

   cTknB := "<" + cSec + ">"
   cTknE := "</" + cSec + ">"

   IF ( n := at( cTknB, cQth ) ) > 0
      IF( nn := at( cTknE, cQth ) ) > 0
         cTxt := substr( cQth, n+len( cTknB ), nn-1-( n+len( cTknB ) ) )
      ENDIF
      IF !empty( cTxt )
         a_:= hb_ATokens( cTxt, _EOL )
      ENDIF
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

STATIC FUNCTION PullOutFuncBody( protos_, nFrom )
   LOCAL s, nTo := 0,  a_:= {}

   FOR EACH s IN protos_
      IF s:__enumIndex() > nFrom
         IF left( s, 1 ) == "}"
            nTo := s:__enumIndex()
            EXIT
         ENDIF
      ENDIF
   NEXT
   IF nTo > nFrom
      FOR EACH s IN protos_
         IF s:__enumIndex() > nFrom .AND. s:__enumIndex() < nTo
            aadd( a_, s )
            s := ""
         ENDIF
      NEXT
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

STATIC FUNCTION isAqtObject( cCast )
   RETURN left( cCast, 1 ) == "Q" .OR. left( cCast, 3 ) == "HBQ"

/*----------------------------------------------------------------------*/

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )

/*----------------------------------------------------------------------*/

STATIC FUNCTION CreateTarget( cFile, txt_ )
   LOCAL cContent := ""

   AEval( txt_, { |e| cContent += RTrim( e ) + hb_eol() } )

   /* Save it only if it has changed. */
   IF !( hb_MemoRead( cFile ) == cContent )

      OutStd( "Creating: " + cFile + hb_eol() )

      hb_MemoWrit( cFile, cContent )
   ENDIF

   RETURN hb_FileExists( cFile )

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildCopyrightText( txt_, nMode, cProFile )

   aadd( txt_, "/*"                                                                            )
   aadd( txt_, " * $" + "Id" + "$"                                                             )
   aadd( txt_, " */"                                                                           )
   aadd( txt_, ""                                                                              )
   aadd( txt_, "/* -------------------------------------------------------------------- */"    )
   aadd( txt_, "/* WARNING: Automatically generated source file. DO NOT EDIT!           */"    )
   aadd( txt_, "/*          Instead, edit corresponding .qth file,                      */"    )
   aadd( txt_, "/*          or the generator tool itself, and run regenarate.           */"    )
   aadd( txt_, "/* -------------------------------------------------------------------- */"    )
   aadd( txt_, ""                                                                              )
   aadd( txt_, "/* "                                                                           )
   aadd( txt_, " * Harbour Project source code:"                                               )
   aadd( txt_, " * QT wrapper main header"                                                     )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>"                 )
   aadd( txt_, " * www - http://harbour-project.org"                                           )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This program is free software; you can redistribute it and/or modify"       )
   aadd( txt_, " * it under the terms of the GNU General Public License as published by"       )
   aadd( txt_, " * the Free Software Foundation; either version 2, or (at your option)"        )
   aadd( txt_, " * any later version."                                                         )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This program is distributed in the hope that it will be useful,"            )
   aadd( txt_, " * but WITHOUT ANY WARRANTY; without even the implied warranty of"             )
   aadd( txt_, " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"              )
   aadd( txt_, " * GNU General Public License for more details."                               )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * You should have received a copy of the GNU General Public License"          )
   aadd( txt_, " * along with this software; see the file COPYING.  If not, write to"          )
   aadd( txt_, " * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,"            )
   aadd( txt_, " * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/)."     )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * As a special exception, the Harbour Project gives permission for"           )
   aadd( txt_, " * additional uses of the text contained in its release of Harbour."           )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * The exception is that, if you link the Harbour libraries with other"        )
   aadd( txt_, " * files to produce an executable, this does not by itself cause the"          )
   aadd( txt_, " * resulting executable to be covered by the GNU General Public License."      )
   aadd( txt_, " * Your use of that executable is in no way restricted on account of"          )
   aadd( txt_, " * linking the Harbour library code into it."                                  )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This exception does not however invalidate any other reasons why"           )
   aadd( txt_, " * the executable file might be covered by the GNU General Public License."    )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * This exception applies only to the code released by the Harbour"            )
   aadd( txt_, " * Project under the name Harbour.  If you copy code from other"               )
   aadd( txt_, " * Harbour Project or Free Software Foundation releases into a copy of"        )
   aadd( txt_, " * Harbour, as the General Public License permits, the exception does"         )
   aadd( txt_, " * not apply to the code that you add in this way.  To avoid misleading"       )
   aadd( txt_, " * anyone as to the status of such modified files, you must delete"            )
   aadd( txt_, " * this exception notice from them."                                           )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * If you write modifications of your own for Harbour, it is your choice"      )
   aadd( txt_, " * whether to permit this exception to apply to your modifications."           )
   aadd( txt_, " * If you do not wish that, delete this exception notice."                     )
   aadd( txt_, " *"                                                                            )
   aadd( txt_, " */"                                                                           )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, "/*                            C R E D I T S                             */"    )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, "/* "                                                                           )
   aadd( txt_, " * Marcos Antonio Gambeta"                                                     )
   aadd( txt_, " *    for providing first ever prototype parsing methods. Though the current"  )
   aadd( txt_, " *    implementation is diametrically different then what he proposed, still"  )
   aadd( txt_, " *    current code shaped on those footsteps."                                 )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Viktor Szakats"                                                             )
   aadd( txt_, " *    for directing the project with futuristic vision; "                      )
   aadd( txt_, " *    for designing and maintaining a complex build system for hbQT, hbIDE;"   )
   aadd( txt_, " *    for introducing many constructs on PRG and C++ levels;"                  )
   aadd( txt_, " *    for streamlining signal/slots and events management classes;"            )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Istvan Bisz"                                                                )
   aadd( txt_, " *    for introducing QPointer<> concept in the generator;"                    )
   aadd( txt_, " *    for testing the library on numerous accounts;"                           )
   aadd( txt_, " *    for showing a way how a GC pointer can be detached;"                     )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Francesco Perillo"                                                          )
   aadd( txt_, " *    for taking keen interest in hbQT development and peeking the code;"      )
   aadd( txt_, " *    for providing tips here and there to improve the code quality;"          )
   aadd( txt_, " *    for hitting bulls eye to describe why few objects need GC detachment;"   )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Carlos Bacco"                                                               )
   aadd( txt_, " *    for implementing HBQT_TYPE_Q*Class enums;"                               )
   aadd( txt_, " *    for peeking into the code and suggesting optimization points;"           )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Przemyslaw Czerpak"                                                         )
   aadd( txt_, " *    for providing tips and trick to manipulate HVM internals to the best"    )
   aadd( txt_, " *    of its use and always showing a path when we get stuck;"                 )
   aadd( txt_, " *    A true tradition of a MASTER..."                                         )
   aadd( txt_, "*/ "                                                                           )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, ""                                                                              )
   IF nMode == 0
      IF !( FNameGetName( cProFile ) == "qtcore" )
         aadd( txt_, '#include "hbqtcore.h"'                                                   )
      ENDIF
   aadd( txt_, '#include "hb' + FNameGetName( cProFile ) + '.h"'                               )
   aadd( txt_, ""                                                                              )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, "#if QT_VERSION >= 0x040500"                                                    )
   aadd( txt_, "/*----------------------------------------------------------------------*/"    )
   aadd( txt_, ""                                                                              )
   ELSEIF nMode == 1
   aadd( txt_, ""                                                                              )
   aadd( txt_, '#include "hbclass.ch"'                                                         )
   aadd( txt_, ""                                                                              )
   ELSEIF nMode == 2
   /* Pure Header */
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildFooter( txt_ )

   aadd( txt_, ""                                                                             )
   aadd( txt_, "/*----------------------------------------------------------------------*/"   )
   aadd( txt_, "#endif             /* #if QT_VERSION >= 0x040500 */"                          )
   aadd( txt_, "/*----------------------------------------------------------------------*/"   )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispHelp()
   LOCAL cHlp := ""

   cHlp += ""                                                                               + hb_eol()
   cHlp += "Syntax:"                                                                        + hb_eol()
   cHlp += "   hbqtgen.exe [Options] [[@]<QtProjectFile.qtp>] [<QtHeaderFile.qth, ...>]"    + hb_eol()
   cHlp += ""                                                                               + hb_eol()
   cHlp += "Options:"                                                                       + hb_eol()
   cHlp += "   -O<OutputPath>   [ e.g. c:\harbour\contrib\hbqt ]        [D] Current folder" + hb_eol()
   cHlp += "   -I<InputPath>    [ e.g. c:\harbour\contrib\hbqt\protos ] [D] Current folder" + hb_eol()
   cHlp += "   -D<DocFilesPath> [ e.g. c:\harbour\contrib\hbqt\doc    ] [D] Current folder" + hb_eol()
   cHlp += " "                                                                              + hb_eol()
   cHlp += "   -c<compile>      If QT env is set, attempts to compile resulting .cpp"       + hb_eol()
   cHlp += ""                                                                               + hb_eol()
   cHlp += "   -noretobject     Skip object returning methods"                              + hb_eol()
   cHlp += ""                                                                               + hb_eol()

   OutStd( cHlp )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispLogo()
   LOCAL cHlp := ""

   cHlp += ""                                                        + hb_eol()
   cHlp += "Harbour Source Generator for QT " + HBRawVersion()       + hb_eol()
   cHlp += "Copyright (c) 2009, Pritpal Bedi <bedipritpal@hotmail.com>" + hb_eol()
   cHlp += "http://harbour-project.org/"                             + hb_eol()
   cHlp += ""                                                        + hb_eol()

   OutStd( cHlp )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION stripLastFrom( cStr, cDlm )
   LOCAL n
   IF ( n := rAt( cDlm, cStr ) ) > 0
      RETURN substr( cStr, 1, n-1 )
   ENDIF
   RETURN cStr

/*----------------------------------------------------------------------*/
