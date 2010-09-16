/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour-Qt wrapper generator.
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "common.ch"
#include "fileio.ch"

#define _EOL   chr( 10 )

/*
 * Force new GC and Qt interface
 */
STATIC s_lNewGCtoQT := .T.
STATIC s_isObject := .F.
STATIC s_trMode   := "HB_TR_DEBUG"
//STATIC s_trMode   := "HB_TR_ALWAYS"

/*----------------------------------------------------------------------*/

FUNCTION Main( ... )
   LOCAL aParam, cLParam
   LOCAL cParam, cPathOut, cPathIn, cProFile, cPathDoc
   LOCAL x, cPath, cFile, cExt, cTrMode, a_
   LOCAL aPrjFiles := {}
   LOCAL aProFiles := {}

   IF !empty( cTrMode := GetEnv( "HBQT_BUILD_TR_LEVEL" ) )
      cTrMode := upper( cTrMode )
      IF cTrMode $ "HB_TR_ALWAYS,HB_TR_WARNING,HB_TR_ERROR"
         s_trMode := cTrMode
      ENDIF
   ENDIF

   DispLogo()

   aParam := hb_AParams()

   FOR EACH cParam IN aParam
      cLParam := lower( cParam )

      DO CASE
      CASE left( cParam,1 ) == "@"
         x := substr( cParam,2 )
         hb_fNameSplit( x, @cPath, @cFile, @cExt )
         IF empty( cExt ) .or. !( lower( cExt ) == ".qtp" )
            cExt := ".qtp"
         ENDIF
         x := iif( empty( cPath ), "", cPath + hb_ps() )+ cFile + cExt
         aadd( aPrjFiles, x )

      CASE right( cLParam,4 ) == ".qtp"
         aadd( aPrjFiles, cParam )

      CASE right( cLParam,4 ) == ".qth"
         aadd( aProFiles, cParam )

      CASE lower( left( cParam,2 ) ) == "-o"
         cPathOut := substr( cParam, 3 )

      CASE lower( left( cParam,2 ) ) == "-i"
         cPathIn := substr( cParam, 3 )

      CASE lower( left( cParam,2 ) ) == "-d"
         cPathDoc := substr( cParam, 3 )

      CASE cParam == "-c"

      CASE cLParam == "-help"
         DispHelp()
         RETURN nil

      ENDCASE
   NEXT

   IF empty( aPrjFiles ) .AND. empty( aProFiles )
      FOR EACH a_ IN directory( "*.qtp" )
         aadd( aPrjFiles, a_[ 1 ] )
      NEXT
   ENDIF

   IF empty( aPrjFiles ) .AND. empty( aProFiles )
      DispHelp()
      RETURN nil
   ENDIF

   IF empty( cPathOut )
      cPathOut := hb_dirBase()
   ENDIF
   IF empty( cPathIn )
      cPathIn  := hb_dirBase()
   ENDIF
   IF empty( cPathDoc )
      cPathDoc := hb_dirBase()
   ENDIF
   IF Right( cPathOut, 1 ) == hb_ps()
      cPathOut := hb_StrShrink( cPathOut, 1 )
   ENDIF
   IF Right( cPathIn, 1 ) == hb_ps()
      cPathIn := hb_StrShrink( cPathIn, 1 )
   ENDIF
   IF Right( cPathDoc, 1 ) == hb_ps()
      cPathDoc := hb_StrShrink( cPathDoc, 1 )
   ENDIF

   /* Manage Project File */
   FOR EACH cProFile IN aPrjFiles
      ManageProject( cProFile, cPathIn, cPathOut, cPathDoc )
   NEXT

   /* Generate .cpp Sources */
   FOR EACH cProFile IN aProFiles
      GenSource( cProFile, cPathIn, cPathOut, cPathDoc, {}, cProFile )
   NEXT

   RETURN nil

STATIC FUNCTION FNameGetName( cFileName )
   LOCAL cName
   hb_FNameSplit( cFileName,, @cName )
   RETURN cName

/*----------------------------------------------------------------------*/

STATIC FUNCTION ManageProject( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cPrj, cTkn, cVal // cPath, cExt,
   LOCAL cPIn, cPOut, cPDoc
   LOCAL n, nn
   LOCAL prj_, cpp_, prg_, a_

   LOCAL aWidgetList := {}

   cFile := cProFile
   IF ! hb_fileExists( cFile )
      RETURN nil
   ENDIF

   cPIn  := cPathIn
   cPOut := cPathOut
   cPDoc := cPathDoc

   cpp_:={}
   prg_:={}

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

         //IF !empty( cVal )
            DO CASE
            CASE cTkn == "-I"
               cPIn := cVal

            CASE cTkn == "-O"
               cPOut := cVal

            CASE cTkn == "-D"
               cPDoc := cVal

            ENDCASE
         //ENDIF

      ELSEIF lower( right( cPrj,4 ) ) == ".qth"
         a_:= GenSource( cPrj, cPIn, cPOut, cPDoc, aWidgetList, cProFile )
         IF !empty( a_[ 1 ] )
            aadd( cpp_, a_[ 1 ] )
            IF !empty( a_[ 2 ] )
               aadd( prg_, a_[ 2 ] )
            ENDIF
         ENDIF

      ENDIF
   NEXT

   IF !empty( cpp_ )
      Build_Makefile( cPOut, aWidgetList, cProFile )
      Build_HeaderFile( cpp_, cPOut, cProFile )
   ENDIF

   RETURN NIL

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

STATIC FUNCTION GenSource( cProFile, cPathIn, cPathOut, cPathDoc, aWidgetList, cProject )
   LOCAL cFile, cWidget, cExt, cPath, cOrg, cCPP, cPRG, lConst //, lList, cWgt
   LOCAL cQth, cFileCpp, s, n, nFuncs, nCnvrtd, n1, i, cFunc, lObject, lDestructor, lList
   LOCAL b_, txt_, enum_, code_, func_, dummy_, cpp_, cmntd_, doc_, varbls_
   LOCAL class_, cls_, protos_, slots_, enums_, docum_, subCls_, new_, old_, fBody_

   hb_fNameSplit( cProFile, @cPath, @cWidget, @cExt )

   IF empty( cPath )
      cFile := cPathIn + hb_ps() + cProFile
   ELSE
      cFile := cProFile
   ENDIF
   IF ! hb_fileExists( cFile )
      OutStd( "Cannot find: " + cFile + hb_eol() )
      RETURN { nil }
   ENDIF

   cQth := memoread( cFile )
   IF empty( cQth )
      OutStd( "Cannot read: " + cFile + hb_eol() )
      RETURN { nil }
   ENDIF

   OutStd( "Processing: " + cFile + hb_eol() )

   /* Prepare to be parsed properly */
   IF ! hb_eol() == _EOL
      cQth := StrTran( cQth, hb_eol(), _EOL )
   ENDIF
   IF ! hb_eol() == Chr( 13 ) + Chr( 10 )
      cQth := StrTran( cQth, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF

   cls_:={}
   IF !empty( class_:= PullOutSection( @cQth, "CLASS" ) )
      FOR EACH s IN class_
         IF ( n := at( "=", s ) ) > 0
            aadd( cls_, { alltrim( substr( s, 1, n-1 ) ), alltrim( substr( s, n+1 ) ) } )
         ENDIF
      NEXT
   ENDIF

   /* Pull out SUBCLASS section */
   subCls_ := PullOutSection( @cQth, "SUBCLASS" )

   /* Pull out Doc Section */
   docum_  := PullOutSection( @cQth, "DOC"   )

   /* Pull out Code Section */
   code_   := PullOutSection( @cQth, "CODE"   )

   /* Separate constructor function */
   new_:={}
   cFunc := "HB_FUNC( QT_" + upper( cWidget ) + " )"
   n := ascan( code_, {|e| cFunc $ e } )
   FOR i := n TO len( code_ )
      aadd( new_, code_[ i ] )
      IF trim( code_[ i ] ) == "}"
         n1 := i
         EXIT
      endif
   NEXT
   old_:={}
   FOR i := 1 TO len( code_ )
      IF i < n .or. i > n1
         aadd( old_, code_[ i ] )
      ENDIF
   NEXT
   code_:= old_

   /* Pull out Enumerators  */
   enums_  := PullOutSection( @cQth, "ENUMS"  )
   enum_:={}
   FOR EACH s IN enums_
      IF ( "enum " $ s .or. "flags " $ s )
         b_:= hb_ATokens( alltrim( s ), " " )
         aadd( enum_, b_[ 2 ] )
      ENDIF
   NEXT

   /* Mark to which sub library class belongs to */
   aadd( aWidgetList, cWidget )

   /* Pull out Prototypes   */
   protos_ := PullOutSection( @cQth, "PROTOS" )

   /* Pull out Variables */
   varbls_ := PullOutSection( @cQth, "VARIABLES" )

   /* Pull Out Signals      */
   slots_  := PullOutSection( @cQth, "SLOTS"  )

   /* Combine signals and protos : same nature */
   aeval( slots_, {|e| aadd( protos_, e ) } )

   dummy_  := {}
   func_   := { { "", 0 } }
   txt_    := {}
   cpp_    := {}
   cmntd_  := {}
   doc_    := {}
   nFuncs  := 0
   nCnvrtd := 0

   s_isObject := ascan( cls_, {|e_| lower( e_[ 1 ] ) == "qobject" .and. lower( e_[ 2 ] ) == "no"} ) == 0

   /* Body */
   lList := ascan( cls_, {|e_| lower( e_[ 1 ] ) == "list"  .AND. lower( e_[ 2 ] ) == "yes" } ) > 0

   FOR EACH s IN protos_
      cOrg := s

      IF empty( s := alltrim( s ) )
         LOOP
      ENDIF

      /* Check if proto is commented out */
      IF left( s,2 ) == "//"
         aadd( cmntd_, cOrg )
         nFuncs++
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

      nFuncs++

      /* Lists - Later */
      #if 0
      IF "<" $ s
         aadd( dummy_, cOrg )
         LOOP
      ENDIF
      #endif

      fBody_:={}
      IF right( s, 1 ) == "{"
         fBody_:= PullOutFuncBody( protos_, s:__enumIndex() )
         s := substr( s, 1, len( s ) - 1 )
      ENDIF
      IF ParseProto( s, cWidget, @txt_, @doc_, enum_, func_, lList, fBody_ )
         nCnvrtd++
      ELSE
         aadd( dummy_, cOrg )
      ENDIF
   NEXT

   FOR EACH s IN varbls_
      cOrg := s

      IF empty( s := alltrim( s ) )
         LOOP
      ENDIF
      /* Check if proto is commented out */
      IF left( s,2 ) == "//"
         aadd( cmntd_, cOrg )
         nFuncs++
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

      nFuncs++

      /* Lists - Later */
      IF "<" $ s
         aadd( dummy_, cOrg )
         LOOP
      ENDIF

      IF ParseVariables( s, cWidget, @txt_, @doc_, enum_, func_ )
         nCnvrtd++
      ELSE
         aadd( dummy_, cOrg )
      ENDIF
   NEXT

   /* Assemble components */
   IF .t.  /* !empty( txt_ ) */

      /* Pull .cpp copyright text */
      BuildHeader( @cpp_, 0, cProject )

      /* Place ENUM definitions into the source */
      #if 1
      IF !empty( enums_ )
         aadd( cpp_, "/*" )
         aeval( enums_, {|e| iif( !empty( e ), aadd( cpp_, " *  " + e ), NIL ) } )
         aadd( cpp_, " */ " )
         aadd( cpp_, "" )
      ENDIF
      #endif

      /* Insert information about prototypes not converted to functions */
      IF !empty( dummy_ )
         aadd( cpp_, "/*" )
         aadd( cpp_, " *  Constructed[ " + hb_ntos( nCnvrtd ) + "/" + hb_ntos( nFuncs ) + " [ " + hb_ntos( nCnvrtd / nFuncs * 100 ) + "% ] ]" )
         aadd( cpp_, " *  " )
         aadd( cpp_, " *  *** Unconvered Prototypes ***" )
         aadd( cpp_, " *  -----------------------------" )
         aadd( cpp_, " *  " )
         aeval( dummy_, {|e| aadd( cpp_, " *  " + e ) } )
         IF !empty( cmntd_ )
            aadd( cpp_, " *  " )
            aadd( cpp_, " *  " + "*** Commented out protos which construct fine but do not compile ***" )
            aadd( cpp_, " *  " )
            aeval( cmntd_, {|e| aadd( cpp_, " *  " + e ) } )
         ENDIF
         aadd( cpp_, " */ " )
         aadd( cpp_, "" )
      ENDIF

      /* Insert user defined code - INCLUDEs */
      aadd( cpp_, "#include <QtCore/QPointer>" )
      IF !empty( code_ )
         aeval( code_, {|e| aadd( cpp_, strtran( e, chr( 13 ), "" ) ) } )
         aadd( cpp_, "" )
      ENDIF

      lList       := ascan( cls_, {|e_| lower( e_[ 1 ] ) == "list"       .AND. lower( e_[ 2 ] ) == "yes" } ) > 0
      lDestructor := ascan( cls_, {|e_| lower( e_[ 1 ] ) == "destructor" .AND. lower( e_[ 2 ] ) == "no"  } ) == 0
      lObject     := ascan( cls_, {|e_| lower( e_[ 1 ] ) == "qobject"    .AND. lower( e_[ 2 ] ) == "no"  } ) == 0

      lConst := .f.
      FOR i := 3 TO len( new_ ) - 1
         IF left( ltrim( new_[ i ] ), 2 ) != "//"
            IF "hb_retptr(" $ new_[ i ]
               lConst := .t.
               EXIT
            ENDIF
         ENDIF
      NEXT

      aadd( cpp_, "typedef struct"                  )
      aadd( cpp_, "{"                               )
      IF lObject
         aadd( cpp_, "   QPointer< "+ cWidget +" > ph;" )
      ELSE
         IF lList
             aadd( cpp_, "   " + cWidget + "< void * > * ph;"                    )
         ELSE
             aadd( cpp_, "   " + cWidget + " * ph;"                    )
         ENDIF
      ENDIF
      aadd( cpp_, "   bool bNew;"                    )
      aadd( cpp_, "   PHBQT_GC_FUNC func;"           )
      aadd( cpp_, "   int type;"           )
      aadd( cpp_, "} HBQT_GC_T_" + cWidget + ";"  )
      aadd( cpp_, " "                               )

      aadd( cpp_, "HBQT_GC_FUNC( hbqt_gcRelease_" + cWidget + " )"  )
      aadd( cpp_, "{"                                     )
      IF ( lDestructor ) .AND. ( lConst )
         IF lObject
            aadd( cpp_, "   " + cWidget + " " + iif( lList, "< void * >", "" )+" * ph = NULL ;" )
            aadd( cpp_, "   HBQT_GC_T_" + cWidget + " * p = ( HBQT_GC_T_" + cWidget + " * ) Cargo; " )
            aadd( cpp_, "   " )
            aadd( cpp_, "   if( p && p->bNew && p->ph )" )
            aadd( cpp_, "   {" )
            aadd( cpp_, "      ph = p->ph; " )
            aadd( cpp_, "      if( ph )" )
            aadd( cpp_, "      {" )
            aadd( cpp_, "         const QMetaObject * m = ( ph )->metaObject();" )
            aadd( cpp_, '         if( ( QString ) m->className() != ( QString ) "QObject" )' )
            aadd( cpp_, "         {" )
            aadd( cpp_, '            HB_TRACE( ' + s_trMode + ', ( "ph=%p %p YES_rel_' + cWidget + '   /.\\   ", (void*) ph, (void*) p->ph ) );' )
            aadd( cpp_, "            delete ( p->ph ); " )
         *  aadd( cpp_, '            HB_TRACE( ' + s_trMode + ', ( "ph=%p %p YES_rel_' + cWidget + '   ====   ", (void*) ph, (void*) p->ph ) );' )
         *  aadd( cpp_, "            delete ( ph ); " )
            aadd( cpp_, '            HB_TRACE( ' + s_trMode + ', ( "ph=%p %p YES_rel_' + cWidget + '   \\./   ", (void*) ph, (void*) p->ph ) );' )
            aadd( cpp_, "            p->ph = NULL;" )
            aadd( cpp_, "         }" )
            aadd( cpp_, "         else" )
            aadd( cpp_, "         {" )
            aadd( cpp_, '            HB_TRACE( ' + s_trMode + ', ( "ph=%p NO__rel_' + cWidget + '          ", ph ) );')
            aadd( cpp_, "            p->ph = NULL;" )
            aadd( cpp_, "         }" )
            aadd( cpp_, "      }" )
            aadd( cpp_, "      else" )
            aadd( cpp_, "      {" )
            aadd( cpp_, '         HB_TRACE( ' + s_trMode + ', ( "ph=%p DEL_rel_' + cWidget + '    :     Object already deleted!", ph ) );' )
            aadd( cpp_, "         p->ph = NULL;" )
            aadd( cpp_, "      }" )
            aadd( cpp_, "   }" )
            aadd( cpp_, "   else" )
            aadd( cpp_, "   {" )
            aadd( cpp_, '      HB_TRACE( ' + s_trMode + ', ( "ph=%p PTR_rel_' + cWidget + '    :    Object not created with new=true", ph ) );' )
            aadd( cpp_, "      p->ph = NULL;" )
            aadd( cpp_, "   }" )
         ELSE
            aadd( cpp_, "   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;" )
            aadd( cpp_, "   " )
            aadd( cpp_, "   if( p && p->bNew )" )
            aadd( cpp_, "   {" )
            aadd( cpp_, "      if( p->ph )" )
            aadd( cpp_, "      {" )
            aadd( cpp_, '         HB_TRACE( ' + s_trMode + ', ( "ph=%p    _rel_' + cWidget + '   /.\\", p->ph ) );' )
            aadd( cpp_, "         delete ( ( " + cWidget + IF( lList, "< void * >", "" ) + " * ) p->ph ); " )
            aadd( cpp_, '         HB_TRACE( ' + s_trMode + ', ( "ph=%p YES_rel_' + cWidget + '   \\./", p->ph ) );' )
            aadd( cpp_, "         p->ph = NULL;" )
            aadd( cpp_, "      }" )
            aadd( cpp_, "      else" )
            aadd( cpp_, "      {" )
            aadd( cpp_, '         HB_TRACE( ' + s_trMode + ', ( "ph=%p DEL_rel_' + cWidget + '    :     Object already deleted!", p->ph ) );' )
            aadd( cpp_, "         p->ph = NULL;" )
            aadd( cpp_, "      }" )
            aadd( cpp_, "   }" )
            aadd( cpp_, "   else" )
            aadd( cpp_, "   {" )
            aadd( cpp_, '      HB_TRACE( ' + s_trMode + ', ( "ph=%p PTR_rel_' + cWidget + '    :    Object not created with new=true", p->ph ) );' )
            aadd( cpp_, "      p->ph = NULL;" )
            aadd( cpp_, "   }" )
         ENDIF
      ELSE
         aadd( cpp_, "   HB_SYMBOL_UNUSED( Cargo );" )
         aadd( cpp_, "   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;" )
         aadd( cpp_, "   " )
         aadd( cpp_, "   if( p && p->bNew )" )
         aadd( cpp_, "   {" )
         aadd( cpp_, "      p->ph = NULL;" )
         aadd( cpp_, "   }" )
      ENDIF
      aadd( cpp_, "}" )
      aadd( cpp_, "" )


      aadd( cpp_, "void * hbqt_gcAllocate_" + cWidget + "( void * pObj, bool bNew )" )
      aadd( cpp_, "{                                      " )
      IF lObject
         aadd( cpp_, "   HBQT_GC_T_" + cWidget + " * p = ( HBQT_GC_T_" + cWidget + " * ) hb_gcAllocate( sizeof( HBQT_GC_T_" + cWidget + " ), hbqt_gcFuncs() );" )
      ELSE
         aadd( cpp_, "   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );" )
      ENDIF
      aadd( cpp_, "" )
      IF lObject
         aadd( cpp_, "   new( & p->ph ) QPointer< "+ cWidget +" >( ( " + cWidget + " * ) pObj );" )
      ELSE
         aadd( cpp_, "   p->ph = ( " + cWidget + iif( lList, "< void * >", "" ) + " * ) pObj;" )
      ENDIF
      aadd( cpp_, "   p->bNew = bNew;" )
      aadd( cpp_, "   p->func = hbqt_gcRelease_" + cWidget + ";" )
      aadd( cpp_, "   p->type = HBQT_TYPE_" + cWidget + ";" )
      aadd( cpp_, "" )
      aadd( cpp_, "   if( bNew )" )
      aadd( cpp_, "   {" )
      aadd( cpp_, '      HB_TRACE( ' + s_trMode + ', ( "ph=%p    _new_' + cWidget + iif( lObject, '  under p->pq', '' ) + '", pObj ) );' )
      aadd( cpp_, "   }" )
      aadd( cpp_, "   else" )
      aadd( cpp_, "   {" )
      aadd( cpp_, '      HB_TRACE( ' + s_trMode + ', ( "ph=%p NOT_new_' + cWidget + '", pObj ) );' )
      aadd( cpp_, "   }" )
      aadd( cpp_, "   return p;" )
      aadd( cpp_, "}" )
      aadd( cpp_, "" )

      aadd( cpp_, new_[ 1 ] )           // Func definition
      aadd( cpp_, new_[ 2 ] )           // {
      IF lConst
         if ( lList )
             aadd( cpp_, "   " + cWidget + "< void * > * pObj = NULL;" )
         else
             aadd( cpp_, "   " + cWidget + " * pObj = NULL;" )
         endif
         aadd( cpp_, " " )
         FOR i := 3 TO len( new_ ) - 1
             IF left( ltrim( new_[ i ] ), 2 ) != "//"
                IF "hb_retptr(" $ new_[ i ]
                   s := new_[ i ]
                   s := trim( strtran( s, "hb_retptr(", "pObj =" ) )
                   s := strtran( s, ");", ";" )
                   s := strtran( s, "( "+ cWidget + "* )", "" )
                   aadd( cpp_, s )
               ELSE
                  aadd( cpp_, new_[ i ] )
               ENDIF
            ENDIF
         NEXT
         aadd( cpp_, " " )
         aadd( cpp_, "   hb_retptrGC( hbqt_gcAllocate_" + cWidget + "( ( void * ) pObj, true ) );" )
      ELSE
         FOR i := 3 TO len( new_ ) - 1
             aadd( cpp_, new_[ i ] )
         NEXT
      ENDIF
      aadd( cpp_, new_[ len( new_ ) ] ) // }
      aadd( cpp_, "" )

      /* Insert Functions */
      aeval( txt_, {|e| aadd( cpp_, strtran( e, chr( 13 ), "" ) ) } )

      /* Footer */
      BuildFooter( @cpp_ )

      /* Build Document File */
      IF !empty( doc_ )
         Build_Document( cProject, cWidget, cls_, doc_, cPathDoc, subCls_, docum_ )
      ENDIF

      /* Build Class PRG Source */
      /* Distribute in specific lib subfolder */
      cFileCpp := GetSourcePathByLib( cWidget, cPathOut, ".cpp", "" )
      CreateTarget( cFileCpp, cpp_ )

      /* Build CLASS */
      IF !empty( cls_ )
         Build_Class( cWidget, cls_, doc_, cPathOut, subCls_ )
         cPRG := cWidget
      ELSE
         cPRG := ""
      ENDIF
      cCPP := cWidget
   ENDIF

   RETURN { cCPP, cPRG }



/*----------------------------------------------------------------------*/

FUNCTION GetSourcePathByLib( cWidget, cPathOut, cExt, cPre )
   RETURN cPathOut + hb_ps() + cPre + cWidget + cExt

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

STATIC FUNCTION ParseProto( cProto, cWidget, txt_, doc_, aEnum, func_, lList, fBody_ )
   LOCAL aRet, aA, aArgus, aArg, aPar, aPre, n, nn, nHBIdx
   LOCAL cPre, cPar, cRet, cFun, cParas, cDocs, cCmd, cPas, s, ss, cFirstParamCast
   LOCAL cWdg, cCmn, cPrgRet, cHBFunc, cHBIdx, cDocNM
   LOCAL lSuccess, FP
   LOCAL cInt         := "int,qint16,quint16,short,ushort,unsigned"
   LOCAL cIntLong     := "qint32,quint32,QRgb"
   LOCAL cIntLongLong := "qint64,quint64,qlonglong,qulonglong,ulong"
   LOCAL lRetIsList

   cParas := ""
   cDocs  := ""

   aRet := {}; aArgus := {}
   n := at( "(", cProto )
   IF n > 0
      nn := rat( ")", cProto )
      IF nn > 0
         /* Pull out pre-mid-post components */
         cPre := alltrim( substr( cProto,   1, n-1    ) )
         cPar := alltrim( substr( cProto, n+1, nn-1-n ) )
         cPas := alltrim( substr( cProto, nn+1        ) )

         lRetIsList := "<" $ cPre

         /* parse cPre, it has two components */
         n := rat( " ", cPre )
         IF n > 0   /* And it must be, otherwise it is constructor function which we write in <CODE>section */
            cFun := alltrim( substr( cPre, n + 1    ) )
            cRet := alltrim( substr( cPre, 1, n - 1 ) )
         ELSE
            cFun := cPre
            cRet := ""
         ENDIF

         aRet := array( PRT_ATTRB_MAX )

         aRet[ PRT_L_CONST      ] := "const"   $ cRet  //.or. "const"   $ cPas
         aRet[ PRT_L_AND        ] := "&"       $ cRet
         aRet[ PRT_L_FAR        ] := "*"       $ cRet
         aRet[ PRT_L_VIRT       ] := "virtual" $ cRet
         aRet[ PRT_L_CONST_LAST ] := "const"   $ cPas

         cRet := strtran( cRet, "virtual ", "" )
         cRet := strtran( cRet, "const "  , "" )
         IF ! lRetIsList
            cRet := strtran( cRet, "& "      , "" )
            cRet := strtran( cRet, "&"       , "" )
            cRet := strtran( cRet, "* "      , "" )
            cRet := strtran( cRet, "*"       , "" )
         ENDIF

         /* Normalize */
         cRet := alltrim( cRet )
         IF lRetIsList
            aRet[ PRT_CAST ] := cRet
         ELSE
            n := at( " ", cRet )
            IF n > 0
               aRet[ PRT_CAST ] := substr( cRet, 1, n-1 )
            ELSE
               aRet[ PRT_CAST ] := cRet
            ENDIF
         ENDIF
         aRet[ PRT_NAME ] := aRet[ PRT_CAST ]

         IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
            aRet[ PRT_CAST ] := cWidget + "::" + aRet[ PRT_CAST ]
         ENDIF

         /* Parse arguments */
         aArg := hb_ATokens( cPar, "," )
         /* Normalize */
         aeval( aArg, {|e,i| aArg[ i ] := alltrim( e ) } )

         cParas := ""
         cDocs  := ""

         /* TO hold arguments by reference */
         aPre := {}

         FOR EACH cPre IN aArg
            aPar := array( PRT_ATTRB_MAX )
            aA := aPar

            aA[ PRT_RAW     ] := cPre

            aA[ PRT_L_CONST ] := "const"   $ cPre
            aA[ PRT_L_AND   ] := "&"       $ cPre
            aA[ PRT_L_FAR   ] := "*"       $ cPre
            aA[ PRT_L_VIRT  ] := "virtual" $ cPre
            /* Check if default value is defined */
            n := at( "=", cPre )
            IF n > 0
               aA[ PRT_DEFAULT ] := alltrim( substr( cPre, n+1 ) )
               cPre := substr( cPre, 1, n-1 )
            ENDIF
            /* Normalize */
            cPre := strtran( cPre, "const "  , ""  )
            cPre := strtran( cPre, "& "      , ""  )
            cPre := strtran( cPre, "&"       , ""  )
            cPre := strtran( cPre, "* "      , ""  )
            cPre := strtran( cPre, "*"       , ""  )
            cPre := strtran( cPre, "virtual ", ""  )
            cPre := strtran( cPre, "   "     , " " )
            cPre := strtran( cPre, "  "      , " " )

            cPre := alltrim( cPre )

            /* left may be two elements, name and cast */
            n := at( " ", cPre )
            IF n > 0
               aA[ PRT_CAST ] := substr( cPre, 1, n-1 )
               aA[ PRT_NAME ] := substr( cPre, n+1 )
            ELSE
               aA[ PRT_CAST ] := cPre
               aA[ PRT_NAME ] := cPre
            ENDIF

            IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aA[ PRT_CAST ] ) } ) > 0
               aA[ PRT_CAST ] := cWidget + "::" + aA[ PRT_CAST ]
            ENDIF

            /* Add to main array */
            aadd( aArgus, aA )

            nHBIdx := cPre:__enumIndex() + 1
            cHBIdx := hb_ntos( nHBIdx )
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

            IF empty( cFirstParamCast )
               cFirstParamCast := aA[ PRT_CAST ]
               IF "::" $ cFirstParamCast
                  cFirstParamCast := substr( cFirstParamCast, at( "::", cFirstParamCast ) + 2 )
               ENDIF
            ENDIF

            DO CASE
            CASE aA[ PRT_CAST ] == "PHB_ITEM"
               aA[ PRT_BODY ] := "hb_param( " + cHBIdx + ", HB_IT_ANY )"
               aA[ PRT_DOC  ] := "x" + cDocNM

            CASE aA[ PRT_CAST ] == "T"
               aA[ PRT_BODY ] := "hb_param( " + cHBIdx + ", HB_IT_ANY )"
               aA[ PRT_DOC  ] := "x" + cDocNM

            /* Values by reference */
            CASE aA[ PRT_CAST ] $ cInt .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + " i" + cDocNM + " = 0;", nHBIdx, "i" + cDocNM, "hb_storni" } )
               aA[ PRT_BODY ] := "&i" + cDocNM
               aA[ PRT_DOC  ] := "@n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + " i" + cDocNM + " = 0;", nHBIdx, "i" + cDocNM, "hb_stornl" } )
               aA[ PRT_BODY ] := "&i" + cDocNM
               aA[ PRT_DOC  ] := "@n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + " i" + cDocNM + " = 0;", nHBIdx, "i" + cDocNM, "hb_stornint" } )
               aA[ PRT_BODY ] := "&i" + cDocNM
               aA[ PRT_DOC  ] := "@n" + cDocNM

            CASE aA[ PRT_CAST ] $ cInt
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := "hb_parnidef( " + cHBIdx + ", " + aA[ PRT_DEFAULT ] + " )"
               ELSE
                  aA[ PRT_BODY ] := "hb_parni( " + cHBIdx + " )"
               ENDIF
               aA[ PRT_DOC  ] := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := "hb_parnldef( " + cHBIdx + ", " + aA[ PRT_DEFAULT ] + " )"
               ELSE
                  aA[ PRT_BODY ] := "hb_parnl( " + cHBIdx + " )"
               ENDIF
               aA[ PRT_DOC  ] := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ "qlonglong,qulonglong"
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := "( " + aA[ PRT_CAST ] + " ) hb_parnintdef( " + cHBIdx + ", " + aA[ PRT_DEFAULT ] + " )"
               ELSE
                  aA[ PRT_BODY ] := "( " + aA[ PRT_CAST ] + " ) hb_parnint( " + cHBIdx + " )"
               ENDIF
               aA[ PRT_DOC  ] := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := "hb_parnintdef( " + cHBIdx + ", " + aA[ PRT_DEFAULT ] + " )"
               ELSE
                  aA[ PRT_BODY ] := "hb_parnint( " + cHBIdx + " )"
               ENDIF
               aA[ PRT_DOC  ] := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ "double,qreal" .and. aA[ PRT_L_FAR ]
               aadd( aPre, { "qreal qr" + cDocNM + " = 0;", nHBIdx, "qr" + cDocNM, "hb_stornd"  } )
               aA[ PRT_BODY ] := "&qr" + cDocNM
               aA[ PRT_DOC  ] := "@n" + cDocNM

            CASE aA[ PRT_CAST ] $ "double,qreal,float"
               aA[ PRT_BODY ] := "hb_parnd( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "n" + cDocNM

            CASE aA[ PRT_CAST ] == "uchar" .and. aA[ PRT_L_FAR ] .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := "( char * ) hb_parc( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "uchar" .and. !( aA[ PRT_L_FAR ] ) .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := "( char ) hb_parni( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "n" + cDocNM

            CASE aA[ PRT_CAST ] == "char" .and. aA[ PRT_L_FAR ] .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := "( char * ) hb_parc( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "char" .and. !( aA[ PRT_L_FAR ] ) .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := "( char ) hb_parni( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "c" + cDocNM

            CASE ( "::" $ aA[ PRT_CAST ] ) .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + " i" + cDocNM + " = ( " + aA[ PRT_CAST ] + " ) 0;", nHBIdx, "i" + cDocNM, "hb_storni" } )
               aA[ PRT_BODY ] := "&i" + cDocNM
               aA[ PRT_DOC  ] := "@n" + cDocNM

            CASE ( "::" $ aA[ PRT_CAST ] )
               s := "( " + aA[ PRT_CAST ] + " ) hb_parni( " + cHBIdx + " )"
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  IF ascan( aEnum, aA[ PRT_DEFAULT ] ) > 0
                     ss := cWidget + "::" + aA[ PRT_DEFAULT ]
                  ELSE
                     ss := iif( "::" $ aA[ PRT_DEFAULT ], aA[ PRT_DEFAULT ], ;
                        iif( isDigit( left( aA[ PRT_DEFAULT ], 1 ) ), aA[ PRT_DEFAULT ], cWidget + "::" + aA[ PRT_DEFAULT ] ) )
                  ENDIF
                  ss := "( " + aA[ PRT_CAST ] + " ) " + ss
                  aA[ PRT_BODY ] := "( HB_ISNUM( " + cHBIdx + " ) ? " + s + " : " + ss + " )"
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := "n" + cDocNM

            CASE aA[ PRT_CAST ] == "bool" .and. aA[ PRT_L_FAR ]
               aadd( aPre, { "bool i" + cDocNM + " = 0;", nHBIdx, "i" + cDocNM, "hb_stornl" } )
               aA[ PRT_BODY ] := "&i" + cDocNM
               aA[ PRT_DOC  ] := "@l" + cDocNM

            CASE aA[ PRT_CAST ] == "bool"
               aA[ PRT_BODY ] := "hb_parl( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "l" + cDocNM

            CASE aA[ PRT_CAST ] == "QString"
               #if 0
               IF !( s_isObject )
                  aA[ PRT_BODY ] := "hbqt_par_QString( " + cHBIdx + " )"
               ELSE
                  //aA[ PRT_BODY ] := cWidget+"::tr( hb_parc( " + cHBIdx + " ) )"
                  aA[ PRT_BODY ] := "hb_parstr_utf8( " + cHBIdx + ", &pText, NULL )"
               ENDIF
               #endif
               aA[ PRT_BODY ] := "hb_parstr_utf8( " + cHBIdx + ", &pText, NULL )"
               aA[ PRT_DOC  ] := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "FT_Face"
               aA[ PRT_BODY ] := "hbqt_par_FT_Face( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "QIcon"
               s := "*hbqt_par_QIcon( " + cHBIdx + " )"
               aA[ PRT_BODY ] := "( HB_ISPOINTER( " + cHBIdx + " ) ? " + s + " : " + "QIcon( hbqt_par_QString( " + cHBIdx + " ) ) )"
               aA[ PRT_DOC  ] := "c" + cDocNM

            CASE aA[ PRT_L_FAR ]
               aA[ PRT_BODY ] := "hbqt_par_" + aA[ PRT_CAST ] + "( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "p" + cDocNM

            CASE aA[ PRT_L_AND ] .and. aA[ PRT_L_CONST ]
               s := "*hbqt_par_" + aA[ PRT_CAST ] + "( " + cHBIdx + " )"
               IF !empty( aA[ PRT_DEFAULT ] ) .and. ( "(" $ aA[ PRT_DEFAULT ] )
                  aA[ PRT_BODY ] := "( HB_ISPOINTER( " + cHBIdx + " ) ? " + s + " : " + aA[ PRT_DEFAULT ] + " )"
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := "p" + cDocNM

            CASE aA[ PRT_L_AND ]
               aA[ PRT_BODY ] := "*hbqt_par_" + aA[ PRT_CAST ] + "( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "p" + cDocNM

            CASE aA[ PRT_CAST ] == "QChar"
               aA[ PRT_BODY ] := "*hbqt_par_" + aA[ PRT_CAST ] + "( " + cHBIdx + " )"
               aA[ PRT_DOC  ] := "p" + cDocNM

            OTHERWISE
               aA[ PRT_BODY ] := ""
               aA[ PRT_DOC  ] := ""

            ENDCASE

            cParas += aA[ PRT_BODY ] + ", "
            cDocs  += aA[ PRT_DOC  ] + ", "
         NEXT

         IF right( cParas, 2 ) == ", "
            cParas := substr( cParas, 1, len( cParas ) - 2 )
            cDocs  := substr( cDocs , 1, len( cDocs  ) - 2 )
         ENDIF

         /* Build complete code line */
         IF .t.
            aA     := aRet       /*     T A K E   C A R E    */
            //
            cWdg   := "hbqt_par_" + cWidget + "( 1 )->"
            cParas := "( " + cParas + " )"
            cCmn   := cWdg + cFun + cParas
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

// hb_retptrGC( hbqt_gcAllocate_QList( new QList<QUrl>( ( p )->urls() ), true ) );
// QList<QUrl> urls () const

            DO CASE
            CASE "<" $ aA[ PRT_CAST ]
               DO CASE
               CASE ! ( "QList" $ aA[ PRT_CAST ] )
                  cCmd := ""
                  cPrgRet := ""

               CASE "::" $ aA[ PRT_CAST ]
                  cCmd := ""
                  cPrgRet := ""

               CASE "QPair" $ aA[ PRT_CAST ]
                  cCmd := ""
                  cPrgRet := ""
               #if 0
               CASE "<T>" $ aA[ PRT_CAST ] .AND. "QList" $ aA[ PRT_CAST ]
                  cCmd := "hb_retptrGC( hbqt_gcAllocate_QList( new QList< void * >( " + cCmn + " ), true ) )"
                  cPrgRet := "p" + cDocNM
               #endif
               CASE "<T>" $ aA[ PRT_CAST ]
                  cCmd := ""
                  cPrgRet := ""

               OTHERWISE
                  cCmd := "hb_retptrGC( hbqt_gcAllocate_QList( new " + aA[ PRT_CAST ] + "( " + cCmn + " ), true ) )"
                  cPrgRet := "p" + cDocNM

               ENDCASE

            CASE aA[ PRT_CAST ] == "T"
               cCmd := "hb_retptr( " + cCmn + " )"
               cPrgRet := "p" + cDocNM

            CASE aA[ PRT_CAST ] == "void"
               cCmd := cCmn
               cPrgRet := "NIL"

            CASE aA[ PRT_CAST ] $ cInt
               cCmd := "hb_retni( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               cCmd := "hb_retnl( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               cCmd := "hb_retnint( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ "double,qreal,float"
               cCmd := "hb_retnd( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE ( "::" $ aA[ PRT_CAST ] )
               cCmd := "hb_retni( ( " + aA[ PRT_CAST ] + " ) " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] == "bool"
               cCmd := "hb_retl( " + cCmn + " )"
               cPrgRet := "l" + cDocNM

            CASE aA[ PRT_CAST ] == "char" .AND. aA[ PRT_L_FAR ]
               cCmd := "hb_retc( " + cCmn + " )"
               cPrgRet := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "char"
               cCmd := "hb_retni( " + cCmn + " )"
               cPrgRet := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "QString"
               //cCmd := "hb_retc( " + cCmn + ".toAscii().data()" + " )"
               cCmd := "hb_retstr_utf8( " + cCmn + ".toUtf8().data()" + " )"
               cPrgRet := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "FT_Face"
               cCmd := "hb_retc( " + cCmn + " )"
               cPrgRet := "c" + cDocNM

            CASE aA[ PRT_L_FAR ] .AND. ( aA[ PRT_CAST ] $ "uchar" )
               cCmd := "hb_retc( ( const char * ) " + cCmn + " )"
               cPrgRet := "p" + cDocNM

            CASE aA[ PRT_L_FAR ] .AND. !( aA[ PRT_L_CONST ] )
               IF ( isAqtObject( aA[ PRT_CAST ] ) )
                  cCmd := Get_Command( aA[ PRT_CAST ], cCmn, .F. )
               ELSE
                  cCmd := "hb_retptr( ( " + aA[ PRT_CAST ] + "* ) " + cCmn + " )"
               ENDIF
               cPrgRet := "p" + cDocNM

            CASE ( isAqtObject( aA[ PRT_CAST ] ) )      .AND. ;
                                      aA[ PRT_L_FAR ]   .AND. ;
                                      aA[ PRT_L_CONST ] .AND. ;
                                      ( "Abstract" $ aA[ PRT_CAST ] )
               cCmd := "hb_retptrGC( hbqt_gcAllocate_" + aA[ PRT_CAST ] + "( ( void * ) " + cCmn + ", false ) )"
               cPrgRet := "p" + cDocNM

            CASE ( isAqtObject( aA[ PRT_CAST ] ) )      .AND. ;
                                      aA[ PRT_L_FAR   ] .AND. ;
                                      aA[ PRT_L_CONST ] .AND. ;
                                      aA[ PRT_L_VIRT  ]
               cCmd := "hb_retptrGC( hbqt_gcAllocate_" + aA[ PRT_CAST ] + "( ( void * ) " + cCmn + ", false ) )"
               cPrgRet := "p" + cDocNM

            CASE ( isAqtObject( aA[ PRT_CAST ] ) )      .AND. ;
                                      aA[ PRT_L_FAR   ] .AND. ;
                                      aA[ PRT_L_CONST ] .AND. ;
                                      aA[ PRT_L_CONST_LAST ]
               cCmd := Get_Command_1( aA[ PRT_CAST ], cCmn )
               cPrgRet := "p" + cDocNM

            CASE aA[ PRT_L_AND ] .AND. aA[ PRT_L_CONST ]
               cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
               cPrgRet := "p" + cDocNM

            CASE aA[ PRT_L_CONST ]
               cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
               cPrgRet := "p" + cDocNM

            CASE aA[ PRT_L_AND ]
               cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
               cPrgRet := "p" + cDocNM

            OTHERWISE
               /* No attribute is attached to return value */
               IF ( isAqtObject( aA[ PRT_CAST ] ) )
                  cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
                  cPrgRet := "p" + cDocNM

               ELSE
                  OutStd( "<<< " + cProto + " | " + aA[ PRT_CAST ] + " >>>" + hb_eol() )
                  cCmd := ""
                  cPrgRet := ""

               ENDIF

            ENDCASE

            /* Lists to be disabled in parameters - TODO */
            IF "<" $ cPar
               cCmd := ""
            ENDIF

            IF ! Empty( cCmd )
               cCmd := StrTran( cCmd, "(  )", "()" ) + ";"
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ( lSuccess := ! Empty( cCmd ) )
      IF ( n := ascan( func_, {|e_| e_[ 1 ] == cFun } ) ) > 0
         func_[ n,2 ]++
         cHBFunc := cFun + "_" + hb_ntos( func_[ n, 2 ] )
         #if 0        /* TODO: Refine to get duplicate name resolution - this mechanism has problems */
         IF empty( cFirstParamCast )
            cHBFunc := cFun + "_" + hb_ntos( func_[ n, 2 ] )
         ELSE
            IF cFirstParamCast == func_[ n,3 ]
               cHBFunc := cFun + "_" + cFirstParamCast + "_" + hb_ntos( func_[ n, 2 ] )
            ELSE
               cHBFunc := cFun + "_" + cFirstParamCast
            ENDIF
         ENDIF
         func_[ n,3 ] := cFirstParamCast
         #endif
      ELSE
         cHBFunc := cFun
         aadd( func_, { cFun, 0, "" } )
      ENDIF

      aadd( txt_, "/*" )
      aadd( txt_, " * " + strtran( cProto, chr(13), "" ) )
      aadd( txt_, " */" )
      aadd( txt_, "HB_FUNC( QT_" + upper( cWidget ) + "_" + upper( cHBFunc ) + " )" )
      aadd( txt_, "{" )
      IF !empty( fBody_ )
         aeval( fBody_, {|e| aadd( txt_, e ) } )

      ELSE
         aadd( txt_, "   " + cWidget + iif( lList, "< void *>", "" ) + " * p = hbqt_par_" + cWidget + "( 1 );" )

         /* Insert parameters by reference */
         IF ! empty( aPre )
            FOR n := 1 TO len( aPre )
               aadd( txt_, "   " + aPre[ n, 1 ] )
            NEXT
            aadd( txt_, "" )
         ENDIF

         /* One line function body */
         FP = strtran( cCmd, "hbqt_par_" + cWidget + "( 1 )", "( p )" )
         aadd( txt_, "   if( p )" )
         aadd( txt_, "   {" )
         IF "hb_parstr_utf8(" $ cCmd
            aadd( txt_, "      void * pText;" )
         ENDIF
         aadd( txt_, "      " + FP )
         IF "hb_parstr_utf8(" $ cCmd
            aadd( txt_, "      hb_strfree( pText );" )
         ENDIF
         aadd( txt_, "   }" )
         #if 0
         aadd( txt_, "   else" )
         aadd( txt_, "   {" )
         aadd( txt_, '      HB_TRACE( ' + s_trMode + ', ( "............................... F=QT_' + upper( cWidget ) + '_' + upper( cHBFunc) + ' FP=' + FP + ' p is NULL" ) );')
         aadd( txt_, "   }" )
         #endif
         /* Return values back to PRG */
         IF ! empty( aPre )
            aadd( txt_, "" )
            FOR n := 1 TO len( aPre )
               aadd( txt_, "   " + aPre[ n, 4 ] + "( " + aPre[ n, 3 ] + ", " + hb_ntos( aPre[ n, 2 ] ) + " );" )
            NEXT
         ENDIF
      ENDIF
      aadd( txt_, "}" )
      aadd( txt_, ""  )

      aadd( doc_, "Qt_" + cWidget + "_" + cHBFunc + "( p" + cWidget + ;
                        iif( empty( cDocs ), "", ", " + cDocs ) + " ) -> " + cPrgRet )
      aadd( doc_, "" )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

STATIC FUNCTION isAqtObject( cCast )
   RETURN left( cCast, 1 ) == "Q" .OR. left( cCast, 3 ) == "HBQ"

/*----------------------------------------------------------------------*/

STATIC FUNCTION ParseVariables( cProto, cWidget, txt_, doc_, aEnum, func_ )
   LOCAL aRet, aA, aPre
   LOCAL n
   LOCAL cRet, cFun, cDocs, cCmd
   LOCAL cWdg, cCmn, cPrgRet, cHBFunc, cDocNM
   LOCAL lSuccess
 * LOCAL cInt         := "int,qint16,quint16,QChar,short,ushort"
   LOCAL cInt         := "int,qint16,quint16,short,ushort"
   LOCAL cIntLong     := "qint32,quint32,QRgb"
   LOCAL cIntLongLong := "qint64,quint64,qlonglong,qulonglong"

   aPre   := {}
   cDocs  := ""

   aRet := {}
   n := at( " ", cProto )
   IF n > 0
      IF .t.
         cRet := alltrim( substr( cProto, 1, n - 1 ) )
         cFun := alltrim( substr( cProto, n + 1    ) )

         aRet := array( PRT_ATTRB_MAX )

         /* Normalize */
         aRet[ PRT_CAST ] := cRet
         aRet[ PRT_NAME ] := aRet[ PRT_CAST ]

         IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
            aRet[ PRT_CAST ] := cWidget + "::" + aRet[ PRT_CAST ]
         ENDIF

         /* Build complete code line */
         IF .t.
            aA     := aRet
            cWdg   := "hbqt_par_" + cWidget + "( 1 )->"
            cCmn   := cWdg + cFun
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

            DO CASE
            CASE aA[ PRT_CAST ] == "T"
               cCmd := "hb_ret( " + cCmn + " )"
               cPrgRet := "x" + cDocNM

            CASE aA[ PRT_CAST ] $ cInt
               cCmd := "hb_retni( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               cCmd := "hb_retnl( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               cCmd := "hb_retnint( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] $ "double,qreal,float"
               cCmd := "hb_retnd( " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE ( "::" $ aA[ PRT_CAST ] )
               cCmd := "hb_retni( ( " + aA[ PRT_CAST ] + " ) " + cCmn + " )"
               cPrgRet := "n" + cDocNM

            CASE aA[ PRT_CAST ] == "bool"
               cCmd := "hb_retl( " + cCmn + " )"
               cPrgRet := "l" + cDocNM

            CASE aA[ PRT_CAST ] == "char*"
               cCmd := "hb_retc( " + cCmn + " )"
               cPrgRet := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "char"
               cCmd := "hb_retni( " + cCmn + " )"
               cPrgRet := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "QString"
//               cCmd := "hb_retc( " + cCmn + ".toLatin1().data()" + " )"
               cCmd := "hb_retstr_utf8( " + cCmn + ".toUtf8().data()" + " )"
               cPrgRet := "c" + cDocNM

            CASE aA[ PRT_CAST ] == "FT_Face"
               cCmd := "hb_retc( " + cCmn + " )"
               cPrgRet := "c" + cDocNM

            OTHERWISE
               /* No attribute is attached to return value */
               IF (left( aA[ PRT_CAST ], 1 ) == "Q")
                  cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
                  cPrgRet := "p" + cDocNM

               ELSE
                  OutStd( "<<< " + cProto + " | " + aA[ PRT_CAST ] + " >>>" + hb_eol() )
                  cCmd := ""
                  cPrgRet := ""

               ENDIF

            ENDCASE

            IF !empty( cCmd )
               cCmd := strtran( cCmd, "(  )", "()" ) + ";"
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ( lSuccess := !empty( cCmd ) )
      IF ( n := ascan( func_, {|e_| e_[ 1 ] == cFun } ) ) > 0
         func_[ n, 2 ]++
         cHBFunc := cFun + "_" + hb_ntos( func_[ n, 2 ] )
      ELSE
         cHBFunc := cFun
         aadd( func_, { cFun, 0 } )
      ENDIF

      aadd( txt_, "/*" )
      aadd( txt_, " * " + strtran( cProto, chr( 13 ), "" ) )
      aadd( txt_, " */" )

      aadd( txt_, "HB_FUNC( QT_" + upper( cWidget ) + "_" + upper( cHBFunc ) + " )" )
      aadd( txt_, "{" )

      /* Insert parameters by reference */
      IF !empty( aPre )
         FOR n := 1 TO len( aPre )
            aadd( txt_, "   " + aPre[ n, 1 ] )
         NEXT
         aadd( txt_, "" )
      ENDIF

      /* One line function body */
      aadd( txt_, "   " + cCmd )

      /* Return values back to PRG */
      IF !empty( aPre )
         aadd( txt_, "" )
         FOR n := 1 TO len( aPre )
            aadd( txt_, "   " + aPre[ n, 4 ] + "( " + aPre[ n, 3 ] + ", " + hb_ntos( aPre[ n, 2 ] ) + " );" )
         NEXT
      ENDIF

      aadd( txt_, "}" )
      aadd( txt_, "" )

      aadd( doc_, "Qt_" + cWidget + "_" + cHBFunc + "( p" + cWidget + ;
                        iif( empty( cDocs ), "", ", " + cDocs ) + " ) -> " + cPrgRet )
      aadd( doc_, "" )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildHeader( txt_, nMode, cProFile )

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
   aadd( txt_, " * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>"                    )
   aadd( txt_, " * "                                                                           )
   aadd( txt_, " * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>"     )
   aadd( txt_, " * www - http://harbour-project.org"                                       )
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
   cHlp += "Copyright (c) 2009, Pritpal Bedi <pritpal@vouchcac.com>" + hb_eol()
   cHlp += "http://harbour-project.org/"                             + hb_eol()
   cHlp += ""                                                        + hb_eol()

   OutStd( cHlp )

   RETURN nil

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

STATIC FUNCTION Build_Class( cWidget, cls_, doc_, cPathOut, subCls_ )
   LOCAL cFile, s, n, cM, ss, cCall, sm, i
   LOCAL nLen := len( cWidget )
   LOCAL txt_ := {}, mth_:= {}

   BuildHeader( @txt_, 1 )

   aadd( txt_, "" )
   aadd( txt_, "FUNCTION " + cWidget + "( ... )")
   aadd( txt_, "   RETURN HB_" + cWidget + "():new( ... )" )
   aadd( txt_, "" )
   aadd( txt_, "" )

   n := ascan( cls_, {|e_| left( lower( e_[ 1 ] ), 7 ) == "inherit" .and. !empty( e_[ 2 ] ) } )
//   s := "CREATE CLASS " + cWidget + " INHERIT HbQtObjectHandler" + iif( n > 0, ", " + cls_[ n, 2 ], "" )
   s := "CREATE CLASS " + cWidget + " INHERIT HbQtObjectHandler" + iif( n > 0, ", " + strtran( cls_[ n, 2 ], "Q", "HB_Q" ), "" ) + " FUNCTION HB_" + cWidget

   aadd( txt_, s                                 )
   aadd( txt_, "   "                             )
   aadd( txt_, "   METHOD  new( ... )"           )
//   aadd( txt_, "   METHOD  initClass( ... )"     )
   aadd( txt_, "   "                             )

   /* Populate METHODS */
   FOR EACH s IN doc_
      n := at( "-> ", s )
      IF n > 0
         s     := substr( s, 1, n-1 )
         s     := strtran( s, "@", "" )    /* Just in Case */
         s     := strtran( s, "::", "_" )  /* Just in Case */

         n     := at( cWidget, s )
         sm    := substr( s, n+nLen+1 )

         ss    := "p" + cWidget + ","
         cM    := strtran( sm, ss, "" )
         ss    := "p" + cWidget
         cM    := strtran( cM, ss, "" )
         cM    := strtran( cM, "(  )", "()" )
         cM    := strtran( cM, "(  ", "( " )
         cM    := iif( len( cM ) > 35, cM, pad( cM,35 ) )

         ss    := "p" + cWidget
         cCall := strtran( s, ss, "::pPtr" )

         aadd( mth_, { cM, cCall } )
         aadd( txt_, "   METHOD  " + cM  )
      ENDIF
   NEXT
   aadd( txt_, "   "                                               )
   aadd( txt_, "   ENDCLASS"                                       )
   aadd( txt_, "   "                                               )
   aadd( txt_, "   "                                               )
   aadd( txt_, "METHOD " + cWidget + ":new( ... )"                 )
 * aadd( txt_, "METHOD " + cWidget + ":initClass( ... )"           )
   aadd( txt_, "   LOCAL p"                                        )
   aadd( txt_, "   FOR EACH p IN { ... }"                          )
   aadd( txt_, "      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )" )
   aadd( txt_, "   NEXT"                                           )
   aadd( txt_, "   ::pPtr := Qt_" + cWidget + "( ... )"            )
 * aadd( txt_, "HB_TRACE( HB_TR_ALWAYS, ::pPtr )"                  )
   aadd( txt_, "   RETURN Self"                                    )
   aadd( txt_, "   "                                               )
   /* Define methods */
   FOR i := 1 TO len( mth_ )
      aadd( txt_, ""                                               )
      aadd( txt_, "METHOD " + cWidget + ":" + mth_[ i, 1 ]         )
      aadd( txt_, "   RETURN " + ParsePtr( mth_[ i, 2 ] )          )
      aadd( txt_, ""                                               )
   NEXT

   IF !empty( subCls_ )
      aadd( txt_, ""                                               )
      aeval( subCls_, {|e| aadd( txt_, e ) } )
      aadd( txt_, ""                                               )
   ENDIF

   /* Generate .prg */
   cFile := GetSourcePathByLib( cWidget, cPathOut, ".prg", "T" )
   CreateTarget( cFile, txt_ )

   RETURN nil

/*----------------------------------------------------------------------*/

#define  QT_VER  "4.5"
#define  QT_WEB  "http://doc.trolltech.com/"

STATIC FUNCTION Build_Document( cProFile, cWidget, cls_, doc_, cPathDoc, subCls_, docum_ )
   LOCAL cText, n, n1, n2, nLen, pWidget, cRet, cLib, cFile, i, cInherits, cHdr

   LOCAL hEntry := { => }

   hb_HKeepOrder( hEntry, .T. )

   HB_SYMBOL_UNUSED( cls_ )
   HB_SYMBOL_UNUSED( subCls_ )

   n := ascan( cls_, {|e_| left( lower( e_[ 1 ] ), 7 ) == "inherit" .and. !empty( e_[ 2 ] ) } )
   cInherits := iif( n > 0, cls_[ n, 2 ], "" )

   cLib := FNameGetName( cProFile )

   hEntry[ "TEMPLATE" ]     := "    " + "Class"
   hEntry[ "NAME" ]         := "    " + cWidget + "()"
   hEntry[ "CATEGORY" ]     := "    " + "Harbour Bindings for Qt"
   hEntry[ "SUBCATEGORY" ]  := "    " + "GUI"
   hEntry[ "EXTERNALLINK" ] := "    " + QT_WEB + QT_VER + "/" + lower( cWidget ) + ".html"
   hEntry[ "ONELINER" ]     := "    " + "Creates a new " + cWidget + " object."
   hEntry[ "INHERITS" ]     := "    " + cInherits
   hEntry[ "SYNTAX" ]       := ""
   hEntry[ "SYNTAX" ]       += "    " + cWidget + "():new( ... )" + hb_eol()
   hEntry[ "SYNTAX" ]       += "    " + cWidget + "():from( pPtr_OR_oObj_of_type_" + cWidget +" )" + hb_eol()
   hEntry[ "SYNTAX" ]       += "    " + cWidget + "():configure( pPtr_OR_oObj_of_type_" + cWidget +" )"
   hEntry[ "ARGUMENTS" ]    := ""
   hEntry[ "RETURNS" ]      := "    " + "An instance of the object of type " + cWidget
   hEntry[ "METHODS" ]      := ""
   nLen    := len( cWidget )
   n       := at( cWidget, doc_[ 1 ] )
   pWidget := "p" + cWidget
   FOR i := 1 TO len( doc_ )
      cText := doc_[ i ]
      IF !empty( cText )
         cText := substr( cText, n+nLen+1 )
         cText := strtran( cText, pWidget + ", ", "" )
         cText := strtran( cText, pWidget, "" )
         cText := strtran( cText, "(  )", "()" )
         n1    := at( "->", cText )
         cRet  := alltrim( substr( cText, n1+2 ) )
         cText := substr( cText, 1, n1-1 )
         n2    := max( 50, len( cText ) )
         cText := padR( cText, n2 )
         IF !empty( cRet )
            hEntry[ "METHODS" ] += "    :" + cText + " -> " + cRet + hb_eol()
         ENDIF
      ENDIF
   NEXT
   hEntry[ "DESCRIPTION" ]  := ""
   hEntry[ "EXAMPLES" ]     := ""
   FOR EACH cText IN docum_
      IF !empty( cText )
         hEntry[ "EXAMPLES" ] += "    " + cText + hb_eol()
      ENDIF
   NEXT
   hEntry[ "TESTS" ]        := ""
   hEntry[ "STATUS" ]       := "    " + "R"
   hEntry[ "COMPLIANCE" ]   := "    " + "Not Clipper compatible"
   hEntry[ "PLATFORMS" ]    := "    " + "Windows, Linux, Mac OS X, OS/2"
   hEntry[ "VERSION" ]      := "    " + "4.5 or upper"
   hEntry[ "FILES" ]        := ""
   hEntry[ "FILES" ]        += "    " + "Harbour source: " + "contrib/hbqt" + iif( Empty( cLib ), "", "/" + cLib ) + "/T" + cWidget + ".prg" + hb_eol()
   hEntry[ "FILES" ]        += "    " + "C++ wrappers  : " + "contrib/hbqt" + iif( Empty( cLib ), "", "/" + cLib ) + "/"  + cWidget + ".cpp" + hb_eol()
   hEntry[ "FILES" ]        += "    " + "Library       : " + "hb" + cLib
   hEntry[ "SEEALSO" ]      := ""
 * hEntry[ "SEEALSO" ]      += "    " + iif( Empty( cInherits ), "", cInherits + ", " ) + QT_WEB + QT_VER + "/" + lower( cWidget ) + ".html" + hb_eol()
   hEntry[ "SEEALSO" ]      += "    " + cInherits

   cFile := cPathDoc + hb_ps() + "en" + hb_ps() + "class_" + lower( cWidget ) + ".txt"

   cHdr := ;
      "/*" + hb_eol() +;
      " * $" + "Id" + "$" + hb_eol() +;
      " */" + hb_eol()

   RETURN hb_MemoWrit( cFile, cHdr + __hbdoc_ToSource( { hEntry } ) )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_HeaderFile( cpp_, cPathOut, cProFile )
   LOCAL cFile := iif( empty( cPathOut ), "", cPathOut + hb_ps() )
   LOCAL hdr_ := {}
   LOCAL txt_
   LOCAL s
   LOCAL tmp
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

   IF s_lNewGCtoQT
      FOR EACH s IN cpp_
         aadd( txt_, "extern HB_EXPORT void * hbqt_gcAllocate_" + s + "( void * pObj, bool bNew );" )
      NEXT
      aadd( txt_, "" )
   ENDIF

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

STATIC FUNCTION Build_MakeFile( cPathOut, aWidgetList, cProFile )
   LOCAL cFile := iif( empty( cPathOut ), "", cPathOut + hb_ps() )
   LOCAL s
   LOCAL hdr_:= {}
   LOCAL hbm_ := {}

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
   FOR EACH s IN aWidgetList
      aadd( hbm_, + s + ".cpp" )
   NEXT
   aadd( hbm_, "" )
   FOR EACH s IN aWidgetList
      aadd( hbm_, + "T" + s + ".prg" )
   NEXT
   //
   CreateTarget( cFile + "filelist.hbm", hbm_ )

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Get_Command_1( cWgt, cCmn )

   RETURN "hb_retptrGC( hbqt_gcAllocate_" + cWgt + "( new " + cWgt + "( *( " + cCmn + " ) ), true ) )"

/*----------------------------------------------------------------------*/

FUNCTION Get_Command( cWgt, cCmn, lNew )
   LOCAL  cRet

   DEFAULT lNew TO .T.

   IF s_lNewGCtoQT
      IF lNew
         cRet := "hb_retptrGC( hbqt_gcAllocate_" + cWgt + "( new " + cWgt + "( " + cCmn + " ), true ) )"
      ELSE
         cRet := "hb_retptrGC( hbqt_gcAllocate_" + cWgt + "( " + cCmn + ", false ) )"
      ENDIF
   ELSE
      cRet := "hb_retptrGC( hbqt_ptrTOgcpointer( new " + cWgt + "( " + cCmn + " ), hbqt_gcRelease_" + cWgt +" ) )"
   ENDIF

   RETURN cRet
/*----------------------------------------------------------------------*/

FUNCTION ParsePtr( cParam )
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
               cPar += "hbqt_ptr( " + s + " )"

            ELSEIF ( n := at( " ", cParam ) ) > 0
               s := substr( cParam, 1, n-1 )
               cParam := substr( cParam, n )
               cPar += "hbqt_ptr( " + s + " )"
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
