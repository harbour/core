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

STATIC s_NewLine
STATIC s_PathSep
STATIC isClassObject
STATIC zWidget
STATIC aCore    := {}
STATIC aGui     := {}
STATIC aNetwork := {}
STATIC aWebkit  := {}

/*
 * Force new GC and Qt interface
 */
STATIC lNewGCtoQT := .T.
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

   s_NewLine := hb_OsNewLine()
   s_PathSep := hb_OsPathSeparator()

   DispLogo()

   aParam := hb_AParams()

   FOR EACH cParam IN aParam
      cLParam := lower( cParam )

      DO CASE
      CASE left( cParam,1 ) == '@'
         x := substr( cParam,2 )
         hb_fNameSplit( x, @cPath, @cFile, @cExt )
         IF empty( cExt ) .or. lower( cExt ) != 'qtp'
            cExt := 'qtp'
         ENDIF
         x := iif( empty( cPath ), "", cPath + s_PathSep )+ cFile + "." + cExt
         aadd( aPrjFiles, x )

      CASE right( cLParam,4 ) == '.qtp'
         aadd( aPrjFiles, cParam )

      CASE right( cLParam,4 ) == '.qth'
         aadd( aProFiles, cParam )

      CASE lower( left( cParam,2 ) ) == '-o'
         cPathOut := substr( cParam, 3 )

      CASE lower( left( cParam,2 ) ) == '-i'
         cPathIn := substr( cParam, 3 )

      CASE lower( left( cParam,2 ) ) == '-d'
         cPathDoc := substr( cParam, 3 )

      CASE cParam == '-c'

      CASE cLParam == '-help'
         DispHelp()
         RETURN nil

      ENDCASE
   NEXT

   IF empty( aPrjFiles ) .AND. empty( aProFiles ) .AND. hb_fileExists( "qt45.qtp" )
      aadd( aPrjFiles, "qt45.qtp" )
   ENDIF

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
   IF Right( cPathOut, 1 ) == s_PathSep
      cPathOut := hb_StrShrink( cPathOut, 1 )
   ENDIF
   IF Right( cPathIn, 1 ) == s_PathSep
      cPathIn := hb_StrShrink( cPathIn, 1 )
   ENDIF
   IF Right( cPathDoc, 1 ) == s_PathSep
      cPathDoc := hb_StrShrink( cPathDoc, 1 )
   ENDIF

   /* Manage Project File */
   FOR EACH cProFile IN aPrjFiles
      ManageProject( cProFile, cPathIn, cPathOut, cPathDoc )
   NEXT

   /* Generate .CPP Sources */
   FOR EACH cProFile IN aProFiles
      GenSource( cProFile, cPathIn, cPathOut, cPathDoc )
   NEXT

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION ManageProject( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cPrj, cTkn, cVal // cPath, cExt,
   LOCAL cPIn, cPOut, cPDoc
   LOCAL n, nn
   LOCAL prj_, cpp_, prg_, a_

#if 0
   hb_fNameSplit( cProFile, @cPath, @cFile, @cExt )

   IF empty( cPath )
      cFile := cPathIn + s_PathSep + cProFile
   ELSE
      cFile := cProFile
   ENDIF
#endif
   cFile := cProFile
   IF ! hb_fileExists( cFile )
      RETURN nil
   ENDIF

   cPIn  := cPathIn
   cPOut := cPathOut
   cPDoc := cPathDoc

   cpp_:={}
   prg_:={}

   OutStd( "Processing: " + cFile + s_NewLine )

   cPrj  := memoread( cFile )

   /* Pullout all ANSI C style comments */
   DO WHILE .t.
      IF ( n := at( '/*', cPrj ) ) == 0
         EXIT
      ENDIF
      /* We must have a matching pair */
      nn := at( '*/', cPrj )
      IF nn == 0
         OutStd( 'Project file has unbalanced comment section...' + s_NewLine )
         RETURN nil
      ENDIF
      cPrj := substr( cPrj, 1, n-1 ) + substr( cPrj, nn+2 )
   ENDDO

   /* Prepare to be parsed properly */
   cPrj := strtran( cPrj, s_NewLine, _EOL )
   cPrj := strtran( cPrj, chr( 13 )+chr( 10 ), _EOL )
   cPrj := strtran( cPrj, chr( 13 ), _EOL )

   prj_:= hb_ATokens( cPrj, _EOL )

   FOR EACH cPrj IN prj_
      cPrj := alltrim( cPrj )

      IF left( cPrj,1 ) $ ':#;/*'
         LOOP
      ENDIF

      IF ( n := at( '=', cPrj ) ) > 0
         cTkn := alltrim( substr( cPrj,1,n-1 ) )
         cVal := alltrim( substr( cPrj,n+1   ) )

         //IF !empty( cVal )
            DO CASE
            CASE cTkn == '-I'
               cPIn := cVal

            CASE cTkn == '-O'
               cPOut := cVal

            CASE cTkn == '-D'
               cPDoc := cVal

            ENDCASE
         //ENDIF

      ELSEIF lower( right( cPrj,4 ) ) == '.qth'
         a_:= GenSource( cPrj, cPIn, cPOut, cPDoc )
         IF !empty( a_[ 1 ] )
            aadd( cpp_, a_[ 1 ] )
            IF !empty( a_[ 2 ] )
               aadd( prg_, a_[ 2 ] )
            ENDIF
         ENDIF

      ENDIF
   NEXT

   IF !empty( cpp_ )
      Build_Makefile( cpp_, prg_, cPOut )
      Build_GarbageFile( cpp_, cPOut )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION PullOutSection( cQth, cSec )
   LOCAL cTxt, n, nn, cTknB, cTknE
   LOCAL a_:={}

   cTknB := '<' + cSec + '>'
   cTknE := '</' + cSec + '>'

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

STATIC FUNCTION GenSource( cProFile, cPathIn, cPathOut, cPathDoc )
   LOCAL cFile, cWidget, cExt, cPath, cOrg, cCPP, cPRG, lConst //, lList, cWgt
   LOCAL cQth, cFileCpp, s, n, nFuncs, nCnvrtd, n1, i, cFunc, lObject, lDestructor, lList
   LOCAL b_, txt_, enum_, code_, func_, dummy_, cpp_, cmntd_, doc_, varbls_
   LOCAL class_, cls_, protos_, slots_, enums_, docum_, subCls_, new_, old_, fBody_

   hb_fNameSplit( cProFile, @cPath, @cWidget, @cExt )

   isClassObject := IsQObject( cWidget )
   zWidget := cWidget

   IF empty( cPath )
      cFile := cPathIn + s_PathSep + cProFile
   ELSE
      cFile := cProFile
   ENDIF
   IF ! hb_fileExists( cFile )
      OutStd( "Cannot find: " + cFile + s_NewLine )
      RETURN { nil }
   ENDIF

   cQth := memoread( cFile )
   IF empty( cQth )
      OutStd( "Cannot read: " + cFile + s_NewLine )
      RETURN { nil }
   ENDIF

   OutStd( "Processing: " + cFile + s_NewLine )

   /* Prepare to be parsed properly */
   IF ! hb_osNewLine() == _EOL
      cQth := StrTran( cQth, hb_osNewLine(), _EOL )
   ENDIF
   IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
      cQth := StrTran( cQth, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF

   cls_:={}
   IF !empty( class_:= PullOutSection( @cQth, 'CLASS' ) )
      FOR EACH s IN class_
         IF ( n := at( '=', s ) ) > 0
            aadd( cls_, { alltrim( substr( s, 1, n-1 ) ), alltrim( substr( s, n+1 ) ) } )
         ENDIF
      NEXT
   ENDIF

   /* Pull out SUBCLASS section */
   subCls_ := PullOutSection( @cQth, 'SUBCLASS' )

   /* Pull out Doc Section */
   docum_  := PullOutSection( @cQth, 'DOC'   )

   /* Pull out Code Section */
   code_   := PullOutSection( @cQth, 'CODE'   )

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

   /* Mark to which sub library class belongs to */
   IF "<QtWebKit/" $ cQth .OR. "<QtDesigner/" $ cQth
      //aadd( aWebkit, cWidget )
   ELSEIF "<QtNetwork/" $ cQth
      aadd( aNetwork, cWidget )
   ELSEIF "<QtGui/" $ cQth
      aadd( aGui, cWidget )
   ELSEIF "<QtCore/" $ cQth
      aadd( aCore, cWidget )
   ENDIF


   /* Pull out Enumerators  */
   enums_  := PullOutSection( @cQth, 'ENUMS'  )
   enum_:={}
   FOR EACH s IN enums_
      IF ( 'enum ' $ s .or. 'flags ' $ s )
         b_:= hb_ATokens( alltrim( s ),' ' )
         aadd( enum_, b_[ 2 ] )
      ENDIF
   NEXT

   /* Pull out Prototypes   */
   protos_ := PullOutSection( @cQth, 'PROTOS' )

   /* Pull out Variables */
   varbls_ := PullOutSection( @cQth, 'VARIABLES' )

   /* Pull Out Signals      */
   slots_  := PullOutSection( @cQth, 'SLOTS'  )

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
      IF left( s,2 ) == '//'
         aadd( cmntd_, cOrg )
         nFuncs++
         LOOP
      ENDIF

      /* Check if it is not ANSI C Comment */
      IF left( alltrim( cOrg ),1 ) $ '/*'
         LOOP
      ENDIF
      /* Another comment tokens */
      IF empty( s ) .or. left( s,1 ) $ '#;'
         LOOP
      ENDIF

      nFuncs++

      /* Lists - Later */
      #if 0
      IF '<' $ s
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
      IF left( s,2 ) == '//'
         aadd( cmntd_, cOrg )
         nFuncs++
         LOOP
      ENDIF
      /* Check if it is not ANSI C Comment */
      IF left( alltrim( cOrg ),1 ) $ '/*'
         LOOP
      ENDIF
      /* Another comment tokens */
      IF empty( s ) .or. left( s,1 ) $ '#;'
         LOOP
      ENDIF

      nFuncs++

      /* Lists - Later */
      IF '<' $ s
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
      BuildHeader( @cpp_, 0 )

      /* Place ENUM definitions into the source */
      #if 1
      IF !empty( enums_ )
         aadd( cpp_, '/*' )
         aeval( enums_, {|e| iif( !empty( e ), aadd( cpp_, ' *  ' + e ), NIL ) } )
         aadd( cpp_, ' */ ' )
         aadd( cpp_, '' )
      ENDIF
      #endif

      /* Insert information about prototypes not converted to functions */
      IF !empty( dummy_ )
         aadd( cpp_, '/*' )
         aadd( cpp_, ' *  Constructed[ ' + hb_ntos( nCnvrtd ) + '/' + hb_ntos( nFuncs ) + ' [ ' + hb_ntos( nCnvrtd / nFuncs * 100 ) + '% ] ]' )
         aadd( cpp_, ' *  ' )
         aadd( cpp_, ' *  *** Unconvered Prototypes ***' )
         aadd( cpp_, ' *  -----------------------------' )
         aadd( cpp_, ' *  ' )
         aeval( dummy_, {|e| aadd( cpp_, ' *  ' + e ) } )
         IF !empty( cmntd_ )
            aadd( cpp_, ' *  ' )
            aadd( cpp_, ' *  ' + '*** Commented out protos which construct fine but do not compile ***' )
            aadd( cpp_, ' *  ' )
            aeval( cmntd_, {|e| aadd( cpp_, ' *  ' + e ) } )
         ENDIF
         aadd( cpp_, ' */ ' )
         aadd( cpp_, '' )
      ENDIF

      /* Insert user defined code - INCLUDEs */
      aadd( cpp_, "#include <QtCore/QPointer>" )
      IF !empty( code_ )
         aeval( code_, {|e| aadd( cpp_, strtran( e, chr( 13 ), '' ) ) } )
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
      aadd( cpp_, "   QT_G_FUNC_PTR func;"           )
      aadd( cpp_, "   int type;"           )
      aadd( cpp_, "} QGC_POINTER_" + cWidget + ";"  )
      aadd( cpp_, " "                               )

      aadd( cpp_, "QT_G_FUNC( hbqt_gcRelease_" + cWidget + " )"  )
      aadd( cpp_, "{"                                     )
      IF ( lDestructor ) .AND. ( lConst )
         IF lObject
            aadd( cpp_, "   " + cWidget + " " + iif( lList, "< void * >", "" )+" * ph = NULL ;" )
            aadd( cpp_, "   QGC_POINTER_" + cWidget + " * p = ( QGC_POINTER_" + cWidget + " * ) Cargo; " )
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
            aadd( cpp_, "   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;" )
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
         aadd( cpp_, "   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;" )
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
         aadd( cpp_, "   QGC_POINTER_" + cWidget + " * p = ( QGC_POINTER_" + cWidget + " * ) hb_gcAllocate( sizeof( QGC_POINTER_" + cWidget + " ), hbqt_gcFuncs() );" )
      ELSE
         aadd( cpp_, "   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );" )
      ENDIF
      aadd( cpp_, "" )
      IF lObject
         aadd( cpp_, "   new( & p->ph ) QPointer< "+ cWidget +" >( ( " + cWidget + " * ) pObj );" )
      ELSE
         aadd( cpp_, "   p->ph = ( " + cWidget + iif( lList, "< void * >", "" ) + " * ) pObj;" )
      ENDIF
      aadd( cpp_, "   p->bNew = bNew;" )
      aadd( cpp_, "   p->func = hbqt_gcRelease_" + cWidget + ";" )
      aadd( cpp_, "   p->type = QT_TYPE_"+cWidget+";" )
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
      aeval( txt_, {|e| aadd( cpp_, strtran( e, chr( 13 ), '' ) ) } )

      /* Footer */
      BuildFooter( @cpp_ )

      /* Build Document File */
      IF !empty( doc_ )
         Build_Document( cWidget, cls_, doc_, cPathDoc, subCls_, docum_ )
      ENDIF

      /* Build Class PRG Source */
      /* Distribute in specific lib subfolder */
      cFileCpp := GetSourcePathByLib( cWidget, cPathOut, '.cpp', NIL, cls_ )
      CreateTarget( cFileCpp, cpp_ )

      /* Build CLASS */
      IF !empty( cls_ )
         Build_Class( cWidget, cls_, doc_, cPathOut, subCls_ )
         cPRG := cWidget
      ELSE
         cPRG := ''
      ENDIF
      cCPP := cWidget
   ENDIF

   RETURN { cCPP, cPRG }



/*----------------------------------------------------------------------*/

FUNCTION GetSourcePathByLib( cWidget, cPathOut, cExt, cPre, cls_ )
   LOCAL cFileOut, n

   DEFAULT cPre TO ""

   IF ( n := ascan( cls_, {|e_| lower( e_[ 1 ] ) == "folder" .AND. !empty( e_[ 2 ] ) } ) ) > 0
      cFileOut := iif( empty( cPathOut ), "", cPathOut + s_PathSep + cls_[ n,2 ] + s_pathSep ) + cPre + cWidget + cExt
   ELSE
      IF ascan( aGui, cWidget ) > 0
         cFileOut := cPathOut + s_PathSep + "qtgui" + s_pathSep + cPre + cWidget + cExt
      ELSEIF ascan( aCore, cWidget ) > 0
         cFileOut := cPathOut + s_PathSep + "qtcore" + s_pathSep + cPre + cWidget + cExt
      ELSEIF ascan( aWebkit, cWidget ) > 0
         cFileOut := cPathOut + s_PathSep + "qtwebkit" + s_pathSep + cPre + cWidget + cExt
      ELSEIF ascan( aNetwork, cWidget ) > 0
         cFileOut := cPathOut + s_PathSep + "qtnetwork" + s_pathSep + cPre + cWidget + cExt
      ELSE
         cFileOut := cPathOut + s_PathSep + cPre + cWidget + cExt
      ENDIF
   ENDIF

   RETURN cFileOut

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
   LOCAL cInt         := 'int,qint16,quint16,short,ushort,unsigned'
   LOCAL cIntLong     := 'qint32,quint32,QRgb'
   LOCAL cIntLongLong := 'qint64,quint64,qlonglong,qulonglong,ulong'
   LOCAL lRetIsList

   cParas := ''
   cDocs  := ''

   aRet := {}; aArgus := {}
   n := at( '(', cProto )
   IF n > 0
      nn := rat( ')', cProto )
      IF nn > 0
         /* Pull out pre-mid-post components */
         cPre := alltrim( substr( cProto,   1, n-1    ) )
         cPar := alltrim( substr( cProto, n+1, nn-1-n ) )
         cPas := alltrim( substr( cProto, nn+1        ) )

         lRetIsList := "<" $ cPre

         /* parse cPre, it has two components */
         n := rat( ' ', cPre )
         IF n > 0   /* And it must be, otherwise it is constructor function which we write in <CODE>section */
            cFun := alltrim( substr( cPre, n + 1    ) )
            cRet := alltrim( substr( cPre, 1, n - 1 ) )
         ELSE
            cFun := cPre
            cRet := ''
         ENDIF

         aRet := array( PRT_ATTRB_MAX )
         JustACall( cPas )  ////////////////////////////////////////////////////////
         aRet[ PRT_L_CONST      ] := 'const'   $ cRet  //.or. 'const'   $ cPas
         aRet[ PRT_L_AND        ] := '&'       $ cRet
         aRet[ PRT_L_FAR        ] := '*'       $ cRet
         aRet[ PRT_L_VIRT       ] := 'virtual' $ cRet
         aRet[ PRT_L_CONST_LAST ] := 'const'   $ cPas

         cRet := strtran( cRet, 'virtual ', '' )
         cRet := strtran( cRet, 'const '  , '' )
         IF ! lRetIsList
            cRet := strtran( cRet, '& '      , '' )
            cRet := strtran( cRet, '&'       , '' )
            cRet := strtran( cRet, '* '      , '' )
            cRet := strtran( cRet, '*'       , '' )
         ENDIF

         /* Normalize */
         cRet := alltrim( cRet )
         IF lRetIsList
            aRet[ PRT_CAST ] := cRet
         ELSE
            n := at( ' ', cRet )
            IF n > 0
               aRet[ PRT_CAST ] := substr( cRet, 1, n-1 )
            ELSE
               aRet[ PRT_CAST ] := cRet
            ENDIF
         ENDIF
         aRet[ PRT_NAME ] := aRet[ PRT_CAST ]

         IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
            aRet[ PRT_CAST ] := cWidget + '::' + aRet[ PRT_CAST ]
         ENDIF

         /* Parse arguments */
         aArg := hb_ATokens( cPar, ',' )
         /* Normalize */
         aeval( aArg, {|e,i| aArg[ i ] := alltrim( e ) } )

         cParas := ''
         cDocs  := ''

         /* TO hold arguments by reference */
         aPre := {}

         FOR EACH cPre IN aArg
            aPar := array( PRT_ATTRB_MAX )
            aA := aPar

            aA[ PRT_RAW     ] := cPre

            aA[ PRT_L_CONST ] := 'const'   $ cPre
            aA[ PRT_L_AND   ] := '&'       $ cPre
            aA[ PRT_L_FAR   ] := '*'       $ cPre
            aA[ PRT_L_VIRT  ] := 'virtual' $ cPre
            /* Check if default value is defined */
            n := at( '=', cPre )
            IF n > 0
               aA[ PRT_DEFAULT ] := alltrim( substr( cPre, n+1 ) )
               cPre := substr( cPre, 1, n-1 )
            ENDIF
            /* Normalize */
            cPre := strtran( cPre, 'const '  , ''  )
            cPre := strtran( cPre, '& '      , ''  )
            cPre := strtran( cPre, '&'       , ''  )
            cPre := strtran( cPre, '* '      , ''  )
            cPre := strtran( cPre, '*'       , ''  )
            cPre := strtran( cPre, 'virtual ', ''  )
            cPre := strtran( cPre, '   '     , ' ' )
            cPre := strtran( cPre, '  '      , ' ' )

            cPre := alltrim( cPre )

            /* left may be two elements, name and cast */
            n := at( ' ', cPre )
            IF n > 0
               aA[ PRT_CAST ] := substr( cPre, 1, n-1 )
               aA[ PRT_NAME ] := substr( cPre, n+1 )
            ELSE
               aA[ PRT_CAST ] := cPre
               aA[ PRT_NAME ] := cPre
            ENDIF

            IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aA[ PRT_CAST ] ) } ) > 0
               aA[ PRT_CAST ] := cWidget + '::' + aA[ PRT_CAST ]
            ENDIF

            /* Add to main array */
            aadd( aArgus, aA )

            nHBIdx := cPre:__enumIndex() + 1
            cHBIdx := hb_ntos( nHBIdx )
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

            IF empty( cFirstParamCast )
               cFirstParamCast := aA[ PRT_CAST ]
               IF '::' $ cFirstParamCast
                  cFirstParamCast := substr( cFirstParamCast, at( '::', cFirstParamCast ) + 2 )
               ENDIF
            ENDIF

            DO CASE
            CASE aA[ PRT_CAST ] == 'PHB_ITEM'
               aA[ PRT_BODY ] := 'hb_param( ' + cHBIdx + ', HB_IT_ANY )'
               aA[ PRT_DOC  ] := 'x' + cDocNM

            CASE aA[ PRT_CAST ] == 'T'
               aA[ PRT_BODY ] := 'hb_param( ' + cHBIdx + ', HB_IT_ANY )'
               aA[ PRT_DOC  ] := 'x' + cDocNM

            /* Values by reference */
            CASE aA[ PRT_CAST ] $ cInt .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + ' i' + cDocNM + ' = 0;', nHBIdx, 'i' + cDocNM, 'hb_storni' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + ' i' + cDocNM + ' = 0;', nHBIdx, 'i' + cDocNM, 'hb_stornl' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + ' i' + cDocNM + ' = 0;', nHBIdx, 'i' + cDocNM, 'hb_stornint' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n' + cDocNM

            CASE aA[ PRT_CAST ] $ cInt
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := 'hb_parnidef( ' + cHBIdx + ", " + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := 'hb_parni( ' + cHBIdx + ' )'
               ENDIF
               aA[ PRT_DOC  ] := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := 'hb_parnldef( ' + cHBIdx + ", " + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := 'hb_parnl( ' + cHBIdx + ' )'
               ENDIF
               aA[ PRT_DOC  ] := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ "qlonglong,qulonglong"
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := '( ' + aA[ PRT_CAST ] + ' ) hb_parnintdef( ' + cHBIdx + ", " + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := '( ' + aA[ PRT_CAST ] + ' ) hb_parnint( ' + cHBIdx + ' )'
               ENDIF
               aA[ PRT_DOC  ] := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  aA[ PRT_BODY ] := 'hb_parnintdef( ' + cHBIdx + ", " + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := 'hb_parnint( ' + cHBIdx + ' )'
               ENDIF
               aA[ PRT_DOC  ] := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal' .and. aA[ PRT_L_FAR ]
               aadd( aPre, { 'qreal qr' + cDocNM + ' = 0;', nHBIdx, 'qr' + cDocNM, 'hb_stornd'  } )
               aA[ PRT_BODY ] := '&qr' + cDocNM
               aA[ PRT_DOC  ] := '@n' + cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal,float'
               aA[ PRT_BODY ] := 'hb_parnd( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'n' + cDocNM

            CASE aA[ PRT_CAST ] == 'uchar' .and. aA[ PRT_L_FAR ] .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := '( char * ) hb_parc( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'uchar' .and. !( aA[ PRT_L_FAR ] ) .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := '( char ) hb_parni( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'n' + cDocNM

            CASE aA[ PRT_CAST ] == 'char' .and. aA[ PRT_L_FAR ] .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := '( char * ) hb_parc( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'char' .and. !( aA[ PRT_L_FAR ] ) .and. !( aA[ PRT_L_CONST ] )
               aA[ PRT_BODY ] := '( char ) hb_parni( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'c' + cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] ) .and. aA[ PRT_L_FAR ]
               aadd( aPre, { aA[ PRT_CAST ] + ' i' + cDocNM + ';', nHBIdx, 'i' + cDocNM, 'hb_storni' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@n' + cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               s := '( ' + aA[ PRT_CAST ] + ' ) hb_parni( ' + cHBIdx + ' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .AND. !( aA[ PRT_DEFAULT ] == "0" )
                  IF ascan( aEnum, aA[ PRT_DEFAULT ] ) > 0
                     ss := cWidget + '::' + aA[ PRT_DEFAULT ]
                  ELSE
                     ss := iif( '::' $ aA[ PRT_DEFAULT ], aA[ PRT_DEFAULT ], ;
                        iif( isDigit( left( aA[ PRT_DEFAULT ], 1 ) ), aA[ PRT_DEFAULT ], cWidget + '::' + aA[ PRT_DEFAULT ] ) )
                  ENDIF
                  ss := '( ' + aA[ PRT_CAST ] + ' ) ' + ss
                  aA[ PRT_BODY ] := '( HB_ISNUM( ' + cHBIdx + ' ) ? ' + s + ' : ' + ss + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'n' + cDocNM

            CASE aA[ PRT_CAST ] == 'bool' .and. aA[ PRT_L_FAR ]
               aadd( aPre, { 'bool i' + cDocNM + ' = 0;', nHBIdx, 'i' + cDocNM, 'hb_stornl' } )
               aA[ PRT_BODY ] := '&i' + cDocNM
               aA[ PRT_DOC  ] := '@l' + cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               aA[ PRT_BODY ] := 'hb_parl( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'l' + cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               IF !( s_isObject )
                  aA[ PRT_BODY ] := 'hbqt_par_QString( ' + cHBIdx + ' )'
               ELSE
                  aA[ PRT_BODY ] := cWidget+'::tr( hb_parc( ' + cHBIdx + ' ) )'
               ENDIF
               aA[ PRT_DOC  ] := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               aA[ PRT_BODY ] := 'hbqt_par_FT_Face( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'QIcon'
               aA[ PRT_BODY ] := 'QIcon( hbqt_par_QString( ' + cHBIdx + ' ) )'
               aA[ PRT_DOC  ] := 'c' + cDocNM

            CASE aA[ PRT_L_FAR ]
               aA[ PRT_BODY ] := 'hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'p' + cDocNM

            CASE aA[ PRT_L_AND ] .and. aA[ PRT_L_CONST ]
               s := '*hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               IF !empty( aA[ PRT_DEFAULT ] ) .and. ( '(' $ aA[ PRT_DEFAULT ] )
                  aA[ PRT_BODY ] := '( HB_ISPOINTER( ' + cHBIdx + ' ) ? ' + s + ' : ' + aA[ PRT_DEFAULT ] + ' )'
               ELSE
                  aA[ PRT_BODY ] := s
               ENDIF
               aA[ PRT_DOC  ] := 'p' + cDocNM

            CASE aA[ PRT_L_AND ]
               aA[ PRT_BODY ] := '*hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'p' + cDocNM

            CASE aA[ PRT_CAST ] == 'QChar'
               aA[ PRT_BODY ] := '*hbqt_par_' + aA[ PRT_CAST ] + '( ' + cHBIdx + ' )'
               aA[ PRT_DOC  ] := 'p' + cDocNM

            OTHERWISE
               aA[ PRT_BODY ] := ''
               aA[ PRT_DOC  ] := ''

            ENDCASE

            cParas += aA[ PRT_BODY ] + ', '
            cDocs  += aA[ PRT_DOC  ] + ', '
         NEXT

         IF right( cParas, 2 ) == ', '
            cParas := substr( cParas, 1, len( cParas ) - 2 )
            cDocs  := substr( cDocs , 1, len( cDocs  ) - 2 )
         ENDIF

         /* Build complete code line */
         IF .t.
            aA     := aRet       /*     T A K E   C A R E    */
            //
            cWdg   := 'hbqt_par_' + cWidget + '( 1 )->'
            cParas := '( ' + cParas + ' )'
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

               CASE "<T>" $ aA[ PRT_CAST ]
                  cCmd := ""
                  cPrgRet := ""

               CASE "QPair" $ aA[ PRT_CAST ]
                  cCmd := ""
                  cPrgRet := ""

               OTHERWISE
                  cCmd := 'hb_retptrGC( hbqt_gcAllocate_QList( new ' + aA[ PRT_CAST ] + '( ' + cCmn + ' ), true ) )'
                  cPrgRet := 'p' + cDocNM

               ENDCASE

            CASE aA[ PRT_CAST ] == 'T'
               cCmd := 'hb_retptr( ' + cCmn + ' )'
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_CAST ] == 'void'
               cCmd := cCmn
               cPrgRet := 'NIL'

            CASE aA[ PRT_CAST ] $ cInt
               cCmd := 'hb_retni( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               cCmd := 'hb_retnl( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               cCmd := 'hb_retnint( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal,float'
               cCmd := 'hb_retnd( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               cCmd := 'hb_retni( ( ' + aA[ PRT_CAST ] + ' ) ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               cCmd := 'hb_retl( ' + cCmn + ' )'
               cPrgRet := 'l' + cDocNM

            CASE aA[ PRT_CAST ] == 'char' .AND. aA[ PRT_L_FAR ]
               cCmd := 'hb_retc( ' + cCmn + ' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'char'
               cCmd := 'hb_retni( ' + cCmn + ' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               cCmd := 'hb_retc( ' + cCmn + '.toAscii().data()' + ' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               cCmd := 'hb_retc( ' + cCmn + ' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_L_FAR ] .AND. ( aA[ PRT_CAST ] $ "uchar" )
               //cCmd := 'hb_retptr( ( ' + aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               cCmd := 'hb_retc( ( const char * ) ' + cCmn + ' )'
               cPrgRet := 'p' + cDocNM

//            CASE aA[ PRT_L_FAR ]
            CASE aA[ PRT_L_FAR ] .AND. !( aA[ PRT_L_CONST ] )
               //cCmd := 'hb_retptr( ( ' + aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               IF ( isAqtObject( aA[ PRT_CAST ] ) )
                  cCmd := Get_Command( aA[ PRT_CAST ], cCmn, .F. )
               ELSE
                  cCmd := 'hb_retptr( ( ' + aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               ENDIF
               cPrgRet := 'p' + cDocNM

            CASE ( isAqtObject( aA[ PRT_CAST ] ) )        .AND. ;
                                      aA[ PRT_L_FAR ]   .AND. ;
                                      aA[ PRT_L_CONST ] .AND. ;
                                      ( "Abstract" $ aA[ PRT_CAST ] )
               //cCmd := 'hb_retptr( ( ' + aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               cCmd := 'hb_retptrGC( hbqt_gcAllocate_' + aA[ PRT_CAST ] + '( ( void * ) ' + cCmn + ', false ) )'
               cPrgRet := 'p' + cDocNM

            CASE ( isAqtObject( aA[ PRT_CAST ] ) )        .AND. ;
                                      aA[ PRT_L_FAR   ] .AND. ;
                                      aA[ PRT_L_CONST ] .AND. ;
                                      aA[ PRT_L_VIRT  ]
               //cCmd := 'hb_retptr( ( ' + aA[ PRT_CAST ] + '* ) ' + cCmn + ' )'
               cCmd := 'hb_retptrGC( hbqt_gcAllocate_' + aA[ PRT_CAST ] + '( ( void * ) ' + cCmn + ', false ) )'
               cPrgRet := 'p' + cDocNM

            CASE ( isAqtObject( aA[ PRT_CAST ] ) )        .AND. ;
                                      aA[ PRT_L_FAR   ] .AND. ;
                                      aA[ PRT_L_CONST ] .AND. ;
                                      aA[ PRT_L_CONST_LAST ]
               cCmd := Get_Command_1( aA[ PRT_CAST ], cCmn )
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_L_AND ] .AND. aA[ PRT_L_CONST ]
               cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_L_CONST ]
               cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
               cPrgRet := 'p' + cDocNM

            CASE aA[ PRT_L_AND ]
               cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
               cPrgRet := 'p' + cDocNM

            OTHERWISE
               /* No attribute is attached to return value */
               IF ( isAqtObject( aA[ PRT_CAST ] ) )
                  cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
                  cPrgRet := 'p' + cDocNM

               ELSE
                  OutStd( '<<< ' + cProto + ' | ' + aA[ PRT_CAST ] + ' >>>' + s_NewLine )
                  cCmd := ''
                  cPrgRet := ''

               ENDIF

            ENDCASE

            /* Lists to be disabled in parameters - TODO */
            IF "<" $ cPar
               cCmd := ""
            ENDIF

            IF ! Empty( cCmd )
               cCmd := StrTran( cCmd, '(  )', '()' ) + ';'
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ( lSuccess := ! Empty( cCmd ) )
      IF ( n := ascan( func_, {|e_| e_[ 1 ] == cFun } ) ) > 0
         func_[ n,2 ]++
         cHBFunc := cFun + '_' + hb_ntos( func_[ n, 2 ] )
         #if 0        /* TODO: Refine to get duplicate name resolution - this mechanism has problems */
         IF empty( cFirstParamCast )
            cHBFunc := cFun + '_' + hb_ntos( func_[ n, 2 ] )
         ELSE
            IF cFirstParamCast == func_[ n,3 ]
               cHBFunc := cFun + '_' + cFirstParamCast + "_" + hb_ntos( func_[ n, 2 ] )
            ELSE
               cHBFunc := cFun + '_' + cFirstParamCast
            ENDIF
         ENDIF
         func_[ n,3 ] := cFirstParamCast
         #endif
      ELSE
         cHBFunc := cFun
         aadd( func_, { cFun, 0, "" } )
      ENDIF

      aadd( txt_, "/*" )
      aadd( txt_, " * " + strtran( cProto, chr(13), '' ) )
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
         aadd( txt_, "      " + FP )
         aadd( txt_, "   else" )
         aadd( txt_, "   {" )
         aadd( txt_, '      HB_TRACE( ' + s_trMode + ', ( "............................... F=QT_' + upper( cWidget ) + '_' + upper( cHBFunc) + ' FP=' + FP + ' p is NULL" ) );')
         aadd( txt_, "   }" )

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

      aadd( doc_, 'Qt_' + cWidget + '_' + cHBFunc + '( p' + cWidget + ;
                        iif( empty( cDocs ), '', ', ' + cDocs ) + ' ) -> ' + cPrgRet )
      aadd( doc_, '' )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

STATIC FUNCTION isAqtObject( cCast )
   RETURN left( cCast, 1 ) == 'Q' .OR. left( cCast, 3 ) == "HBQ"

/*----------------------------------------------------------------------*/

STATIC FUNCTION ParseVariables( cProto, cWidget, txt_, doc_, aEnum, func_ )
   LOCAL aRet, aA, aPre
   LOCAL n
   LOCAL cRet, cFun, cDocs, cCmd
   LOCAL cWdg, cCmn, cPrgRet, cHBFunc, cDocNM
   LOCAL lSuccess
 * LOCAL cInt         := 'int,qint16,quint16,QChar,short,ushort'
   LOCAL cInt         := 'int,qint16,quint16,short,ushort'
   LOCAL cIntLong     := 'qint32,quint32,QRgb'
   LOCAL cIntLongLong := 'qint64,quint64,qlonglong,qulonglong'

   aPre   := {}
   cDocs  := ''

   aRet := {}
   n := at( ' ', cProto )
   IF n > 0
      IF .t.
         cRet := alltrim( substr( cProto, 1, n - 1 ) )
         cFun := alltrim( substr( cProto, n + 1    ) )

         aRet := array( PRT_ATTRB_MAX )

         /* Normalize */
         aRet[ PRT_CAST ] := cRet
         aRet[ PRT_NAME ] := aRet[ PRT_CAST ]

         IF ascan( aEnum, {|e| iif( empty( e ), .f., e == aRet[ PRT_CAST ] ) } ) > 0
            aRet[ PRT_CAST ] := cWidget + '::' + aRet[ PRT_CAST ]
         ENDIF

         /* Build complete code line */
         IF .t.
            aA     := aRet
            cWdg   := 'hbqt_par_' + cWidget + '( 1 )->'
            cCmn   := cWdg + cFun
            cDocNM := THIS_PROPER( aA[ PRT_NAME ] )

            DO CASE
            CASE aA[ PRT_CAST ] == 'T'
               cCmd := 'hb_ret( ' + cCmn + ' )'
               cPrgRet := 'x' + cDocNM

            CASE aA[ PRT_CAST ] $ cInt
               cCmd := 'hb_retni( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLong
               cCmd := 'hb_retnl( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ cIntLongLong
               cCmd := 'hb_retnint( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] $ 'double,qreal,float'
               cCmd := 'hb_retnd( ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE ( '::' $ aA[ PRT_CAST ] )
               cCmd := 'hb_retni( ( ' + aA[ PRT_CAST ] + ' ) ' + cCmn + ' )'
               cPrgRet := 'n' + cDocNM

            CASE aA[ PRT_CAST ] == 'bool'
               cCmd := 'hb_retl( ' + cCmn + ' )'
               cPrgRet := 'l' + cDocNM

            CASE aA[ PRT_CAST ] == 'char*'
               cCmd := 'hb_retc( ' + cCmn + ' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'char'
               cCmd := 'hb_retni( ' + cCmn + ' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'QString'
               cCmd := 'hb_retc( ' + cCmn + '.toLatin1().data()' + ' )'
               cPrgRet := 'c' + cDocNM

            CASE aA[ PRT_CAST ] == 'FT_Face'
               cCmd := 'hb_retc( ' + cCmn + ' )'
               cPrgRet := 'c' + cDocNM

            OTHERWISE
               /* No attribute is attached to return value */
               IF (left( aA[ PRT_CAST ], 1 ) == 'Q')
                  cCmd := Get_Command( aA[ PRT_CAST ], cCmn )
                  cPrgRet := 'p' + cDocNM

               ELSE
                  OutStd( '<<< ' + cProto + ' | ' + aA[ PRT_CAST ] + ' >>>'  + s_NewLine )
                  cCmd := ''
                  cPrgRet := ''

               ENDIF

            ENDCASE

            IF !empty( cCmd )
               cCmd := strtran( cCmd, '(  )', '()' ) + ';'
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ( lSuccess := !empty( cCmd ) )
      IF ( n := ascan( func_, {|e_| e_[ 1 ] == cFun } ) ) > 0
         func_[ n, 2 ]++
         cHBFunc := cFun + '_' + hb_ntos( func_[ n, 2 ] )
      ELSE
         cHBFunc := cFun
         aadd( func_, { cFun, 0 } )
      ENDIF

      aadd( txt_, "/*" )
      aadd( txt_, " * " + strtran( cProto, chr( 13 ), '' ) )
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

      aadd( doc_, 'Qt_' + cWidget + '_' + cHBFunc + '( p' + cWidget + ;
                        iif( empty( cDocs ), '', ', ' + cDocs ) + ' ) -> ' + cPrgRet )
      aadd( doc_, '' )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildHeader( txt_, nMode )

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
   aadd( txt_, '#include "../hbqt.h"'                                                          )
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
   LOCAL cHlp := ''

   cHlp += ''                                                                               + s_NewLine
   cHlp += 'Syntax:'                                                                        + s_NewLine
   cHlp += '   hbqtgen.exe [Options] [[@]<QtProjectFile.qtp>] [<QtHeaderFile.qth, ...>]'    + s_NewLine
   cHlp += ''                                                                               + s_NewLine
   cHlp += 'Options:'                                                                       + s_NewLine
   cHlp += '   -O<OutputPath>   [ e.g. c:\harbour\contrib\hbqt ]        [D] Current folder' + s_NewLine
   cHlp += '   -I<InputPath>    [ e.g. c:\harbour\contrib\hbqt\protos ] [D] Current folder' + s_NewLine
   cHlp += '   -D<DocFilesPath> [ e.g. c:\harbour\contrib\hbqt\doc    ] [D] Current folder' + s_NewLine
   cHlp += ' '                                                                              + s_NewLine
   cHlp += '   -c<compile>      If QT env is set, attempts to compile resulting .cpp'       + s_NewLine
   cHlp += ''                                                                               + s_NewLine
   cHlp += '   -noretobject     Skip object returning methods'                              + s_NewLine
   cHlp += ''                                                                               + s_NewLine

   OutStd( cHlp )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispLogo()
   LOCAL cHlp := ''

   cHlp += ''                                                        + s_NewLine
   cHlp += "Harbour Source Generator for QT " + HBRawVersion()       + s_NewLine
   cHlp += "Copyright (c) 2009, Pritpal Bedi <pritpal@vouchcac.com>" + s_NewLine
   cHlp += "http://harbour-project.org/"                             + s_NewLine
   cHlp += ''                                                        + s_NewLine

   OutStd( cHlp )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour ", "" )

/*----------------------------------------------------------------------*/

STATIC FUNCTION CreateTarget( cFile, txt_ )
   LOCAL cContent := ""

   AEval( txt_, { |e| cContent += RTrim( e ) + hb_osNewLine() } )

   /* Save it only if it has changed. */
   IF !( hb_MemoRead( cFile ) == cContent )

      OutStd( "Creating: " + cFile + hb_osNewLine() )

      hb_MemoWrit( cFile, cContent )
   ENDIF

   RETURN hb_FileExists( cFile )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Class( cWidget, cls_, doc_, cPathOut, subCls_ )
   LOCAL cFile, s, n, cM, ss, cCall, sm, i
   LOCAL nLen := len( cWidget )
   LOCAL txt_ := {}, mth_:= {}

   BuildHeader( @txt_, 1 )

   aadd( txt_, '' )

   n := ascan( cls_, {|e_| left( lower( e_[ 1 ] ), 7 ) == 'inherit' .and. !empty( e_[ 2 ] ) } )
   //s := 'CREATE CLASS ' + cWidget + iif( n > 0, ' INHERIT HbQtObjectHandler' + iif( empty( cls_[ n, 2 ] ), "" , ", " + cls_[ n, 2 ] ), '' )
   s := 'CREATE CLASS ' + cWidget + ' INHERIT HbQtObjectHandler' + iif( n > 0, ", " + cls_[ n, 2 ], '' )

   aadd( txt_, s                                 )
   aadd( txt_, '   '                             )
   #if 0
   aadd( txt_, '   VAR     pPtr'                 )
   aadd( txt_, '   '                             )
   aadd( txt_, '   ERROR HANDLER onError()'      )
   aadd( txt_, '   '                             )
   #endif
   aadd( txt_, '   METHOD  new( ... )'           )
   //aadd( txt_, '   METHOD  configure( xObject )' )
   aadd( txt_, '   '                             )

   /* Populate METHODS */
   FOR EACH s IN doc_
      n := at( '-> ', s )
      IF n > 0
         s     := substr( s, 1, n-1 )
         s     := strtran( s, '@', '' )    /* Just in Case */
         s     := strtran( s, '::', '_' )  /* Just in Case */

         n     := at( cWidget, s )
         sm    := substr( s, n+nLen+1 )

         ss    := 'p' + cWidget + ','
         cM    := strtran( sm, ss, '' )
         ss    := 'p' + cWidget
         cM    := strtran( cM, ss, '' )
         cM    := strtran( cM, '(  )', '()' )
         cM    := strtran( cM, '(  ', '( ' )
         cM    := iif( len( cM ) > 35, cM, pad( cM,35 ) )

         ss    := 'p' + cWidget
         cCall := strtran( s, ss, '::pPtr' )

         aadd( mth_, { cM, cCall } )
         aadd( txt_, '   METHOD  ' + cM  )
      ENDIF
   NEXT
   aadd( txt_, '   '                                               )
   aadd( txt_, '   ENDCLASS'                                       )
   aadd( txt_, '   '                                               )
   aadd( txt_, '   '                                               )
   aadd( txt_, 'METHOD ' + cWidget + ':new( ... )'                 )
   aadd( txt_, '   LOCAL p'                                        )
   aadd( txt_, '   FOR EACH p IN { ... }'                          )
   aadd( txt_, '      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )' )
   aadd( txt_, '   NEXT'                                           )
   aadd( txt_, '   ::pPtr := Qt_' + cWidget + '( ... )'            )
   aadd( txt_, '   RETURN Self'                                    )
   aadd( txt_, '   '                                               )
   #if 0
   aadd( txt_, '   '                                               )
   aadd( txt_, 'METHOD ' + cWidget + ':configure( xObject )'       )
   aadd( txt_, '   IF hb_isObject( xObject )'                      )
   aadd( txt_, '      ::pPtr := xObject:pPtr'                      )
   aadd( txt_, '   ELSEIF hb_isPointer( xObject )'                 )
   aadd( txt_, '      ::pPtr := xObject'                           )
   aadd( txt_, '   ENDIF'                                          )
   aadd( txt_, '   RETURN Self'                                    )
   aadd( txt_, '   '                                               )
   aadd( txt_, '   '                                               )
   aadd( txt_, 'METHOD ' + cWidget + ':onError()'                  )
   aadd( txt_, '   RETURN hbqt_showError( __GetMessage() )'        )
   aadd( txt_, '   '                                               )
   #endif
   /* Define methods */
   FOR i := 1 TO len( mth_ )
      aadd( txt_, ''                                               )
      aadd( txt_, 'METHOD ' + cWidget + ':' + mth_[ i, 1 ]         )
      aadd( txt_, '   RETURN ' + ParsePtr( mth_[ i, 2 ] )          )
      aadd( txt_, ''                                               )
   NEXT

   IF !empty( subCls_ )
      aadd( txt_, ''                                               )
      aeval( subCls_, {|e| aadd( txt_, e ) } )
      aadd( txt_, ''                                               )
   ENDIF

   /* Generate .prg */
   cFile := GetSourcePathByLib( cWidget, cPathOut, '.prg', 'T', cls_ )
   CreateTarget( cFile, txt_ )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Document( cWidget, cls_, doc_, cPathDoc, subCls_, docum_ )
   LOCAL cText, n, n1, n2, nLen, pWidget, cRet, cLib, cFile, i, cInherits
   LOCAL txt_:= {}

   HB_SYMBOL_UNUSED( cls_    )
   HB_SYMBOL_UNUSED( subCls_ )

   n := ascan( cls_, {|e_| left( lower( e_[ 1 ] ), 7 ) == 'inherit' .and. !empty( e_[ 2 ] ) } )
   cInherits := iif( n > 0, cls_[ n, 2 ], '' )

   IF ascan( aGui, cWidget ) > 0
      cLib := "qtgui"
   ELSEIF ascan( aCore, cWidget ) > 0
      cLib := "qtcore"
   ELSEIF ascan( aWebkit, cWidget ) > 0
      cLib := "qtwebkit"
   ELSEIF ascan( aNetwork, cWidget ) > 0
      cLib := "qtnetwork"
   ELSE
      cLib := ""
   ENDIF

   aadd( txt_, '/* '  )
   aadd( txt_, ' *  hbQTgen v1.0 - Harbour Callable Wrappers Generator for Qt v4.5.3+' )
   aadd( txt_, ' *  Please do not modify this document as it is subject to change in future.' )
   aadd( txt_, ' *  Pritpal Bedi <bedipritpal@hotmail.com>' )
   aadd( txt_, ' */ ' )
   aadd( txt_, '    '  )
   aadd( txt_, '/*  $DOC$         ' )
   aadd( txt_, '    $TEMPLATE$    ' )
   aadd( txt_, '        Class' )
   aadd( txt_, '    $NAME$        ' )
   aadd( txt_, '        ' + cWidget + '()' )
   aadd( txt_, '    $CATEGORY$    ' )
   aadd( txt_, '        ' + 'Harbour Bindings for Qt' )
   aadd( txt_, '    $SUBCATEGORY$ ' )
   aadd( txt_, '        ' + 'GUI'   )
   aadd( txt_, '    $EXTERNALLINK$' )
   aadd( txt_, '        ' + 'http://doc.trolltech.com/4.5/' + lower( cWidget ) + '.html' )
   aadd( txt_, '    $ONELINER$    ' )
   aadd( txt_, '        ' + 'Creates a new ' + cWidget + ' object.' )
   aadd( txt_, '    $INHERITS$    ' )
   aadd( txt_, '        ' + cInherits )
   aadd( txt_, '    $SYNTAX$      ' )
   aadd( txt_, '        ' + cWidget + '():new( ... )' )
   aadd( txt_, '        ' + cWidget + '():from( pPtr_OR_oObj_of_type_' + cWidget +' )' )
   aadd( txt_, '        ' + cWidget + '():configure( pPtr_OR_oObj_of_type_' + cWidget +' )' )
   aadd( txt_, '    $ARGUMENTS$   ' )
   aadd( txt_, '        ' )
   aadd( txt_, '    $RETURNS$     ' )
   aadd( txt_, '        ' + 'An instance of the object of type ' + cWidget )
   aadd( txt_, '    $METHODS$     ' )
   nLen    := len( cWidget )
   n       := at( cWidget, doc_[ 1 ] )
   pWidget := 'p' + cWidget
   FOR i := 1 TO len( doc_ )
      cText := doc_[ i ]
      IF !empty( cText )
         cText := substr( cText, n+nLen+1 )
         cText := strtran( cText, pWidget + ', ', '' )
         cText := strtran( cText, pWidget, '' )
         cText := strtran( cText, '(  )', '()' )
         n1    := at( '->', cText )
         cRet  := alltrim( substr( cText, n1+2 ) )
         cText := substr( cText, 1, n1-1 )
         n2    := max( 50, len( cText ) )
         cText := padR( cText, n2 )
         IF !empty( cRet )
            aadd( txt_, '        :' + cText + ' -> ' + cRet )
         ENDIF
      ENDIF
   NEXT
   aadd( txt_, '        ' )
   aadd( txt_, '    $DESCRIPTION$ ' )
   aadd( txt_, '        ' )
   aadd( txt_, '    $EXAMPLES$    ' )
   FOR EACH cText IN docum_
      IF !empty( cText )
         aadd( txt_, '        ' + cText )
      ENDIF
   NEXT
   aadd( txt_, '        ' )
   aadd( txt_, '    $TESTS$       ' )
   aadd( txt_, '        ' )
   aadd( txt_, '    $STATUS$      ' )
   aadd( txt_, '        ' + 'R' )
   aadd( txt_, '    $COMPLIANCE$  ' )
   aadd( txt_, '        ' + 'Not Clipper Compatible' )
   aadd( txt_, '    $PLATFORMS$   ' )
   aadd( txt_, '        ' + 'Windows, Linux, MacOS, OS2' )
   aadd( txt_, '    $VERSION$     ' )
   aadd( txt_, '        ' + '4.5.3' )
   aadd( txt_, '    $FILES$       ' )
   aadd( txt_, '        ' + 'Prg Source   : ' + 'contrib/hbqt' + iif( empty( cLib ), '', '/' + cLib ) + '/T' + cWidget + '.prg' )
   aadd( txt_, '        ' + 'C++ Wrappers : ' + 'contrib/hbqt' + iif( empty( cLib ), '', '/' + cLib ) + '/'  + cWidget + '.cpp' )
   aadd( txt_, '        ' + 'Library      : ' + 'hb' + cLib )
   aadd( txt_, '    $SEEALSO$     ' )
 * aadd( txt_, '        ' + iif( empty( cInherits ), "", cInherits + ", " ) + 'http://doc.trolltech.com/4.5/' + lower( cWidget ) + '.html' )
   aadd( txt_, '        ' + cInherits )
   aadd( txt_, '    $END$         ' )
   aadd( txt_, ' */               ' )

   cFile := cPathDoc + s_PathSep + 'en' + s_PathSep + 'class_' + lower( cWidget ) + ".txt"

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_GarbageFile( cpp_, cPathOut )
   LOCAL cFile := iif( empty( cPathOut ), "", cPathOut + s_PathSep ) + "hbqt_garbage.h"
   LOCAL txt_ := {}
   LOCAL s

   aadd( txt_, "/*"                                                                            )
   aadd( txt_, " * $" + "Id" + "$"                                                             )
   aadd( txt_, " */"                                                                           )
   aadd( txt_, ""                                                                              )
   aadd( txt_, "/* -------------------------------------------------------------------- */"    )
   aadd( txt_, "/* WARNING: Automatically generated source file. DO NOT EDIT!           */"    )
   aadd( txt_, "/*          Instead, edit corresponding .qth file,                      */"    )
   aadd( txt_, "/*          or the generator tool itself, and run regenarate.           */"    )
   aadd( txt_, "/* -------------------------------------------------------------------- */"    )
   aadd( txt_, " " )

   FOR EACH s IN cpp_
      aadd( txt_, "extern QT_G_FUNC( hbqt_gcRelease_" + s + " );" )
   NEXT
   aadd( txt_, "" )

   IF ( lNewGCtoQT )
      FOR EACH s IN cpp_
         aadd( txt_, "extern void * hbqt_gcAllocate_" + s + "( void * pObj, bool bNew );" )
      NEXT
      aadd( txt_, "" )
   ENDIF

   RETURN CreateTarget( cFile, txt_ )

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MakeFile( cpp_, prg_, cPathOut )
   LOCAL cFile, s, i
   LOCAL txt_ := {}, hdr_:= {}, aSubs := {}
   LOCAL hbm_ := {}

   HB_SYMBOL_UNUSED( cpp_ )
   HB_SYMBOL_UNUSED( prg_ )

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

   /* Sub Libraries */
   IF !empty( aWebkit )
      aadd( aSubs, { "qtwebkit" , aWebkit  } )
   ENDIF
   IF !empty( aNetwork )
      aadd( aSubs, { "qtnetwork", aNetwork } )
   ENDIF
   IF !empty( aGui )
      aadd( aSubs, { "qtgui"    , aGui     } )
   ENDIF
   IF !empty( aCore )
      aadd( aSubs, { "qtcore"   , aCore    } )
   ENDIF
   //
   FOR i := 1 TO len( aSubs )
      txt_:= {}
      aeval( hdr_, {|e| aadd( txt_, e ) } )
      hbm_ := {}
      aeval( hdr_, {|e| aadd( hbm_, e ) } )
      aadd( txt_, "CPP_SOURCES := \" )
      //
      FOR EACH s IN aSubs[ i, 2 ]
         aadd( txt_, "   " + s + ".cpp \" )
         aadd( hbm_, + s + ".cpp" )
      NEXT
      aadd( hbm_, "" )
      aadd( txt_, "" )
      aadd( txt_, "" )
      aadd( txt_, "" )
      aadd( txt_, "PRG_SOURCES := \" )
      FOR EACH s IN aSubs[ i, 2 ]
         aadd( txt_, "   " + "T" + s + ".prg \" )
         aadd( hbm_, + "T" + s + ".prg" )
      NEXT
      aadd( txt_, "" )
      aadd( txt_, "# Don't delete this comment, it's here to ensure empty" )
      aadd( txt_, "# line above is kept intact." )
      //
      cFile := iif( empty( cPathOut ), "", cPathOut + s_PathSep + aSubs[ i, 1 ] + s_PathSep )
      CreateTarget( cFile + "filelist.mk", txt_ )
      CreateTarget( cFile + "filelist.hbm", hbm_ )
   NEXT

   RETURN NIL

/*----------------------------------------------------------------------*/

#define  CRLF   chr( 13 )+chr( 10 )
#define  QT_VER  "4.5"
#define  QT_WEB  "http://doc.trolltech.com/"

FUNCTION Build_HTML( cWidget, aHM_, aHF_, cPathOut, docum_ )
   LOCAL cFile := cPathOut + s_PathSep + 'html' + s_PathSep + cWidget + '.htm'
   LOCAL i, j, s, nCounter := 0, cPara
   LOCAL nCols, aHTML
   LOCAL setColorBG, setColorText, setColorTable
   LOCAL aColumns, cCell, uColData

   HB_SYMBOL_UNUSED( aHM_ )

   setColorText  := '#000000'
   SetColorBG    := '#FFFFFF'
   SetColorTable := '#D0D0D0'
   aColumns      := { { 1,'Function', 'C', 100 },;
                      { 2,'Returns' , 'C',  20 } }

   aHTML  := {}
   nCols  := len( aColumns )

   aadd( aHtml, "<HTML>" )
   Build_HtmlHeader( @aHTML )

   s := '<BODY BGCOLOR="' + SetColorBG + '" TEXT="' + SetColorText + '"' + '>'
   aadd( aHtml, s )

   Build_HtmlTable( @aHTML, , SetColorTable )
   aadd( aHtml, '<TBODY>' )

   /*       Class Documentation */
   s := "<TR><TD colspan=" + hb_ntos( nCols ) + " align=CENTER bgcolor=#ffff80><B>" + "CLASS REFERENCE" + "</B></TD></TR>"
   aadd( aHtml, s )
   s := "<TR><TD colspan=" + hb_ntos( nCols ) + " align=CENTER bgcolor=#ffff80><B>" + "Source: /harbour/contrib/hbqt/T" + cWidget + ".prg" + "</B></TD></TR>"
   aadd( aHtml, s )
   s := QT_WEB + QT_VER + "/" + lower( cWidget ) + ".htm"
   s := "<TR><TD colspan=" + hb_ntos( nCols ) + ' align=CENTER bgcolor=#CFBFA1><B><a href="' + s + '">' + s + "</a></B></TD></TR>"
   aadd( aHtml, s )
   //
   IF !empty( docum_ )
      s := "<TR>"

      cPara := 'pr' + hb_ntos( ++nCounter )

      s += '<TD  class="only" ' + 'colspan=' + hb_ntos( nCols ) + '>'
      s += '<PRE id="' + cPara + '">'
      s += CRLF
      s += CRLF
      s += "IMPORTANT:  Call the constructor with exact number of parameter. "+ CRLF
      s += "            No defaults, otherwise application will GPF" + CRLF
      for i := 1 to len( docum_ )
         s += docum_[ i ] + CRLF
      next
      s += '</PRE>'

      aadd( aHtml, s + "</TR>" )
   ENDIF
   //
   FOR j := 1 TO len( aHM_ )
      s := "<TR>"

      FOR i := 1 TO nCols
         uColData := aHM_[ j, i ]
         if Empty( uColData )
            cCell := "&nbsp"
         else
            cCell := uColData
         endif
         s += '<TD>' + cCell
      next

      aadd( aHtml, s + "</TR>" )
   NEXT

   /* Function Documentation */
   s := "<TR><TD colspan=" + hb_ntos( nCols ) + " align=CENTER bgcolor=#ffff80><B>" + "FUNCTIONS REFERENCE" + "</B></TD></TR>"
   aadd( aHtml, s )
   s := "<TR><TD colspan=" + hb_ntos( nCols ) + " align=CENTER bgcolor=#ffff80><B>" + "Source: /harbour/contrib/hbqt/" + cWidget + ".cpp" + "</B></TD></TR>"
   aadd( aHtml, s )
   FOR j := 1 TO len( aHF_ )
      s := "<TR>"

      FOR i := 1 TO nCols
         uColData := aHF_[ j, i ]
         if Empty( uColData )
            cCell := "&nbsp"
         else
            cCell := uColData
         endif
         s += '<TD>' + cCell
      next

      aadd( aHtml, s + "</TR>" )
   NEXT

   aadd( aHtml, "</TABLE>"  )
   aadd( aHtml, "</CENTER>" )
   aadd( aHtml, "</BODY>"   )
   aadd( aHtml, "</HTML>"   )

   Return CreateTarget( cFile, aHTML )

/*----------------------------------------------------------------------*/

FUNCTION Build_HtmlTable( aHTML, cTitle, SetColorTable )
   Local s
   LOCAL nBorder       := 1
   LOCAL nCellSpacing  := 0
   LOCAL nCellPadding  := 4
   LOCAL nCols         := 2

   aadd( aHtml, '<CENTER>' )

   s := '<TABLE ' +;
        'BGCOLOR="'    + SetColorTable + '" ' +;
        'BORDER='      + hb_ntos( nBorder ) + ' ' +;
        'FRAME=ALL '   +;
        'CellPadding=' + hb_ntos( nCellPadding ) + ' ' +;
        'CellSpacing=' + hb_ntos( nCellSpacing ) + ' ' +;
        'COLS='        + hb_ntos( nCols ) + ' ' +;
        'WIDTH=90% '   +;
        '>'
   aadd( aHtml, s )

   if !Empty( cTitle )
      aadd( aHtml, '<CAPTION ALIGN=top><B>' + cTitle + '</B></CAPTION>' )
   endif

   Return NIL

/*----------------------------------------------------------------------*/

FUNCTION Build_HtmlHeader( aHTML )

   aadd( aHtml, '<head>                                                              ' )
   aadd( aHtml, '  <meta name="Author" CONTENT="Pritpal Bedi [pritpal@vouchcac.com]">' )
   aadd( aHtml, '  <meta http-equiv="content-style-type" content="text/css" >        ' )
   aadd( aHtml, '  <meta http-equiv="content-script-type" content="text/javascript"> ' )
   aadd( aHtml, '                                                                    ' )
   aadd( aHtml, '  <style type="text/css">                                           ' )
   aadd( aHtml, '    th                                                              ' )
   aadd( aHtml, '    {                                                               ' )
   aadd( aHtml, '      colspan          : 1;                                         ' )
   aadd( aHtml, '      text-align       : center;                                    ' )
   aadd( aHtml, '      vertical-align   : baseline;                                  ' )
   aadd( aHtml, '      horizontal-align : left;                                      ' )
   aadd( aHtml, '    }                                                               ' )
   aadd( aHtml, '    td                                                              ' )
   aadd( aHtml, '    {                                                               ' )
   aadd( aHtml, '      vertical-align   : top;                                       ' )
   aadd( aHtml, '      horizontal-align : left;                                      ' )
   aadd( aHtml, '    }                                                               ' )
   aadd( aHtml, '    td.only                                                         ' )
   aadd( aHtml, '    {                                                               ' )
   aadd( aHtml, '      cursor           : hand;                                      ' )
   aadd( aHtml, '      vertical-align   : top;                                       ' )
   aadd( aHtml, '      horizontal-align : left;                                      ' )
   aadd( aHtml, '    }                                                               ' )
   aadd( aHtml, '    pre                                                             ' )
   aadd( aHtml, '    {                                                               ' )
   aadd( aHtml, '      font-family      : Courier New;                               ' )
   aadd( aHtml, '      font-size        : .7em;                                      ' )
   aadd( aHtml, '      color            : black;                                     ' )
   aadd( aHtml, '      cursor           : text;                                      ' )
   aadd( aHtml, '    }                                                               ' )
   aadd( aHtml, '  </style>                                                          ' )
   aadd( aHtml, '                                                                    ' )
   aadd( aHtml, '</head>                                                             ' )

   RETURN Nil

/*----------------------------------------------------------------------*/

FUNCTION IsQObject( cWidget )
   STATIC aTree := {}

   IF empty( aTree )
      aadd( aTree, "QObject                                       " )
      aadd( aTree, "   QAbstractEventDispatcher                   " )
      aadd( aTree, "   QAbstractItemDelegate                      " )
      aadd( aTree, "      QItemDelegate                           " )
      aadd( aTree, "          QSqlRelationalDelegate              " )
      aadd( aTree, "      QStyledItemDelegate                     " )
      aadd( aTree, "   QAbstractItemModel                         " )
      aadd( aTree, "      QAbstractListModel                      " )
      aadd( aTree, "         QStringListModel                     " )
      aadd( aTree, "            QHelpIndexModel                   " )
      aadd( aTree, "      QAbstractProxyModel                     " )
      aadd( aTree, "         QSortFilterProxyModel                " )
      aadd( aTree, "      QAbstractTableModel                     " )
      aadd( aTree, "         QSqlQueryModel                       " )
      aadd( aTree, "            QSqlTableModel                    " )
      aadd( aTree, "               QSqlRelationalTableModel       " )
      aadd( aTree, "      QDirModel                               " )
      aadd( aTree, "      QFileSystemModel                        " )
      aadd( aTree, "      QHelpContentModel                       " )
      aadd( aTree, "      QProxyModel                             " )
      aadd( aTree, "      QStandardItemModel                      " )
      aadd( aTree, "   QAbstractMessageHandler                    " )
      aadd( aTree, "   QAbstractNetworkCache                      " )
      aadd( aTree, "      QNetworkDiskCache                       " )
      aadd( aTree, "   QAbstractTextDocumentLayout                " )
      aadd( aTree, "      QPlainTextDocumentLayout                " )
      aadd( aTree, "   QAbstractUriResolver                       " )
      aadd( aTree, "   QAccessibleBridgePlugin                    " )
      aadd( aTree, "   QAccessiblePlugin                          " )
      aadd( aTree, "   QAction                                    " )
      aadd( aTree, "      QMenuItem                               " )
      aadd( aTree, "      QWidgetAction                           " )
      aadd( aTree, "   QActionGroup                               " )
      aadd( aTree, "   QAssistantClient                           " )
      aadd( aTree, "   QAxFactory                                 " )
      aadd( aTree, "   QAxObject                                  " )
      aadd( aTree, "   QAxScript                                  " )
      aadd( aTree, "   QAxScriptManager                           " )
      aadd( aTree, "   QButtonGroup                               " )
      aadd( aTree, "   QClipboard                                 " )
      aadd( aTree, "   QCompleter                                 " )
      aadd( aTree, "   QCoreApplication                           " )
      aadd( aTree, "      QApplication                            " )
      aadd( aTree, "   QDataWidgetMapper                          " )
      aadd( aTree, "   QDBusAbstractAdaptor                       " )
      aadd( aTree, "   QDBusAbstractInterface                     " )
      aadd( aTree, "      QDBusConnectionInterface                " )
      aadd( aTree, "      QDBusInterface                          " )
      aadd( aTree, "   QDBusPendingCallWatcher                    " )
      aadd( aTree, "   QDBusServer                                " )
      aadd( aTree, "   QDesignerFormEditorInterface               " )
      aadd( aTree, "   QDesignerFormWindowManagerInterface        " )
      aadd( aTree, "   QDrag                                      " )
      aadd( aTree, "   QEventLoop                                 " )
      aadd( aTree, "   QExtensionFactory                          " )
      aadd( aTree, "   QExtensionManager                          " )
      aadd( aTree, "   QFileSystemWatcher                         " )
      aadd( aTree, "   QFtp                                       " )
      aadd( aTree, "   QFutureWatcher                             " )
      aadd( aTree, "   QGraphicsItemAnimation                     " )
      aadd( aTree, "   QGraphicsScene                             " )
      aadd( aTree, "   QGraphicsSvgItem                           " )
      aadd( aTree, "   QGraphicsTextItem                          " )
      aadd( aTree, "   QGraphicsWidget                            " )
      aadd( aTree, "   QHelpEngineCore                            " )
      aadd( aTree, "      QHelpEngine                             " )
      aadd( aTree, "   QHelpSearchEngine                          " )
      aadd( aTree, "   QHttp                                      " )
      aadd( aTree, "   QIconEnginePlugin                          " )
      aadd( aTree, "   QIconEnginePluginV2                        " )
      aadd( aTree, "   QImageIOPlugin                             " )
      aadd( aTree, "   QInputContext                              " )
      aadd( aTree, "   QInputContextPlugin                        " )
      aadd( aTree, "   QIODevice                                  " )
      aadd( aTree, "      Q3Socket                                " )
      aadd( aTree, "      Q3SocketDevice                          " )
      aadd( aTree, "      QAbstractSocket                         " )
      aadd( aTree, "         QTcpSocket                           " )
      aadd( aTree, "            QSslSocket                        " )
      aadd( aTree, "         QUdpSocket                           " )
      aadd( aTree, "      QBuffer                                 " )
      aadd( aTree, "         QTemporaryFile                       " )
      aadd( aTree, "      QFile                                   " )
      aadd( aTree, "      QLocalSocket                            " )
      aadd( aTree, "      QNetworkReply                           " )
      aadd( aTree, "      QProcess                                " )
      aadd( aTree, "   QItemSelectionModel                        " )
      aadd( aTree, "   QLayout                                    " )
      aadd( aTree, "      QBoxLayout                              " )
      aadd( aTree, "         Q3HBoxLayout                         " )
      aadd( aTree, "         Q3VBoxLayout                         " )
      aadd( aTree, "         QHBoxLayout                          " )
      aadd( aTree, "         QVBoxLayout                          " )
      aadd( aTree, "      QFormLayout                             " )
      aadd( aTree, "      QGridLayout                             " )
      aadd( aTree, "      QStackedLayout                          " )
      aadd( aTree, "   QLibrary                                   " )
      aadd( aTree, "   QLocalServer                               " )
      aadd( aTree, "   QMimeData                                  " )
      aadd( aTree, "   QMovie                                     " )
      aadd( aTree, "   QNetworkAccessManager                      " )
      aadd( aTree, "   QNetworkCookieJar                          " )
      aadd( aTree, "   QObjectCleanupHandler                      " )
      aadd( aTree, "   QPictureFormatPlugin                       " )
      aadd( aTree, "   QPluginLoader                              " )
      aadd( aTree, "   QScriptEngine                              " )
      aadd( aTree, "   QScriptEngineDebugger                      " )
      aadd( aTree, "   QScriptExtensionPlugin                     " )
      aadd( aTree, "   QSessionManager                            " )
      aadd( aTree, "   QSettings                                  " )
      aadd( aTree, "   QSharedMemory                              " )
      aadd( aTree, "   QShortcut                                  " )
      aadd( aTree, "   QSignalMapper                              " )
      aadd( aTree, "   QSignalSpy                                 " )
      aadd( aTree, "   QSocketNotifier                            " )
      aadd( aTree, "   QSound                                     " )
      aadd( aTree, "   QSqlDriver                                 " )
      aadd( aTree, "   QSqlDriverPlugin                           " )
      aadd( aTree, "   QStyle                                     " )
      aadd( aTree, "      QCommonStyle                            " )
      aadd( aTree, "         QMotifStyle                          " )
      aadd( aTree, "            QCDEStyle                         " )
      aadd( aTree, "         QWindowsStyle                        " )
      aadd( aTree, "            QCleanlooksStyle                  " )
      aadd( aTree, "               QGtkStyle                      " )
      aadd( aTree, "            QPlastiqueStyle                   " )
      aadd( aTree, "            QWindowsXPStyle                   " )
      aadd( aTree, "               QWindowsVistaStyle             " )
      aadd( aTree, "   QStylePlugin                               " )
      aadd( aTree, "   QSvgRenderer                               " )
      aadd( aTree, "   QSyntaxHighlighter                         " )
      aadd( aTree, "   QSystemTrayIcon                            " )
      aadd( aTree, "   QTcpServer                                 " )
      aadd( aTree, "   QTextCodecPlugin                           " )
      aadd( aTree, "   QTextDocument                              " )
      aadd( aTree, "   QTextObject                                " )
      aadd( aTree, "      QTextBlockGroup                         " )
      aadd( aTree, "         QTextList                            " )
      aadd( aTree, "      QTextFrame                              " )
      aadd( aTree, "         QTextTable                           " )
      aadd( aTree, "   QThread                                    " )
      aadd( aTree, "   QThreadPool                                " )
      aadd( aTree, "   QTimeLine                                  " )
      aadd( aTree, "   QTimer                                     " )
      aadd( aTree, "   QTranslator                                " )
      aadd( aTree, "   QUiLoader                                  " )
      aadd( aTree, "   QUndoGroup                                 " )
      aadd( aTree, "   QUndoStack                                 " )
      aadd( aTree, "   QValidator                                 " )
      aadd( aTree, "   QWebFrame                                  " )
      aadd( aTree, "   QWebHistoryInterface                       " )
      aadd( aTree, "   QWebPage                                   " )
      aadd( aTree, "   QWebPluginFactory                          " )
      aadd( aTree, "   QWidget                                    " )
      aadd( aTree, "      QAbstractButton                         " )
      aadd( aTree, "         Q3Button                             " )
      aadd( aTree, "         QCheckBox                            " )
      aadd( aTree, "         QPushButton                          " )
      aadd( aTree, "            QCommandLinkButton                " )
      aadd( aTree, "         QRadioButton                         " )
      aadd( aTree, "         QToolButton                          " )
      aadd( aTree, "      QAbstractSlider                         " )
      aadd( aTree, "         QDial                                " )
      aadd( aTree, "         QScrollBar                           " )
      aadd( aTree, "         QSlider                              " )
      aadd( aTree, "      QAbstractSpinBox                        " )
      aadd( aTree, "         QDateTimeEdit                        " )
      aadd( aTree, "            QDateEdit                         " )
      aadd( aTree, "            QTimeEdit                         " )
      aadd( aTree, "         QDoubleSpinBox                       " )
      aadd( aTree, "         QSpinBox                             " )
      aadd( aTree, "      QAxWidget                               " )
      aadd( aTree, "      QCalendarWidget                         " )
      aadd( aTree, "      QComboBox                               " )
      aadd( aTree, "         QFontComboBox                        " )
      aadd( aTree, "      QDesignerActionEditorInterface          " )
      aadd( aTree, "      QDesignerFormWindowInterface            " )
      aadd( aTree, "      QDesignerObjectInspectorInterface       " )
      aadd( aTree, "      QDesignerPropertyEditorInterface        " )
      aadd( aTree, "      QDesignerWidgetBoxInterface             " )
      aadd( aTree, "      QDesktopWidget                          " )
      aadd( aTree, "      QDialog                                 " )
      aadd( aTree, "         QAbstractPrintDialog                 " )
      aadd( aTree, "            QPrintDialog                      " )
      aadd( aTree, "         QColorDialog                         " )
      aadd( aTree, "         QErrorMessage                        " )
      aadd( aTree, "         QFileDialog                          " )
      aadd( aTree, "         QFontDialog                          " )
      aadd( aTree, "         QInputDialog                         " )
      aadd( aTree, "         QMessageBox                          " )
      aadd( aTree, "         QPageSetupDialog                     " )
      aadd( aTree, "         QPrintPreviewDialog                  " )
      aadd( aTree, "         QProgressDialog                      " )
      aadd( aTree, "         QWizard                              " )
      aadd( aTree, "      QDialogButtonBox                        " )
      aadd( aTree, "      QDockWidget                             " )
      aadd( aTree, "      QFocusFrame                             " )
      aadd( aTree, "      QFrame                                  " )
      aadd( aTree, "         QAbstractScrollArea                  " )
      aadd( aTree, "            QAbstractItemView                 " )
      aadd( aTree, "               QColumnView                    " )
      aadd( aTree, "               QHeaderView                    " )
      aadd( aTree, "               QListView                      " )
      aadd( aTree, "                  QHelpIndexWidget            " )
      aadd( aTree, "                  QListWidget                 " )
      aadd( aTree, "                  QUndoView                   " )
      aadd( aTree, "               QTableView                     " )
      aadd( aTree, "                  QTableWidget                " )
      aadd( aTree, "               QTreeView                      " )
      aadd( aTree, "                  QHelpContentWidget          " )
      aadd( aTree, "                  QTreeWidget                 " )
      aadd( aTree, "            QGraphicsView                     " )
      aadd( aTree, "            QMdiArea                          " )
      aadd( aTree, "            QPlainTextEdit                    " )
      aadd( aTree, "            QScrollArea                       " )
      aadd( aTree, "            QTextEdit                         " )
      aadd( aTree, "               QTextBrowser                   " )
      aadd( aTree, "         QLabel                               " )
      aadd( aTree, "         QLCDNumber                           " )
      aadd( aTree, "         QSplitter                            " )
      aadd( aTree, "         QStackedWidget                       " )
      aadd( aTree, "         QToolBox                             " )
      aadd( aTree, "      QGLWidget                               " )
      aadd( aTree, "      QGroupBox                               " )
      aadd( aTree, "      QHelpSearchQueryWidget                  " )
      aadd( aTree, "      QHelpSearchResultWidget                 " )
      aadd( aTree, "      QLineEdit                               " )
      aadd( aTree, "      QMainWindow                             " )
      aadd( aTree, "      QMdiSubWindow                           " )
      aadd( aTree, "      QMenu                                   " )
      aadd( aTree, "      QMenuBar                                " )
      aadd( aTree, "      QPrintPreviewWidget                     " )
      aadd( aTree, "      QProgressBar                            " )
      aadd( aTree, "      QRubberBand                             " )
      aadd( aTree, "      QSizeGrip                               " )
      aadd( aTree, "      QSplashScreen                           " )
      aadd( aTree, "      QSplitterHandle                         " )
      aadd( aTree, "      QStatusBar                              " )
      aadd( aTree, "      QSvgWidget                              " )
      aadd( aTree, "      QTabBar                                 " )
      aadd( aTree, "      QTabWidget                              " )
      aadd( aTree, "      QToolBar                                " )
      aadd( aTree, "      QWebView                                " )
      aadd( aTree, "      QWizardPage                             " )
      aadd( aTree, "      QWorkspace                              " )

      aeval( aTree, {| e,i | aTree[ i ] := alltrim( e ) } )
   ENDIF

   RETURN ascan( aTree, {|e| e == cWidget } ) > 0

/*----------------------------------------------------------------------*/

FUNCTION IsMemObject( cWidget )
   STATIC aObj := {}

   IF empty( aObj )
      aadd( aObj, "QBitArray             " )
      aadd( aObj, "QBitmap               " )
      aadd( aObj, "QBrush                " )
      aadd( aObj, "QByteArray            " )
      aadd( aObj, "QColor                " )
      aadd( aObj, "QCursor               " )
      aadd( aObj, "QDate                 " )
      aadd( aObj, "QDateTime             " )
      aadd( aObj, "QDir                  " )
      aadd( aObj, "QFileInfoList         " )
      aadd( aObj, "QFont                 " )
      aadd( aObj, "QFontInfo             " )
      aadd( aObj, "QFontMetrics          " )
      aadd( aObj, "QGradientStops        " )
      aadd( aObj, "QHttpRequestHeader    " )
      aadd( aObj, "QHttpResponseHeader   " )
      aadd( aObj, "QIcon                 " )
      aadd( aObj, "QImage                " )
      aadd( aObj, "QKeySequence          " )
      aadd( aObj, "QLine                 " )
      aadd( aObj, "QLineF                " )
      aadd( aObj, "QLocale               " )
      aadd( aObj, "QMatrix               " )
      aadd( aObj, "QModelIndex           " )
      aadd( aObj, "QObjectList           " )
      aadd( aObj, "QPainterPath          " )
      aadd( aObj, "QPalette              " )
      aadd( aObj, "QPen                  " )
      aadd( aObj, "QPixmap               " )
      aadd( aObj, "QPointF               " )
      aadd( aObj, "QRect                 " )
      aadd( aObj, "QRectF                " )
      aadd( aObj, "QRegExp               " )
      aadd( aObj, "QRegion               " )
      aadd( aObj, "QSize                 " )
      aadd( aObj, "QSizeF                " )
      aadd( aObj, "QSizePolicy           " )
      aadd( aObj, "QStringList           " )
      aadd( aObj, "QTableWidgetItem      " )
      aadd( aObj, "QTextBlockFormat      " )
      aadd( aObj, "QTextCharFormat       " )
      aadd( aObj, "QTextCursor           " )
      aadd( aObj, "QTextDocumentFragment " )
      aadd( aObj, "QTextFormat           " )
      aadd( aObj, "QTextFrameFormat      " )
      aadd( aObj, "QTextImageFormat      " )
      aadd( aObj, "QTextLength           " )
      aadd( aObj, "QTextLine             " )
      aadd( aObj, "QTextListFormat       " )
      aadd( aObj, "QTextOption           " )
      aadd( aObj, "QTextTableCellFormat  " )
      aadd( aObj, "QTextTableFormat      " )
      aadd( aObj, "QTime                 " )
      aadd( aObj, "QTransform            " )
      aadd( aObj, "QUrl                  " )
      aadd( aObj, "QVariant              " )
      aadd( aObj, "QWebHistoryItem       " )
      aadd( aObj, "QWebHitTestResult     " )
      aadd( aObj, "QWebSecurityOrigin    " )
      aadd( aObj, "QWidgetList           " )

      aeval( aObj, {| e,i | aObj[ i ] := alltrim( e ) } )
   ENDIF

   RETURN ascan( aObj, {| e | e == cWidget } ) > 0

/*----------------------------------------------------------------------*/

FUNCTION Get_Command_1( cWgt, cCmn )

   RETURN 'hb_retptrGC( hbqt_gcAllocate_' + cWgt + '( new ' + cWgt + '( *( ' + cCmn + ' ) ), true ) )'

/*----------------------------------------------------------------------*/

FUNCTION Get_Command( cWgt, cCmn, lNew )
   LOCAL  cRet

   DEFAULT lNew TO .T.

   IF ( lNewGCtoQT )
      IF lNew
         cRet := 'hb_retptrGC( hbqt_gcAllocate_' + cWgt + '( new ' + cWgt + '( ' + cCmn + ' ), true ) )'
      ELSE
         cRet := 'hb_retptrGC( hbqt_gcAllocate_' + cWgt + '( ' + cCmn + ', false ) )'
      ENDIF
   ELSE
      cRet := 'hb_retptrGC( hbqt_ptrTOgcpointer( new ' + cWgt + '( ' + cCmn + ' ), hbqt_gcRelease_' + cWgt +' ) )'
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

FUNCTION JustACall()
   RETURN nil

/*----------------------------------------------------------------------*/
