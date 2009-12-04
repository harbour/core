/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               23Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"
#include "hbqt.ch"

#include "hbide.ch"

/*----------------------------------------------------------------------*/

PROCEDURE AppSys()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE JustACall()
   RETURN

/*----------------------------------------------------------------------*/

FUNCTION ExecPopup( aPops, aPos, qParent )
   LOCAL i, qPop, qPoint, qAct, nAct, cAct, xRet, pAct

   qPop := QMenu():new( IF( hb_isObject( qParent ), QT_PTROF( qParent ), NIL ) )

   FOR i := 1 TO len( aPops )
      qPop:addAction( aPops[ i, 1 ] )
   NEXT

   qPoint := QPoint():new( aPos[ 1 ], aPos[ 2 ] )
   pAct := qPop:exec_1( QT_PTROF( qPoint ) )
   qAct := QAction():configure( pAct )
   IF !empty( qAct:pPtr ) .and. !empty( cAct := qAct:text() )
      IF ( nAct := ascan( aPops, {|e_| e_[ 1 ] == cAct } ) ) > 0
         xRet := eval( aPops[ nAct,2 ] )
      ENDIF
   ENDIF

   qPop:pPtr := 0

   RETURN xRet

/*----------------------------------------------------------------------*/

FUNCTION MenuAddSep( oMenu )

   oMenu:addItem( { NIL, NIL, XBPMENUBAR_MIS_SEPARATOR, NIL } )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION CreateTarget( cFile, txt_ )
   LOCAL hHandle := fcreate( cFile )
   LOCAL cNewLine := hb_OsNewLine()

   IF hHandle != -1
      aeval( txt_, { |e| fWrite( hHandle, e + cNewLine ) } )
      fClose( hHandle )
   ENDIF

   RETURN file( cFile )

/*----------------------------------------------------------------------*/

FUNCTION PosAndSize( qWidget )

   RETURN hb_ntos( qWidget:x() )     + "," + hb_ntos( qWidget:y() )      + "," + ;
          hb_ntos( qWidget:width() ) + "," + hb_ntos( qWidget:height() ) + ","

/*----------------------------------------------------------------------*/

FUNCTION GetYesNo( cMsg, cInfo, cTitle )
   LOCAL oMB

   DEFAULT cTitle TO "Option Please!"

   oMB := QMessageBox():new()
   oMB:setText( "<b>"+ cMsg +"</b>" )
   IF !empty( cInfo )
      oMB:setInformativeText( cInfo )
   ENDIF
   oMB:setIcon( QMessageBox_Information )
   oMB:setParent( SetAppWindow():pWidget )
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setWindowTitle( cTitle )
   oMB:setStandardButtons( QMessageBox_Yes + QMessageBox_No )

   RETURN ( oMB:exec() == QMessageBox_Yes )

/*----------------------------------------------------------------------*/

FUNCTION FetchAFile( oWnd, cTitle, aFlt, cDftDir )
   LOCAL oDlg, cFile

   DEFAULT cTitle  TO "Please Select a File"
   DEFAULT aFlt    TO { { "All Files", "*.*" } }
   DEFAULT cDftDir TO hb_dirBase()

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )

   oDlg:title       := cTitle
   oDlg:center      := .t.
   oDlg:fileFilters := aFlt

   cFile := oDlg:open( cDftDir, , .f. )

   RETURN cFile

/*----------------------------------------------------------------------*/

FUNCTION ReadSource( cTxtFile )
   LOCAL cLine, nHandle, aTxt :={}

   if ( nHandle := fopen( cTxtFile ) ) != -1
      do WHILE ( hb_fReadLine( nHandle, @cLine ) == 0 )
         aadd( aTxt, cLine )
      enddo
      aadd( aTxt, cLine )
      fclose( nHandle )
   endif

   RETURN aTxt

/*----------------------------------------------------------------------*/

FUNCTION EvalAsString( cExp )
   LOCAL cValue

   BEGIN SEQUENCE
      cValue := eval( &( "{|| " + cExp + "}" ) )
   RECOVER
      cValue := cExp
   END SEQUENCE

   IF !hb_isChar( cValue )
      cValue := ""
   ENDIF

   RETURN cValue

/*----------------------------------------------------------------------*/

FUNCTION FetchHbiStructFromBuffer( cBuffer )
   RETURN PullHbiStruct( hb_atokens( cBuffer, _EOL ) )

/*----------------------------------------------------------------------*/

FUNCTION FetchHbiStructFromFile( cProject )
   RETURN PullHbiStruct( ReadSource( cProject ) )

/*----------------------------------------------------------------------*/

STATIC FUNCTION PullHbiStruct( a_ )
   LOCAL n, s, nPart, cKey, cVal, ss
   LOCAL aPrp := { "Type", "Title", "Location", "WorkingFolder", "DestinationFolder", ;
                                            "Output", "LaunchParams", "LaunchProgram" }

   LOCAL a1_0 := afill( array( PRJ_PRP_PRP_VRBLS ), "" )
   LOCAL a1_1 := {}
   local a2_0 := {}
   local a2_1 := {}
   local a3_0 := {}
   local a3_1 := {}
   local a4_0 := {}
   local a4_1 := {}

   IF .t.
      FOR EACH ss IN a_
         s := alltrim( ss )

         IF .t.
            DO CASE
            CASE s == "[ PROPERTIES ]"
               nPart := PRJ_PRP_PROPERTIES
            CASE s == "[ FLAGS ]"
               nPart := PRJ_PRP_FLAGS
            CASE s == "[ SOURCES ]"
               nPart := PRJ_PRP_SOURCES
            CASE s == "[ METADATA ]"
               nPart := PRJ_PRP_METADATA
            OTHERWISE
               DO CASE
               CASE nPart == PRJ_PRP_PROPERTIES
                  IF ( n := at( "=", s ) ) > 0
                     cKey := alltrim( substr( s, 1, n-1 ) )
                     cVal := alltrim( substr( s, n+1 ) )
                     IF ( n := ascan( aPrp, cKey ) ) > 0
                        a1_0[ n ] := cVal
                     ENDIF
                  ENDIF
               CASE nPart == PRJ_PRP_FLAGS
                  aadd( a2_0, s )

               CASE nPart == PRJ_PRP_SOURCES
                  aadd( a3_0, s )

               CASE nPart == PRJ_PRP_METADATA
                  aadd( a4_0, s )
                  IF !( "#" == left( s,1 ) )
                     IF ( n := at( "=", s ) ) > 0
                        cKey := alltrim( substr( s, 1, n-1 ) )
                        cVal := EvalAsString( alltrim( substr( s, n+1 ) ) )
                        aadd( a4_1, { "<"+ cKey +">", cVal } )
                     ENDIF
                  ENDIF
               ENDCASE
            ENDCASE
         ENDIF
      NEXT

      /* General Properties */
      FOR EACH s IN a1_0
         aadd( a1_1, ParseWithMetaData( s, a4_1 ) )
      NEXT

      /* Parse Flags */
      IF !empty( a2_0 )
         FOR EACH s IN a2_0
            aadd( a2_1, ParseWithMetaData( s, a4_1 ) )
         NEXT
      ENDIF

      /* Parse Files */
      IF !empty( a3_0 )
         FOR EACH s IN a3_0
            IF !( "#" == left( s,1 ) ) .and. !empty( s )
               aadd( a3_1, ParseWithMetaData( s, a4_1 ) )
            ENDIF
         NEXT
      ENDIF

   ENDIF

   RETURN { { a1_0, a1_1 }, { a2_0, a2_1 }, { a3_0, a3_1 }, { a4_0, a4_1 } }

/*----------------------------------------------------------------------*/

FUNCTION SetupMetaKeys( a_ )
   LOCAL s, n, cKey, cVal
   LOCAL a4_1 := {}

   FOR EACH s IN a_
      IF !( "#" == left( s,1 ) )
         IF ( n := at( "=", s ) ) > 0
            cKey := alltrim( substr( s, 1, n-1 ) )
            cVal := EvalAsString( alltrim( substr( s, n+1 ) ) )
            aadd( a4_1, { "<"+ cKey +">", cVal } )
         ENDIF
      ENDIF
   NEXT

   RETURN a4_1

/*----------------------------------------------------------------------*/

FUNCTION ApplyMetaData( s, a_ )
   LOCAL k

   IF !empty( a_ )
      FOR k := 1 TO len( a_ )
         s := strtran( s, a_[ k,2 ], a_[ k,1 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION ParseWithMetaData( s, a_ )
   LOCAL k

   IF !empty( a_ )
      FOR k := 1 TO len( a_ )
         s := strtran( s, a_[ k,1 ], a_[ k,2 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION ArrayToMemo( a_ )
   LOCAL s := ""

   aeval( a_, {|e| s += e + CRLF } )

   RETURN s

/*----------------------------------------------------------------------*/
