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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 *    hbide_PathProc()
 *
 * See COPYING for licensing terms.
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
#include "fileio.ch"

#include "xbp.ch"

#include "hbide.ch"

STATIC aRegList

/*----------------------------------------------------------------------*/

PROCEDURE AppSys()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE JustACall()
   RETURN

/*----------------------------------------------------------------------*/

FUNCTION ExecPopup( aPops, aPos, qParent )
   LOCAL i, qPop, qPoint, qAct, nAct, cAct, xRet, pAct

   qPop := QMenu():new( IIF( hb_isObject( qParent ), qParent, NIL ) )

   FOR i := 1 TO len( aPops )
      IF empty( aPops[ i,1 ] )
         qPop:addSeparator()
      ELSE
         qPop:addAction( aPops[ i, 1 ] )
      ENDIF
   NEXT

   qPoint := QPoint():new( aPos[ 1 ], aPos[ 2 ] )
   pAct   := qPop:exec_1( qPoint )
   qAct   := QAction():configure( pAct )

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

   IF hHandle != F_ERROR
      aeval( txt_, { |e| fWrite( hHandle, e + cNewLine ) } )
      fClose( hHandle )
   ENDIF

   RETURN file( cFile )

/*----------------------------------------------------------------------*/

FUNCTION PosAndSize( qWidget )

   RETURN hb_ntos( qWidget:x() )     + "," + hb_ntos( qWidget:y() )      + "," + ;
          hb_ntos( qWidget:width() ) + "," + hb_ntos( qWidget:height() ) + ","

/*----------------------------------------------------------------------*/

FUNCTION ShowWarning( cMsg, cInfo, cTitle )
   LOCAL oMB

   DEFAULT cTitle TO "Information"

   oMB := QMessageBox():new()
   oMB:setText( cMsg )
   IF !empty( cInfo )
      oMB:setInformativeText( cInfo )
   ENDIF
   oMB:setIcon( QMessageBox_Critical )
   oMB:setParent( SetAppWindow():pWidget )
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setWindowTitle( cTitle )

   RETURN oMB:exec()

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

FUNCTION GetYesNoCancel( cMsg, cInfo, cTitle )
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
   oMB:setStandardButtons( QMessageBox_Yes + QMessageBox_No + QMessageBox_Cancel )

   RETURN oMB:exec()

/*----------------------------------------------------------------------*/

FUNCTION FetchAFile( oWnd, cTitle, aFlt, cDftDir, cDftSuffix )
   LOCAL oDlg, cFile

   DEFAULT cTitle  TO "Please Select a File"
   DEFAULT aFlt    TO { { "All Files", "*.*" } }
   DEFAULT cDftDir TO hb_dirBase()

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )

   oDlg:title       := cTitle
   oDlg:center      := .t.
   oDlg:fileFilters := aFlt
   IF hb_isChar( cDftSuffix )
      oDlg:oWidget:setDefaultSuffix( cDftSuffix )
   ENDIF

   cFile := oDlg:open( cDftDir, , .f. )

   RETURN cFile

/*----------------------------------------------------------------------*/

FUNCTION SaveAFile( oWnd, cTitle, aFlt, cDftFile, cDftSuffix )
   LOCAL oDlg, cFile

   DEFAULT cTitle  TO "Please Select a File"

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )

   oDlg:title       := cTitle
   oDlg:center      := .t.
   oDlg:fileFilters := aFlt
   IF hb_isChar( cDftSuffix )
      oDlg:oWidget:setDefaultSuffix( cDftSuffix )
   ENDIF

   cFile := oDlg:saveAs( cDftFile, .f., .t. )

   RETURN cFile

/*----------------------------------------------------------------------*/
/* Function to user select a existing folder
 * 25/12/2009 - 19:10:41 - vailtom
 */
FUNCTION FetchADir( oWnd, cTitle, cDftDir )
   LOCAL oDlg, cFile

   DEFAULT cTitle  TO "Please Select a Folder"
   DEFAULT cDftDir TO hb_dirBase()

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )

   oDlg:title  := cTitle
   oDlg:center := .t.
   oDlg:oWidget:setFileMode( 4 )

   cFile := oDlg:open( cDftDir, , .f. )

   IF hb_isChar( cFile )
      //cFile := strtran( cFile, "/", HB_OSPATHSEPARATOR() )
      RETURN cFile
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

FUNCTION ReadSource( cTxtFile )
   LOCAL cFileBody := hb_MemoRead( cTxtFile )

   cFileBody := StrTran( cFileBody, Chr( 13 ) )

   RETURN hb_ATokens( cFileBody, Chr( 10 ) )

/*----------------------------------------------------------------------*/

FUNCTION EvalAsString( cExp )
   LOCAL cValue

   BEGIN SEQUENCE WITH { || break() }
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

   IF ! Empty( a_ )
      FOR EACH k IN a_
         s := StrTran( s, PathNormalized( k[ 2 ], .f. ), k[ 1 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION ParseWithMetaData( s, a_ )
   LOCAL k

   IF ! Empty( a_ )
      FOR EACH k IN a_ DESCEND
         s := StrTran( s, k[ 1 ], k[ 2 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION ArrayToMemo( a_ )
   LOCAL s := ""

   aeval( a_, {|e| s += e + CRLF } )

   s += CRLF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION MemoToArray( s )
   LOCAL aLine := hb_ATokens( StrTran( RTrim( s ), CRLF, _EOL ), _EOL )
   LOCAL nNewSize := 0
   LOCAL line

   FOR EACH line IN aLine DESCEND
      IF ! Empty( line )
         nNewSize := line:__enumIndex()
         EXIT
      ENDIF
   NEXT

   ASize( aLine, nNewSize )

   RETURN aLine

/*----------------------------------------------------------------------*/

FUNCTION IsValidPath( cPath, cPathDescr )

   DEFAULT cPathDescr TO ''

   IF hb_dirExists( cPath )
      RETURN .T.
   End

   IF empty( cPathDescr )
      MsgBox( 'The specified path is invalid "' + cPath + '"' )
   ELSE
      MsgBox( 'The specified path is invalid for ' + cPathDescr + ': "' + cPath + '"' )
   End
   RETURN .F.

/*----------------------------------------------------------------------*/

FUNCTION IsValidText( cSourceFile )
   LOCAL cExt

   hb_fNameSplit( cSourceFile, , , @cExt )
   cExt := lower( cExt )

   RETURN ( cExt $ ".c,.cpp,.prg,.h,.ch,.txt,.log,.ini,.env,.ppo,"+;
                   ".cc,.hbc,.hbp,.hbm,.xml,.bat,.sh" )

/*----------------------------------------------------------------------*/

FUNCTION IsValidSource( cSourceFile )
   LOCAL cExt

   hb_fNameSplit( cSourceFile, , , @cExt )
   cExt := lower( cExt )

   RETURN ( cExt $ ".c,.cpp,.prg,.res,.rc" )

/*----------------------------------------------------------------------*/

FUNCTION PathNormalized( cPath, lLower )
   LOCAL S

   DEFAULT lLower TO .T.

   s := strtran( cPath, "\", "/" )

   RETURN IIF( lLower, lower( s ), s )

/*----------------------------------------------------------------------*/
/*
 * This function fills an array with the list of regular expressions that will
 * identify the errors messages retrieved from during the build process.
 * 29/12/2009 - 12:43:26 - vailtom
 */
#define MSG_TYPE_ERR    1
#define MSG_TYPE_INFO   2
#define MSG_TYPE_WARN   3

#define CLR_MSG_ERR     'red'
#define CLR_MSG_INFO    'brown'
#define CLR_MSG_WARN    'blue'

STATIC FUNCTION BuildRegExpressList( aRegList )
   AAdd( aRegList, { MSG_TYPE_WARN, hb_RegexComp( ".*: warning.*"                 ) } )
   AAdd( aRegList, { MSG_TYPE_WARN, hb_RegexComp( ".*\) Warning W.*"              ) } )
   AAdd( aRegList, { MSG_TYPE_WARN, hb_RegexComp( "^Warning W([0-9]+).*"          ) } )

   AAdd( aRegList, { MSG_TYPE_ERR , hb_RegexComp( ".*: error.*"                   ) } )
   AAdd( aRegList, { MSG_TYPE_ERR , hb_RegexComp( ".*\) Error E.*"                ) } )
   AAdd( aRegList, { MSG_TYPE_ERR , hb_RegexComp( "^Error E([0-9]+).*"            ) } )
   AAdd( aRegList, { MSG_TYPE_ERR , hb_RegexComp( "^Error: ."                     ) } )
   AAdd( aRegList, { MSG_TYPE_ERR , hb_RegexComp( ".*:([0-9]+):([\w|\s]*)error.*" ) } )
   AAdd( aRegList, { MSG_TYPE_ERR , hb_RegexComp( ".*:\(\.\w+\+.*\):.*"           ) } )
   AAdd( aRegList, { MSG_TYPE_ERR , hb_RegexComp( ".*: fatal\s.*"                 ) } )

   AAdd( aRegList, { MSG_TYPE_INFO, hb_RegexComp( ".*: note.*"                    ) } )
   AAdd( aRegList, { MSG_TYPE_INFO, hb_RegexComp( ".*: In function '.*"           ) } )
   AAdd( aRegList, { MSG_TYPE_INFO, hb_RegexComp( "^(\s*).*\s: see.*"             ) } )

   RETURN aRegList

/*----------------------------------------------------------------------*//*
* Catch source file name & line error from an msg status from compiler result.
 * 29/12/2009 - 13:22:29 - vailtom
 */
FUNCTION ParseFNfromStatusMsg( cText, cFileName, nLine, lValidText )
   LOCAL regLineN := hb_RegexComp( ".*(\(([0-9]+)\)|:([0-9]+):|\s([0-9]+):).*" )
   LOCAL aList
   LOCAL nPos

   DEFAULT lValidText TO .T.

   cFileName := ''
   nLine     := 0

 * Validate if current text is a error/warning/info message.
 * 29/12/2009 - 22:51:39 - vailtom
   IF lValidText
      nPos := aScan( aRegList, {| reg | !Empty( hb_RegEx( reg[ 2 ], cText ) ) } )

      IF ( nPos <= 0 )
         RETURN .F.
      End
   End

   aList     := hb_RegEx( regLineN, cText )

   IF !Empty(aList)
      nLine := alltrim( aList[2] )
      cText := Substr( cText, 1, At( nLine, cText ) -1 )
      cText := alltrim( cText ) + '('

      nLine := strtran( nLine, ":", "" )
      nLine := strtran( nLine, "(", "" )
      nLine := strtran( nLine, ")", "" )
      nLine := VAL( alltrim( nLine ) )
   End

   IF (nPos := hb_At( '(', cText )) > 0
      cFileName := alltrim( Subst( cText, 1, nPos -1 ) )
   ELSE
      IF (nPos := At( 'referenced from', Lower( cText ) )) <> 00
         cFileName := Subst( cText, nPos + Len( 'referenced from' ) )
      ELSE
       * GCC & MSVC filename detect...
         IF Subst( cText, 2, 1 ) == ':'
            nPos := hb_At( ':', cText, 3 )
         ELSE
            nPos := hb_At( ':', cText )
         End
         IF nPos <> 00
            cFileName := Subst( cText, 1, nPos-1 )
         End
      End
   End

   cFileName := strtran( cFileName, "(", "" )
   cFileName := strtran( cFileName, ")", "" )
   cFileName := alltrim( cFileName )

   cFileName := strtran( cFileName, "\\", "/" )        && Fix for the BCC
   cFileName := strtran( cFileName, "\" , "/" )

   IF (nPos := Rat( ' ', cFileName )) <> 00
      cFileName := Subst( cFileName, nPos+1 )
   End

   IF Subst( cFileName, 2, 1 ) == ':'
      nPos := hb_At( ':', cFileName, 3 )
   ELSE
      nPos := hb_At( ':', cFileName )
   End

   IF nPos <> 00
      cFileName := Subst( cFileName, 1, nPos-1 )
   End

   cFileName := alltrim( cFileName )
   RETURN !Empty( cFileName )

/*----------------------------------------------------------------------*/
/*
 * This function parses compiler result and hightlight errors & warnings using
 * regular expressions. (vailtom)
 *
 * More about Qt Color names:
 *  http://www.w3.org/TR/SVG/types.html#ColorKeywords
 *
 * 28/12/2009 - 16:17:37
 */
FUNCTION ConvertBuildStatusMsgToHtml( cText, oWidget )
   LOCAL aColors  := { CLR_MSG_ERR, CLR_MSG_INFO, CLR_MSG_WARN }
   LOCAL aLines
   LOCAL cLine
   LOCAL nPos

   IF aRegList == NIL
      aRegList := {}
      BuildRegExpressList( aRegList )
   End

   oWidget:clear()

   cText := StrTran( cText, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   cText := StrTran( cText, Chr( 13 )            , Chr( 10 ) )
   cText := StrTran( cText, Chr( 10 ) + Chr( 10 ), Chr( 10 ) )

   /* Convert some chars to valid HTML chars */
   DO WHILE "<" $ cText
      cText := StrTran( cText, "<", "&lt;" )
   ENDDO
   DO WHILE ">" $ cText
      cText := StrTran( cText, ">", "&gt;" )
   ENDDO
   aLines := hb_aTokens( cText, Chr( 10 ) )

   FOR EACH cLine IN aLines

      IF !Empty( cLine )
         nPos := aScan( aRegList, {| reg | !Empty( hb_RegEx( reg[ 2 ], cLine ) ) } )

         IF ( nPos > 0 )
            cLine := '<font color=' + aColors[ aRegList[nPos,1] ] + '>' + cLine + '</font>'
         End
      ENDIF

      oWidget:append( cLine )
   NEXT

   RETURN cText

/*----------------------------------------------------------------------*/

FUNCTION FilesToSources( aFiles )
   LOCAL aSrc := {}
   LOCAL s

   FOR EACH s IN aFiles
      IF IsValidSource( s )
         aadd( aSrc, s )
      ENDIF
   NEXT

   RETURN aSrc

/*----------------------------------------------------------------------*/

FUNCTION ParseKeyValPair( s, cKey, cVal )
   LOCAL n, lYes := .f.

   IF ( n := at( "=", s ) ) > 0
      cKey := alltrim( substr( s, 1, n - 1 ) )
      cVal := alltrim( substr( s, n + 1 ) )
      lYes := ( !empty( cKey ) .and. !empty( cVal ) )
   ENDIF

   RETURN ( lYes )

/*----------------------------------------------------------------------*/

FUNCTION IdeDbg( ... )
   HB_TRACE( HB_TR_ALWAYS, ... )
   RETURN nil

/*----------------------------------------------------------------------*/

/*
 * Return the next untitled filename available.
 * 01/01/2010 - 19:40:17 - vailtom
 */
FUNCTION GetNextUntitled()
   STATIC nCount := 0
      nCount ++
   RETURN nCount

/*----------------------------------------------------------------------*/

/*
 * Return the next TAB_ID or IDE_ID available.
 * 02/01/2010 - 10:47:16 - vailtom
 */
FUNCTION GetNextUniqueID()
   STATIC nCount := 0
   
   IF nCount > 4294967295
      nCount := 0
   ENDIF
   RETURN ++nCount

/*----------------------------------------------------------------------*/
/*
 * Check if cFilename has a extension... and add cDefaultExt if not exist.
 * 01/01/2010 - 20:48:10 - vailtom
 */
FUNCTION CheckDefaultExtension( cFileName, cDefaultExt )
   LOCAL cPath, cFile, cExt
   hb_fNameSplit( cFileName, @cPath, @cFile, @cExt )
   IF Empty( cExt )
      cExt := cDefaultExt
   End
   RETURN cPath + HB_OSPATHSEPARATOR() + cFile + HB_OSPATHSEPARATOR() + cExt

/*----------------------------------------------------------------------*/

FUNCTION hbide_PathProc( cPathR, cPathA )
   LOCAL cDirA
   LOCAL cDirR, cDriveR, cNameR, cExtR

   IF Empty( cPathA )
      RETURN cPathR
   ENDIF

   hb_FNameSplit( cPathR, @cDirR, @cNameR, @cExtR, @cDriveR )

   IF ! Empty( cDriveR ) .OR. ( ! Empty( cDirR ) .AND. Left( cDirR, 1 ) $ hb_osPathDelimiters() )
      RETURN cPathR
   ENDIF

   hb_FNameSplit( cPathA, @cDirA )

   IF Empty( cDirA )
      RETURN cPathR
   ENDIF

   RETURN hb_FNameMerge( cDirA + cDirR, cNameR, cExtR )

/*----------------------------------------------------------------------*/

