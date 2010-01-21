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
 *                               27Dec2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"
#include "hbide.ch"

/*----------------------------------------------------------------------*/

#define customContextMenuRequested                1
#define textChanged                               2
#define copyAvailable                             3
#define modificationChanged                       4
#define redoAvailable                             5
#define selectionChanged                          6
#define undoAvailable                             7
#define updateRequest                             8
#define cursorPositionChanged                     9

#define blockCountChanged                         21
#define contentsChange                            22

#define EDT_LINNO_WIDTH                           50

/*----------------------------------------------------------------------*/

CLASS IdeEditsManager INHERIT IdeObject

   DATA   qContextMenu
   DATA   aActions                                INIT  {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD prepareTabWidget()
   METHOD removeSourceInTree( cSourceFile )
   METHOD addSourceInTree( cSourceFile )
   METHOD exeEvent( nMode, p )
   METHOD buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme )
   METHOD getTabBySource( cSource )
   METHOD getTabCurrent()
   METHOD getDocumentCurrent()
   METHOD getEditObjectCurrent()
   METHOD getEditCurrent()
   METHOD getEditorCurrent()
   METHOD getEditorByIndex( nIndex )
   METHOD getEditorByTabObject( oTab )
   METHOD getEditorByTabPosition( nPos )
   METHOD getEditorBySource( cSource )
   METHOD reLoad( cSource )
   METHOD isOpen( cSource )
   METHOD setSourceVisible( cSource )
   METHOD setSourceVisibleByIndex( nIndex )
   METHOD undo()
   METHOD redo()
   METHOD cut()
   METHOD copy()
   METHOD paste()
   METHOD selectAll()
   METHOD switchToReadOnly()
   METHOD convertSelection( cKey )
   METHOD insertText( cKey )
   METHOD insertSeparator()
   METHOD zoom( nKey )
   METHOD printPreview()
   METHOD paintRequested( pPrinter )
   METHOD setMark()
   METHOD gotoMark()
   METHOD goto()
   METHOD removeTabs()
   METHOD RemoveTrailingSpaces()
   METHOD getSelectedText()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:create( oIde )
   LOCAL oSub

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   ::qContextMenu := QMenu():new()

   aadd( ::aActions, { "TB_Cut"       , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Cut"        ) ) } )
   aadd( ::aActions, { "TB_Copy"      , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Copy"       ) ) } )
   aadd( ::aActions, { "TB_Paste"     , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Paste"      ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "TB_Undo"      , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Undo"       ) ) } )
   aadd( ::aActions, { "TB_Redo"      , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Redo"       ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "TB_Save"      , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Save"       ) ) } )
   aadd( ::aActions, { "TB_Close"     , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Close"      ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "TB_Compile"   , ::qContextMenu:addAction_4( ::oAC:getAction( "TB_Compile"    ) ) } )
   aadd( ::aActions, { "TB_CompilePPO", ::qContextMenu:addAction_4( ::oAC:getAction( "TB_CompilePPO" ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "Apply Theme"  , ::qContextMenu:addAction( "Apply Theme"                        ) } )

   oSub := QMenu():configure( ::qContextMenu:addMenu_1( "Split" ) )
   //
   aadd( ::aActions, { "Split H"      , oSub:addAction( "Split Horizontally" ) } )
   aadd( ::aActions, { "Split V"      , oSub:addAction( "Split Vertically"   ) } )
   aadd( ::aActions, { ""             , oSub:addSeparator() } )
   aadd( ::aActions, { "Close Split"  , oSub:addAction( "Close Split Window" ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:prepareTabWidget()

   ::oIde:oDa:oTabWidget := XbpTabWidget():new():create( ::oDa, , {0,0}, {10,10}, , .t. )
   ::oIde:oTabWidget := ::oDa:oTabWidget
   ::oIde:qTabWidget := ::oDa:oTabWidget:oWidget

   ::qTabWidget:setUsesScrollButtons( .f. )
   ::qTabWidget:setMovable( .t. )

   ::qTabWidget:setContextMenuPolicy( Qt_CustomContextMenu )
   ::connect( ::qTabWidget, "customContextMenuRequested(QPoint)", {|o,p| ::exeEvent( 1, p, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:removeSourceInTree( cSourceFile )
   LOCAL n

   IF !Empty( cSourceFile )
      IF ( n := aScan( ::aProjData, {|e_| e_[ TRE_ORIGINAL ] == cSourceFile .AND. e_[ 2 ] == "Opened Source" } ) ) > 0
         ::aProjData[ n,3 ]:delItem( ::oIde:aProjData[ n,1 ] )
         hb_adel( ::aProjData, n, .T. )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:addSourceInTree( cSourceFile )
   LOCAL cPath, cPathA, cFile, cExt, n, oParent
   LOCAL oGrand := ::oOpenedSources

   IF Empty( cSourceFile )
      RETURN nil
   End

   hb_fNameSplit( cSourceFile, @cPath, @cFile, @cExt )
   cPathA := hbide_pathNormalized( cPath )

   IF ( n := ascan( ::aEditorPath, {|e_| e_[ 2 ] == cPathA } ) ) == 0
      oParent := oGrand:addItem( cPath )
      aadd( ::aProjData, { oParent, "Editor Path", oGrand, cPathA, cSourceFile } )
      aadd( ::aEditorPath, { oParent, cPathA } )
   ELSE
      oParent := ::aEditorPath[ n,1 ]
   ENDIF

   aadd( ::aProjData, { oParent:addItem( cFile + cExt ), "Opened Source", oParent, ;
                                   cSourceFile, hbide_pathNormalized( cSourceFile ) } )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:exeEvent( nMode, p )
   //LOCAL qObj

   HB_SYMBOL_UNUSED( p )

   DO CASE
   CASE nMode == 1  // "customContextMenuRequested(QPoint)"
      #if 0
      qObj := QWidget():configure( ::qTabWidget:childAt_1( QPoint():configure( p ) ) )
      IF !e
      hbide_dbg( qObj:x(), qObj:y() )
      #endif
   CASE nMode == 2
   CASE nMode == 3
   ENDCASE

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme )

   IdeEditor():new():create( ::oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getTabBySource( cSource )

   cSource := hbide_pathNormalized( cSource, .t. )

   RETURN ascan( ::aTabs, {|e_| e_[ TAB_OEDITOR ]:pathNormalized == cSource } )

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getTabCurrent()
   LOCAL qTab, nTab

   qTab := ::qTabWidget:currentWidget()
   nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } )

   RETURN nTab

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getDocumentCurrent()
   LOCAL qTab, nTab

   IF ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } ) ) > 0
         RETURN QTextDocument():configure( ::aTabs[ nTab, TAB_OEDITOR ]:document() )
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditObjectCurrent()
   LOCAL qTab, nTab

   IF ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]:qCoEdit
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditCurrent()
   LOCAL qTab, nTab

   IF ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]:qCqEdit
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorCurrent()
   LOCAL qTab, nTab

   IF ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorByIndex( nIndex ) /* Index is 0 based */
   LOCAL pTab, a_

   IF hb_isNumeric( nIndex ) .AND. nIndex >= 0 .AND. nIndex < ::qTabWidget:count()
      pTab := ::qTabWidget:widget( nIndex )
      FOR EACH a_ IN ::aTabs
         IF !empty( a_[ TAB_OTAB ] ) .AND. hbqt_IsEqualGcQtPointer( a_[ TAB_OTAB ]:oWidget:pPtr, pTab )
            RETURN ::aTabs[ a_:__enumIndex(), TAB_OEDITOR ]
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorByTabObject( oTab )
   LOCAL nPos

   IF hb_isObject( oTab )
      IF ( nPos := ascan( ::aTabs, {|e_| e_[ TAB_OTAB ] == oTab } ) ) > 0
         RETURN ::aTabs[ nPos, TAB_OEDITOR ]
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorByTabPosition( nPos )

   IF hb_isNumeric( nPos ) .AND. nPos > 0 .AND. nPos <= len( ::aTabs )
      IF !empty( ::aTabs[ nPos, TAB_OEDITOR ] )
         RETURN ::aTabs[ nPos, TAB_OEDITOR ]
      ENDIF
   ENDIF
   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorBySource( cSource )
   LOCAL n

   cSource := hbide_pathNormalized( cSource, .t. )
   IF ( n := ascan( ::aTabs, {|e_| e_[ TAB_OEDITOR ]:pathNormalized == cSource } ) ) > 0
      RETURN ::aTabs[ n, TAB_OEDITOR ]
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:reLoad( cSource )
   LOCAL oEdit

   IF hb_fileExists( cSource ) .AND. hbide_isValidText( cSource )
      IF !empty( oEdit := ::getEditorBySource( cSource ) )
         oEdit:qEdit:clear()
         oEdit:qEdit:setPlainText( hb_memoread( hbide_pathToOSPath( cSource ) ) )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:isOpen( cSource )
   RETURN !empty( ::getEditorBySource( cSource ) )

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:setSourceVisible( cSource )
   LOCAL oEdit, nIndex

   IF !empty( oEdit := ::getEditorBySource( cSource ) )
      nIndex := ::qTabWidget:indexOf( oEdit:oTab:oWidget )
      IF ::qTabWidget:currentIndex() != nIndex
         ::qTabWidget:setCurrentIndex( nIndex )
      ELSE
         oEdit:setDocumentProperties()
      ENDIF
      RETURN .t.
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:setSourceVisibleByIndex( nIndex ) /* nIndex is 0 based */

   IF ::qTabWidget:count() == 0 .OR. nIndex >= ::qTabWidget:count()
      nIndex := 0
   ENDIF
   ::qTabWidget:setCurrentIndex( nIndex )
   ::getEditorByIndex( nIndex ):setDocumentProperties()

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:undo()
   IF !empty( ::qCurEdit )
      ::qCurEdit:undo()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:redo()
   IF !empty( ::qCurEdit )
      ::qCurEdit:redo()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:cut()
   IF !empty( ::qCurEdit )
      ::qCurEdit:cut()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:copy()
   IF !empty( ::qCurEdit )
      ::qCurEdit:copy()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:paste()
   IF !empty( ::qCurEdit )
      ::qCurEdit:paste()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:selectAll()
   IF !empty( ::qCurEdit )
      ::qCurEdit:selectAll()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:switchToReadOnly()
   IF !empty( ::qCurEdit )
      ::qCurEdit:setReadOnly( !( ::qCurEdit:isReadOnly() ) )
      ::oCurEditor:setTabImage()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:convertSelection( cKey )
   LOCAL cBuffer, i, s, nLen, c, nB, nL, qCursor

   IF !empty( ::qCurEdit )
      qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
      IF qCursor:hasSelection() .and. !empty( cBuffer := qCursor:selectedText() )
         DO CASE
         CASE cKey == "ToUpper"
            cBuffer := upper( cBuffer )
         CASE cKey == "ToLower"
            cBuffer := lower( cBuffer )
         CASE cKey == "Invert"
            s := ""
            nLen := len( cBuffer )
            FOR i := 1 TO nLen
               c := substr( cBuffer, i, 1 )
               s += iif( isUpper( c ), lower( c ), upper( c ) )
            NEXT
            cBuffer := s
         ENDCASE
         nL := len( cBuffer )
         nB := qCursor:position() - nL

         qCursor:beginEditBlock()
         qCursor:removeSelectedText()
         qCursor:insertText( cBuffer )
         qCursor:setPosition( nB )
         qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nL )

         ::qCurEdit:setTextCursor( qCursor )

         qCursor:endEditBlock()
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:insertSeparator()
   LOCAL oEdit, qCursor

   IF empty( oEdit := ::getEditObjectCurrent() )
      RETURN Self
   ENDIF

   qCursor := QTextCursor():configure( oEdit:qEdit:textCursor() )

   qCursor:beginEditBlock()
   qCursor:movePosition( QTextCursor_StartOfBlock )
   qCursor:insertBlock()
   qCursor:movePosition( QTextCursor_PreviousBlock )
   qCursor:insertText( ::cSeparator )
   qCursor:movePosition( QTextCursor_NextBlock )
   qCursor:movePosition( QTextCursor_StartOfBlock )
   qCursor:endEditBlock()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:insertText( cKey )
   LOCAL nB, nL, qCursor, cFile, cText, oEdit

   IF empty( oEdit := ::getEditObjectCurrent() )
      RETURN Self
   ENDIF

   qCursor := QTextCursor():configure( oEdit:qEdit:textCursor() )

   DO CASE

   CASE cKey == "InsertDateTime"
      cText := DTOC( Date() ) + ' - ' + Time()

   CASE cKey == "InsertRandomName"
      cText := hbide_getUniqueFuncName()

   CASE cKey == "InsertExternalFile"
      cFile := ::oSM:selectSource( "open" )
      IF Empty( cFile ) .OR. !hb_FileExists( cFile )
         RETURN Self
      ENDIF
      IF !( hbide_isValidText( cFile ) )
         MsgBox( 'File type unknown or unsupported: ' + cFile )
         RETURN Self
      ENDIF
      cText := hb_memoread( cFile )

   OTHERWISE
      RETURN Self

   ENDCASE

   IF !Empty( cText )
      nL := len( cText )
      nB := qCursor:position() + nL

      qCursor:beginEditBlock()
      qCursor:removeSelectedText()
      qCursor:insertText( cText )
      qCursor:setPosition( nB )
      qCursor:endEditBlock()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:RemoveTabs()
   LOCAL qEdit, qDoc, cText, cSpaces

   IF empty( qEdit := ::getEditCurrent() )
      RETURN Self
   ENDIF

   qDoc := QTextDocument():configure( qedit:document() )

   IF !( qDoc:isEmpty() )
      cSpaces := space( ::nTabSpaces )

      qDoc:setUndoRedoEnabled( .f. )

      cText := qDoc:toPlainText()
      qDoc:clear()
      qDoc:setPlainText( strtran( cText, chr( 9 ), cSpaces ) )

      qDoc:setUndoRedoEnabled( .t. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:RemoveTrailingSpaces()
   LOCAL qEdit, qDoc, cText, a_, s

   IF empty( qEdit := ::getEditCurrent() )
      RETURN Self
   ENDIF

   qDoc := QTextDocument():configure( qedit:document() )

   IF !( qDoc:isEmpty() )
      qDoc:setUndoRedoEnabled( .f. )

      cText := qDoc:toPlainText()

      a_:= hbide_memoToArray( cText )
      FOR EACH s IN a_
         s := trim( s )
      NEXT
      cText := hbide_arrayToMemo( a_ )

      qDoc:clear()
      qDoc:setPlainText( cText )

      qDoc:setUndoRedoEnabled( .t. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:zoom( nKey )
   LOCAL nPointSize, qFont, oEdit, oEditor

   IF empty( oEditor := ::getEditorCurrent() )
      RETURN Self
   ENDIF
   oEdit := oEditor:oEdit

   qFont := QFont():configure( oEdit:qEdit:font() )
   qFont:setFamily( "Courier New" )
   qFont:setFixedPitch( .t. )
   nPointSize := qFont:pointSize()
   nPointSize += iif( nKey == 1, 1, -1 )

   IF nPointSize > 4 .AND. nPointSize < 37
      qFont:setPointSize( nPointSize )

      oEdit:qEdit:setFont( qFont )
      oEdit:qLineNos:setFont( qFont )

      FOR EACH oEdit IN oEditor:aEdits
         oEdit:qEdit:setFont( qFont )
         oEdit:qLineNos:setFont( qFont )
      NEXT
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:printPreview()
   LOCAL qDlg

   IF empty( ::qCurEdit )
      RETURN Self
   ENDIF

   qDlg := QPrintPreviewDialog():new( ::oDlg:oWidget )
   qDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
   Qt_Slots_Connect( ::pSlots, qDlg, "paintRequested(QPrinter)", {|o,p| ::paintRequested( p,o ) } )
   qDlg:exec()
   Qt_Slots_disConnect( ::pSlots, qDlg, "paintRequested(QPrinter)" )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getSelectedText()
   LOCAL qEdit

   IF !empty( qEdit := ::oEM:getEditCurrent() )
      RETURN QTextCursor():configure( qEdit:textCursor() ):selectedText()
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:paintRequested( pPrinter )
   LOCAL qPrinter

   qPrinter := QPrinter():configure( pPrinter )

   ::qCurEdit:print( qPrinter )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:setMark()
   LOCAL oEdit

   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:setNewMark()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:gotoMark()
   LOCAL oEdit

   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:gotoLastMark()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:goto()
   LOCAL qGo, nLine, qCursor, qEdit, nRows

   IF empty( qEdit := ::oEM:getEditCurrent() )
      RETURN Self
   ENDIF

   qCursor := QTextCursor():configure( qEdit:textCursor() )
   nLine := qCursor:blockNumber()
   nRows := qEdit:blockCount()

   qGo := QInputDialog():new( ::oDlg:oWidget )
   qGo:setIntMinimum( 1 )
   qGo:setIntMaximum( nRows )
   qGo:setIntValue( nLine + 1 )
   qGo:setLabelText( "Goto Line Number [1-" + hb_ntos( nRows ) + "]" )
   qGo:setWindowTitle( "Harbour-Qt" )

   ::setPosByIni( qGo, GotoDialogGeometry )
   qGo:exec()
   ::aIni[ INI_HBIDE, GotoDialogGeometry ] := hbide_posAndSize( qGo )

   nLine := qGo:intValue() -  nLine

   qGo:pPtr := 0

   IF nLine < 0
      qCursor:movePosition( QTextCursor_Up, QTextCursor_MoveAnchor, abs( nLine ) + 1 )
   ELSEIF nLine > 0
      qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine - 1 )
   ENDIF
   qEdit:setTextCursor( qCursor )

   RETURN nLine

/*----------------------------------------------------------------------*/
//
//                            CLASS IdeEditor
//                     Holds One Document in One Tab
//
/*----------------------------------------------------------------------*/

CLASS IdeEditor INHERIT IdeObject

   DATA   oTab
   DATA   cPath
   DATA   cFile                                   INIT   ""
   DATA   cExt                                    INIT   ""
   DATA   cType                                   INIT   ""
   DATA   cTheme                                  INIT   ""
   DATA   qDocument
   DATA   qHiliter
   DATA   sourceFile                              INIT   ""
   DATA   pathNormalized
   DATA   qLayout
   DATA   lLoaded                                 INIT   .F.

   DATA   aEdits                                  INIT   {}   /* Hold IdeEdit Objects */
   DATA   oEdit
   DATA   qEdit
   DATA   qCqEdit
   DATA   qCoEdit

   DATA   nBlock                                  INIT   -1
   DATA   nColumn                                 INIT   -1
   DATA   nPrevBlocks                             INIT   0
   DATA   nBlocks                                 INIT   0

   DATA   nPos                                    INIT   0
   DATA   nHPos                                   INIT   0
   DATA   nVPos                                   INIT   0
   DATA   nID

   DATA   qCursor
   DATA   aSplits                                 INIT   {}

   DATA   qHLayout
   DATA   qLineNos
   DATA   qLabel
   DATA   nnRow                                   INIT -99
   DATA   qPoint                                  INIT QPoint():new()

   DATA   qEvents
   DATA   qSlots

   METHOD new( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )
   METHOD create( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )
   METHOD split( nOrient, oEditP )
   METHOD relay( oEdit )
   METHOD destroy()
   METHOD exeEvent( nMode, o, p, p1, p2 )
   METHOD setDocumentProperties()
   METHOD activateTab( mp1, mp2, oXbp )
   METHOD buildTabPage( cSource )
   METHOD dispEditInfo( qEdit )
   METHOD setTabImage( qEdit )
   METHOD applyTheme( cTheme )
   METHOD findLastIndent()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEditor:new( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )

   DEFAULT oIde        TO ::oIde
   DEFAULT cSourceFile TO ::sourceFile
   DEFAULT nPos        TO ::nPos
   DEFAULT nHPos       TO ::nHPos
   DEFAULT nVPos       TO ::nVPos
   DEFAULT cTheme      TO ::cTheme

   ::oIde       := oIde
   ::sourceFile := cSourceFile
   ::nPos       := nPos
   ::nHPos      := nHPos
   ::nVPos      := nVPos
   ::cTheme     := cTheme
   ::nID        := hbide_getNextUniqueID()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:create( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )

   ::qSlots := HBSlots():new()

   DEFAULT oIde        TO ::oIde
   DEFAULT cSourceFile TO ::sourceFile
   DEFAULT nPos        TO ::nPos
   DEFAULT nHPos       TO ::nHPos
   DEFAULT nVPos       TO ::nVPos
   DEFAULT cTheme      TO ::cTheme

   ::oIde           := oIde
   ::SourceFile     := hbide_pathNormalized( cSourceFile, .F. )
   ::nPos           := nPos
   ::nHPos          := nHPos
   ::nVPos          := nVPos
   ::cTheme         := cTheme
   ::pathNormalized := hbide_pathNormalized( cSourceFile, .t. )

   hb_fNameSplit( cSourceFile, @::cPath, @::cFile, @::cExt )

   ::cType := upper( strtran( ::cExt, ".", "" ) )
   ::cType := iif( ::cType $ "PRG,C,CPP,H,CH,PPO", ::cType, "U" )

   ::buildTabPage( ::sourceFile )

   ::qLayout := QGridLayout():new()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setHorizontalSpacing( 5 )
   ::qLayout:setVerticalSpacing( 5 )
   //
   ::oTab:oWidget:setLayout( ::qLayout )

   ::oEdit   := IdeEdit():new( Self, 0 ):create()
   ::qEdit   := ::oEdit:qEdit
   ::qCqEdit := ::oEdit:qEdit
   ::qCoEdit := ::oEdit

   ::qDocument := QTextDocument():configure( ::qEdit:document() )
   #if 1
   ::connect( ::qDocument, "blockCountChanged(int)"     , {|o,p      | ::exeEvent( 21, o, p )         } )
   ::connect( ::qDocument, "contentsChange(int,int,int)", {|o,p,p1,p2| ::exeEvent( 22, o, p, p1, p2 ) } )
   #else
hbide_dbg(  2001, ::qSlots:hbConnect( ::qDocument, "blockCountChanged(int)"     , {|o,p      | ::exeEvent( 21, o, p )         } ) )
   ::qSlots:hbConnect( ::qDocument, "contentsChange(int,int,int)", {|o,p,p1,p2| ::exeEvent( 22, o, p, p1, p2 ) } )
   #endif
   IF ::cType != "U"
      ::qHiliter := ::oThemes:SetSyntaxHilighting( ::oEdit:qEdit, @::cTheme )
   ENDIF
   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   ::qLayout:addLayout( ::oEdit:qHLayout, 0, 0 )

   /* Populate Tabs Array */
   aadd( ::aTabs, { ::oTab, Self } )

   /* Populate right at creation */
   ::oEM:addSourceInTree( ::sourceFile )

   ::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget" ) )
   ::setTabImage()

   RETURN Self

/*----------------------------------------------------------------------*/

FUNCTION hbide_isHarbourKeyword( cWord )
   LOCAL b_:= { 'function','return','static','local','default', ;
                'if','else','elseif','endif','end', ;
                'docase','case','endcase','otherwise', ;
                'do','while','exit',;
                'for','each','next','step','to',;
                'class','endclass','method','data','var','destructor','inline','assign','access',;
                'inherit','init','create','virtual','message',;
                'begin','sequence','try','catch','always','recover','hb_symbol_unused', ;
                'error','handler' }

   cWord := lower( cWord )

   RETURN ascan( b_, {|e| e == cWord } ) > 0

/*----------------------------------------------------------------------*/

FUNCTION hbide_isIndentableKeyword( cWord )
   LOCAL b_:= { 'function',;
                'if','else','elseif', ;
                'docase','case','otherwise', ;
                'do','while',;
                'for',;
                'class','method',;
                'begin','sequence','try','catch','always','recover','finally' }

   cWord := lower( cWord )

   RETURN ascan( b_, {|e| e == cWord } ) > 0

/*----------------------------------------------------------------------*/

FUNCTION hbide_isStartingKeyword( cWord )
   LOCAL b_:= { 'function','method' }

   cWord := lower( cWord )

   RETURN ascan( b_, {|e| e == cWord } ) > 0

/*----------------------------------------------------------------------*/

FUNCTION hbide_handlePreviousWord( qEdit )
   LOCAL qCursor, qTextBlock, cText, nPos, cWord, nB, nL

   qCursor    := QTextCursor():configure( qEdit:textCursor() )
   qTextBlock := QTextBlock():configure( qCursor:block() )
   cText      := qTextBlock:text()
   nPos       := qCursor:columnNumber()
   cWord      := hbide_getPreviousWord( cText, nPos + 1 )

   IF !empty( cWord )
      nL := len( cWord + " " )
      nB := qCursor:position() - nL

      IF hbide_isHarbourKeyword( cWord )
         qCursor:beginEditBlock()
         qCursor:setPosition( nB )
         qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nL )
         qCursor:removeSelectedText()
         qCursor:insertText( upper( cWord ) + " " )

         qEdit:setTextCursor( qCursor )
         qCursor:endEditBlock()
      ENDIF

      IF hbide_isStartingKeyword( cWord )
         qCursor:setPosition( nB )
         nPos := qCursor:columnNumber()

         IF nPos > 0
            qCursor:beginEditBlock()
            qCursor:movePosition( QTextCursor_StartOfBlock )

            qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nPos )
            qCursor:removeSelectedText()

            qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_MoveAnchor, nL )
            qCursor:endEditBlock()
         ENDIF
      ENDIF
   ENDIF
   RETURN .t.

/*----------------------------------------------------------------------*/

FUNCTION hbide_getPreviousWord( cText, nPos )
   LOCAL cWord, n

   cText := alltrim( substr( cText, 1, nPos ) )
   IF ( n := rat( " ", cText ) ) > 0
      cWord := substr( cText, n + 1 )
   ELSE
      cWord := cText
   ENDIF

   RETURN cWord

/*----------------------------------------------------------------------*/

FUNCTION hbide_getFirstWord( cText )
   LOCAL cWord, n

   cText := alltrim( cText )
   IF ( n := at( " ", cText ) ) > 0
      cWord := left( cText, n-1 )
   ELSE
      cWord := cText
   ENDIF

   RETURN cWord

/*----------------------------------------------------------------------*/

FUNCTION hbide_getFrontSpacesAndWord( cText, cWord )
   LOCAL n := 0

   DO WHILE .t.
      IF substr( cText, ++n, 1 ) != " "
         EXIT
      ENDIF
   ENDDO
   n--

   cWord := hbide_getFirstWord( cText )

   RETURN n

/*----------------------------------------------------------------------*/

METHOD IdeEditor:findLastIndent()
   LOCAL qCursor, qTextBlock, cText, cWord
   LOCAL nSpaces := 0

   qCursor := QTextCursor():configure( ::qCqEdit:textCursor() )
   qTextBlock := QTextBlock():configure( qCursor:block() )

   DO WHILE .t.
      IF !( qTextBlock:isValid() )
         EXIT
      ENDIF
      IF !empty( cText := qTextBlock:text() )
         nSpaces := hbide_getFrontSpacesAndWord( cText, @cWord )
         IF !empty( cWord )
            IF hbide_isIndentableKeyword( cWord )
               nSpaces += ::nTabSpaces
            ENDIF
            EXIT
         ENDIF
      ENDIF
      qTextBlock := QTextBlock():configure( qTextBlock:previous() )
   ENDDO

   RETURN nSpaces

/*----------------------------------------------------------------------*/

METHOD IdeEditor:exeEvent( nMode, o, p, p1, p2 )
   LOCAL qCursor, qChar, nSpaces

   HB_SYMBOL_UNUSED( o  )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH nMode
   CASE blockCountChanged
      hbide_dbg( "blockCountChanged(int)", p )
      ::nPrevBlocks := ::nBlocks
      ::nBlocks     := p
      //
      IF ::qCoEdit:nPrevLineNo <= 0 .OR. ::qCoEdit:nCurLineNo == ::qCoEdit:nPrevLineNo + 1
         IF ( nSpaces := ::findLastIndent() ) > 0
            qCursor := QTextCursor():configure( ::qCqEdit:textCursor() )
            qCursor:insertText( space( nSpaces ) )
         ENDIF
      ENDIF
      ::qCoEdit:nPrevLineNo := ::qCoEdit:nCurLineNo

      EXIT
   CASE contentsChange
      IF p2 == 1                     /* Characters Added */
         qChar := QChar():configure( ::qDocument:characterAt( p ) )
         SWITCH qChar:toAscii()
         CASE  9                    /* Tab Key */
            qCursor := QTextCursor():configure( ::qCqEdit:textCursor() )
            qCursor:deletePreviousChar()
            qCursor:insertText( ::cTabSpaces )
            EXIT
         CASE 32                    /* Space Key */
            hbide_handlePreviousWord( ::qCqEdit )
            EXIT
         ENDSWITCH
      ENDIF
      EXIT
   ENDSWITCH

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditor:relay( oEdit )
   LOCAL nCols, oEdt, nR, nC

   ::qLayout:removeItem( ::oEdit:qHLayout )
   FOR EACH oEdt IN ::aEdits
      ::qLayout:removeItem( oEdt:qHLayout )
      //
      oEdt:qHLayout:removeWidget( oEdt:qLineNos )
      oEdt:qHLayout:removeWidget( oEdt:qEdit )
      oEdt:qHLayout := QHBoxLayout():new()
      oEdt:qHLayout:addWidget( oEdt:qLineNos )
      oEdt:qHLayout:addWidget( oEdt:qEdit )
   NEXT

   IF hb_isObject( oEdit )
      aadd( ::aEdits, oEdit )
   ENDIF
   ::qLayout:addLayout( ::oEdit:qHLayout, 0, 0 )

   nR := 0 ; nC := 0
   FOR EACH oEdt IN ::aEdits
      IF oEdt:nOrient == 1     // Horiz
         nC++
         ::qLayout:addLayout_1( oEdt:qHLayout, 0, nC, 1, 1, Qt_Vertical )
      ENDIF
   NEXT
   nCols := ::qLayout:columnCount()
   FOR EACH oEdt IN ::aEdits
      IF oEdt:nOrient == 2     // Verti
         nR++
         ::qLayout:addLayout_1( oEdt:qHLayout, nR, 0, 1, nCols, Qt_Horizontal )
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:split( nOrient, oEditP )
   LOCAL oEdit

   HB_SYMBOL_UNUSED( oEditP  )

   oEdit := IdeEdit():new( Self, 1 ):create()
   oEdit:qEdit:setDocument( ::qDocument )
   oEdit:nOrient := nOrient

   ::relay( oEdit )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:destroy()
   LOCAL n, oEdit

   ::disconnect( ::qDocument, "blockCountChanged(int)"      )
   ::disconnect( ::qDocument, "contentsChange(int,int,int)" )

   ::qCqEdit := NIL
   ::qEdit         := NIL
   DO WHILE len( ::aEdits ) > 0
      oEdit := ::aEdits[ 1 ]
      hb_adel( ::aEdits, 1, .t. )
      oEdit:destroy()
   ENDDO
   ::oEdit:destroy()

   IF !Empty( ::qDocument )
      ::qDocument:pPtr := 0
      ::qDocument      := nil
   ENDIF

   IF !Empty( ::qHiliter )
      ::qHiliter:pPtr  := 0
      ::qHiliter       := nil
   ENDIF

   ::oEdit := NIL

   IF !Empty( ::qLayout )
      ::qLayout:pPtr   := 0
      ::qLayout        := nil
   ENDIF

   IF ( n := ascan( ::aTabs, {|e_| e_[ TAB_OEDITOR ] == Self } ) ) > 0
      hb_adel( ::oIde:aTabs, n, .T. )
   ENDIF

   ::oEM:removeSourceInTree( ::sourceFile )

   ::qTabWidget:removeTab( ::qTabWidget:indexOf( ::oTab:oWidget ) )
   ::oTab := NIL

   IF ::qTabWidget:count() == 0
      IF ::lDockRVisible
         ::oDockR:hide()
         ::oIde:lDockRVisible := .f.
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:setDocumentProperties()
   LOCAL qCursor

   qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   IF !( ::lLoaded )       /* First Time */
      ::qEdit:setPlainText( hb_memoRead( ::sourceFile ) )
      qCursor:setPosition( ::nPos )
      ::qEdit:setTextCursor( qCursor )

      QScrollBar():configure( ::qEdit:horizontalScrollBar() ):setValue( ::nHPos )
      QScrollBar():configure( ::qEdit:verticalScrollBar() ):setValue( ::nVPos )

      QTextDocument():configure( ::qEdit:document() ):setModified( .f. )

      ::qTabWidget:setTabIcon( ::qTabWidget:indexOf( ::oTab:oWidget ), ::resPath + "tabunmodified.png" )
      ::lLoaded := .T.
   ENDIF

   ::nBlock  := qCursor:blockNumber()
   ::nColumn := qCursor:columnNumber()

   ::oIde:aSources := { ::sourceFile }
   ::oIde:createTags()
   ::oIde:updateFuncList()
   ::oIde:updateTitleBar()

   ::dispEditInfo( ::qEdit )

   ::oIde:manageFocusInEditor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:activateTab( mp1, mp2, oXbp )
   LOCAL oEdit

   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   IF !empty( oEdit := ::oEM:getEditorByTabObject( oXbp ) )
      oEdit:setDocumentProperties()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:buildTabPage( cSource )

   ::oTab := XbpTabPage():new( ::oIde:oDA, , { 5,5 }, { 700,400 }, , .t. )

   IF Empty( cSource )
      ::oTab:caption := "Untitled " + hb_ntos( hbide_getNextUntitled() )
   ELSE
      ::oTab:caption := ::cFile + ::cExt
   ENDIF
   ::oTab:minimized := .F.

   ::oTab:create()

   ::qTabWidget:setCurrentIndex( ::qTabWidget:indexOf( ::oTab:oWidget ) )
   ::qTabWidget:setTabTooltip( ::qTabWidget:indexOf( ::oTab:oWidget ), cSource )

   ::oTab:tabActivate    := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:dispEditInfo( qEdit )
   LOCAL s, qDocument, qCursor

   DEFAULT qEdit TO ::qEdit

   qDocument := QTextDocument():configure( qEdit:document() )
   qCursor   := QTextCursor():configure( qEdit:textCursor() )

   s := "<b>Line "+ hb_ntos( qCursor:blockNumber() + 1 ) + " of " + ;
                    hb_ntos( qDocument:blockCount() ) + "</b>"

   ::oIde:oSBar:getItem( SB_PNL_MAIN     ):caption := "Success"
   ::oIde:oSBar:getItem( SB_PNL_READY    ):caption := "Ready"
   ::oIde:oSBar:getItem( SB_PNL_LINE     ):caption := s
   ::oIde:oSBar:getItem( SB_PNL_COLUMN   ):caption := "Col " + hb_ntos( qCursor:columnNumber() + 1 )
   ::oIde:oSBar:getItem( SB_PNL_INS      ):caption := iif( qEdit:overwriteMode() , " ", "Ins" )
   ::oIde:oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( qDocument:isModified(), "Modified", iif( qEdit:isReadOnly(), "ReadOnly", " " ) )

   ::oIde:oSBar:getItem( SB_PNL_STREAM   ):caption := "Stream"
   ::oIde:oSBar:getItem( SB_PNL_EDIT     ):caption := "Edit"

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:setTabImage( qEdit )
   LOCAL nIndex, lModified, lReadOnly, cIcon

   DEFAULT qEdit TO ::oEdit:qEdit

   nIndex    := ::qTabWidget:indexOf( ::oTab:oWidget )
   lModified := ::qDocument:isModified()
   lReadOnly := qEdit:isReadOnly()

   IF lModified
      cIcon := "tabmodified.png"
   ELSEIF lReadOnly
      cIcon := "tabreadonly.png"
   ELSE
      cIcon := "tabunmodified.png"
   ENDIF

   ::qTabWidget:setTabIcon( nIndex, ::resPath + cIcon )
   ::oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( lModified, "Modified", iif( lReadOnly, "ReadOnly", " " ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:applyTheme( cTheme )

   IF ::cType != "U"
      IF empty( cTheme )
         cTheme := ::oThemes:selectTheme()
      ENDIF

      IF ::oThemes:contains( cTheme )
         ::cTheme := cTheme
         ::qHiliter := ::oIde:oThemes:SetSyntaxHilighting( ::qEdit, @::cTheme )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//                             Class IdeEdit
//         Holds One QPlainTextEdit with its ancilliary Components
/*----------------------------------------------------------------------*/

CLASS IdeEdit INHERIT IdeObject

   DATA   oEditor

   DATA   qEdit
   DATA   qHLayout
   DATA   qLineNos
   DATA   nOrient                                 INIT  0

   DATA   nMode                                   INIT  0
   DATA   nLineNo                                 INIT  -99
   DATA   nMaxDigits                              INIT  5       // Tobe
   DATA   nMaxRows                                INIT  100
   DATA   nLastLine                               INIT  -99
   DATA   qBlockFormat                            INIT  QTextBlockFormat():new()
   DATA   nCurLineNo                              INIT  0
   DATA   nPrevLineNo                             INIT  -1

   DATA   qPoint                                  INIT  QPoint():new( 0,0 )
   DATA   qBrushCL                                INIT  QBrush():new( "QColor", QColor():new( 240,240,240 ) )
   DATA   qBrushNR                                INIT  QBrush():new( "QColor", QColor():new( 255,255,255 ) )
   DATA   qBrushMark                              INIT  QBrush():new( "QColor", QColor():new(   0,255,255 ) )
   DATA   qActionTab
   DATA   qLastCursor                             INIT  QTextCursor():new()

   DATA   qCursorMark
   DATA   qMarkUData                              INIT  HBQTextBlockUserData():new()
   DATA   aBookMarks                              INIT  {}

   DATA   qSlots
   DATA   lCursorPosChanging                      INIT  .F.
   DATA   lModified                               INIT  .F.

   METHOD new( oEditor, nMode )
   METHOD create( oEditor, nMode )
   METHOD destroy()
   METHOD execSlot( nMode, oEdit, o, p, p1 )
   METHOD execEvent( nMode, nEvent, p )
   METHOD connectEditSlots( oEdit )
   METHOD disConnectEditSlots( oEdit )
   METHOD highlightCurrentLine()
   METHOD setNewMark()
   METHOD gotoLastMark()
   METHOD getUserDataState( qB )
   METHOD deHighlightPreviousLine()
   METHOD duplicateLine()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEdit:new( oEditor, nMode )

   ::oEditor := oEditor
   ::nMode   := nMode

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:create( oEditor, nMode )

   DEFAULT oEditor TO ::oEditor
   DEFAULT nMode   TO ::nMode

   ::oEditor := oEditor
   ::nMode   := nMode
   ::oIde    := ::oEditor:oIde

   ::qEdit   := QPlainTextEdit():new()
   //
   ::qEdit:setLineWrapMode( QTextEdit_NoWrap )
   ::qEdit:setFont( ::oFont:oWidget )
   ::qEdit:ensureCursorVisible()
   ::qEdit:setContextMenuPolicy( Qt_CustomContextMenu )

   ::qEdit:installEventFilter( ::pEvents )
   Qt_Events_Connect( ::pEvents, ::qEdit, QEvent_KeyPress, {|o,p| ::execEvent( 1, QEvent_KeyPress, p, o ) } )
   Qt_Events_Connect( ::pEvents, ::qEdit, QEvent_Wheel   , {|o,p| ::execEvent( 1, QEvent_Wheel   , p, o ) } )

   ::qHLayout := QHBoxLayout():new()
   ::qHLayout:setSpacing( 0 )

   ::qLineNos := QTextEdit():new()
   ::qLineNos:setTextBackgroundColor( QColor():new( 240,240,240 ) )
   ::qLineNos:setAcceptRichText( .t. )
   ::qLineNos:setAlignment( Qt_AlignRight )
   ::qLineNos:setVerticalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
   ::qLineNos:setHorizontalScrollBarPolicy( Qt_ScrollBarAlwaysOff )
   ::qLineNos:setFrameStyle( QFrame_NoFrame )
   ::qLineNos:setReadOnly( .t. )
   ::qLineNos:setLineWrapMode( QTextEdit_NoWrap )
   ::qLineNos:setFont( ::oFont:oWidget )
   ::qLineNos:setContextMenuPolicy( Qt_NoContextMenu )
   ::qLineNos:setMinimumWidth( EDT_LINNO_WIDTH )
   ::qLineNos:setMaximumWidth( EDT_LINNO_WIDTH )
   ::qLineNos:setStyleSheet( "background-color: rgb( 240,240,240 );" )
   //
   ::qLineNos:installEventFilter( ::pEvents )
   Qt_Events_Connect( ::pEvents, ::qLineNos, QEvent_Wheel   , {|o,p| ::execEvent( 2, QEvent_Wheel   , p, o ) } )
   //
   ::qHLayout:addWidget( ::qLineNos )
   ::qHLayout:addWidget( ::qEdit    )

   #if 0
   ::qActionTab := QAction():new( ::qEdit )
   ::qActionTab:setText( "Editor Tab" )
   ::qActionTab:setShortcut( QKeySequence():new( "Ctrl+F2" ) )
   ::connect( ::qActionTab, "triggered(bool)", {|| ::execSlot( 71, Self ) } )
   #endif

   ::connectEditSlots( Self )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:destroy()

   ::disConnectEditSlots( Self )

   ::oEditor:qLayout:removeItem( ::qHLayout )
   //
   ::qHLayout:removeWidget( ::qEdit )
   ::qHLayout:removeWidget( ::qLineNos )

   ::qLineNos := NIL
   ::qEdit    := NIL
   ::qHLayout := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:disConnectEditSlots( oEdit )

   ::disConnect( oEdit:qEdit, "customContextMenuRequested(QPoint)" )
   ::disConnect( oEdit:qEdit, "textChanged()"                      )
   ::disConnect( oEdit:qEdit, "copyAvailable(bool)"                )
   ::disConnect( oEdit:qEdit, "modificationChanged(bool)"          )
   ::disConnect( oEdit:qEdit, "redoAvailable(bool)"                )
   ::disConnect( oEdit:qEdit, "selectionChanged()"                 )
 * ::disConnect( oEdit:qEdit, "undoAvailable(bool)"                )
   ::disConnect( oEdit:qEdit, "updateRequest(QRect,int)"           )
   ::disConnect( oEdit:qEdit, "cursorPositionChanged()"            )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:connectEditSlots( oEdit )

   ::Connect( oEdit:qEdit, "updateRequest(QRect,int)"          , {|o,p,p1| ::execSlot( 8, oEdit, o, p, p1 ) } )
   ::connect( oEdit:qEdit, "customContextMenuRequested(QPoint)", {|o,p   | ::execSlot( 1, oEdit, o, p     ) } )
   ::Connect( oEdit:qEdit, "textChanged()"                     , {|o     | ::execSlot( 2, oEdit, o        ) } )
   ::Connect( oEdit:qEdit, "copyAvailable(bool)"               , {|o,p   | ::execSlot( 3, oEdit, o, p     ) } )
   ::Connect( oEdit:qEdit, "modificationChanged(bool)"         , {|o,p   | ::execSlot( 4, oEdit, o, p     ) } )
   ::Connect( oEdit:qEdit, "redoAvailable(bool)"               , {|o,p   | ::execSlot( 5, oEdit, o, p     ) } )
   ::Connect( oEdit:qEdit, "selectionChanged()"                , {|o,p   | ::execSlot( 6, oEdit, o, p     ) } )
 * ::Connect( oEdit:qEdit, "undoAvailable(bool)"               , {|o,p   | ::execSlot( 7, oEdit, o, p     ) } )
   ::Connect( oEdit:qEdit, "cursorPositionChanged()"           , {|o     | ::execSlot( 9, oEdit, o        ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:deHighlightPreviousLine()
   LOCAL qB, qDoc

   qDoc := QTextDocument():configure( ::qEdit:document() )

   IF ( qB := QTextBlock():configure( qDoc:findBlockByNumber( ::nLastLine ) ) ):isValid()
      IF ::getUserDataState( qB ) != 99
         ::qLastCursor:setBlockFormat( QTextBlockFormat():new() )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:highlightCurrentLine()
   LOCAL nCurLine, nLastLine, qCursor, qDoc, qEdit, qBlockFmt, qB, lOldModified

   qEdit     := ::qEdit
   qCursor   := QTextCursor():new( qEdit:textCursor() )

   qDoc := QTextDocument():configure( qEdit:document() )
   lOldModified := ::lModified
   ::lModified := qDoc:isModified()

   qCursor:beginEditBlock()

   nCurLine  := qCursor:blockNumber()
   nLastLine := ::nLastLine

   IF !( nCurLine == nLastLine )

      ::deHighlightPreviousLine()

      IF ( qB := QTextBlock():configure( qCursor:block() ) ):isValid()
         IF ::getUserDataState( qB ) != 99
            qBlockFmt := QTextBlockFormat():configure( qB:blockFormat() )
            qBlockFmt:setBackground( ::qBrushCL )
            qCursor:setBlockFormat( qBlockFmt )
         ENDIF
      ENDIF
   ENDIF

   qCursor:endEditBlock()

   ::nLastLine := nCurLine
   IF !( qCursor:isNull() )
      ::qLastCursor := qCursor
   ENDIF

   /* Infact these must not be called from here but because changing the format  */
   /* Qt consider that document has been modified, hence I need to put them here */
   IF ( lOldModified != ::lModified )
      qDoc:setModified( ::lModified )
      ::qTabWidget:setTabIcon( ::qTabWidget:indexOf( ::oEditor:oTab:oWidget ), ;
                                   ::resPath + iif( ::lModified, "tabmodified.png", "tabunmodified.png" ) )
      ::oDK:setStatusText( SB_PNL_MODIFIED, ::lModified )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setNewMark()
   LOCAL qBlockFmt, qTextBlock, qDoc, lModified, nState

   qDoc := QTextDocument():configure( ::qEdit:document() )
   lModified := qDoc:isModified()

   IF empty( ::qCursorMark )
      ::qCursorMark := QTextCursor():configure( ::qEdit:textCursor() )

      qTextBlock := QTextBlock():configure( ::qCursorMark:block() )
      qBlockFmt := QTextBlockFormat():new()

      qBlockFmt:setBackground( ::qBrushMark )
      ::qCursorMark:setBlockFormat( qBlockFmt )

      ::qMarkUData := HBQTextBlockUserData():new()
      ::qMarkUData:hbSetState( 99 )
      qTextBlock:setUserData( ::qMarkUData )

   ELSE
      IF ( qTextBlock := QTextBlock():configure( ::qCursorMark:block() ) ):isValid()
         nState := ::getUserDataState( qTextBlock )

         IF nState == 99  /* Marked */
            ::qMarkUData:hbSetState( -1 )
            ::qCursorMark:setBlockFormat( QTextBlockFormat():new() )
            ::qCursorMark:pPtr := NIL  // Should We ?
            ::qCursorMark      := NIL
         ENDIF
      ENDIF
   ENDIF

   qDoc:setModified( lModified )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getUserDataState( qB )
   LOCAL qUData := HBQTextBlockUserData():configure( qB:userData() )

   IF !empty( qUData:pPtr )
      RETURN qUData:hbState()
   ENDIF

   RETURN -1

/*----------------------------------------------------------------------*/

METHOD IdeEdit:gotoLastMark()

   IF !empty( ::qCursorMark )
      ::qEdit:setTextCursor( ::qCursorMark )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:duplicateLine()
   LOCAL qCursor, qC, cLine, qEdit

   qEdit   := ::qEdit
   qCursor := QTextCursor():configure( qEdit:textCursor() )
   qC      := QTextCursor():configure( qEdit:textCursor() )

   qCursor:beginEditBlock()
   qCursor:movePosition( QTextCursor_StartOfLine )
   qCursor:movePosition( QTextCursor_EndOfLine, QTextCursor_KeepAnchor )
   //
   cLine := qCursor:selectedText()
   //
   qCursor:movePosition( QTextCursor_EndOfLine )
   qEdit:setTextCursor( qCursor )
   qEdit:insertPlainText( CRLF + cLine )
   qEdit:setTextCursor( qC )
   qCursor:endEditBlock()

    RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:execSlot( nMode, oEdit, o, p, p1 )
   LOCAL pAct, qAct, n, qCursor, qEdit, oo, nBlocks, nLineNo

   HB_SYMBOL_UNUSED( o  )
   HB_SYMBOL_UNUSED( p1 )

   qEdit   := oEdit:qEdit
   qCursor := QTextCursor():configure( qEdit:textCursor() )
   oEdit:nCurLineNo := qCursor:blockNumber()

   SWITCH nMode

   CASE customContextMenuRequested
      pAct := ::oEM:qContextMenu:exec_1( qEdit:mapToGlobal( p ) )
      IF !hbqt_isEmptyQtPointer( pAct )
         qAct := QAction():configure( pAct )
         DO CASE
         CASE qAct:text() == "Split Horizontally"
            ::oEditor:split( 1, oEdit )
         CASE qAct:text() == "Split Vertically"
            ::oEditor:split( 2, oEdit )
         CASE qAct:text() == "Close Split Window"
            IF ( n := ascan( ::oEditor:aEdits, {|o| o == oEdit } ) ) > 0  /* 1 == Main Edit */
               oo := ::oEditor:aEdits[ n ]
               hb_adel( ::oEditor:aEdits, n, .t. )
               oo:destroy()
               ::oEditor:relay()
               ::oEditor:qCqEdit := ::oEditor:qEdit
               ::oEditor:qCoEdit := ::oEditor:oEdit
               ::oIde:manageFocusInEditor()
            ENDIF
         CASE qAct:text() == "Apply Theme"
            ::oEditor:applyTheme()
         ENDCASE
      ENDIF
      EXIT
   CASE textChanged
      //hbide_dbg( "textChanged()" )
      ::oEditor:setTabImage( qEdit )
      EXIT
   CASE copyAvailable
      //hbide_dbg( "copyAvailable(bool)", p )
      EXIT
   CASE modificationChanged
      //hbide_dbg( "modificationChanged(bool)", p )
      EXIT
   CASE redoAvailable
      //hbide_dbg( "redoAvailable(bool)", p )
      EXIT
   CASE selectionChanged
      //hbide_dbg( "selectionChanged()" )
      ::oEditor:qCqEdit := qEdit
      ::oEditor:qCoEdit := oEdit

      qCursor := QTextCursor():configure( qEdit:TextCursor() )
      ::oDK:setStatusText( SB_PNL_SELECTEDCHARS, len( qCursor:selectedText() ) )
      EXIT
   CASE undoAvailable
      //hbide_dbg( "undoAvailable(bool)", p )
      EXIT
   CASE updateRequest
      //hbide_dbg( "updateRequest" )
      qCursor := QTextCursor():configure( qEdit:cursorForPosition( ::qPoint ) )
      nLineNo := qCursor:blockNumber()
      nBlocks := ::oEditor:qDocument:blockCount()
      IF oEdit:nLineNo != nLineNo .OR. !( ::oEditor:lLoaded ) .OR. !( nBlocks == ::oEditor:nPrevBlocks )
         ::oEditor:nPrevBlocks := nBlocks
         oEdit:nLineNo := nLineNo
         oEdit:qLineNos:setHTML( hbide_buildLinesLabel( ::nLineNo + 1, ;
                                  ::oEditor:qDocument:blockCount(), ::nMaxDigits, ::nMaxRows ) )
      ENDIF
      EXIT
   CASE cursorPositionChanged
      // hbide_dbg( "cursorPositionChanged()" )
      ::lCursorPosChanging := .T.
      ::oEditor:dispEditInfo( qEdit )
      ::highlightCurrentLine()
      ::lCursorPosChanging := .F.
      EXIT

   ENDSWITCH

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEdit:execEvent( nMode, nEvent, p )
   LOCAL key, kbm, txt, qEvent, qCursor
   LOCAL lAlt   := .f.
   LOCAL lCtrl  := .f.
   LOCAL lShift := .f.

   SWITCH nEvent
   CASE QEvent_KeyPress

      qEvent := QKeyEvent():configure( p )

      key := qEvent:key()
      kbm := qEvent:modifiers()
      txt := qEvent:text()

      IF hb_bitAnd( kbm, Qt_AltModifier     ) == Qt_AltModifier
         lAlt := .t.
      ENDIF
      IF hb_bitAnd( kbm, Qt_ControlModifier ) == Qt_ControlModifier
         lCtrl := .t.
      ENDIF
      IF hb_bitAnd( kbm, Qt_ShiftModifier   ) == Qt_ShiftModifier
         lShift := .t.
      ENDIF

      SWITCH ( key )
      CASE Qt_Key_D
         IF lCtrl
            ::duplicateLine()
            RETURN .T.          /* NOTE */
         ENDIF
         EXIT
      CASE Qt_Key_Return
      CASE Qt_Key_Enter
         qCursor := QTextCursor():configure( ::qEdit:textCursor() )
         qCursor:setBlockFormat( QTextBlockFormat():new() )
         ::qEdit:setTextCursor( qCursor )
         EXIT
      CASE Qt_Key_Tab
      CASE Qt_Key_Backtab

         EXIT
      CASE Qt_Key_Backspace

         EXIT
      ENDSWITCH

      EXIT
   CASE QEvent_Wheel
      #if 0    /* Of no use */
      qEvent := QWheelEvent():configure( p )

      IF nMode == 1  /* QPlainTextEdit */
         IF hb_bitAnd( qEvent:modifiers(), Qt_ControlModifier ) == Qt_ControlModifier
            IF qEvent:delta() > 0
               ::oEM:zoom( 1 )

            ELSE
               ::oEM:zoom( 0 )

            ENDIF

            qEvent:ignore()

            RETURN .t.  /* Tells Qt that we are done, no more processing */
         ENDIF

      ELSE

         qEvent:setAccepted( .f. )
         RETURN .f.  /* We do not want it to be processed at all - it is line no area */

      ENDIF
      #endif

      EXIT
   ENDSWITCH

   hbide_justACall( txt, lAlt, lShift, lCtrl, qEvent, nMode )
   RETURN .F.  /* Important */

/*----------------------------------------------------------------------*/

