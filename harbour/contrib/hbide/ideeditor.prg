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

CLASS IdeEditsManager INHERIT IdeObject

   DATA   qContextMenu
   DATA   aActions                                INIT  {}

   METHOD new()
   METHOD create()

   METHOD goto()
   METHOD undo()
   METHOD redo()
   METHOD cut()
   METHOD copy()
   METHOD paste()
   METHOD selectAll()
   METHOD switchToReadOnly()
   METHOD insertText()

   METHOD printPreview()
   METHOD paintRequested()

   METHOD showPPO()
   METHOD closePPO()

   METHOD convertSelection()

   METHOD zoom()

   METHOD isOpen()
   METHOD reLoad()
   METHOD buildEditor()

   METHOD setSourceVisible()
   METHOD setSourceVisibleByIndex()

   METHOD getEditorCurrent()
   METHOD getEditorBySource()
   METHOD getEditorByTabPosition()
   METHOD getEditorByTabObject()
   METHOD getEditorByIndex()

   METHOD prepareTabWidget()
   METHOD exeBlock()
   METHOD addSourceInTree()
   METHOD removeSourceInTree()
   METHOD splitEdit()

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
   aadd( ::aActions, { "Split H"      , oSub:addAction( "Split Horiz..." ) } )
   aadd( ::aActions, { "Split V"      , oSub:addAction( "Split Verti..." ) } )
   aadd( ::aActions, { "Close Split"  , oSub:addAction( "Close Split"    ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:prepareTabWidget()

   ::oIde:oDa:oTabWidget := XbpTabWidget():new():create( ::oDa, , {0,0}, {10,10}, , .t. )
   ::oIde:oTabWidget := ::oDa:oTabWidget
   ::oIde:qTabWidget := ::oDa:oTabWidget:oWidget

   ::qTabWidget:setUsesScrollButtons( .f. )
   ::qTabWidget:setMovable( .t. )

   ::qTabWidget:setContextMenuPolicy( Qt_CustomContextMenu )
   ::connect( ::qTabWidget, "customContextMenuRequested(QPoint)", {|o,p| ::exeBlock( 1, p, o ) } )

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

METHOD IdeEditsManager:exeBlock( nMode, p )
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

METHOD IdeEditsManager:splitEdit()
   LOCAL oEdit

   IF !empty( oEdit := ::getEditorCurrent() )
      oEdit:split()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme )

   aadd( ::aEdits, IdeEditor():new():create( ::oIde, cSourceFile, nPos, nHPos, nVPos, cTheme ) )

   RETURN Self

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

METHOD IdeEditsManager:insertText( cKey )
   LOCAL nB, nL, qCursor, cFile, cText

   IF Empty( ::qCurEdit )
      RETURN Self
   ENDIF

   qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )

   DO CASE

   CASE cKey == "InsertDateTime"
      cText := DTOC( Date() ) + ' - ' + Time()

   CASE cKey == "InsertRandomName"
      cText := hbide_getUniqueFuncName()

   CASE cKey == "InsertExternalFile"
      cFile := ::selectSource( "open" )
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

METHOD IdeEditsManager:zoom( cKey )

   IF empty( ::qCurEdit )
      RETURN Self
   ENDIF

   IF     upper( cKey ) == "ZOOMIN"
   ELSEIF upper( cKey ) == "ZOOMOUT"
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

METHOD IdeEditsManager:paintRequested( pPrinter )
   LOCAL qPrinter

   qPrinter := QPrinter():configure( pPrinter )

   ::qCurEdit:print( qPrinter )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:goto()
   LOCAL qGo, nLine, qCursor

   IF empty( ::qCurEdit )
      RETURN Self
   ENDIF

   qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
   nLine := qCursor:blockNumber()

   qGo := QInputDialog():new( ::oDlg:oWidget )
   qGo:setIntMinimum( 1 )
   qGo:setIntMaximum( ::qCurDocument:blockCount() )
   qGo:setIntValue( nLine + 1 )
   qGo:setLabelText( "Goto Line Number [1-" + hb_ntos( ::qCurDocument:blockCount() ) + "]" )
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
   ::qCurEdit:setTextCursor( qCursor )

   RETURN nLine

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:showPPO( cFile )
   LOCAL qEdit, qHiliter

   IF hb_fileExists( cFile )
      qEdit := QPlainTextEdit():new()
      qEdit:setPlainText( hb_memoRead( cFile ) )
      qEdit:setLineWrapMode( QTextEdit_NoWrap )
      qEdit:setFont( ::oIde:oFont:oWidget )
      qEdit:ensureCursorVisible()

      qEdit:setWindowTitle( cFile )
      qEdit:resize( 600, 400 )

      qHiliter := ::oIde:oThemes:SetSyntaxHilighting( qEdit )

      Qt_Events_Connect( ::pEvents, qEdit, QEvent_Close, {|| ::closePPO( qEdit, qHiliter, cFile, .t. ) } )

      qEdit:show()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:closePPO( qEdit, qHiliter, cFile, lDel )

   Qt_Events_DisConnect( ::pEvents, qEdit, QEvent_Close )

   qHiliter:pPtr := 0
   qEdit:close()
   qEdit:pPtr := 0

   IF lDel
      ferase( cFile )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//
//                            CLASS IdeEditor
//
/*----------------------------------------------------------------------*/

CLASS IdeEditor INHERIT IdeObject

   DATA   oTab
   DATA   cPath
   DATA   cFile                                   INIT   ""
   DATA   cExt                                    INIT   ""
   DATA   cType                                   INIT   ""
   DATA   cTheme                                  INIT   ""
   DATA   qEdit
   DATA   qDocument
   DATA   qHiliter
   DATA   sourceFile                              INIT   ""
   DATA   pathNormalized
   DATA   qLayout
   DATA   lLoaded                                 INIT   .F.

   DATA   nBlock                                  INIT   -1
   DATA   nColumn                                 INIT   -1
   DATA   nBlocks                                 INIT   0

   DATA   nPos                                    INIT   0
   DATA   nHPos                                   INIT   0
   DATA   nVPos                                   INIT   0
   DATA   nID

   DATA   aTab                                    INIT   {}
   DATA   qCursor
   DATA   aSplits                                 INIT   {}

   METHOD new()
   METHOD create()
   METHOD destroy()

   METHOD buildTabPage()
   METHOD buildEditor()
   METHOD removeTabPage()
   METHOD activateTab()
   METHOD closeTab()
   METHOD dispEditInfo()
   METHOD setTabImage()
   METHOD applyTheme()
   METHOD setDocumentProperties()
   METHOD exeBlock()
   METHOD split()

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

   ::qEdit := ::buildEditor( 0 )  /* Main Editor */

   ::qDocument := QTextDocument():configure( ::qEdit:document() )
   //
   ::Connect( ::qDocument, "blockCountChanged(int)"   , {|o,p   | ::exeBlock( 21, p, o     ) } )
   ::Connect( ::qDocument, "contentsChanged()"        , {|      | ::exeBlock( 22 ) } )

   ::qLayout := QGridLayout():new()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setHorizontalSpacing( 5 )
   ::qLayout:setVerticalSpacing( 5 )
   //
   ::oTab:oWidget:setLayout( ::qLayout )
   //                           Row Col RSpn CSpn
   ::qLayout:addWidget_1( ::qEdit, 0, 1, 1, 1 )

   IF ::cType != "U"
      ::qHiliter := ::oThemes:SetSyntaxHilighting( ::qEdit, @::cTheme )
   ENDIF

   ::qEdit:show()
   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   /* Populate Tabs Array */
   ::aTab := { ::oTab, ::qEdit, ::qHiliter, ::qLayout, ::sourceFile, ::qDocument, Self }
   aadd( ::aTabs, ::aTab )

   ::oIde:nCurTab := len( ::oIde:aTabs )

   /* Populate right at creation */
   ::oEM:addSourceInTree( ::sourceFile )

   ::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget" ) )
   ::setTabImage()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:split( nMode, qEditP )
   LOCAL qEdit, nRows, nCols

   HB_SYMBOL_UNUSED( qEditP )

   nRows := ::qLayout:rowCount()
   nCols := ::qlayout:columnCount()

   qEdit := ::buildEditor()
   IF     nMode == 1  /* Horizontal */
      ::qLayout:addWidget( qEdit, nRows - 1, nCols )
   ELSEIF nMode == 2  /* Vertical   */
      ::qLayout:addWidget( qEdit, nRows, nCols - 1 )
   ENDIF

   aadd( ::aSplits, qEdit )

   qEdit:setDocument( ::qEdit:document() )

   qEdit:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:buildEditor()
   LOCAL qEdit

   qEdit := QPlainTextEdit():new()
   qEdit:setLineWrapMode( QTextEdit_NoWrap )
   qEdit:setFont( ::oFont:oWidget )
   qEdit:ensureCursorVisible()

   qEdit:setContextMenuPolicy( Qt_CustomContextMenu )
   ::connect( qEdit, "customContextMenuRequested(QPoint)", {|o,p| ::exeBlock( 1, p, o, qEdit ) } )

   ::Connect( qEdit, "textChanged()"            , {|      | ::setTabImage()           } )
   ::Connect( qEdit, "cursorPositionChanged()"  , {|      | ::dispEditInfo( qEdit )   } )
   ::Connect( qEdit, "updateRequest(QRect,int)" , {|o,p,p1| ::exeBlock( 8, p, p1, o ) } )
   ::Connect( qEdit, "copyAvailable(bool)"      , {|o,p   | ::exeBlock( 3, p, o     ) } )
   ::Connect( qEdit, "modificationChanged(bool)", {|o,p   | ::exeBlock( 4, p, o     ) } )
   ::Connect( qEdit, "redoAvailable(bool)"      , {|o,p   | ::exeBlock( 5, p, o     ) } )
   ::Connect( qEdit, "selectionChanged()"       , {|o,p   | ::exeBlock( 6, p, o     ) } )
   ::Connect( qEdit, "undoAvailable(bool)"      , {|o,p   | ::exeBlock( 7, p, o     ) } )
   ::Connect( qEdit, "updateRequest(QRect,int)" , {|o,p,p1| ::exeBlock( 8, p, p1, o ) } )

   RETURN qEdit

/*----------------------------------------------------------------------*/

METHOD IdeEditor:exeBlock( nMode, p, p1, qEdit )
   LOCAL pAct, qAct, n
   //LOCAL qRect

   HB_SYMBOL_UNUSED( p  )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH nMode

   /* QPlainTextEdit */
   CASE 1        // "customContextMenuRequested(QPoint)"
      IF !empty( pAct := ::oEM:qContextMenu:exec_1( qEdit:mapToGlobal( p ) ) )
         qAct := QAction():configure( pAct )
         DO CASE
         CASE qAct:text() == "Split Horiz..."
            ::split( 1, qEdit )
         CASE qAct:text() == "Split Verti..."
            ::split( 2, qEdit )
         CASE qAct:text() == "Close Split"
            IF ( n := ascan( ::aSplits, {|o| o == qEdit } ) ) > 0
               ::qLayout:removeWidget( qEdit )
               hb_adel( ::aSplits, n, .t. )
               qEdit:close()
               qEdit:pPtr := 0
            ENDIF
         CASE qAct:text() == "Apply Theme"
            ::applyTheme()
         ENDCASE
      ENDIF
      EXIT
   CASE 2
      EXIT
   CASE 3        // "copyAvailable(bool)"
      hbide_dbg( "copyAvailable(bool)" )
      EXIT
   CASE 4        // "modificationChanged(bool)"
      hbide_dbg( "modificationChanged(bool)" )
      EXIT
   CASE 5        // "redoAvailable(bool)"
      hbide_dbg( "redoAvailable(bool)" )
      EXIT
   CASE 6        // "selectionChanged()"
      hbide_dbg( "selectionChanged()" )
      EXIT
   CASE 7        // "undoAvailable(bool)"
      hbide_dbg( "undoAvailable(bool)" )
      EXIT
   CASE 8        // "updateRequest(QRect,int)"
      //qRect := QRect():configure( p )
      //hbide_dbg( "updateRequest(QRect,int)", qRect:x(), qRect:y(), qRect:width(), qRect:height(), p1 )
      EXIT

   /* QTabPage */
   CASE 11       // QEvent_ContextMenu
      hbide_dbg( "QEvent_ContextMenu" )
      EXIT
   CASE 12       // QEvent_ContextMenu
      hbide_dbg( "QEvent_ContextMenu" )
      EXIT

   /* QTextDocument */
   CASE 21       // "blockCountChanged(int)"
      ::nBlock := QTextCursor():configure( ::qEdit:textCursor() ):blockNumber()
      EXIT
   CASE 22       // "contentsChanged()"
      hbide_dbg( "contentsChanged()" )
      EXIT


   ENDSWITCH

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditor:setDocumentProperties()
   LOCAL qCursor

   qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   IF !( ::lLoaded )       /* First Time */
      ::lLoaded := .T.
      ::qEdit:setPlainText( hb_memoRead( ::sourceFile ) )
      qCursor:setPosition( ::nPos )
      ::qEdit:setTextCursor( qCursor )
      QScrollBar():configure( ::qEdit:horizontalScrollBar() ):setValue( ::nHPos )
      QScrollBar():configure( ::qEdit:verticalScrollBar() ):setValue( ::nVPos )
      ::qTabWidget:setTabIcon( ::qTabWidget:indexOf( ::oTab:oWidget ), ::resPath + "tabunmodified.png" )
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

   IF !empty( oEdit := ::oEM:getEditorByTabObject( oXbp ) )
      ::oIde:nCurTab := mp2
      oEdit:setDocumentProperties()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:destroy()

   ::RemoveTabPage()

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
   ::oTab:closeRequested := {|mp1,mp2,oXbp| ::closeTab( mp1, mp2, oXbp ) }

   ::oTab:oWidget:setContextMenuPolicy( Qt_CustomContextMenu )
   ::connect( ::oTab:oWidget, "customContextMenuRequested(QPoint)", {|o,e| ::exeBlock( 11, e, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:closeTab( mp1, mp2, oXbp )

   IF PCount() == 00
      mp1 := ::nID
      mp2 := ascan( ::aTabs, {|e_| e_[ TAB_OEDITOR ]:nID == mp1  } )
   ELSE
      mp2 := ascan( ::aTabs, {|e_| e_[ TAB_OTAB ] == oXbp } )
   ENDIF

 * Requested tab exists?
   IF !Empty( mp2 )
      ::oSM:closeSource( mp2 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Remove the tab of the main screen and clean the objects from memory.Note that
 * this function does not question the User if he wants to save or not the
 * content combined with this TAB if you want to confirm the action use
 * HbIde:closeSource() instead.
 * 02/01/2010 - 12:58:53 - vailtom
 */
METHOD IdeEditor:removeTabPage()
   LOCAL cSource := ::sourceFile
   LOCAL n, qEdit

   FOR EACH qEdit IN ::aSplits
      ::qLayout:removeWidget( qEdit )
      qEdit:close()
      qEdit:pPtr := 0
      qEdit := NIL
   NEXT
   ::aSplits := {}

   n := aScan( ::oIde:aTabs, {|e_| e_[ TAB_OEDITOR ]:nID == ::nID } )
   IF n > 0
      hb_aDel( ::oIde:aTabs, n, .T. )
   ENDIF

   n := ::oIde:qTabWidget:indexOf( ::oTab:oWidget )
   ::oIde:qTabWidget:removeTab( n  )

   // { oTab, qEdit, qHiliter, qLayout, cSourceFile, qDocument }
   //
   IF !Empty( ::qEdit )
      Qt_Slots_disConnect( ::pSlots, ::qEdit, "textChanged()" )
      Qt_Slots_disConnect( ::pSlots, ::qEdit, "cursorPositionChanged()" )
   ENDIF

   IF !Empty( ::qDocument )
      Qt_Slots_disConnect( ::pSlots, ::qDocument, "blockCountChanged(int)" )
      ::qDocument:pPtr := 0
      ::qDocument      := nil
   ENDIF

   IF !Empty( ::qLayout )
      ::qLayout:pPtr := 0
      ::qLayout      := nil
   ENDIF

   IF !Empty( ::qHiliter )
      ::qHiliter:pPtr := 0
      ::qHiliter      := nil
   ENDIF

   IF !Empty( ::qEdit )
      ::qEdit:pPtr := 0
      ::qEdit      := nil

      ::oIde:oFuncList:clear()
   ENDIF

   ::oEM:removeSourceInTree( cSource )

   IF ( n := aScan( ::oIde:aEdits, {|e_| e_:nID == ::nID } ) ) > 0
      hb_aDel( ::oIde:aEdits, n, .T. )
   ENDIF

   IF ::qTabWidget:count() == 0
      IF ::lDockRVisible
         ::oDockR:hide()
         ::oIde:lDockRVisible := .f.
      ENDIF
   ENDIF

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

METHOD IdeEditor:setTabImage()
   LOCAL nIndex    := ::qTabWidget:indexOf( ::oTab:oWidget )
   LOCAL lModified := ::qDocument:isModified()
   LOCAL lReadOnly := ::qEdit:isReadOnly()
   LOCAL cIcon

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

