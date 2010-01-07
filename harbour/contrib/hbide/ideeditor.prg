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

   ENDCLASS

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

   IF hb_isNumeric( nIndex ) .AND. nIndex > 0 .AND. nIndex < ::qTabWidget:count()
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

METHOD IdeEditsManager:setSourceVisibleByIndex( nIndex )

   IF ::qTabWidget:count() > 0 .AND. ::qTabWidget:count() > nIndex  /* nIndex is 0 based */
      IF ::qTabWidget:currentIndex() != nIndex
         ::qTabWidget:setCurrentIndex( nIndex )
      ELSE
         ::getEditorByIndex( nIndex ):setDocumentProperties()
      ENDIF
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   RETURN Self

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
   DATA   cFile
   DATA   cExt
   DATA   cType
   DATA   cTheme
   DATA   qEdit
   DATA   qDocument
   DATA   qHiliter
   DATA   sourceFile
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

   DATA   qCursor

   METHOD new()
   METHOD create()
   METHOD destroy()

   METHOD buildTabPage()
   METHOD removeTabPage()
   METHOD activateTab()
   METHOD closeTab()
   METHOD dispEditInfo()
   METHOD onBlockCountChanged()
   METHOD setTabImage()
   METHOD applyTheme()
   METHOD setDocumentProperties()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEditor:new( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )

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
   ::cType := iif( ::cType $ "PRG,C,CPP,H,CH", ::cType, "U" )

   ::buildTabPage( ::sourceFile )

   ::qEdit := QPlainTextEdit():new( ::oTab:oWidget )
   ::qEdit:setLineWrapMode( QTextEdit_NoWrap )
   ::qEdit:setFont( ::oFont:oWidget )
   ::qEdit:ensureCursorVisible()
 * ::qEdit:setStyleSheet( GetStyleSheet( "QPlainTextEdit" ) )

   ::qDocument := QTextDocument():configure( ::qEdit:document() )

   ::qLayout := QBoxLayout():new()
   ::qLayout:setDirection( 0 )
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:addWidget( ::qEdit )

   ::oTab:oWidget:setLayout( ::qLayout )

   IF ::cType != "U"
      ::qHiliter := ::oThemes:SetSyntaxHilighting( ::qEdit, @::cTheme )
   ENDIF

   Qt_Slots_Connect( ::pSlots, ::qEdit    , "textChanged()"          , {|| ::setTabImage() } )
   Qt_Slots_Connect( ::pSlots, ::qEdit    , "cursorPositionChanged()", {|| ::dispEditInfo() } )
   Qt_Slots_Connect( ::pSlots, ::qDocument, "blockCountChanged(int)" , {|o,i| ::onBlockCountChanged( i, o ) } )

   ::qEdit:show()
   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   /* Populate Tabs Array */
   aadd( ::aTabs, { ::oTab, ::qEdit, ::qHiliter, ::qLayout, ::sourceFile, ::qDocument, Self } )

   ::oIde:nCurTab := len( ::oIde:aTabs )

   /* Populate right at creation */
   ::oIde:addSourceInTree( ::sourceFile )

   ::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget" ) )
   ::setTabImage()

   hbide_dbg( "   ." )
   hbide_dbg( ".......................................", cSourceFile )
   hbide_dbg( "   ." )

   RETURN Self

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
   ENDIF

   ::nBlock  := qCursor:blockNumber()
   ::nColumn := qCursor:columnNumber()

   ::oIde:aSources := { ::sourceFile }
   ::oIde:createTags()
   ::oIde:updateFuncList()
   ::oIde:updateTitleBar()
   ::dispEditInfo()

   ::oIde:manageFocusInEditor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:activateTab( mp1, mp2, oXbp )
   LOCAL oEdit

   HB_SYMBOL_UNUSED( mp1 )

   IF !empty( oEdit := ::oED:getEditorByTabObject( oXbp ) )
      ::oIde:nCurTab := mp2
      oEdit:setDocumentProperties()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:destroy()

   ::RemoveTabPage()

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
   LOCAL n

   n := aScan( ::oIde:aTabs, {|e_| e_[ TAB_OEDITOR ]:nID == ::nID } )

   IF n > 0
      hb_aDel( ::oIde:aTabs, n, .T. )
   ENDIF

   n := ::oIde:qTabWidget:indexOf( ::oTab:oWidget )
   ::oIde:qTabWidget:removeTab( n  )

   /* Destroy all objects */
   // { oTab, qEdit, qHiliter, qLayout, cSourceFile, qDocument }
   //
   IF !Empty( ::qEdit )
      Qt_Slots_disConnect( ::pSlots, ::qEdit, "textChanged()" )
      Qt_Slots_disConnect( ::pSlots, ::qEdit, "cursorPositionChanged()" )
      /* To avoid recursive calls on invalid pointers */
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

   IF !Empty( ::oTab )
//      ::oTab:Destroy()
//      ::oTab:pPtr := 0
//      ::oTab      := nil
   ENDIF

   IF ( n := aScan( ::oIde:aProjData, {|e_| e_[ 4 ] == cSource } ) ) > 0
      ::aProjData[ n,3 ]:delItem( ::oIde:aProjData[ n,1 ] )
      hb_aDel( ::aProjData, n, .T. )
   ENDIF

   IF ( n := aScan( ::oIde:aEdits, {|e_| e_:nID == ::nID } ) ) > 0
      hb_aDel( ::oIde:aEdits, n, .T. )
   ENDIF

   /*
    * TOFIX: Release memory from these objects & arrays
    *
    *  aTabs     - OK
    *  aSources
    *  aEdits    - OK
    *
    */
   IF ::qTabWidget:count() == 0
      IF ::lDockRVisible
         ::oDockR:hide()
         ::lDockRVisible := .f.
      ENDIF
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
   ::oTab:closeRequested := {|mp1,mp2,oXbp| ::closeTab( mp1, mp2, oXbp ) }

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
      ::oIde:closeSource( mp2 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:onBlockCountChanged( nNewBlocks )
   LOCAL nLine

   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )
   nLine := ::qCursor:blockNumber()
   IF ::nBlock != nLine
      HB_TRACE( HB_TR_ALWAYS, nNewBlocks, nLine, ::nBlock )
   ENDIF
   ::nBlock := nLine

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:dispEditInfo()
   LOCAL s

   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   s := "<b>Line "+ hb_ntos( ::qCursor:blockNumber() + 1 ) + " of " + ;
                    hb_ntos( ::qDocument:blockCount() ) + "</b>"

   ::oIde:oSBar:getItem( SB_PNL_MAIN     ):caption := "Success"
   ::oIde:oSBar:getItem( SB_PNL_READY    ):caption := "Ready"
   ::oIde:oSBar:getItem( SB_PNL_LINE     ):caption := s
   ::oIde:oSBar:getItem( SB_PNL_COLUMN   ):caption := "Col " + hb_ntos( ::qCursor:columnNumber() + 1 )
   ::oIde:oSBar:getItem( SB_PNL_INS      ):caption := iif( ::qEdit:overwriteMode() , " ", "Ins" )
   ::oIde:oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( ::qDocument:isModified(), "Modified", iif( ::qEdit:isReadOnly(), "ReadOnly", " " ) )

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

