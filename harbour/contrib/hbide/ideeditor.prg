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
#include "xbp.ch"

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
#define timerTimeout                              23

#define qcompleter_activated                      101

#define EDT_LINNO_WIDTH                           50

/*----------------------------------------------------------------------*/

CLASS IdeEditsManager INHERIT IdeObject

   DATA   qContextMenu
   DATA   aActions                                INIT  {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD removeSourceInTree( cSourceFile )
   METHOD addSourceInTree( cSourceFile, cView )
   METHOD execEvent( nMode, p )
   METHOD buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme, cView )
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
   METHOD gotoMark( nIndex )
   METHOD goto()
   METHOD formatBraces()
   METHOD removeTabs()
   METHOD RemoveTrailingSpaces()
   METHOD getSelectedText()
   METHOD duplicateLine()
   METHOD deleteLine()
   METHOD moveLine( nDirection )
   METHOD streamComment()
   METHOD blockComment()
   METHOD indent( nStep )
   METHOD convertQuotes()
   METHOD convertDQuotes()
   METHOD toggleSelectionMode()
   METHOD toggleLineNumbers()

   METHOD getText()
   METHOD getWord( lSelect )
   METHOD getLine( nLine, lSelect )

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

   aadd( ::aActions, { "GotoFunc"     , ::qContextMenu:addAction_4( ::oAC:getAction( "GotoFunc"      ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
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
   aadd( ::aActions, { "Save as Skltn", ::qContextMenu:addAction( "Save as Skeleton..."                ) } )

   oSub := QMenu():configure( ::qContextMenu:addMenu_1( "Split" ) )
   //
   aadd( ::aActions, { "Split H"      , oSub:addAction( "Split Horizontally" ) } )
   aadd( ::aActions, { "Split V"      , oSub:addAction( "Split Vertically"   ) } )
   aadd( ::aActions, { ""             , oSub:addSeparator() } )
   aadd( ::aActions, { "Close Split"  , oSub:addAction( "Close Split Window" ) } )

   ::oIde:qProtoList := QStringList():new()
   ::oIde:qCompModel := QStringListModel():new()
   ::oIde:qCompleter := QCompleter():new()

   ::qCompModel:setStringList( ::qProtoList )
   ::qCompleter:setModel( ::qCompModel )
   ::qCompleter:setModelSorting( QCompleter_CaseInsensitivelySortedModel )
   ::qCompleter:setCaseSensitivity( Qt_CaseInsensitive )
   ::qCompleter:setCompletionMode( QCompleter_PopupCompletion )
   ::qCompleter:setWrapAround( .f. )

   ::connect( ::qCompleter, "activated(QString)", {|p| ::execEvent( qcompleter_activated, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:destroy()
   LOCAL a_

   ::disconnect( ::qCompleter, "activated(QString)" )
   ::oIde:qCompleter := NIL

   FOR EACH a_ IN ::aActions
      a_[ 2 ] := NIL
   NEXT
   ::aActions := NIL
   ::qContextMenu := NIL

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

METHOD IdeEditsManager:addSourceInTree( cSourceFile, cView )
   LOCAL cPath, cFile, cExt, oItem
   //LOCAL oGrand := ::oOpenedSources
   LOCAL oParent := ::oOpenedSources

   IF Empty( cSourceFile )
      RETURN Self
   ENDIF

   hb_fNameSplit( cSourceFile, @cPath, @cFile, @cExt )
   #if 0
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
   #endif

   oItem := oParent:addItem( cFile + cExt )
   oItem:tooltipText := cSourceFile
   oItem:oWidget:setIcon( 0, ::oDK:getPanelIcon( cView ) )
   aadd( ::aProjData, { oItem, "Opened Source", oParent, ;
                                   cSourceFile, hbide_pathNormalized( cSourceFile ) } )

   ::oEditTree:oWidget:sortItems( 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:execEvent( nMode, p )
   LOCAL oEdit

   DO CASE
   CASE nMode == qcompleter_activated
      IF !empty( oEdit := ::getEditObjectCurrent() )
         oEdit:completeCode( p )
      ENDIF

   ENDCASE

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme, cView )

   IdeEditor():new():create( ::oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getTabBySource( cSource )

   cSource := hbide_pathNormalized( cSource, .t. )

   RETURN ascan( ::aTabs, {|e_| e_[ TAB_OEDITOR ]:pathNormalized == cSource } )

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getTabCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget )
      qTab := ::qTabWidget:currentWidget()
      nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } )
   ENDIF
   RETURN nTab

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getDocumentCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } ) ) > 0
         RETURN QTextDocument():configure( ::aTabs[ nTab, TAB_OEDITOR ]:document() )
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditObjectCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]:qCoEdit
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ TAB_OTAB ]:oWidget:pPtr, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]:qCqEdit
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
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
      ::oDK:setView( oEdit:cView )

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

   IF ::qTabWidget:count() == 0
      RETURN .f.
   ENDIF

   IF nIndex >= ::qTabWidget:count()
      nIndex := 0
   ENDIF

   ::qTabWidget:setCurrentIndex( nIndex )
   ::getEditorByIndex( nIndex ):setDocumentProperties()

   RETURN .f.

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:undo()
   IF !empty( ::qCurEdit )
      ::qCurEdit:undo()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:redo()
   IF !empty( ::qCurEdit )
      ::qCurEdit:redo()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:cut()
   IF !empty( ::qCurEdit )
      ::qCurEdit:cut()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:copy()
   IF !empty( ::qCurEdit )
      ::qCurEdit:copy()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:paste()
   IF !empty( ::qCurEdit )
      ::qCurEdit:paste()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:selectAll()
   IF !empty( ::qCurEdit )
      ::qCurEdit:selectAll()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:toggleSelectionMode()

   IF !empty( ::qCurEdit )
      ::qCurEdit:hbHighlightSelectedColumns( ::isColumnSelectionEnabled )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:duplicateLine()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:duplicateLine()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:moveLine( nDirection )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:moveLine( nDirection )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:deleteLine()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:deleteLine()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:streamComment()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:streamComment()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:blockComment()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:blockComment()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:indent( nStep )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:blockIndent( nStep )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:convertQuotes()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:convertQuotes()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:convertDQuotes()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:convertDQuotes()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
////
METHOD IdeEditsManager:switchToReadOnly()
   IF !empty( ::qCurEdit )
      ::qCurEdit:setReadOnly( !( ::qCurEdit:isReadOnly() ) )
      ::oCurEditor:setTabImage()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleLineNumbers()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleLineNumbers()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getText()
   LOCAL oEdit, cText := ""
   IF !empty( oEdit := ::getEditObjectCurrent() )
      cText := oEdit:getText()
   ENDIF
   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getWord( lSelect )
   LOCAL oEdit, cText := ""
   IF !empty( oEdit := ::getEditObjectCurrent() )
      cText := oEdit:getWord( lSelect )
   ENDIF
   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getLine( nLine, lSelect )
   LOCAL oEdit, cText := ""
   IF !empty( oEdit := ::getEditObjectCurrent() )
      cText := oEdit:getLine( nLine, lSelect )
   ENDIF
   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:convertSelection( cKey )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      SWITCH cKey
      CASE "ToUpper"
         oEdit:caseUpper()
         EXIT
      CASE "ToLower"
         oEdit:caseLower()
         EXIT
      CASE "Invert"
         oEdit:caseInvert()
         EXIT
      ENDSWITCH
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:insertSeparator()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:insertSeparator()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:insertText( cKey )
   LOCAL cFile, cText, oEdit

   IF ! empty( oEdit := ::getEditObjectCurrent() )
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

      oEdit:insertText( cText )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
////
METHOD IdeEditsManager:formatBraces()
   LOCAL qEdit, qDoc, cText

   IF !empty( qEdit := ::getEditCurrent() )

      qDoc := QTextDocument():configure( qedit:document() )

      IF !( qDoc:isEmpty() )
         qDoc:setUndoRedoEnabled( .f. )

         cText := qDoc:toPlainText()

         cText := strtran( cText, "( ", "(" )
         cText := strtran( cText, "(  ", "(" )
         cText := strtran( cText, "(   ", "(" )
         cText := strtran( cText, "(    ", "(" )
         cText := strtran( cText, "(     ", "(" )
         cText := strtran( cText, "(      ", "(" )
         cText := strtran( cText, " (", "(" )
         cText := strtran( cText, "  (", "(" )
         cText := strtran( cText, "   (", "(" )
         cText := strtran( cText, "    (", "(" )
         cText := strtran( cText, "     (", "(" )

         cText := strtran( cText, "      )", ")" )
         cText := strtran( cText, "     )", ")" )
         cText := strtran( cText, "    )", ")" )
         cText := strtran( cText, "   )", ")" )
         cText := strtran( cText, "  )", ")" )
         cText := strtran( cText, " )", ")" )

         cText := strtran( cText, "(", "( " )
         cText := strtran( cText, ")", " )" )

         cText := strtran( cText, "(     )", "()" )
         cText := strtran( cText, "(    )", "()" )
         cText := strtran( cText, "(   )", "()" )
         cText := strtran( cText, "(  )", "()" )
         cText := strtran( cText, "( )", "()" )

         qDoc:clear()
         qDoc:setPlainText( cText )

         qDoc:setUndoRedoEnabled( .t. )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//////
METHOD IdeEditsManager:RemoveTabs()
   LOCAL qEdit, qDoc, cText, cSpaces

   IF ! empty( qEdit := ::getEditCurrent() )

      qDoc := QTextDocument():configure( qedit:document() )

      IF !( qDoc:isEmpty() )
         cSpaces := space( ::nTabSpaces )

         qDoc:setUndoRedoEnabled( .f. )

         cText := qDoc:toPlainText()
         qDoc:clear()
         qDoc:setPlainText( strtran( cText, chr( 9 ), cSpaces ) )

         qDoc:setUndoRedoEnabled( .t. )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:RemoveTrailingSpaces()
   LOCAL qEdit, qDoc, cText, a_, s

   IF ! empty( qEdit := ::getEditCurrent() )

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
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:zoom( nKey )
   LOCAL nPointSize, qFont, oEdit, oEditor

   IF ! empty( oEditor := ::getEditorCurrent() )
      oEdit := oEditor:oEdit

      qFont := QFont():configure( oEdit:qEdit:font() )
      qFont:setFamily( "Courier New" )
      qFont:setFixedPitch( .t. )
      nPointSize := qFont:pointSize()
      nPointSize += iif( nKey == 1, 1, -1 )

      IF nPointSize > 4 .AND. nPointSize < 37
         qFont:setPointSize( nPointSize )

         oEdit:qEdit:setFont( qFont )

         FOR EACH oEdit IN oEditor:aEdits
            oEdit:qEdit:setFont( qFont )
         NEXT
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
////
METHOD IdeEditsManager:printPreview()
   LOCAL qDlg
   IF ! empty( ::qCurEdit )
      qDlg := QPrintPreviewDialog():new( ::oDlg:oWidget )
      qDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
      Qt_Slots_Connect( ::pSlots, qDlg, "paintRequested(QPrinter)", {|p| ::paintRequested( p ) } )
      qDlg:exec()
      Qt_Slots_disConnect( ::pSlots, qDlg, "paintRequested(QPrinter)" )
   ENDIF
   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:paintRequested( pPrinter )
   LOCAL qPrinter
   qPrinter := QPrinter():configure( pPrinter )
   ::qCurEdit:print( qPrinter )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getSelectedText()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      RETURN oEdit:getSelectedText()
   ENDIF
   RETURN ""

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:setMark()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:setNewMark()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:gotoMark( nIndex )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:gotoMark( nIndex )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:goto()
   LOCAL qGo, nLine, qCursor, nRows, oEdit

   IF ! empty( oEdit := ::oEM:getEditObjectCurrent() )
      qCursor := QTextCursor():configure( oEdit:qEdit:textCursor() )
      nLine   := qCursor:blockNumber()
      nRows   := oEdit:qEdit:blockCount()

      qGo := QInputDialog():new( ::oDlg:oWidget )
      qGo:setIntMinimum( 1 )
      qGo:setIntMaximum( nRows + 1 )
      qGo:setIntValue( nLine + 1 )
      qGo:setLabelText( "Goto Line Number [1-" + hb_ntos( nRows ) + "]" )
      qGo:setWindowTitle( "Harbour-Qt" )

      ::setPosByIni( qGo, GotoDialogGeometry )
      qGo:exec()
      ::aIni[ INI_HBIDE, GotoDialogGeometry ] := hbide_posAndSize( qGo )

      oEdit:goto( qGo:intValue() )
   ENDIF

   RETURN nLine

/*----------------------------------------------------------------------*/
//
//                            CLASS IdeEditor
//                     Holds One Document in One Tab
//
/*----------------------------------------------------------------------*/

#define qTimeSave_timeout                         101

CLASS IdeEditor INHERIT IdeObject

   DATA   oTab
   DATA   cPath
   DATA   cFile                                   INIT   ""
   DATA   cExt                                    INIT   ""
   DATA   cType                                   INIT   ""
   DATA   cTheme                                  INIT   ""
   DATA   cView
   DATA   qDocument
   DATA   qDocLayout
   DATA   qHiliter
   DATA   qTimerSave
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

   DATA   nPos                                    INIT   0
   DATA   nHPos                                   INIT   0
   DATA   nVPos                                   INIT   0
   DATA   nID

   DATA   qCursor
   DATA   aSplits                                 INIT   {}

   DATA   qHLayout
   DATA   qLabel
   DATA   nnRow                                   INIT -99
   DATA   qPoint                                  INIT QPoint():new()

   DATA   qEvents
   DATA   qSlots
   DATA   qMarkLayoutOld

   METHOD new( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView )
   METHOD create( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView )
   METHOD split( nOrient, oEditP )
   METHOD relay( oEdit )
   METHOD destroy()
   METHOD execEvent( nMode, p )
   METHOD setDocumentProperties()
   METHOD activateTab( mp1, mp2, oXbp )
   METHOD buildTabPage( cSource )
   METHOD dispEditInfo( qEdit )
   METHOD setTabImage( qEdit )
   METHOD applyTheme( cTheme )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEditor:new( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView )

   DEFAULT oIde        TO ::oIde
   DEFAULT cSourceFile TO ::sourceFile
   DEFAULT nPos        TO ::nPos
   DEFAULT nHPos       TO ::nHPos
   DEFAULT nVPos       TO ::nVPos
   DEFAULT cTheme      TO ::cTheme
   DEFAULT cView       TO ::cView

   ::oIde       := oIde
   ::sourceFile := cSourceFile
   ::nPos       := nPos
   ::nHPos      := nHPos
   ::nVPos      := nVPos
   ::cTheme     := cTheme
   ::cView      := cView

   ::nID        := hbide_getNextUniqueID()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:create( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView )
   LOCAL cFileTemp

   ::qSlots := HBSlots():new()

   DEFAULT oIde        TO ::oIde
   DEFAULT cSourceFile TO ::sourceFile
   DEFAULT nPos        TO ::nPos
   DEFAULT nHPos       TO ::nHPos
   DEFAULT nVPos       TO ::nVPos
   DEFAULT cTheme      TO ::cTheme
   DEFAULT cView       TO ::cView

   ::oIde           := oIde
   ::SourceFile     := hbide_pathNormalized( cSourceFile, .F. )
   ::nPos           := nPos
   ::nHPos          := nHPos
   ::nVPos          := nVPos
   ::cTheme         := cTheme
   ::cView          := cView

   DEFAULT ::cView TO iif( ::cWrkView == "Stats", "Main", ::cWrkView )
   ::oDK:setView( ::cView )

   ::pathNormalized := hbide_pathNormalized( cSourceFile, .t. )

   hb_fNameSplit( cSourceFile, @::cPath, @::cFile, @::cExt )

   cFileTemp := hbide_pathToOSPath( ::cPath + ::cFile + ::cExt + ".tmp" )
   IF hb_fileExists( cFileTemp )
      IF hbide_getYesNo( "An auto saved version already exists, restore ?", cSourceFile, "Last run crash detected" )
         hb_memowrit( hbide_pathToOSPath( cSourceFile ), hb_memoread( cFileTemp ) )
      ELSE
         ferase( cFileTemp )
      ENDIF
   ENDIF

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
   ::qDocLayout := QPlainTextDocumentLayout():new( ::qDocument )
   ::qDocument:setDocumentLayout( ::qDocLayout )

   IF ::cType != "U"
      ::qHiliter := ::oTH:SetSyntaxHilighting( ::oEdit:qEdit, @::cTheme )
   ENDIF
   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   ::qLayout:addLayout( ::oEdit:qHLayout, 0, 0 )

   /* Populate Tabs Array */
   aadd( ::aTabs, { ::oTab, Self } )

   /* Populate right at creation */
   ::oEM:addSourceInTree( ::sourceFile, ::cView )

   ::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget", ::nAnimantionMode ) )
   ::setTabImage()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:destroy()
   LOCAL n, oEdit

   IF !empty( ::qTimerSave )
      ::disconnect( ::qTimerSave, "timeout()" )
      ::qTimerSave:stop()
      ::qTimerSave := NIL
   ENDIF
   /* This code is reached under normal circumstances, so delete auto saved file */
   ferase( hbide_pathToOSPath( ::cPath + ::cFile + ::cExt + ".tmp" ) )

   ::qHiliter := NIL

   ::qCqEdit := NIL
   ::qCoEdit := NIL
   ::qEdit   := NIL
   DO WHILE len( ::aEdits ) > 0
      oEdit := ::aEdits[ 1 ]
      hb_adel( ::aEdits, 1, .t. )
      oEdit:destroy()
   ENDDO
   ::oEdit:destroy()

   IF !Empty( ::qDocument )
      ::qDocument := NIL
   ENDIF

   IF !Empty( ::qHiliter )
      ::qHiliter  := NIL
   ENDIF

   ::oEdit := NIL

   IF !Empty( ::qLayout )
      ::qLayout   := NIL
   ENDIF

   IF ( n := ascan( ::aTabs, {|e_| e_[ TAB_OEDITOR ] == Self } ) ) > 0
      hb_adel( ::oIde:aTabs, n, .T. )
   ENDIF

   ::oEM:removeSourceInTree( ::sourceFile )

   ::qTabWidget:removeTab( ::qTabWidget:indexOf( ::oTab:oWidget ) )
   ::oTab := NIL

   IF ::qTabWidget:count() == 0
      IF ::lDockRVisible
         ::oFuncDock:hide()
         ::oIde:lDockRVisible := .f.
      ENDIF
   ENDIF
//hbide_dbg( "IdeEditor:destroy()", 1, "-------------------------------------" )
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

      IF ::cType $ "PRG,C,CPP,H,CH"
         ::qTimerSave := QTimer():New()
         ::qTimerSave:setInterval( 60000 )  // 1 minutes
         ::connect( ::qTimerSave, "timeout()", {|| ::execEvent( qTimeSave_timeout ) } )
         ::qTimerSave:start()
      ENDIF
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

METHOD IdeEditor:execEvent( nMode, p )
   LOCAL cFileTemp

   p := p

   SWITCH nMode
   CASE qTimeSave_timeout
      IF ::qDocument:isModified()
         cFileTemp := hbide_pathToOSPath( ::cPath + ::cFile + ::cExt + ".tmp" )
         hb_memowrit( cFileTemp, ::qEdit:toPlainText() )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:relay( oEdit )
   LOCAL nCols, oEdt, nR, nC

   ::qLayout:removeItem( ::oEdit:qHLayout )
   FOR EACH oEdt IN ::aEdits
      ::qLayout:removeItem( oEdt:qHLayout )
      //
      oEdt:qHLayout:removeWidget( oEdt:qEdit )
      oEdt:qHLayout := QHBoxLayout():new()
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

METHOD IdeEditor:activateTab( mp1, mp2, oXbp )
   LOCAL oEdit

   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   IF !empty( oEdit := ::oEM:getEditorByTabObject( oXbp ) )
      oEdit:setDocumentProperties()
      oEdit:qCoEdit:relayMarkButtons()
      oEdit:qCoEdit:toggleLineNumbers()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:buildTabPage( cSource )

   ::oTab := XbpTabPage():new( ::oTabParent, , { 5,5 }, { 700,400 }, , .t. )

   IF Empty( cSource )
      ::oTab:caption := "Untitled " + hb_ntos( hbide_getNextUntitled() )
   ELSE
      ::oTab:caption := ::cFile + ::cExt
   ENDIF
   ::oTab:minimized := .F.

   ::oTab:create()

   ::qTabWidget:setCurrentIndex( ::qTabWidget:indexOf( ::oTab:oWidget ) )
   ::qTabWidget:setTabTooltip( ::qTabWidget:indexOf( ::oTab:oWidget ), cSource )

   ::oTab:tabActivate := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }

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
   ::oDK:setStatusText( SB_PNL_MODIFIED, iif( lModified, "Modified", iif( lReadOnly, "ReadOnly", " " ) ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:applyTheme( cTheme )

   IF ::cType != "U"
      IF empty( cTheme )
         cTheme := ::oTH:selectTheme()
      ENDIF

      IF ::oTH:contains( cTheme )
         ::cTheme := cTheme
         ::qHiliter := ::oTH:SetSyntaxHilighting( ::qEdit, @::cTheme )
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
   DATA   qSelColor                               INIT  QColor():new( 255,0,255 )

   DATA   qCursorMark
   DATA   qMarkUData                              INIT  HBQTextBlockUserData():new()
   DATA   aBookMarks                              INIT  {}

   DATA   qSlots
   DATA   lModified                               INIT  .F.
   DATA   lIndentIt                               INIT  .f.
   DATA   lUpdatePrevWord                         INIT  .f.
   DATA   lCopyWhenDblClicked                     INIT  .f.
   DATA   cCurLineText                            INIT  ""

   DATA   cProto                                  INIT ""
   DATA   qTimer
   DATA   nProtoLine                              INIT -1
   DATA   nProtoCol                               INIT -1
   DATA   isSuspended                             INIT .f.

   METHOD new( oEditor, nMode )
   METHOD create( oEditor, nMode )
   METHOD destroy()
   METHOD execEvent( nMode, oEdit, p, p1 )
   METHOD execKeyEvent( nMode, nEvent, p, p1 )
   METHOD connectEditSignals( oEdit )
   METHOD disconnectEditSignals( oEdit )

   METHOD setNewMark()
   METHOD gotoMark( nIndex )
   METHOD duplicateLine()
   METHOD deleteLine()
   METHOD blockComment()
   METHOD streamComment()
   METHOD blockIndent( nMode )
   METHOD moveLine( nDirection )
   METHOD caseUpper()
   METHOD caseLower()
   METHOD caseInvert()
   METHOD convertQuotes()
   METHOD convertDQuotes()
   METHOD findLastIndent()
   METHOD reLayMarkButtons()
   METHOD presentSkeletons()
   METHOD handleCurrentIndent()
   METHOD handlePreviousWord( lUpdatePrevWord )
   METHOD loadFuncHelp()
   METHOD clickFuncHelp()
   METHOD goto( nLine )
   METHOD gotoFunction()
   METHOD toggleLineNumbers()

   METHOD getWord( lSelect )
   METHOD getLine( nLine, lSelect )
   METHOD getText()
   METHOD getSelectedText()
   METHOD getColumnNo()
   METHOD getLineNo()
   METHOD insertSeparator()
   METHOD insertText( cText )

   METHOD suspendPrototype()
   METHOD resumePrototype()
   METHOD showPrototype( cProto )
   METHOD hidePrototype()
   METHOD completeCode( p )

   METHOD setLineNumbersBkColor( nR, nG, nB )
   METHOD setCurrentLineColor( nR, nG, nB )
   METHOD getCursor()                             INLINE QTextCursor():from( ::qEdit:textCursor() )
   METHOD down()
   METHOD up()
   METHOD home()
   METHOD find( cText, nPosFrom )
   METHOD refresh()
   METHOD isModified()                            INLINE ::oEditor:qDocument:isModified()

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

   ::qEdit   := HBQPlainTextEdit():new()
   //
   ::qEdit:setLineWrapMode( QTextEdit_NoWrap )
   ::qEdit:setFont( ::oFont:oWidget )
   ::qEdit:ensureCursorVisible()
   ::qEdit:setContextMenuPolicy( Qt_CustomContextMenu )
   ::qEdit:installEventFilter( ::pEvents )

   ::qEdit:hbHighlightCurrentLine( .t. )              /* Via user-setup */
   ::qEdit:hbSetSpaces( ::nTabSpaces )

   ::qEdit:hbSetCompleter( ::qCompleter )

   ::toggleLineNumbers()

   ::qHLayout := QHBoxLayout():new()
   ::qHLayout:setSpacing( 0 )

   ::qHLayout:addWidget( ::qEdit )

   ::connectEditSignals( Self )

   Qt_Events_Connect( ::pEvents, ::qEdit, QEvent_KeyPress           , {|p| ::execKeyEvent( 101, QEvent_KeyPress, p ) } )
   Qt_Events_Connect( ::pEvents, ::qEdit, QEvent_Wheel              , {|p| ::execKeyEvent( 102, QEvent_Wheel   , p ) } )
   Qt_Events_Connect( ::pEvents, ::qEdit, QEvent_FocusIn            , {| | ::execKeyEvent( 104, QEvent_FocusIn     ) } )
   Qt_Events_Connect( ::pEvents, ::qEdit, QEvent_FocusOut           , {| | ::execKeyEvent( 105, QEvent_FocusOut    ) } )
   Qt_Events_Connect( ::pEvents, ::qEdit, QEvent_MouseButtonDblClick, {|p| ::execKeyEvent( 103, QEvent_MouseButtonDblClick, p ) } )

   ::qEdit:hbSetEventBlock( {|p,p1| ::execKeyEvent( 115, 1001, p, p1 ) } )

   ::qTimer := QTimer():new()
   ::qTimer:setInterval( 2000 )
   ::connect( ::qTimer, "timeout()",  {|| ::execEvent( timerTimeout, Self ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:destroy()

   ::disconnect( ::qTimer, "timeout()" )
   IF ::qTimer:isActive()
      ::qTimer:stop()
   ENDIF
   ::qTimer := NIL

   Qt_Events_DisConnect( ::pEvents, ::qEdit, QEvent_KeyPress            )
   Qt_Events_DisConnect( ::pEvents, ::qEdit, QEvent_Wheel               )
   Qt_Events_DisConnect( ::pEvents, ::qEdit, QEvent_FocusIn             )
   Qt_Events_DisConnect( ::pEvents, ::qEdit, QEvent_FocusOut            )
   Qt_Events_DisConnect( ::pEvents, ::qEdit, QEvent_MouseButtonDblClick )

   ::disconnectEditSignals( Self )

   ::oEditor:qLayout:removeItem( ::qHLayout )
   //
   ::qHLayout:removeWidget( ::qEdit )
   ::qEdit    := NIL
   ::qHLayout := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:disconnectEditSignals( oEdit )
   HB_SYMBOL_UNUSED( oEdit )

   ::disConnect( oEdit:qEdit, "customContextMenuRequested(QPoint)" )
   ::disConnect( oEdit:qEdit, "textChanged()"                      )
   ::disConnect( oEdit:qEdit, "selectionChanged()"                 )
   ::disConnect( oEdit:qEdit, "cursorPositionChanged()"            )
   ::disConnect( oEdit:qEdit, "copyAvailable(bool)"                )

   #if 0
   ::disConnect( oEdit:qEdit, "modificationChanged(bool)"          )
   ::disConnect( oEdit:qEdit, "updateRequest(QRect,int)"           )
   ::disConnect( oEdit:qEdit, "redoAvailable(bool)"                )
   ::disConnect( oEdit:qEdit, "undoAvailable(bool)"                )
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:connectEditSignals( oEdit )
   HB_SYMBOL_UNUSED( oEdit )

   ::connect( oEdit:qEdit, "customContextMenuRequested(QPoint)", {|p   | ::execEvent( 1, oEdit, p     ) } )
   ::Connect( oEdit:qEdit, "textChanged()"                     , {|    | ::execEvent( 2, oEdit,       ) } )
   ::Connect( oEdit:qEdit, "selectionChanged()"                , {|p   | ::execEvent( 6, oEdit, p     ) } )
   ::Connect( oEdit:qEdit, "cursorPositionChanged()"           , {|    | ::execEvent( 9, oEdit,       ) } )
   ::Connect( oEdit:qEdit, "copyAvailable(bool)"               , {|p   | ::execEvent( 3, oEdit, p     ) } )

   #if 0
   ::Connect( oEdit:qEdit, "modificationChanged(bool)"         , {|p   | ::execEvent( 4, oEdit, p     ) } )
   ::Connect( oEdit:qEdit, "updateRequest(QRect,int)"          , {|p,p1| ::execEvent( 8, oEdit, p, p1 ) } )
   ::Connect( oEdit:qEdit, "redoAvailable(bool)"               , {|p   | ::execEvent( 5, oEdit, p     ) } )
   ::Connect( oEdit:qEdit, "undoAvailable(bool)"               , {|p   | ::execEvent( 7, oEdit, p     ) } )
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:execEvent( nMode, oEdit, p, p1 )
   LOCAL pAct, qAct, n, qCursor, qEdit, oo, nLine

   HB_SYMBOL_UNUSED( p1 )

   qEdit   := oEdit:qEdit
   qCursor := QTextCursor():configure( qEdit:textCursor() )
   oEdit:nCurLineNo := qCursor:blockNumber()

   SWITCH nMode

   CASE customContextMenuRequested
      QAction():from( ::oEM:aActions[ 17, 2 ] ):setEnabled( !empty( qCursor:selectedText() ) )

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
         CASE qAct:text() == "Save as Skeleton..."
            ::oSK:saveAs( ::getSelectedText() )
         CASE qAct:text() == "Apply Theme"
            ::oEditor:applyTheme()
         CASE qAct:text() == "Goto Function"
            ::gotoFunction()
         ENDCASE
      ENDIF
      EXIT

   CASE textChanged
      //hbide_dbg( "textChanged()" )
      ::oEditor:setTabImage( qEdit )
      EXIT

   CASE selectionChanged
      //hbide_dbg( "selectionChanged()" )
      ::oEditor:qCqEdit := qEdit
      ::oEditor:qCoEdit := oEdit

      qCursor := QTextCursor():configure( qEdit:TextCursor() )

      /* Book Marks reach-out buttons */
      ::relayMarkButtons()
      ::toggleLineNumbers()

      ::updateTitleBar()

      /* An experimental move but seems a lot is required to achieve column selection */
      qEdit:hbHighlightSelectedColumns( ::isColumnSelectionEnabled )

      ::oDK:setStatusText( SB_PNL_SELECTEDCHARS, len( qCursor:selectedText() ) )
      EXIT

   CASE cursorPositionChanged
      // hbide_dbg( "cursorPositionChanged()", ::nProtoLine, ::nProtoCol, ::isSuspended, ::getLineNo(), ::getColumnNo(), ::cProto )
      ::oEditor:dispEditInfo( qEdit )
      ::handlePreviousWord( ::lUpdatePrevWord )
      ::handleCurrentIndent()

      IF ::nProtoLine != -1
         nLine := ::getLineNo()
         IF ! ::isSuspended
            IF nLine != ::nProtoLine .OR. ::getColumnNo() <= ::nProtoCol
               ::suspendPrototype()
            ENDIF
         ELSE
            IF nLine == ::nProtoLine .AND. ::getColumnNo() >= ::nProtoCol
               ::resumePrototype()
            ENDIF
         ENDIF
      ENDIF

      EXIT

   CASE copyAvailable
      IF p .AND. ::lCopyWhenDblClicked
         ::qEdit:copy()
      ENDIF
      ::lCopyWhenDblClicked := .f.
      EXIT

   CASE timerTimeout
      IF empty( ::cProto )
         ::hidePrototype()
      ELSE
         ::showPrototype()
      ENDIF
      EXIT

   #if 0
   CASE modificationChanged
      //hbide_dbg( "modificationChanged(bool)", p )
      EXIT
   CASE redoAvailable
      //hbide_dbg( "redoAvailable(bool)", p )
      EXIT
   CASE undoAvailable
      //hbide_dbg( "undoAvailable(bool)", p )
      EXIT
   CASE updateRequest
      EXIT
   #endif
   ENDSWITCH

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEdit:execKeyEvent( nMode, nEvent, p, p1 )
   LOCAL key, kbm, txt, qEvent
   LOCAL lAlt   := .f.
   LOCAL lCtrl  := .f.
   LOCAL lShift := .f.

   p1 := p1

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
      CASE Qt_Key_Space
         IF !lAlt .AND. !lShift .AND. !lCtrl
            ::lUpdatePrevWord := .t.
         ENDIF
         EXIT
      CASE Qt_Key_Return
      CASE Qt_Key_Enter
         ::handlePreviousWord( .t. )
         ::lIndentIt := .t.
         EXIT
      CASE Qt_Key_Tab
         IF lCtrl
            ::blockIndent( 1 )
            RETURN .T.
         ENDIF
         EXIT
      CASE Qt_Key_Backtab
         IF lCtrl
            ::blockIndent( -1 )
            RETURN .t.
         ENDIF
         EXIT
      CASE Qt_Key_Q                   /* All these actions will be pulled from user-setup */
         IF lCtrl .AND. lShift
            ::streamComment()
         ENDIF
         EXIT
      CASE Qt_Key_Slash
         IF lCtrl
            ::blockComment()
         ENDIF
         EXIT
      CASE Qt_Key_D
         IF lCtrl
            ::duplicateLine()
         ENDIF
         EXIT
      CASE Qt_Key_K
         IF lCtrl
            ::presentSkeletons()
         ENDIF
         EXIT
      CASE Qt_Key_Backspace
         hbide_justACall( txt, lAlt, lShift, lCtrl, qEvent, nMode )
         EXIT
      CASE Qt_Key_Delete
         IF lCtrl
            ::deleteLine()
            RETURN .t.
         ENDIF
         EXIT
      CASE Qt_Key_Up
         IF lCtrl .AND. lShift
            ::moveLine( -1 )
            RETURN .t.
         ENDIF
      CASE Qt_Key_Down
         IF lCtrl .AND. lShift
            ::moveLine( 1 )
            RETURN .t.
         ENDIF
      CASE Qt_Key_ParenLeft
         IF ! lCtrl .AND. ! lAlt
            ::loadFuncHelp()     // Also invokes prototype display
         ENDIF
         EXIT
      CASE Qt_Key_ParenRight
         IF ! lCtrl .AND. ! lAlt
            ::hidePrototype()
         ENDIF
         EXIT
      CASE Qt_Key_T
         IF lCtrl
            ::gotoFunction()
         ENDIF
         EXIT
      CASE Qt_Key_F1
         ::gotoFunction()
         EXIT
      ENDSWITCH

      EXIT

   CASE QEvent_FocusIn
      ::resumePrototype()
      EXIT

   CASE QEvent_FocusOut
      ::suspendPrototype()
      EXIT

   CASE QEvent_Wheel
      EXIT

   CASE QEvent_MouseButtonDblClick
      ::lCopyWhenDblClicked := .t.
      EXIT

   CASE 1001
      IF p == QEvent_MouseButtonDblClick
         ::lCopyWhenDblClicked := .f.       /* not intuitive */
         ::clickFuncHelp()

      ELSEIF p == QEvent_Paint
         // ::oIde:testPainter( p1 )

      ENDIF
      EXIT

   ENDSWITCH

   RETURN .F.  /* Important */

/*----------------------------------------------------------------------*/

METHOD IdeEdit:presentSkeletons()

   ::oSK:selectByMenuAndPostText( ::qEdit )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:toggleLineNumbers()

   ::qEdit:hbNumberBlockVisible( ::lLineNumbersVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:gotoMark( nIndex )
   IF len( ::aBookMarks ) >= nIndex
      ::qEdit:hbGotoBookmark( ::aBookMarks[ nIndex ] )
      ::qEdit:centerCursor()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:relayMarkButtons()
   LOCAL oBtn
   FOR EACH oBtn IN ::aMarkTBtns
      oBtn:hide()
   NEXT
   FOR EACH oBtn IN ::aBookMarks
      ::aMarkTBtns[ oBtn:__enumIndex() ]:show()
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setNewMark()
   LOCAL qCursor, nBlock, n

   IF !( qCursor := QTextCursor():configure( ::qEdit:textCursor() ) ):isNull()
      nBlock := qCursor:blockNumber() + 1

      IF ( n := ascan( ::aBookMarks, nBlock ) ) > 0
         hb_adel( ::aBookMarks, n, .t. )
         ::aMarkTBtns[ len( ::aBookMarks ) + 1 ]:hide()
      ELSE
         IF len( ::aBookMarks ) == 6
            RETURN Self
         ENDIF
         aadd( ::aBookMarks, nBlock )
         n := len( ::aBookMarks )
         ::aMarkTBtns[ n ]:show()
      ENDIF

      ::qEdit:hbBookMarks( nBlock )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setLineNumbersBkColor( nR, nG, nB )
   ::qEdit:hbSetLineAreaBkColor( QColor():new( nR, nG, nB ) )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:setCurrentLineColor( nR, nG, nB )
   ::qEdit:hbSetCurrentLineColor( QColor():new( nR, nG, nB ) )
   RETURN Self

/*----------------------------------------------------------------------*/
/* TO BE EXTENDED */
METHOD IdeEdit:find( cText, nPosFrom )
   LOCAL lFound, nPos
   LOCAL qCursor := ::getCursor()

   nPos := qCursor:position()
   IF hb_isNumeric( nPosFrom )
      qCursor:setPosition( nPosFrom )
   ENDIF
   ::qEdit:setTextCursor( qCursor )
   IF ( lFound := ::qEdit:find( cText, QTextDocument_FindCaseSensitively ) )
      ::qEdit:centerCursor()
   ELSE
      qCursor:setPosition( nPos )
      ::qEdit:setTextCursor( qCursor )
   ENDIF

   RETURN lFound

/*----------------------------------------------------------------------*/

METHOD IdeEdit:refresh()
   ::qEdit:hbRefresh()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:home()
   LOCAL qCursor := ::getCursor()

   qCursor:movePosition( QTextCursor_StartOfBlock )
   ::qEdit:setTextCursor( qCursor )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:down()
   LOCAL qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   qCursor:movePosition( QTextCursor_Down )
   ::qEdit:setTextCursor( qCursor )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:up()
   LOCAL qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   qCursor:movePosition( QTextCursor_Up )
   ::qEdit:setTextCursor( qCursor )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:duplicateLine()
   ::qEdit:hbDuplicateLine()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:deleteLine()
   ::qEdit:hbDeleteLine()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:moveLine( nDirection )
   ::qEdit:hbMoveLine( nDirection )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:blockComment()
   ::qEdit:hbBlockComment()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:streamComment()
   ::qEdit:hbStreamComment()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:blockIndent( nMode )
   ::qEdit:hbBlockIndent( nMode )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:caseUpper()
   ::qEdit:hbCaseUpper()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:caseLower()
   ::qEdit:hbCaseLower()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:convertQuotes()
   ::qEdit:hbConvertQuotes()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:convertDQuotes()
   ::qEdit:hbConvertDQuotes()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:caseInvert()
   LOCAL i, c, s, cBuffer, nLen

   IF !empty( cBuffer := ::getSelectedText() )
      s    := ""
      nLen := len( cBuffer )
      FOR i := 1 TO nLen
         c := substr( cBuffer, i, 1 )
         IF isAlpha( c )
            s += iif( isUpper( c ), lower( c ), upper( c ) )
         ELSE
            s += c
         ENDIF
      NEXT
      ::qEdit:hbReplaceSelection( s )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getSelectedText()
   RETURN ::qEdit:hbGetSelectedText()

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getText()
   RETURN QTextCursor():from( ::qEdit:textCursor() ):selectedText()

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getWord( lSelect )
   LOCAL cText, qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   DEFAULT lSelect TO .F.

   qCursor:select( QTextCursor_WordUnderCursor )
   cText := qCursor:selectedText()

   IF lSelect
      ::qEdit:setTextCursor( qCursor )
   ENDIF
   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEdit:goto( nLine )
   LOCAL qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   qCursor:movePosition( QTextCursor_Start )
   qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine - 1 )
   ::qEdit:setTextCursor( qCursor )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getLine( nLine, lSelect )
   LOCAL cText, qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   DEFAULT nLine   TO qCursor:blockNumber() + 1
   DEFAULT lSelect TO .F.

   IF nLine != qCursor:blockNumber() + 1
      qCursor:movePosition( QTextCursor_Start )
      qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine - 1 )
   ENDIF

   qCursor:select( QTextCursor_LineUnderCursor )
   cText := qCursor:selectedText()
   IF lSelect
      ::qEdit:setTextCursor( qCursor )
   ENDIF

   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getColumnNo()
   RETURN QTextCursor():from( ::qEdit:textCursor() ):columnNumber() + 1

/*----------------------------------------------------------------------*/

METHOD IdeEdit:getLineNo()
   RETURN QTextCursor():from( ::qEdit:textCursor() ):blockNumber() + 1

/*----------------------------------------------------------------------*/

METHOD IdeEdit:insertSeparator()
   LOCAL qCursor := QTextCursor():configure( ::qEdit:textCursor() )

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

METHOD IdeEdit:insertText( cText )
   LOCAL qCursor, nL, nB

   IF !Empty( cText )
      qCursor := QTextCursor():configure( ::qEdit:textCursor() )

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

METHOD IdeEdit:handlePreviousWord( lUpdatePrevWord )
   LOCAL qCursor, qTextBlock, cText, cWord, nB, nL, qEdit, lPrevOnly, nCol, nSpace, nSpaces, nOff

   IF ! lUpdatePrevWord
      RETURN Self
   ENDIF
   ::lUpdatePrevWord := .f.

   qEdit := ::qEdit

   qCursor    := QTextCursor():configure( qEdit:textCursor() )
   qTextBlock := QTextBlock():configure( qCursor:block() )
   cText      := qTextBlock:text()
   nCol       := qCursor:columnNumber()
   IF ( substr( cText, nCol - 1, 1 ) == " " )
      RETURN nil
   ENDIF
   nSpace := iif( substr( cText, nCol, 1 ) == " ", 1, 0 )
   cWord  := hbide_getPreviousWord( cText, nCol + 1 )

   IF !empty( cWord ) .AND. hbide_isHarbourKeyword( cWord )
      lPrevOnly := left( lower( ltrim( cText ) ), len( cWord ) ) == lower( cWord )

      nL := len( cWord ) + nSpace
      nB := qCursor:position() - nL

      IF ::oEditor:cExt $ ".prg"
         qCursor:beginEditBlock()
         qCursor:setPosition( nB )
         qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nL )
         qCursor:removeSelectedText()
         qCursor:insertText( upper( cWord ) + space( nSpace ) )
         qCursor:endEditBlock()
         qEdit:setTextCursor( qCursor )
      ENDIF

      IF hbide_isStartingKeyword( cWord )
         IF lPrevOnly
            qCursor:setPosition( nB )
            IF ( nCol := qCursor:columnNumber() ) > 0
               qCursor:beginEditBlock()
               qCursor:movePosition( QTextCursor_StartOfBlock )
               qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nCol )
               qCursor:removeSelectedText()
               qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_MoveAnchor, nL )
               qCursor:endEditBlock()
               qEdit:setTextCursor( qCursor )
            ENDIF
         ENDIF

      ELSEIF hbide_isMinimumIndentableKeyword( cWord )
         IF lPrevOnly
            qCursor:setPosition( nB )
            IF ( nCol := qCursor:columnNumber() ) >= 0
               qCursor:beginEditBlock()
               qCursor:movePosition( QTextCursor_StartOfBlock )
               qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nCol )
               qCursor:removeSelectedText()
               qCursor:insertText( space( ::nTabSpaces ) )
               qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_MoveAnchor, nL )
               qEdit:setTextCursor( qCursor )
               qCursor:endEditBlock()
            ENDIF
         ENDIF

      ELSEIF hbide_isIndentableKeyword( cWord )
         IF lPrevOnly
            nSpaces := hbide_getFrontSpacesAndWord( cText )
            IF nSpaces > 0 .AND. ( nOff := nSpaces % ::nTabSpaces ) > 0
               qCursor:setPosition( nB )
               qCursor:beginEditBlock()
               qCursor:movePosition( QTextCursor_PreviousCharacter, QTextCursor_KeepAnchor, nOff )
               qCursor:removeSelectedText()
               qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_MoveAnchor, nL )
               qEdit:setTextCursor( qCursor )
               qCursor:endEditBlock()
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeEdit:findLastIndent()
   LOCAL qCursor, qTextBlock, cText, cWord
   LOCAL nSpaces := 0

   qCursor := QTextCursor():configure( ::qEdit:textCursor() )
   qTextBlock := QTextBlock():configure( qCursor:block() )

   qTextBlock := QTextBlock():configure( qTextBlock:previous() )
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

METHOD IdeEdit:handleCurrentIndent()
   LOCAL qCursor, nSpaces

   IF ::lIndentIt
      ::lIndentIt := .f.
      IF ( nSpaces := ::findLastIndent() ) > 0
         qCursor := QTextCursor():configure( ::qEdit:textCursor() )
         qCursor:insertText( space( nSpaces ) )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:gotoFunction()
   LOCAL cWord
   IF !empty( cWord := ::getWord( .f. ) )
      ::oFN:jumpToFunction( cWord, .t. )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:clickFuncHelp()
   LOCAL cWord
   IF !empty( cWord := ::getWord( .f. ) )
      IF ! empty( ::oHL )
         ::oHL:jumpToFunction( cWord )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:resumePrototype()

   ::isSuspended := .f.
   IF !empty( ::qEdit )
      ::qEdit:hbShowPrototype( ::cProto )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:suspendPrototype()

   ::isSuspended := .t.
   IF !empty( ::qEdit )
      ::qEdit:hbShowPrototype( "" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:showPrototype( cProto )

   IF ! ::isSuspended  .AND. !empty( ::qEdit )
      IF !empty( cProto )
         ::cProto     := cProto
         ::nProtoLine := ::getLineNo()
         ::nProtoCol  := ::getColumnNo()
         ::qTimer:start()
      ENDIF
      ::qEdit:hbShowPrototype( ::cProto )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:hidePrototype()

   IF !empty( ::qedit )
      ::nProtoLine := -1
      ::nProtoCol  := -1
      ::cProto     := ""
      ::qTimer:stop()
      ::qEdit:hbShowPrototype( "" )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:completeCode( p )
   LOCAL qCursor := QTextCursor():from( ::qEdit:textCursor() )

   qCursor:movePosition( QTextCursor_Left )

   qCursor:movePosition( QTextCursor_StartOfWord )
   qCursor:movePosition( QTextCursor_EndOfWord, QTextCursor_KeepAnchor )
   qCursor:insertText( p )
   qCursor:movePosition( QTextCursor_Left )
   qCursor:movePosition( QTextCursor_Right )

   ::qEdit:setTextCursor( qCursor )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEdit:loadFuncHelp()
   LOCAL qEdit, qCursor, qTextBlock, cText, cWord, nCol, cPro

   qEdit := ::qEdit

   qCursor    := QTextCursor():configure( qEdit:textCursor() )
   qTextBlock := QTextBlock():configure( qCursor:block() )
   cText      := qTextBlock:text()
   nCol       := qCursor:columnNumber()
   cWord      := hbide_getPreviousWord( cText, nCol )
   IF !empty( cWord )
      IF ! empty( ::oHL )
         ::oHL:jumpToFunction( cWord )
      ENDIF
      IF !empty( cPro := ::oFN:positionToFunction( cWord, .t. ) )
         IF empty( ::cProto )
            ::showPrototype( ::cProto := hbide_formatProto( cPro ) )
         ENDIF
      ENDIF
   ENDIF
   RETURN Self

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

FUNCTION hbide_isStartingKeyword( cWord )
   STATIC s_b_ := { ;
                    'function' => NIL,;
                    'class' => NIL,;
                    'method' => NIL }

   RETURN Lower( cWord ) $ s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_isMinimumIndentableKeyword( cWord )
   STATIC s_b_ := { ;
                    'local' => NIL,;
                    'static' => NIL,;
                    'return' => NIL,;
                    'default' => NIL }

   RETURN Lower( cWord ) $ s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_isIndentableKeyword( cWord )
   STATIC s_b_ := { ;
                    'if' => NIL,;
                    'else' => NIL,;
                    'elseif' => NIL,;
                    'docase' => NIL,;
                    'case' => NIL,;
                    'otherwise' => NIL,;
                    'do' => NIL,;
                    'while' => NIL,;
                    'switch' => NIL,;
                    'for' => NIL,;
                    'next' => NIL,;
                    'begin' => NIL,;
                    'sequence' => NIL,;
                    'try' => NIL,;
                    'catch' => NIL,;
                    'always' => NIL,;
                    'recover' => NIL,;
                    'finally' => NIL }

   RETURN Lower( cWord ) $ s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_isHarbourKeyword( cWord )
   STATIC s_b_ := { ;
                    'function' => NIL,;
                    'return' => NIL,;
                    'static' => NIL,;
                    'local' => NIL,;
                    'default' => NIL,;
                    'if' => NIL,;
                    'else' => NIL,;
                    'elseif' => NIL,;
                    'endif' => NIL,;
                    'end' => NIL,;
                    'endswitch' => NIL,;
                    'docase' => NIL,;
                    'case' => NIL,;
                    'endcase' => NIL,;
                    'otherwise' => NIL,;
                    'switch' => NIL,;
                    'do' => NIL,;
                    'while' => NIL,;
                    'enddo' => NIL,;
                    'exit' => NIL,;
                    'for' => NIL,;
                    'each' => NIL,;
                    'next' => NIL,;
                    'step' => NIL,;
                    'to' => NIL,;
                    'class' => NIL,;
                    'endclass' => NIL,;
                    'method' => NIL,;
                    'data' => NIL,;
                    'var' => NIL,;
                    'destructor' => NIL,;
                    'inline' => NIL,;
                    'assign' => NIL,;
                    'access' => NIL,;
                    'inherit' => NIL,;
                    'init' => NIL,;
                    'create' => NIL,;
                    'virtual' => NIL,;
                    'message' => NIL,;
                    'begin' => NIL,;
                    'sequence' => NIL,;
                    'try' => NIL,;
                    'catch' => NIL,;
                    'always' => NIL,;
                    'recover' => NIL,;
                    'hb_symbol_unused' => NIL,;
                    'error' => NIL,;
                    'handler' => NIL }

   RETURN Lower( cWord ) $ s_b_

/*----------------------------------------------------------------------*/

FUNCTION hbide_formatProto( cProto )
   LOCAL n, n1, cArgs

   n  := at( "(", cProto )
   n1 := at( ")", cProto )

   IF n > 0 .AND. n1 > 0
      cArgs  := substr( cProto, n + 1, n1 - n - 1 )
      cArgs  := strtran( cArgs, ",", "<font color=red><b>" + "," + "</b></font>" )
      cProto := "<p style='white-space:pre'>" + "<b>" + substr( cProto, 1, n - 1 ) + "</b>" + ;
                   "<font color=red><b>" + "(" + "</b></font>" + ;
                      cArgs + ;
                         "<font color=red><b>" + ")" + "</font>" + "</b></p>"
   ENDIF
   RETURN cProto

/*----------------------------------------------------------------------*/

