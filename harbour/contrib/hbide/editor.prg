/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
#include "hbqtgui.ch"
#include "hbide.ch"
#include "xbp.ch"
#include "fileio.ch"

/*----------------------------------------------------------------------*/

#define EDT_LINNO_WIDTH                           50

#define __qcompleter_activated__                  2001
#define __qFldsCompleter_activated__              2002
#define __qDocModificationChanged__               2003
#define __qDocContentsChange__                    2004
#define __qTimeSave_timeout__                     2005
#define __qTab_contextMenu__                      2006
#define __qTabWidget_tabCloseRequested__          2007


#define __selectionMode_stream__                  1
#define __selectionMode_column__                  2
#define __selectionMode_line__                    3

/*----------------------------------------------------------------------*/

CLASS IdeEditsManager INHERIT IdeObject

   DATA   qContextMenu
   DATA   qContextSub
   DATA   qSrcControlSub
   DATA   aActions                                INIT  {}
   DATA   aProtos                                 INIT  {}

   DATA   qFldsStrList
   DATA   qFldsModel

   DATA   hEditingWords                           INIT {=>}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD removeSourceInTree( cSourceFile )
   METHOD addSourceInTree( cSourceFile, cView )
   METHOD execEvent( nEvent, p )
   METHOD buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme, cView, aBookMarks )
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
   METHOD insertSeparator( cSep )
   METHOD zoom( nKey )
   METHOD printPreview()
   METHOD paintRequested( qPrinter )
   METHOD setMark()
   METHOD setTooltipMark( nIndex )
   METHOD gotoMark( nIndex )
   METHOD goto( nLine )
   METHOD formatBraces()
   METHOD upperCaseKeywords()
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
   METHOD toggleStreamSelectionMode()
   METHOD toggleColumnSelectionMode()
   METHOD toggleLineSelectionMode()

   METHOD toggleLineNumbers()
   METHOD toggleHorzRuler()
   METHOD toggleCurrentLineHighlightMode()
   METHOD toggleCodeCompetion()
   METHOD toggleCompetionTips()

   METHOD getText()
   METHOD getWord( lSelect )
   METHOD getLine( nLine, lSelect )
   METHOD presentSkeletons()
   METHOD gotoFunction()
   METHOD clearSelection()

   METHOD home()
   METHOD end()
   METHOD down()
   METHOD up()
   METHOD goBottom()
   METHOD goTop()
   METHOD left()
   METHOD right()
   METHOD panEnd()
   METHOD panHome()
   METHOD pageUp()
   METHOD pageDown()

   METHOD find( cString, nPosFrom )
   METHOD showThumbnail()
   METHOD changeThumbnail()
   METHOD spaces2tabs()
   METHOD qscintilla()
   METHOD setStyleSheet( nMode )
   METHOD updateCompleter()
   METHOD updateFieldsList( cAlias )
   METHOD getProto( cWord )
   METHOD alignAt()
   METHOD stringify()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:new( oIde )

   ::oIde := oIde

   hb_hCaseMatch( ::hEditingWords, .F. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:destroy()
   LOCAL a_

   ::qCompleter:disconnect( "activated(QString)" )

   ::oIde:qCompModel := NIL
   ::oIde:qProtoList := NIL

   FOR EACH a_ IN ::aActions
      a_[ 2 ] := NIL
      a_:= NIL
   NEXT
   ::aActions     := NIL
   ::aProtos      := NIL

   ::qContextMenu := NIL
   ::qFldsStrList := NIL
   ::qFldsModel   := NIL

   FOR EACH a_ IN ::oIde:aTabs
      a_[ 2 ]:destroy()
      a_:= NIL
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:create( oIde )
   LOCAL qAct

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   ::qContextMenu := QMenu()

   aadd( ::aActions, { "GotoFunc"     , ::qContextMenu:addAction( ::oAC:getAction( "GotoFunc"      ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "TB_Cut"       , ::qContextMenu:addAction( ::oAC:getAction( "TB_Cut"        ) ) } )
   aadd( ::aActions, { "TB_Copy"      , ::qContextMenu:addAction( ::oAC:getAction( "TB_Copy"       ) ) } )
   aadd( ::aActions, { "TB_Paste"     , ::qContextMenu:addAction( ::oAC:getAction( "TB_Paste"      ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "TB_Undo"      , ::qContextMenu:addAction( ::oAC:getAction( "TB_Undo"       ) ) } )
   aadd( ::aActions, { "TB_Redo"      , ::qContextMenu:addAction( ::oAC:getAction( "TB_Redo"       ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "TB_Save"      , ::qContextMenu:addAction( ::oAC:getAction( "TB_Save"       ) ) } )
   aadd( ::aActions, { "TB_Close"     , ::qContextMenu:addAction( ::oAC:getAction( "TB_Close"      ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "TB_Compile"   , ::qContextMenu:addAction( ::oAC:getAction( "TB_Compile"    ) ) } )
   aadd( ::aActions, { "TB_CompilePPO", ::qContextMenu:addAction( ::oAC:getAction( "TB_CompilePPO" ) ) } )
   aadd( ::aActions, { ""             , ::qContextMenu:addSeparator() } )
   aadd( ::aActions, { "Apply Theme"  , ::qContextMenu:addAction( QIcon( hbide_image( "syntaxhiliter" ) ), "Apply Theme"                      ) } )
   aadd( ::aActions, { "Save as Skltn", ::qContextMenu:addAction( "Save as Skeleton..."              ) } )
   ::qContextSub := ::qContextMenu:addMenu( QIcon( hbide_image( "split" ) ), "Split" )
   //
   ::qContextSub:addAction( qAct := ::oAC:getAction( "SplitH" ) )
   aadd( ::aActions, { "Split H"      , qAct } )
   ::qContextSub:addAction( qAct := ::oAC:getAction( "SplitV" ) )
   aadd( ::aActions, { "Split V"      , qAct } )
   aadd( ::aActions, { ""             , ::qContextSub:addSeparator() } )
   ::qContextSub:addAction( qAct := ::oAC:getAction( "SplitClose" ) )
   aadd( ::aActions, { "Close Split"  , qAct } )
   aadd( ::aActions, { ""             , ::qContextSub:addSeparator() } )
   ::qContextMenu:addAction( qAct := ::oFormatDock:oWidget:toggleViewAction() )
   aadd( ::aActions, { "Format"       , qAct } )
   //
   ::qSrcControlSub := ::qContextMenu:addMenu( "Source Control - VSS" )
   aadd( ::aActions, { "Get"          , ::qSrcControlSub:addAction( "Get Latest Version" ) } )
   aadd( ::aActions, { ""             , ::qSrcControlSub:addSeparator() } )
   aadd( ::aActions, { "Checkout"     , ::qSrcControlSub:addAction( "Checkout"           ) } )
   aadd( ::aActions, { "UndoCheckout" , ::qSrcControlSub:addAction( "Undo Checkout"      ) } )
   aadd( ::aActions, { ""             , ::qSrcControlSub:addSeparator() } )
   aadd( ::aActions, { "Checkin"      , ::qSrcControlSub:addAction( "Checkin"            ) } )
   aadd( ::aActions, { ""             , ::qSrcControlSub:addSeparator() } )
   aadd( ::aActions, { "Diff"         , ::qSrcControlSub:addAction( "Diff"               ) } )


   /* Define code completer */
   ::oIde:qProtoList := QStringList()
   ::oIde:qCompModel := QStringListModel()
   ::oIde:qCompleter := QCompleter()
   //
   ::qCompleter:connect( "activated(QString)", {|p| ::execEvent( __qcompleter_activated__, p ) } )

   /* Define fields completer */
   ::qFldsStrList   := QStringList()
   ::qFldsModel     := QStringListModel()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:setStyleSheet( nMode )

   ::qContextMenu:setStyleSheet( GetStyleSheet( "QMenuPop", nMode ) )
   ::qContextSub:setStyleSheet( GetStyleSheet( "QMenuPop", nMode ) )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeEditsManager:updateFieldsList( cAlias )
   LOCAL aFlds

   IF ! empty( cAlias ) .AND. ! empty( aFlds := ::oBM:fetchFldsList( cAlias ) )

      asort( aFlds, , , {|e,f| lower( e ) < lower( f ) } )

      ::qFldsStrList:clear()
      aeval( aFlds, {|e| ::qFldsStrList:append( e ) } )
      ::qFldsModel:setStringList( ::qFldsStrList )

      ::qCompleter:setModel( ::qFldsModel )

      RETURN .t.
   ELSE
      ::qCompleter:setModel( ::qCompModel )

   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:updateCompleter()
   LOCAL aFun, aHrb, aUsr, n, s, k_

   /* Collection of prototypes can be extended to honor plugins and defined in "setup" */

   aFun := ::oFN:getFunctionPrototypes()
   aHrb := ::oHL:getFunctionPrototypes()
   aUsr := hbide_getUserPrototypes()

   ::qCompleter:disconnect( "activated(QString)" )

   ::aProtos := {}
   aeval( aHrb, {|e| aadd( ::aProtos, e ) } )
   aeval( aFun, {|e| aadd( ::aProtos, e ) } )
   aeval( aUsr, {|e| aadd( ::aProtos, e ) } )
   // Current session words
   FOR EACH s IN ::hEditingWords
      AAdd( ::aProtos, s )
   NEXT

   k_:= {}
   FOR EACH s IN ::aProtos
      // s := alltrim( s )
      IF ! ::oINI:lCompletionWithArgs
         IF ( n := at( "(", s ) ) == 0
            IF ( n := at( " ", s ) ) > 0
               s := substr( s, 1, n - 1 )
            ENDIF
         ELSE
            s := substr( s, 1, n - 1 )
         ENDIF
      ENDIF
      s := alltrim( s )
      IF ascan( k_, s ) == 0
         aadd( k_, s )
      ENDIF
   NEXT
   asort( k_, , , {|e,f| lower( e ) < lower( f ) } )

   ::qProtoList:clear()

   aeval( k_, {|e| ::qProtoList:append( e ) } )

   ::qCompModel:setStringList( ::qProtoList )
   //
   ::qCompleter:setWrapAround( .t. )
   ::qCompleter:setCaseSensitivity( Qt_CaseInsensitive )
   ::qCompleter:setModelSorting( QCompleter_CaseInsensitivelySortedModel )
   ::qCompleter:setModel( ::qCompModel )
   ::qCompleter:setCompletionMode( QCompleter_PopupCompletion )
   ::qCompleter:popup():setAlternatingRowColors( .t. )
   ::qCompleter:popup():setFont( QFont( "Courier New", 8 ) )
   ::qCompleter:popup():setMaximumWidth( 400 )
// ::qCompleter:popup():setHorizontalScrollBarPolicy ( Qt_ScrollBarAsNeeded )

   ::qCompleter:connect( "activated(QString)", {|p| ::execEvent( __qcompleter_activated__, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getProto( cWord )
   LOCAL n, nLen

   cWord := upper( cWord )
   nLen := Len( cWord )

   /* This can be rationalized */
   IF ( n := ascan( ::aProtos, {|e| upper( left( e, nLen ) ) == cWord } ) ) > 0
      RETURN ::aProtos[ n ]
   ENDIF

   RETURN ""

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
   LOCAL oParent := ::oOpenedSources

   IF Empty( cSourceFile )
      RETURN Self
   ENDIF

   hb_fNameSplit( cSourceFile, @cPath, @cFile, @cExt )

   oItem := oParent:addItem( cFile + cExt )
   oItem:tooltipText := cSourceFile
   oItem:oWidget:setIcon( 0, QIcon( ::oDK:getPanelIcon( cView ) ) )
   aadd( ::aProjData, { oItem, "Opened Source", oParent, ;
                                   cSourceFile, hbide_pathNormalized( cSourceFile ) } )

   ::oEditTree:oWidget:sortItems( 0, Qt_AscendingOrder )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:execEvent( nEvent, p )
   LOCAL oEdit

   IF ::lQuitting
      RETURN Self
   ENDIF

   SWITCH nEvent
   CASE __qFldsCompleter_activated__
      IF !empty( oEdit := ::getEditObjectCurrent() )
         oEdit:completeFieldName( p )
      ENDIF
      EXIT
   CASE __qcompleter_activated__
      IF !empty( oEdit := ::getEditObjectCurrent() )
         oEdit:completeCode( p )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme, cView, aBookMarks )

   IdeEditor():new():create( ::oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView, aBookMarks )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getTabBySource( cSource )

   cSource := hbide_pathNormalized( cSource, .t. )

   RETURN ascan( ::aTabs, {|e_| hb_FileMatch( e_[ TAB_OEDITOR ]:pathNormalized, cSource ) } )

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getTabCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget )
      qTab := ::qTabWidget:currentWidget()
      nTab := ascan( ::aTabs, {|e_| hbqt_IsEqual( e_[ TAB_OTAB ]:oWidget, qTab ) } )
   ENDIF
   RETURN nTab

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getDocumentCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqual( e_[ TAB_OTAB ]:oWidget, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]:document()
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditObjectCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqual( e_[ TAB_OTAB ]:oWidget, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]:qCoEdit
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqual( e_[ TAB_OTAB ]:oWidget, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]:qCqEdit
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorCurrent()
   LOCAL qTab, nTab

   IF !empty( ::qTabWidget ) .AND. ::qTabWidget:count() > 0
      qTab := ::qTabWidget:currentWidget()
      IF ( nTab := ascan( ::aTabs, {|e_| hbqt_IsEqual( e_[ TAB_OTAB ]:oWidget, qTab ) } ) ) > 0
         RETURN ::aTabs[ nTab, TAB_OEDITOR ]
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorByIndex( nIndex ) /* Index is 0 based */
   LOCAL qTab, a_

   IF HB_ISNUMERIC( nIndex ) .AND. nIndex >= 0 .AND. nIndex < ::qTabWidget:count()
      qTab := ::qTabWidget:widget( nIndex )
      FOR EACH a_ IN ::aTabs
         IF !empty( a_[ TAB_OTAB ] ) .AND. hbqt_IsEqual( a_[ TAB_OTAB ]:oWidget, qTab )
            RETURN ::aTabs[ a_:__enumIndex(), TAB_OEDITOR ]
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorByTabObject( oTab )
   LOCAL nPos

   IF HB_ISOBJECT( oTab )
      IF ( nPos := ascan( ::aTabs, {|e_| e_[ TAB_OTAB ] == oTab } ) ) > 0
         RETURN ::aTabs[ nPos, TAB_OEDITOR ]
      ENDIF
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorByTabPosition( nPos )

   IF HB_ISNUMERIC( nPos ) .AND. nPos > 0 .AND. nPos <= Len( ::aTabs )
      IF !empty( ::aTabs[ nPos, TAB_OEDITOR ] )
         RETURN ::aTabs[ nPos, TAB_OEDITOR ]
      ENDIF
   ENDIF
   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:getEditorBySource( cSource )
   LOCAL n

   cSource := hbide_pathNormalized( cSource, .t. )
   IF ( n := ascan( ::aTabs, {|e_| hb_FileMatch( e_[ TAB_OEDITOR ]:pathNormalized, cSource ) } ) ) > 0
      RETURN ::aTabs[ n, TAB_OEDITOR ]
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:reLoad( cSource )
   LOCAL oEdit

   IF empty( cSource )
      IF ! empty( oEdit := ::getEditObjectCurrent() )
         oEdit:reload()
      ENDIF
   ELSE
      IF hb_fileExists( cSource ) .AND. hbide_isValidText( cSource )
         IF !empty( oEdit := ::getEditorBySource( cSource ) )
            oEdit:qEdit:clear()
            oEdit:qEdit:setPlainText( hb_memoread( hbide_pathToOSPath( cSource ) ) )
         ENDIF
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

METHOD IdeEditsManager:qscintilla()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditorCurrent() )
      oEdit:qscintilla()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:undo()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:undo()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:redo()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:redo()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:cut()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:cut()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:copy()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:copy()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:paste()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:paste()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:selectAll()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:selectAll()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleSelectionMode()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleSelectionMode()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleStreamSelectionMode()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleStreamSelectionMode()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleColumnSelectionMode()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleColumnSelectionMode()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleLineSelectionMode()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleLineSelectionMode()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleCurrentLineHighlightMode()
   LOCAL oEdit
   ::oIde:lCurrentLineHighlightEnabled := ! ::lCurrentLineHighlightEnabled
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleCurrentLineHighlightMode()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleLineNumbers()
   LOCAL oEdit
   ::oIde:lLineNumbersVisible := ! ::lLineNumbersVisible
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleLineNumbers()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleHorzRuler()
   LOCAL oEdit
   ::oIde:lHorzRulerVisible := ! ::lHorzRulerVisible
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleHorzRuler()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleCodeCompetion()
   LOCAL oEdit
   ::oIde:lHorzRulerVisible := ! ::lHorzRulerVisible
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleCodeCompetion()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:toggleCompetionTips()
   LOCAL oEdit
   ::oIde:lHorzRulerVisible := ! ::lHorzRulerVisible
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:toggleCompetionTips()
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

METHOD IdeEditsManager:alignAt()
   LOCAL oEdit, cWord
   IF !empty( oEdit := ::getEditObjectCurrent() )
      IF oEdit:aSelectionInfo[ 5 ] == __selectionMode_column__
         IF ! Empty( cWord := hbide_fetchAString( ::oDlg:oWidget, "", "Align At ?", "Selected-Text Alignment Proto" ) )
            oEdit:alignAt( cWord )
         ENDIF
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:stringify()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:stringify()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:switchToReadOnly()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:setReadOnly()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:presentSkeletons()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:presentSkeletons()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:clearSelection()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:clearSelection()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:gotoFunction()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:gotoFunction()
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

METHOD IdeEditsManager:insertSeparator( cSep )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:insertSeparator( cSep )
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
            MsgBox( "File type unknown or unsupported: " + cFile )
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

METHOD IdeEditsManager:upperCaseKeywords()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:upperCaseKeywords()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:formatBraces()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:formatBraces()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:removeTabs()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:tabs2spaces()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:spaces2tabs()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:spaces2tabs()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:removeTrailingSpaces()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:removeTrailingSpaces()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:zoom( nKey )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:zoom( nKey )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:printPreview()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:printPreview()
   ENDIF
   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeEditsManager:paintRequested( qPrinter )
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

METHOD IdeEditsManager:setTooltipMark( nIndex )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:setTooltipMark( nIndex )
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

METHOD IdeEditsManager:goto( nLine )
   LOCAL oEdit
   IF ! empty( oEdit := ::oEM:getEditObjectCurrent() )
      oEdit:goto( nLine )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//                            Navigation
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:home()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:home()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:end()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:end()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:down()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:down()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:up()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:up()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:goBottom()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:goBottom()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:goTop()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:goTop()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:left()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:left()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:right()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:right()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:panEnd()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:panEnd()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:panHome()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:panHome()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:pageUp()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:pageUp()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:pageDown()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      oEdit:pageDown()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:showThumbnail()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditorCurrent() )
      oEdit:showThumbnail()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:changeThumbnail()
   LOCAL oEdit
   IF !empty( oEdit := ::getEditorCurrent() )
      oEdit:changeThumbnail()
   ENDIF
   RETURN Self
/*----------------------------------------------------------------------*/
//                            Locating
/*----------------------------------------------------------------------*/
METHOD IdeEditsManager:find( cString, nPosFrom )
   LOCAL oEdit
   IF !empty( oEdit := ::getEditObjectCurrent() )
      IF empty( cString )
         ::oFR:show()
      ELSE
         oEdit:find( cString, nPosFrom )
      ENDIF
   ENDIF
   RETURN Self
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
   DATA   lInitLoad                               INIT   .t.

   DATA   qThumbnail
   DATA   qTNFont
   DATA   qTNHiliter
   DATA   qHSpltr
   DATA   qVSpltr

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

   DATA   qEvents
   DATA   lReadOnly                               INIT  .F.

   DATA   cEol                                    INIT  ""
   DATA   nSplOrient                              INIT  -1
   DATA   qSplitter

   DATA   lIsPRG                                  INIT .t.

   METHOD new( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView )
   METHOD create( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView, aBookMarks )
   METHOD split( nOrient, oEditP )
   METHOD relay( oEdit )
   METHOD destroy()
   METHOD execEvent( nEvent, p, p1, p2 )
   METHOD setDocumentProperties()
   METHOD activateTab( mp1, mp2, oXbp )
   METHOD buildTabPage( cSource )
   METHOD dispEditInfo( qEdit )
   METHOD setTabImage( qEdit )
   METHOD applyTheme( cTheme )
   METHOD showThumbnail()
   METHOD changeThumbnail()
   METHOD scrollThumbnail()
   METHOD qscintilla()
   METHOD prepareBufferToLoad( cBuffer )
   METHOD prepareBufferToSave( cBuffer )
   METHOD reload()
   METHOD vssExecute( cAction )
   METHOD updateComponents()

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

METHOD IdeEditor:create( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme, cView, aBookMarks )
   LOCAL cFileTemp, nAttr

   DEFAULT oIde        TO ::oIde
   DEFAULT cSourceFile TO ::sourceFile
   DEFAULT nPos        TO ::nPos
   DEFAULT nHPos       TO ::nHPos
   DEFAULT nVPos       TO ::nVPos
   DEFAULT cTheme      TO ::cTheme
   DEFAULT cView       TO ::cView
   DEFAULT aBookMarks  TO {}

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
   IF hb_fGetAttr( cSourceFile, @nAttr )
      ::lReadOnly := hb_bitAnd( nAttr, FC_READONLY ) == FC_READONLY
   ENDIF

   ::cType := upper( strtran( ::cExt, ".", "" ) )
   ::cType := iif( ::cType $ "PRG,C,CPP,H,CH,PPO,HBS", ::cType, "U" )

   ::lIsPRG := ::cType $ "PRG,HB"

   ::buildTabPage( ::sourceFile )

   ::qLayout := QBoxLayout(  Qt_Vertical )
   ::qLayout:setContentsMargins( 0,0,0,0 )
   //
   ::oTab:oWidget:setLayout( ::qLayout )

   ::oEdit   := IdeEdit():new( ::oIde, Self, 0 )
   ::oEdit:aBookMarks := aBookMarks
   ::oEdit:create()
   ::qEdit   := ::oEdit:qEdit
   ::qCqEdit := ::oEdit:qEdit
   ::qCoEdit := ::oEdit

   ::qLayout:addWidget( ::oEdit:qEdit )

   ::oEdit:qEdit:connect( "updateRequest(QRect,int)", {|| ::scrollThumbnail() } )

   ::qDocument  := ::qEdit:document()
   ::qCursor := ::qEdit:textCursor()

   /* Populate Tabs Array */
   aadd( ::aTabs, { ::oTab, Self } )

   /* Populate right at creation */
   ::oEM:addSourceInTree( ::sourceFile, ::cView )

   //::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget", ::nAnimantionMode ) )
   IF ::lReadOnly
      ::oEdit:setReadOnly( .t. )
      ::qEdit:setTextInteractionFlags( Qt_TextSelectableByMouse + Qt_TextSelectableByKeyboard )
   ENDIF

   ::qDocument:connect( "modificationChanged(bool)"  , {|p      | ::execEvent( __qDocModificationChanged__, p         ) } )
   ::qDocument:connect( "contentsChange(int,int,int)", {|p,p1,p2| ::execEvent( __qDocContentsChange__     , p, p1, p2 ) } )

   ::qDocument:setModified( .f. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:destroy()
   LOCAL n, oEdit

   HB_TRACE( HB_TR_DEBUG, "..........................................................IdeEditor:destroy()", 0 )

   ::oEdit:qEdit:disconnect( "updateRequest(QRect,int)" )

   IF !empty( ::qTimerSave )
      ::qTimerSave:disconnect( "timeout()" )
      ::qTimerSave:stop()
      ::qTimerSave := NIL
   ENDIF
   /* This code is reached under normal circumstances, so delete auto saved file */
   ferase( hbide_pathToOSPath( ::cPath + ::cFile + ::cExt + ".tmp" ) )

   ::qCqEdit  := NIL
   ::qCoEdit  := NIL
   ::qEdit    := NIL

   DO WHILE Len( ::aEdits ) > 0
      oEdit := ::aEdits[ 1 ]
      hb_adel( ::aEdits, 1, .t. )
      oEdit:destroy()
   ENDDO
   ::oEdit:destroy()

   IF !Empty( ::qDocument )
      ::qDocument := NIL
   ENDIF

   IF !Empty( ::qHiliter )
      ::qHiliter := NIL
   ENDIF

   ::qSplitter := NIL
   ::oEdit := NIL
   ::qLayout := NIL

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
   HB_TRACE( HB_TR_DEBUG, "................................................................IdeEditor:destroy()", 1 )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:relay( oEdit )
   LOCAL oEdt

   IF Len( ::aEdits ) == 0
      IF ::nSplOrient > -1
         ::nSplOrient := -1
         ::qLayout:removeWidget( ::qSplitter )
         ::qLayout:addWidget( ::oEdit:qEdit )
      ENDIF
   ENDIF

   IF HB_ISOBJECT( oEdit )
      aadd( ::aEdits, oEdit )
   ENDIF

   IF ::nSplOrient == -1
      ::nSplOrient := oEdit:nOrient

      IF oEdit:nOrient == 1
         ::qSplitter := QSplitter( Qt_Horizontal )
      ELSE
         ::qSplitter := QSplitter( Qt_Vertical )
      ENDIF

      ::qLayout:removeWidget( ::oEdit:qEdit )
      ::qLayout:addWidget( ::qSplitter )

      ::qSplitter:addWidget( ::oEdit:qEdit )
   ENDIF

   FOR EACH oEdt IN ::aEdits
      ::qSplitter:addWidget( oEdt:qEdit )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:split( nOrient, oEditP )
   LOCAL oEdit

   HB_SYMBOL_UNUSED( oEditP  )

   oEdit := IdeEdit():new( ::oIde, Self, 1 ):create()
   oEdit:qEdit:setDocument( ::qDocument )
   oEdit:nOrient := nOrient

   ::relay( oEdit )

   IF HB_ISOBJECT( ::qHiliter )
      oEdit:qEdit:hbSetHighLighter( ::qHiliter )
      oEdit:qEdit:hbHighlightPage()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:prepareBufferToSave( cBuffer )
   LOCAL cE, cEOL, a_, s

   cE := ::oSetup:eol()

   // here we can extercise user settings via Setup
   //
   cEOL := iif( ::cEOL == "", cE, ::cEOL )
   IF !( cEOL == cE )
      // MsgBox( "Difference in set EOL and current file EOL mode, saving with original mode!" )
   ENDIF

   cBuffer := strtran( cBuffer, chr( 13 ) )
   IF !( cEOL == chr( 10 ) )
      cBuffer := strtran( cBuffer, chr( 10 ), cEOL )
   ENDIF
   IF ::oINI:lTrimTrailingBlanks
      a_:= hb_atokens( cBuffer, cEOL )
      FOR EACH s IN a_
         s := trim( s )
      NEXT
      cBuffer := ""
      aeval( a_, {|e| cBuffer += e + cEOL } )
      cBuffer := substr( cBuffer, 1, Len( cBuffer ) - len( cEOL ) )
   ENDIF

   RETURN cBuffer

/*----------------------------------------------------------------------*/

METHOD IdeEditor:prepareBufferToLoad( cBuffer )
   LOCAL cSpaces

   ::cEOL := hbide_getEol( @cBuffer )

   IF ::oINI:lConvTabToSpcWhenLoading
      cSpaces := space( ::nTabSpaces )
      cBuffer := strtran( cBuffer, chr( 9 ), cSpaces )
   ENDIF

   RETURN cBuffer

/*----------------------------------------------------------------------*/

METHOD IdeEditor:vssExecute( cAction )
   LOCAL cPath, cFile, cExt, cCmd, cC, oProcess, cBatch, cOutput := ""
   LOCAL aCmd := {}

   IF ! empty( ::oINI:cVSSExe ) .AND. ! empty( ::oINI:cVSSDatabase )
      hb_fNameSplit( ::sourceFile, @cPath, @cFile, @cExt )

      aadd( aCmd, "SET ssdir=" + hbide_pathToOSPath( ::oINI:cVSSDatabase ) )
      aadd( aCmd, "SET Force_dir=YES" )
      IF cAction == "Checkin"
         aadd( aCmd, "call " + '"' + ::oINI:cVSSExe + '/ss.exe' + '" ' + cAction + " " + cFile + cExt + " -ChbIDE" )
      ELSEIF cAction == "Checkout"
         aadd( aCmd, "call " + '"' + ::oINI:cVSSExe + '/ss.exe' + '" ' + cAction + " " + cFile + cExt + " -C-" )
      ELSE
         aadd( aCmd, "call " + '"' + ::oINI:cVSSExe + '/ss.exe' + '" ' + cAction + " " + cFile + cExt )
      ENDIF

      cBatch := hbide_getShellCommandsTempFile( aCmd )

      cCmd   := hbide_getShellCommand()
      cC     := iif( hbide_getOS() == "nix", "", "/C " )

      oProcess := HbpProcess():new()
      //
      oProcess:output      := {|cOut| cOutput += cOut }
      oProcess:finished    := {|| iif( !empty( cOutput ), ::reload(), NIL ), MsgBox( cOutput ) }
      oProcess:workingPath := hbide_pathToOSPath( cPath )

      oProcess:addArg( cC + cBatch )
      oProcess:start( cCmd )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:reload()
   LOCAL nAttr, nPos, qCursor, nHPos, nVPos

   qCursor := ::qEdit:textCursor()
   nPos    := qCursor:position()
   nHPos   := ::qEdit:horizontalScrollBar():value()
   nVPos   := ::qEdit:verticalScrollBar():value()


   IF hb_fGetAttr( ::sourceFile, @nAttr )
      ::lReadOnly := hb_bitAnd( nAttr, FC_READONLY ) == FC_READONLY
   ENDIF

   ::oEdit:setReadOnly( ::lReadOnly )
   ::setTabImage()

   ::qEdit:clear()
   ::qEdit:setPlainText( ::prepareBufferToLoad( hb_memoread( ::sourceFile ) ) )

   qCursor:setPosition( nPos )
   ::qEdit:setTextCursor( qCursor )
   ::qEdit:horizontalScrollBar():setValue( nHPos )
   ::qEdit:verticalScrollBar():setValue( nVPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:setDocumentProperties()
   LOCAL qCursor

   qCursor := ::qEdit:textCursor()

   IF !( ::lLoaded )       /* First Time */
      ::qEdit:setPlainText( ::prepareBufferToLoad( hb_memoread( ::sourceFile ) ) )

      IF !( ::cType == "U" )
         ::qHiliter := ::oTH:setSyntaxHilighting( ::qEdit, @::cTheme )
      ENDIF

      qCursor:setPosition( ::nPos )
      ::qEdit:setTextCursor( qCursor )

      ::qEdit:horizontalScrollBar():setValue( ::nHPos )
      ::qEdit:verticalScrollBar():setValue( ::nVPos )

      ::qEdit:document():setModified( .f. )
//    ::qEdit:document():setMetaInformation( QTextDocument_DocumentTitle, hbide_pathNormalized( ::sourceFile ) )
      ::qEdit:document():setMetaInformation( QTextDocument_DocumentTitle, hb_FNameName( ::sourceFile ) )

      ::lLoaded := .T.

      IF HB_ISOBJECT( ::qHiliter )
         ::qHiliter:hbSetInitialized( .t. )
         ::qEdit:hbHighlightPage()
      ENDIF

      IF ::cType $ "PRG,C,CPP,H,CH,HBS"
         ::qTimerSave := QTimer()
         ::qTimerSave:setInterval( max( 30000, ::oINI:nTmpBkpPrd * 1000 ) )
         ::qTimerSave:connect( "timeout()", {|| ::execEvent( __qTimeSave_timeout__ ) } )
         ::qTimerSave:start()
      ENDIF
      ::oUpDn:show()
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

METHOD IdeEditor:execEvent( nEvent, p, p1, p2 )
   LOCAL cFileTemp

   IF ::lQuitting
      RETURN Self
   ENDIF

   SWITCH nEvent

   CASE __qTabWidget_tabCloseRequested__
      ::oSM:closeSource( p + 1 )
      EXIT

   CASE __qDocModificationChanged__
      ::setTabImage()
      EXIT

   CASE __qDocContentsChange__
      IF p1 + p2 > 0
         ::qCoEdit:reformatLine( p, p1, p2 )
      ENDIF
      EXIT

   CASE __qTimeSave_timeout__
      IF ::qDocument:isModified()
         cFileTemp := hbide_pathToOSPath( ::cPath + ::cFile + ::cExt + ".tmp" )
         hb_memowrit( cFileTemp, ::qEdit:toPlainText() )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:updateComponents()
   LOCAL qCoEdit := ::qCoEdit

   ::setDocumentProperties()
   qCoEdit:relayMarkButtons()
   qCoEdit:updateTitleBar()
   qCoEdit:toggleLineNumbers()
   qCoEdit:toggleHorzRuler()
   qCoEdit:toggleCurrentLineHighlightMode()
   qCoEdit:dispStatusInfo()
   ::oUpDn:show()
   ::oDK:showSelectedTextToolbar()
   ::changeThumbnail()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:activateTab( mp1, mp2, oXbp )
   LOCAL oEdit

   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   IF !empty( oEdit := ::oEM:getEditorByTabObject( oXbp ) )
      oEdit:updateComponents()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:buildTabPage( cSource )

   ::oTab := XbpTabPage():new( ::oTabParent, , { 5,5 }, { 700,400 }, , .t. )

   IF Empty( cSource )
      ::oTab:caption := "Untitled " + hb_ntos( hbide_getNextUntitled() )
   ELSE
      ::oTab:caption := ::cFile + iif( ::oINI:lTabRemoveExt, "", ::cExt )
   ENDIF
   ::oTab:minimized := .F.

   ::oTab:create()

   IF ::oINI:lTabAddClose
      ::qTabWidget:setTabsClosable( .T. )
      ::qTabWidget:connect( "tabCloseRequested(int)", {|i|  ::execEvent( __qTabWidget_tabCloseRequested__, i ) } )
   ENDIF
   ::qTabWidget:setTabTooltip( ::qTabWidget:indexOf( ::oTab:oWidget ), cSource )
   ::oTab:tabActivate := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:dispEditInfo( qEdit )
   LOCAL s, qDocument, qCursor

   DEFAULT qEdit TO ::qEdit

   qDocument := qEdit:document()
   qCursor   := qEdit:textCursor()

   s := "<b>Line "+ hb_ntos( qCursor:blockNumber() + 1 ) + " of " + ;
                    hb_ntos( qDocument:blockCount() ) + "</b>"

   ::oIde:oSBar:getItem( SB_PNL_MAIN     ):caption := "Success"
   ::oIde:oSBar:getItem( SB_PNL_READY    ):caption := "Ready"
   ::oIde:oSBar:getItem( SB_PNL_LINE     ):caption := s
   ::oIde:oSBar:getItem( SB_PNL_COLUMN   ):caption := "Col " + hb_ntos( qCursor:columnNumber() + 1 )
   ::oIde:oSBar:getItem( SB_PNL_INS      ):caption := iif( qEdit:overwriteMode() , " ", "Ins" )
   ::oIde:oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( qDocument:isModified(), "Modified", iif( qEdit:isReadOnly(), "ReadOnly", " " ) )
   ::oIde:oSBar:getItem( SB_PNL_EDIT     ):caption := "Edit"

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:setTabImage( qEdit )
   LOCAL nIndex, lModified, lReadOnly, cIcon

   DEFAULT qEdit TO ::oEdit:qEdit

   nIndex    := ::qTabWidget:indexOf( ::oTab:oWidget )
   lModified := ::qDocument:isModified()
   lReadOnly := iif( ::lReadOnly, ::lReadOnly, qEdit:isReadOnly() )

   IF lReadOnly
      cIcon := "tabreadonly"
   ELSE
      IF lModified
         cIcon := "tabmodified"
      ELSE
         cIcon := "tabunmodified"
      ENDIF
   ENDIF

   ::qTabWidget:setTabIcon( nIndex, QIcon( hbide_image( cIcon ) ) )
   ::oDK:setStatusText( SB_PNL_MODIFIED, iif( lModified, "Modified", iif( lReadOnly, "ReadOnly", " " ) ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:applyTheme( cTheme )

   IF !( ::cType == "U" )
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

METHOD IdeEditor:showThumbnail()

   IF empty( ::qThumbnail )
      ::qThumbnail := IdeEdit():new( ::oIde, Self, 0 ):create()
      ::qThumbnail:currentPointSize := 4
      ::qThumbnail:fontFamily := "Courier New"
      ::qThumbnail:setFont()
      ::qThumbnail:setReadOnly( .t. )
      IF !( ::cType == "U" )
         ::qTNHiliter := ::oTH:SetSyntaxHilighting( ::qThumbnail:qEdit, @::cTheme )
      ENDIF
      ::qThumbnail:qEdit:setTextInteractionFlags( Qt_TextSelectableByMouse + Qt_TextSelectableByKeyboard )
   ENDIF

   ::oSourceThumbnailDock:oWidget:setWidget( ::qThumbnail:qEdit )
   ::qThumbnail:qEdit:clear()
   ::qThumbnail:qEdit:setPlainText( hb_memoRead( ::sourceFile ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:changeThumbnail()

   IF ::lLoaded .AND. ::oSourceThumbnailDock:oWidget:isVisible()
      ::showThumbnail()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:scrollThumbnail()
   LOCAL qScroll

   IF ::lLoaded .AND. ::oSourceThumbnailDock:oWidget:isVisible() .AND. !empty( ::qThumbnail )
      qScroll := ::oEdit:qEdit:verticalScrollBar()
      ::qThumbnail:qEdit:verticalScrollBar():setValue( qScroll:value() )

      ::oEdit:qEdit:hbGetViewportInfo()

      ::qThumbnail:qEdit:hbHighlightArea( ::oEdit:aViewportInfo[ 1 ], 0, ::oEdit:aViewportInfo[ 1 ]+::oEdit:aViewportInfo[ 3 ]-1, 0, 1 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//                            Generic Tests
/*----------------------------------------------------------------------*/

METHOD IdeEditor:qscintilla()

   //  hbide_qtDesigner()

   #ifdef HB_WITH_QSCINTILLA
   #include "hbqscintilla.ch"

   #if 0
   SCE_FS_DEFAULT
   SCE_FS_COMMENT
   SCE_FS_COMMENTLINE
   SCE_FS_COMMENTDOC
   SCE_FS_COMMENTLINEDOC
   SCE_FS_COMMENTDOCKEYWORD
   SCE_FS_COMMENTDOCKEYWORDERROR
   SCE_FS_KEYWORD
   SCE_FS_KEYWORD2
   SCE_FS_KEYWORD3
   SCE_FS_KEYWORD4
   SCE_FS_NUMBER
   SCE_FS_STRING
   SCE_FS_PREPROCESSOR
   SCE_FS_OPERATOR
   SCE_FS_IDENTIFIER
   SCE_FS_DATE
   SCE_FS_STRINGEOL
   SCE_FS_CONSTANT
   SCE_FS_WORDOPERATOR
   SCE_FS_DISABLEDCODE
   SCE_FS_DEFAULT_C
   SCE_FS_COMMENTDOC_C
   SCE_FS_COMMENTLINEDOC_C
   SCE_FS_KEYWORD_C
   SCE_FS_KEYWORD2_C
   SCE_FS_NUMBER_C
   SCE_FS_STRING_C
   SCE_FS_PREPROCESSOR_C
   SCE_FS_OPERATOR_C
   SCE_FS_IDENTIFIER_C
   SCE_FS_STRINGEOL_C
   SCE_FS_BRACE
   #endif

   STATIC oSci, qLexer, qAPIs, fontBold, fontNormal, fontItalic, c1, c2, c3

   IF empty( oSci )
      oSci := HBQsciScintilla()
      //
      oSci:setAutoIndent( .t. )
      oSci:setCaretLineVisible( .t. )
      oSci:setCaretWidth( 2 )
      oSci:setFolding( QsciScintilla_BoxedTreeFoldStyle )
      oSci:setTabWidth( 3 )
      oSci:setMarginLineNumbers( 0,.t. )
      oSci:setMarginWidth( 0,30 )
      oSci:setSelectionBackgroundColor( QColor( 255,0,0 ) )
      oSci:setEdgeColumn( 40 )
      oSci:setCallTipsVisible( 3 )
      oSci:setFont( ::oFont:oWidget )
      oSci:setEdgeColor( QColor( 0,0,255 ) )
      oSci:setMarginsFont( ::oFont:oWidget )
      oSci:setIndentationGuides( .t. )

      oSci:setCallTipsHighlightColor( QColor( 255,127,0 ) )

      /* Auto Completion */
      oSci:setAutoCompletionSource( QsciScintilla_AcsAll )
      oSci:setAutoCompletionThreshold( 3 )
      oSci:setAutoCompletionCaseSensitivity( .t. )
      oSci:setAutoCompletionShowSingle( .t. )
      oSci:setAutoCompletionFillupsEnabled( .t. )

      HB_TRACE( HB_TR_DEBUG, time() )
      oSci:setText( hb_memoread( "c:\harbour\contrib\hbide\idemisc.prg" ) )
   // oSci:setText( hb_memoread( "c:\harbour\contrib\hbide\ideparseexpr.c" ) )
      HB_TRACE( HB_TR_DEBUG, time(), "after" )

      c1 := QColor( 0,0,255 )
      c2 := QColor( 0,12,133 )
      c3 := QColor( 20,122,144 )
      oSci:setBraceMatching( QsciScintilla_StrictBraceMatch )
      oSci:setMatchedBraceForegroundColor( c1 )
      oSci:setMatchedBraceBackgroundColor( c2 )
      oSci:setUnmatchedBraceForegroundColor( c3 )

      qLexer := QsciLexerFlagship()
      //qLexer := QsciLexerCPP()
      qLexer:setDefaultFont( ::oFont:oWidget )
      qLexer:setFoldAtElse( .f. )

      fontBold := QFont()
      fontBold:setFamily( "Courier New" )
      fontBold:setPointSize(10)
      fontBold:setWeight(100)

      fontNormal := QFont()
      fontNormal:setFamily( "Courier New" )
      fontNormal:setPointSize(10)

      fontItalic := QFont()
      fontItalic:setFamily( "Courier New" )
      fontItalic:setPointSize( 10 )
      fontItalic:setItalic( .t. )

      qLexer:setFont( fontBold, SCE_FS_COMMENTLINEDOC )
      qLexer:setFont( fontBold, SCE_FS_COMMENTDOCKEYWORD )
      qLexer:setFont( fontBold, SCE_FS_NUMBER )

      qLexer:setColor( QColor( 255, 127,  67 ), SCE_FS_KEYWORD  )
      qLexer:setColor( QColor( 255,   0, 127 ), SCE_FS_KEYWORD2 )
      qLexer:setColor( QColor( 127,  67, 255 ), SCE_FS_OPERATOR )
      qLexer:setColor( QColor( 255,   0,   0 ), SCE_FS_BRACE    )

      qApis := QsciAPIs( qLexer )
      qApis:load( "c:/temp/cpp.api" )
      qApis:prepare()
      qLexer:setAPIs( qApis )

      oSci:setLexer( qLexer )
      qLexer:setEditor( oSci )

   ENDIF
   ::oQScintillaDock:oWidget:setWidget( oSci )
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/

#if 0
STATIC FUNCTION hbide_qtDesigner()

   #ifdef __DESIGNER__
   LOCAL n
   STATIC oEdt, oWM

   oEdt := QDesignerFormEditorInterface( ::oDlg:oWidget )
   HB_TRACE( HB_TR_DEBUG, 1 )
   oWM := oEdt:formWindowManager()
   HB_TRACE( HB_TR_DEBUG, 2 )
   oWM:createFormWindow( ::oQScintillaDock:oWidget )
   HB_TRACE( HB_TR_DEBUG, 3 )
   ::oQScintillaDock:oWidget:show()
   HB_TRACE( HB_TR_DEBUG, 4 )
   n := oWM:formWindowCount()
   HB_TRACE( HB_TR_DEBUG, n )

   /*
   QDesignerActionEditorInterface * actionEditor () const
   QDesignerFormWindowManagerInterface * formWindowManager () const
   QDesignerObjectInspectorInterface * objectInspector () const
   QDesignerPropertyEditorInterface * propertyEditor () const
   void setActionEditor ( QDesignerActionEditorInterface * actionEditor )
   void setObjectInspector ( QDesignerObjectInspectorInterface * objectInspector )
   void setPropertyEditor ( QDesignerPropertyEditorInterface * propertyEditor )
   void setWidgetBox ( QDesignerWidgetBoxInterface * widgetBox )
   QWidget * topLevel () const
   QDesignerWidgetBoxInterface * widgetBox () const
   */

   #endif
   RETURN NIL
#endif

/*----------------------------------------------------------------------*/

