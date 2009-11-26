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
 *                               17Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"
#include "appevent.ch"
#include "inkey.ch"
#include "gra.ch"
#include "set.ch"
#include "hbclass.ch"

#ifdef __HARBOUR__
   #define UNU( x ) HB_SYMBOL_UNUSED( x )
#else
   #define UNU( x ) ( x := x )
#endif

/*----------------------------------------------------------------------*/

#define CRLF    chr( 13 )+chr( 10 )

STATIC s_resPath

/*----------------------------------------------------------------------*/

PROCEDURE Main( cProjectOrSource )
   LOCAL oIde

   s_resPath := hb_DirBase() + "resources" + hb_OsPathSeparator()

   oIde := HbIde():new( cProjectOrSource )
   oIde:create()
   oIde:destroy()

   RETURN

/*----------------------------------------------------------------------*/

CLASS HbIde

   DATA   mp1, mp2, oXbp, nEvent
   DATA   aTabs                                   INIT {}
   DATA   cProjFile

   DATA   oCurTab                                 INIT NIL
   DATA   nCurTab                                 INIT 0
   DATA   nPrevTab                                INIT 0

   /* HBQT Objects */
   DATA   qLeftArea
   DATA   qMidArea
   DATA   qRightArea
   DATA   qLeftLayout
   DATA   qMidLayout
   DATA   qRightLayout
   DATA   qLayout
   DATA   qSplitter
   DATA   qSplitterL
   DATA   qSplitterR
   DATA   qTabWidget
   DATA   qFindDlg

   ACCESS qCurEdit                                INLINE iif( ::getCurrentTab() > 0, ::aTabs[ ::getCurrentTab(), 2 ], NIL )

   /* XBP Objects */
   DATA   oDlg
   DATA   oDa
   DATA   oSBar
   DATA   oMenu
   DATA   oTBar
   DATA   oFont
   DATA   oProjTree
   DATA   oDockR
   DATA   oDockB
   DATA   oDockB1
   DATA   oDockB2
   DATA   oFuncList
   DATA   oOutputResult
   DATA   oCompileResult
   DATA   oLinkResult
   DATA   oNewDlg
   DATA   oTabWidget
   DATA   oPBFind, oPBRepl, oPBClose, oFind, oRepl

   DATA   oCurProjItem

   DATA   oProjRoot
   DATA   oExes
   DATA   oLibs
   DATA   oDlls
   DATA   aProjData                               INIT {}

   DATA   lDockRVisible                           INIT .f.
   DATA   lDockBVisible                           INIT .f.
   DATA   lTabCloseRequested                      INIT .f.

   METHOD new( cProjectOrSource )
   METHOD create( cProjectOrSource )
   METHOD destroy()

   METHOD buildDialog()
   METHOD buildStatusBar()
   METHOD executeAction()
   METHOD buildTabPage()
   METHOD buildProjectTree()

   METHOD manageFuncContext()
   METHOD manageProjectContext()

   METHOD buildFuncList()
   METHOD buildBottomArea()
   METHOD buildCompileResults()
   METHOD buildLinkResults()
   METHOD buildOutputResults()

   METHOD editSource()
   METHOD selectSource()
   METHOD closeSource()
   METHOD closeAllSources()
   METHOD saveSource()

   METHOD updateFuncList()
   METHOD gotoFunction()
   METHOD fetchProjectProperties()
   METHOD closeTab()
   METHOD activateTab()
   METHOD getCurrentTab()
   METHOD getYesNo()

   DATA   aTags                                   INIT {}
   DATA   aText                                   INIT {}
   DATA   aSources                                INIT {}
   DATA   aFuncList                               INIT {}
   DATA   aLines                                  INIT {}
   DATA   aComments                               INIT {}

   METHOD createTags()
   METHOD findReplace()
   METHOD manageFocusInEditor()
   METHOD convertSelection()
   METHOD printPreview()
   METHOD paintRequested()
   METHOD setTabImage()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbIde:destroy()

   ::oSBar := NIL
   ::oMenu := NIL
   ::oTBar := NIL

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:new( cProjectOrSource )

   ::cProjFile := cProjectOrSource

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:create( cProjectOrSource )
   // LOCAL qWidget

   IF hb_isChar( cProjectOrSource )
      ::cProjFile := cProjectOrSource
   ENDIF

   ::BuildDialog()
   ::oDa := ::oDlg:drawingArea
   SetAppWindow( ::oDlg )
   ::oDlg:Show()

   ::oDa:oTabWidget := XbpTabWidget():new():create( ::oDa, , {0,0}, {10,10}, , .t. )
//   ::oDa:oTabWidget:oWidget:setTabsClosable( .t. )
   ::oDa:oTabWidget:oWidget:setUsesScrollButtons( .f. )
   ::oTabWidget := ::oDa:oTabWidget
   ::qTabWidget := ::oDa:oTabWidget:oWidget

   ::buildProjectTree()
   ::buildFuncList()
   ::buildBottomArea()

#if 0
   ::qLeftLayout := QGridLayout():new()
   ::qLeftLayout:setContentsMargins( 0,0,0,0 )
   ::qLeftLayout:setHorizontalSpacing( 0 )
   ::qLeftLayout:setVerticalSpacing( 0 )

   ::qMidLayout  := QGridLayout():new()
   ::qMidLayout:setContentsMargins( 0,0,0,0 )
   ::qMidLayout:setHorizontalSpacing( 0 )
   ::qMidLayout:setVerticalSpacing( 0 )

   ::qLeftArea   := QWidget():new()
   ::qLeftArea:setLayout( QT_PTROF( ::qLeftLayout ) )
//   qWidget:oWidget:setLayout( QT_PTROF( ::qLeftLayout ) )
   ::qMidArea    := QWidget():new()
   ::qMidArea:setLayout( QT_PTROF( ::qMidLayout ) )

   ::qLeftLayout:addWidget_1( QT_PTROFXBP( ::oProjTree ), 0, 0, 1, 1 )
   ::qLeftLayout:addWidget_1( QT_PTROFXBP( qWidget ), 1, 0, 1, 1 )

   ::qMidLayout:addWidget_1( QT_PTROFXBP( ::oDa:oTabWidget ), 0, 0, 1, 1 )

   ::qSplitter := QSplitter():new( QT_PTROF( ::oDa:oWidget ) )

   ::qSplitter:addWidget( QT_PTROF( ::qLeftArea   ) )
   ::qSplitter:addWidget( QT_PTROF( ::qMidArea    ) )

   ::qSplitter:show()

#else
   ::qLayout := QGridLayout():new()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setHorizontalSpacing( 0 )
   ::qLayout:setVerticalSpacing( 0 )

   ::oDa:oWidget:setLayout( QT_PTROF( ::qLayout ) )

   ::qSplitter := QSplitter():new( QT_PTROF( ::oDa:oWidget ) )

   ::qLayout:addWidget_1( QT_PTROF( ::qSplitter ), 0, 0, 1, 1 )

   ::qSplitter:addWidget( QT_PTROFXBP( ::oProjTree      ) )
   ::qSplitter:addWidget( QT_PTROFXBP( ::oDa:oTabWidget ) )

   ::qSplitter:show()
#endif

   ::oDlg:setPos( { 100, 60 } )

   /* Editor's Font */
   ::oFont := XbpFont():new()
   ::oFont:fixed := .t.
   ::oFont:create( "10.Courier" )

   buildMainMenu( ::oDlg, Self )
   ::oTBar := buildToolBar( ::oDlg, Self )
   ::buildStatusBar()

   ::editSource( ::cProjFile )

   ::oDlg:Show()

   /* Enter Xbase++ Event Loop - working */
   DO WHILE .t.
      ::nEvent := AppEvent( @::mp1, @::mp2, @::oXbp )

      IF ::nEvent == xbeP_Quit
         HBXBP_DEBUG( "xbeP_Quit" )
         EXIT
      ENDIF

      // HBXBP_DEBUG( ::nEvent, ::mp1, ::mp2 )

      IF ::nEvent == xbeP_Close
         HBXBP_DEBUG( "xbeP_Close" )
         ::closeAllSources()
         EXIT

      ELSEIF ::nEvent == xbeP_Keyboard
         DO CASE

         CASE ::mp1 == xbeK_ESC
            ::closeSource()
            IF ::qTabWidget:count() == 0
               ::oDockR:hide()
               ::lDockRVisible := .f.
            ENDIF

         CASE ::mp1 == xbeK_CTRL_S
            ::saveSource( ::getCurrentTab(), .f. )

         CASE ::mp1 == xbeK_CTRL_F
            IF !empty( ::qCurEdit )
               ::findReplace( "finddialog" )
            ENDIF

         ENDCASE
      ENDIF

      ::oXbp:handleEvent( ::nEvent, ::mp1, ::mp2 )
   ENDDO

   HBXBP_DEBUG( "EXITING.................." )

   /* Very important - destroy resources */
   ::oDlg:destroy()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:executeAction( cKey )
   LOCAL cFile

   DO CASE

   CASE cKey == "Exit"
      PostAppEvent( xbeP_Close, NIL, NIL, ::oDlg )
   CASE cKey == "NewProject"
      ::fetchProjectProperties( .t. )
   CASE cKey == "Open"
      IF !empty( cFile := ::selectSource( "open" ) )
         ::oProjRoot:addItem( cFile )
         ::editSource( cFile )
      ENDIF
   CASE cKey == "Save"
      ::saveSource( ::getCurrentTab(), .f. )
   CASE cKey == "Close"
      ::closeSource()
   CASE cKey == "Print"
      IF !empty( ::qCurEdit )
         ::printPreview()
      ENDIF
   CASE cKey == "Undo"
      IF !empty( ::qCurEdit )
         ::qCurEdit:undo()
      ENDIF
   CASE cKey == "Redo"
      IF !empty( ::qCurEdit )
         ::qCurEdit:redo()
      ENDIF
   CASE cKey == "Cut"
      IF !empty( ::qCurEdit )
         ::qCurEdit:cut()
      ENDIF
   CASE cKey == "Copy"
      IF !empty( ::qCurEdit )
         ::qCurEdit:copy()
      ENDIF
   CASE cKey == "Paste"
      IF !empty( ::qCurEdit )
         ::qCurEdit:paste()
      ENDIF
   CASE cKey == "SelectAll"
      IF !empty( ::qCurEdit )
         ::qCurEdit:selectAll()
      ENDIF
   CASE cKey == "Find"
      IF !empty( ::qCurEdit )
         ::findReplace( "finddialog" )
      ENDIF
   CASE cKey == "ToUpper"
      ::convertSelection( cKey )
   CASE cKey == "ToLower"
      ::convertSelection( cKey )
   CASE cKey == "Invert"
      ::convertSelection( cKey )
   CASE cKey == "ZoomIn"
      IF !empty( ::qCurEdit )
         ::qCurEdit:zoomIn()
      ENDIF
   CASE cKey == "ZoomOut"
      IF !empty( ::qCurEdit )
         ::qCurEdit:zoomOut()
      ENDIF
   CASE cKey == "11"
      IF ::lDockBVisible
         ::oDockB:hide()
         ::oDockB1:hide()
         ::oDockB2:hide()
         ::lDockBVisible := .f.
      ELSEIF ::qTabWidget:count() > 0
         ::oDockB:show()
         ::oDockB1:show()
         ::oDockB2:show()
         ::lDockBVisible := .t.
      ENDIF
   CASE cKey == "12"
      IF ::lDockRVisible
         ::oDockR:hide()
      ELSE
         ::oDockR:show()
      ENDIF
      ::lDockRVisible := !( ::lDockRVisible )

   CASE cKey == "Compile"
   CASE cKey == "CompilePPO"

   ENDCASE

   ::manageFocusInEditor()

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD HbIde:convertSelection( cKey )
   LOCAL qCursor, cBuffer, i, s, nLen, c
   //LOCAL nB, nE

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
               s += IF( isUpper( c ), lower( c ), upper( c ) )
            NEXT
            cBuffer := s
         ENDCASE
         //nB := qCursor:selectionStart()
         //nE := qCursor:selectionEnd()
         qCursor:removeSelectedText()
         //qCursor:beginEditBlock()
         qCursor:insertText( cBuffer )
         //qCursor:select( QTextCursor_BlockUnderCursor )
         //qCursor:endEditBlock()
         //qCursor:setPosition( nB-len(cBuffer)+1, QTextCursor_MoveAnchor )
         //::qCurEdit:find( cBuffer )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageFocusInEditor()

   IF ::getCurrentTab() > 0
      ::aTabs[ ::getCurrentTab(), 2 ]:setFocus()
   ENDIF

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:updateFuncList()

   ::oFuncList:clear()
   IF !empty( ::aTags )
      aeval( ::aTags, {|e_| ::oFuncList:addItem( e_[ 7 ] ) } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:setTabImage( cState, oTab )
   LOCAL nIndex := ::qTabWidget:indexOf( QT_PTROFXBP( oTab ) )

   DO CASE
   CASE cState == "modified"
      ::qTabWidget:setTabIcon( nIndex, s_resPath + "tabmodified.png" )

   CASE cState == "unmodified"
      ::qTabWidget:setTabIcon( nIndex, s_resPath + "tabunmodified.png" )

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildTabPage( oWnd, cSource )
   LOCAL oTab, cPath, cFile, cExt

   hb_fNameSplit( cSource, @cPath, @cFile, @cExt )

   oTab := XbpTabPage():new( oWnd, , { 5,5 }, { 700,400 }, , .t. )
   oTab:caption   := cFile + cExt
   oTab:minimized := .F.

   oTab:create()

   ::qTabWidget:setTabTooltip( ::qTabWidget:indexOf( QT_PTROFXBP( oTab ) ), cSource )

   oTab:tabActivate    := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }
   oTab:closeRequested := {|mp1,mp2,oXbp| ::closeTab( mp1, mp2, oXbp ) }

   RETURN oTab

/*----------------------------------------------------------------------*/

METHOD HbIde:editSource( cSourceFile )
   LOCAL oTab, qEdit, qHiliter, qLayout, qDocument

   DEFAULT cSourceFile TO ::cProjFile

   oTab := ::buildTabPage( ::oDa, cSourceFile )

   qEdit := QTextEdit():new( QT_PTROFXBP( oTab ) )
   qEdit:setLineWrapMode( QTextEdit_NoWrap )
   qEdit:setPlainText( memoread( cSourceFile ) )
   qEdit:setFont( QT_PTROFXBP( ::oFont ) )
   qEdit:setTextBackgroundColor( QT_PTROF( QColor():new( 255,255,255 ) ) )

   qDocument := QTextDocument():configure( qEdit:document() )

   qLayout := QBoxLayout():new()
   qLayout:setDirection( 0 )
   qLayout:setContentsMargins( 0,0,0,0 )
   qLayout:addWidget( QT_PTROF( qEdit ) )

   oTab:oWidget:setLayout( QT_PTROF( qLayout ) )

   qHiliter := QSyntaxHighlighter():new( qEdit:document() )

   qEdit:show()

   aadd( ::aTabs, { oTab, qEdit, qHiliter, qLayout, cSourceFile, qDocument } )

   ::nPrevTab := ::nCurTab
   ::nCurTab  := len( ::aTabs )
   ::qTabWidget:setCurrentIndex( ::qTabWidget:indexOf( QT_PTROFXBP( oTab ) ) )

   ::aSources := { cSourceFile }
   ::createTags()
   ::updateFuncList()
   ::manageFocusInEditor()

   Qt_Connect_Signal( QT_PTROF( qEdit ), "textChanged()", ;
              {|| ::setTabImage( IF( qDocument:isModified(),"modified","unmodified" ), oTab ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:closeTab( mp1, mp2, oXbp )

   HB_SYMBOL_UNUSED( mp1 )

   IF ( mp2 := ascan( ::aTabs, {|e_| e_[ 1 ] == oXbp } ) ) > 0
      ::closeSource( mp2, .t. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:activateTab( mp1, mp2, oXbp )

   HB_SYMBOL_UNUSED( mp1 )

   IF ( mp2 := ascan( ::aTabs, {|e_| e_[ 1 ] == oXbp } ) ) > 0
      ::nCurTab  := mp2
      ::aSources := { ::aTabs[ ::nCurTab, 5 ] }
      ::createTags()
      ::updateFuncList()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:getCurrentTab()
   LOCAL qTab, nTab

   qTab := ::qTabWidget:currentWidget()
   nTab := ascan( ::aTabs, {|e_| HBQT_QTPTR_FROM_GCPOINTER( e_[ 1 ]:oWidget:pPtr ) == qTab } )

   RETURN nTab

/*----------------------------------------------------------------------*/

METHOD HbIde:closeSource()
   LOCAL nTab

   IF !empty( nTab := ::getCurrentTab() )
      ::oFuncList:clear()
      ::saveSource( nTab )
      /* Destroy at Qt level */
      ::qTabWidget:removeTab( ::qTabWidget:currentIndex() )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:closeAllSources()
   LOCAL nTab
   LOCAL nTabs := ::qTabWidget:count()

   FOR nTab := 1 TO nTabs
      ::closeSource()
   NEXT
   ::aTabs := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:getYesNo( cMsg, cInfo, cTitle )
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

METHOD HbIde:saveSource( nTab, lConfirm )
   LOCAL cBuffer, qDocument
   LOCAL lSave := .t.

   DEFAULT lConfirm TO .t.

   IF nTab > 0
      qDocument := QTextDocument():configure( ::aTabs[ nTab, 2 ]:document() )

      IF qDocument:isModified()
         IF lConfirm .and. !::getYesNo( ::aTabs[ nTab, 5 ] + " : has changed !", "Save this source ?" )
            lSave := .f.
         ENDIF

         IF lSave
            cBuffer := ::aTabs[ nTab, 2 ]:toPlainText()
            memowrit( ::aTabs[ nTab, 5 ], cBuffer )
            qDocument:setModified( .f. )
            ::createTags()
            ::updateFuncList()
         ELSE
         ENDIF
      ENDIF

      ::setTabImage( "unmodified", ::aTabs[ nTab, 1 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:selectSource( cMode )
   LOCAL oDlg, cFile//, aFiles

   oDlg := XbpFileDialog():new():create( ::oDa, , { 10,10 } )
   IF cMode == "open"
      oDlg:title       := "Select a Source File"
      oDlg:center      := .t.
      oDlg:fileFilters := { { "PRG Sources", "*.prg" }, { "C Sources", "*.c" }, { "CPP Sources", "*.cpp" }, ;
                                                            { "H Headers", "*.h" }, { "CH Headers", "*.ch" } }

      cFile := oDlg:open( , , .f. )
   ELSE
      oDlg:title       := "Save this Database"
      oDlg:fileFilters := { { "Database Files", "*.dbf" } }
      oDlg:quit        := {|| MsgBox( "Quitting the Dialog" ), 1 }
      cFile := oDlg:saveAs( "myfile.dbf" )
      IF !empty( cFile )
         HBXBP_DEBUG( cFile )
      ENDIF
   ENDIF

   RETURN cFile

/*----------------------------------------------------------------------*/

METHOD HbIde:buildProjectTree()
   LOCAL aExe, aExeD, i, j, aPrjs

   ::oProjTree := XbpTreeView():new()
   ::oProjTree:hasLines   := .T.
   ::oProjTree:hasButtons := .T.
   ::oProjTree:create( ::oDa, , { 0,0 }, { 10,10 }, , .t. )
   ::oProjTree:setColorBG( GraMakeRGBColor( { 223,240,255 } ) )
   ::oProjTree:itemMarked    := {|oItem| ::oCurProjItem := oItem }
   ::oProjTree:hbContextMenu := {|mp1, mp2, oXbp| ::manageProjectContext( mp1, mp2, oXbp ) }

   ::oProjTree:oWidget:setMaximumWidth( 200 )

   ::oProjRoot := ::oProjTree:rootItem:addItem( "Projects" )

   aadd( ::aProjData, { ::oProjRoot:addItem( "Executables" ), "Executables", NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Libs" )       , "Libs"       , NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Dlls" )       , "Dlls"       , NIL, NIL } )

   ::oProjRoot:expand( .t. )

   /* Just a prototype : to be filled with project data */

   /* Executables */
   aExeD := {}
   aExe  := ::aProjData[ 1 ]
   aPrjs := { "Vouch", "CacheMGR" }
   FOR i := 1 TO len( aPrjs )
      aadd( aExeD, { aExe[ 1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
   NEXT
   ::aProjData[ 1,3 ] := aExeD
   ::aProjData[ 1,1 ]:expand( .t. )

   /* Libs */
   aExeD := {}
   aExe  := ::aProjData[ 2 ]
   aPrjs := { "V32Lib", "Vouch32", "CacheRDD" }
   FOR i := 1 TO len( aPrjs )
      aadd( aExeD, { aExe[ 1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
   NEXT
   ::aProjData[ 2,3 ] := aExeD
   ::aProjData[ 2,1 ]:expand( .t. )

   /* Libs */
   aExeD := {}
   aExe  := ::aProjData[ 3 ]
   aPrjs := { "VouchActiveX" }
   FOR i := 1 TO len( aPrjs )
      aadd( aExeD, { aExe[ 1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
   NEXT
   ::aProjData[ 3,3 ] := aExeD
   ::aProjData[ 3,1 ]:expand( .t. )

   /* Next classification by type of source */
   aExe  := ::aProjData[ 1,3 ]
   aPrjs := { "PRG Sources", "C Sources", "CPP Sources", "CH Headers", "H Headers" }
   FOR j := 1 TO len( aExe )
      aExeD := {}
      FOR i := 1 TO len( aPrjs )
         aadd( aExeD, { aExe[ j,1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
      NEXT
      aExe[ j,3 ] := aExeD
      aExe[ j,1 ]:expand( .t. )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildStatusBar()
   LOCAL oPanel

   ::oSBar := XbpStatusBar():new()
   ::oSBar:create( ::oDlg, , { 0,0 }, { ::oDlg:currentSize()[1],30 } )

   oPanel := ::oSBar:getItem( 1 )
   oPanel:caption  := "Ready"
   oPanel:autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildDialog()

   ::oDlg := XbpDialog():new( , , {10,10}, {1100,700}, , .f. )

   ::oDlg:icon := s_resPath + "vr.png" // "hbide.png"
   ::oDlg:title := "Harbour-Qt IDE"

   ::oDlg:create()

   ::oDlg:close := {|| MsgBox( "You can also close me by pressing [ESC]" ), .T. }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:gotoFunction( mp1, mp2, oListBox )
   LOCAL n, cAnchor

   mp1 := oListBox:getData()
   mp2 := oListBox:getItem( mp1 )

   IF ( n := ascan( ::aTags, {|e_| mp2 == e_[ 7 ] } ) ) > 0
      cAnchor := trim( ::aText[ ::aTags[ n,3 ] ] )
      IF !( ::aTabs[ ::nCurTab, 2 ]:find( cAnchor, QTextDocument_FindCaseSensitively ) )
         ::aTabs[ ::nCurTab, 2 ]:find( cAnchor, QTextDocument_FindBackward + QTextDocument_FindCaseSensitively )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageProjectContext( mp1 )
   LOCAL aPops := {}

   /* Decide the contex options from */

   IF !empty( ::oCurProjItem )
      IF ::oCurProjItem:caption == "Projects"
         aadd( aPops, { "Properties", {|| ::fetchProjectProperties( .f. ) } } )

      ELSEIF ::oCurProjItem:caption == "Executables"
      ELSEIF ::oCurProjItem:caption == "Libs"
      ELSEIF ::oCurProjItem:caption == "Dlls"
      ELSEIF ::oCurProjItem:caption == "PRG Sources"
      ELSEIF ::oCurProjItem:caption == "C Sources"
      ELSEIF ::oCurProjItem:caption == "CPP Sources"
      ELSEIF ::oCurProjItem:caption == "CH Headers"
      ELSEIF ::oCurProjItem:caption == "H Headers"
      ELSE
         aadd( aPops, { ::oCurProjItem:caption, {|| NIL } } )
         aadd( aPops, { ::oCurProjItem:caption, {|| NIL } } )
         aadd( aPops, { ::oCurProjItem:caption, {|| NIL } } )
      ENDIF

      ExecPopup( aPops, mp1, ::oProjTree:oWidget )

      ::manageFocusInEditor()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageFuncContext( mp1 )
   LOCAL aPops := {}

   IF ::oFuncList:numItems() > 0
      aadd( aPops, { 'Comment out'           , {|| NIL } } )
      aadd( aPops, { 'Reformat'              , {|| NIL } } )
      aadd( aPops, { 'Print'                 , {|| NIL } } )
      aadd( aPops, { 'Delete'                , {|| NIL } } )
      aadd( aPops, { 'Move to another source', {|| NIL } } )

      ExecPopup( aPops, mp1, ::oFuncList:oWidget )

      ::manageFocusInEditor()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildFuncList()

   ::oDockR := XbpWindow():new( ::oDa )
   ::oDockR:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockR )
   ::oDockR:oWidget:setFeatures( QDockWidget_DockWidgetClosable + QDockWidget_DockWidgetMovable )
   ::oDockR:oWidget:setAllowedAreas( Qt_RightDockWidgetArea )
   ::oDockR:oWidget:setWindowTitle( "Module Function List" )
   ::oDockR:oWidget:setMinimumWidth( 100 )
   ::oDockR:oWidget:setMaximumWidth( 150 )
   ::oDockR:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oFuncList := XbpListBox():new( ::oDockR ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oFuncList:setStyleSheet( GetStyleSheet( "QListView" ) )

   //::oFuncList:ItemMarked := {|mp1, mp2, oXbp| ::gotoFunction( mp1, mp2, oXbp ) }
   ::oFuncList:ItemSelected  := {|mp1, mp2, oXbp| ::gotoFunction( mp1, mp2, oXbp ) }
   /* Harbour Extension : prefixed with "hb" */
   ::oFuncList:hbContextMenu := {|mp1, mp2, oXbp| ::manageFuncContext( mp1, mp2, oXbp ) }

   ::oFuncList:oWidget:setEditTriggers( QAbstractItemView_NoEditTriggers )

   ::oDockR:oWidget:setWidget( QT_PTROFXBP( ::oFuncList ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, QT_PTROFXBP( ::oDockR ), Qt_Horizontal )
   ::oDockR:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildBottomArea()

   ::buildCompileResults()
   ::buildLinkResults()
   ::buildOutputResults()

   ::oDlg:oWidget:tabifyDockWidget( QT_PTROFXBP( ::oDockB ), QT_PTROFXBP( ::oDockB1 ) )
   ::oDlg:oWidget:tabifyDockWidget( QT_PTROFXBP( ::oDockB1 ), QT_PTROFXBP( ::oDockB2 ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildCompileResults()

   ::oDockB := XbpWindow():new( ::oDa )
   ::oDockB:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockB )
   ::oDockB:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB:oWidget:setWindowTitle( "Compile Results" )
   ::oDockB:oWidget:setMinimumHeight(  75 )
   ::oDockB:oWidget:setMaximumHeight( 100 )
   ::oDockB:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oCompileResult := XbpMLE():new( ::oDockB ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oDockB:oWidget:setWidget( QT_PTROFXBP( ::oCompileResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB ), Qt_Vertical )
   ::oDockB:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildLinkResults()

   ::oDockB1 := XbpWindow():new( ::oDa )
   ::oDockB1:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockB1 )
   ::oDockB1:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB1:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB1:oWidget:setWindowTitle( "Link Results" )
   ::oDockB1:oWidget:setMinimumHeight(  75 )
   ::oDockB1:oWidget:setMaximumHeight( 100 )
   ::oDockB1:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oLinkResult := XbpMLE():new( ::oDockB1 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oDockB1:oWidget:setWidget( QT_PTROFXBP( ::oLinkResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB1 ), Qt_Vertical )
   ::oDockB1:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildOutputResults()

   ::oDockB2 := XbpWindow():new( ::oDa )
   ::oDockB2:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockB2 )
   ::oDockB2:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB2:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB2:oWidget:setWindowTitle( "Output Console" )
   ::oDockB2:oWidget:setMinimumHeight(  75 )
   ::oDockB2:oWidget:setMaximumHeight( 100 )
   ::oDockB2:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oOutputResult := XbpMLE():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oDockB2:oWidget:setWidget( QT_PTROFXBP( ::oOutputResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB2 ), Qt_Vertical )
   ::oDockB2:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:CreateTags()
   LOCAL aSumData := ""
   LOCAL cComments, aSummary, i, cPath, cSource, cExt

   ::aTags := {}

   FOR i := 1 TO Len( ::aSources )
      HB_FNameSplit( ::aSources[ i ], @cPath, @cSource, @cExt )

      IF Upper( cExt ) $ ".PRG.CPP"
         IF !empty( ::aText := ReadSource( ::aSources[ i ] ) )
            aSumData  := {}

            cComments := CheckComments( ::aText )
            aSummary  := Summarize( ::aText, cComments, @aSumData , IIf( Upper( cExt ) == ".PRG", 9, 1 ) )
            ::aTags   := UpdateTags( ::aSources[ i ], aSummary, aSumData, @::aFuncList, @::aLines )

            #if 0
            IF !empty( aTags )
               aeval( aTags, {|e_| aadd( ::aTags, e_ ) } )
               ::hData[ cSource+cExt ] := { a[ i ], aTags, aclone( ::aText ), cComments, ::aFuncList, ::aLines }
               aadd( ::aSrcLines, ::aText   )
               aadd( ::aComments, cComments )
            ENDIF
            #endif
         ENDIF
      ENDIF
   NEXT

   RETURN ( NIL )

//----------------------------------------------------------------------//

METHOD HbIde:printPreview()
   LOCAL qDlg

   qDlg := QPrintPreviewDialog():new( QT_PTROFXBP( ::oDlg ) )
   qDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
   Qt_Connect_Signal( qDlg:pPtr, "paintRequested(QPrinter)", {|o,p| ::paintRequested( p,o ) } )
   qDlg:exec()
   Qt_DisConnect_Signal( qDlg:pPtr, "paintRequested(QPrinter)" )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:paintRequested( pPrinter )
   LOCAL qPrinter

   qPrinter := QPrinter():configure( pPrinter )

   ::qCurEdit:print( QT_PTROF( qPrinter ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:fetchProjectProperties( lNewProject )
   LOCAL nRet
   LOCAL qLayout, qHBLayout
   LOCAL oDlg, oBtnOK, oBtnCn
   LOCAL qPrjType , oPrjTtl  , oPrjLoc  , oPrjWrk  , oPrjDst  , oPrjOut  , oPrjInc  , oPrjLau  , oPrjLEx
   LOCAL qTypLabel, qPrjLabel, qLocLabel, qWrkLabel, qDstLabel, qOutLabel, qIncLabel, qLauLabel, qLExLabel

   DEFAULT lNewProject TO .t.

   HB_SYMBOL_UNUSED( lNewProject )

   oDlg := XbpWindow():new()
   oDlg:oWidget := QDialog():new( QT_PTROFXBP( ::oDlg ) )
   oDlg:oWidget:setWindowTitle( "New Project Properties" )

   qTypLabel := QLabel():new() ; qTypLabel:setText( "<b>Project Type</b>"     )
   qPrjLabel := QLabel():new() ; qPrjLabel:setText( "Project Title"           )
   qLocLabel := QLabel():new() ; qLocLabel:setText( "Project Location"        )
   qWrkLabel := QLabel():new() ; qWrkLabel:setText( "Working Directory"       )
   qDstLabel := QLabel():new() ; qDstLabel:setText( "Destination Directory"   )
   qOutLabel := QLabel():new() ; qOutLabel:setText( "Output Name"             )
   qIncLabel := QLabel():new() ; qIncLabel:setText( "Compile/Link Flags"      )
   qLauLabel := QLabel():new() ; qLauLabel:setText( "Launch Parameters"       )
   qLExLabel := QLabel():new() ; qLExLabel:setText( "Launch Program"          )

   qPrjType := QComboBox():New()
   qPrjType:addItem( "Executable" )
   qPrjType:addItem( "Library"    )
   qPrjType:addItem( "Dll"        )
   //
   oPrjTtl := XbpSLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )
   oPrjLoc := XbpSLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )
   oPrjWrk := XbpSLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )
   oPrjDst := XbpSLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )
   oPrjOut := XbpSLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )
   oPrjInc := XbpMLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )
   oPrjLau := XbpSLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )
   oPrjLEx := XbpSLE():new():create( oDlg, , {0,0}, {10,10}, , .t. )

   /* Buttons at lower right end */
   qHBLayout := QHBoxLayout():new()
   qHBLayout:setSpacing( 10 )
   //
   oBtnOk := XbpPushButton():new( oDlg, , {0,0}, {10,30}, , .t. ):create()
   oBtnOk:setCaption( "Ok" )
   oBtnOk:activate := {|| oDlg:oWidget:done( 1 ) }
   //
   oBtnCn := XbpPushButton():new( oDlg, , {0,0}, {10,30}, , .t. ):create()
   oBtnCn:setCaption( "Cancel" )
   oBtnCn:activate := {|| oDlg:oWidget:done( 2 ) }
   //
   qHBLayout:addWidget( QT_PTROFXBP( oBtnOK ) )
   qHBLayout:addWidget( QT_PTROFXBP( oBtnCN ) )

   /*  Grid layout and */
   qLayout := QGridLayout():new()
   qLayout:setColumnStretch( 0,1 )
   qLayout:setColumnMinimumWidth( 0,75 )
   qLayout:setColumnMinimumWidth( 1,200 )
   qLayout:setVerticalSpacing( 5 )
   qLayout:setHorizontalSpacing( 5 )
   //                                        R  C                                                 R  C
   qLayout:addWidget( QT_PTROF( qTypLabel ), 0, 0 ) ; qLayout:addWidget( QT_PTROF( qPrjType )   , 0, 1 )
   qLayout:addWidget( QT_PTROF( qPrjLabel ), 1, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjTtl ) , 1, 1 )
   qLayout:addWidget( QT_PTROF( qLocLabel ), 2, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjLoc  ), 2, 1 )
   qLayout:addWidget( QT_PTROF( qWrkLabel ), 3, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjWrk  ), 3, 1 )
   qLayout:addWidget( QT_PTROF( qDstLabel ), 4, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjDst  ), 4, 1 )
   qLayout:addWidget( QT_PTROF( qOutLabel ), 5, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjOut  ), 5, 1 )
   qLayout:addWidget( QT_PTROF( qIncLabel ), 6, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjInc  ), 6, 1 )
   qLayout:addWidget( QT_PTROF( qLauLabel ), 7, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjLau  ), 7, 1 )
   qLayout:addWidget( QT_PTROF( qLExLabel ), 8, 0 ) ; qLayout:addWidget( QT_PTROFXBP( oPrjLEx  ), 8, 1 )
   //
   qLayout:addWidget( QT_PTROF( QWidget():new() ), 12, 1 )
   qLayout:addLayout( QT_PTROF( qHBLayout )      , 13, 1 )

   oDlg:oWidget:setLayout( QT_PTROF( qLayout ) )

   nRet := oDlg:oWidget:exec()

   JustACall( nRet )

   HBXBP_DEBUG( "Done", nRet )

   oDlg:destroy()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:findReplace( cUi )
   LOCAL qUiLoader, qFile, cUiFull

   IF ::qFindDlg == NIL
      cUiFull := s_resPath + cUi + ".ui"
      qFile := QFile():new( cUiFull )
      IF qFile:open( 1 )
         qUiLoader  := QUiLoader():new()
         ::qFindDlg := QDialog():configure( qUiLoader:load( QT_PTROF( qFile ), QT_PTROFXBP( ::oDlg ) ) )
         ::qFindDlg:setWindowFlags( Qt_Sheet )
         qFile:close()
         //
         ::oFind := XbpComboBox():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "comboFindWhat" ) )
         ::oRepl := XbpComboBox():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "comboReplaceWith" ) )

         ::oPBFind := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "buttonFind" ) )
         ::oPBFind:activate := {|| ::qCurEdit:find( QLineEdit():configure( ::oFind:oWidget:lineEdit() ):text() ) }

         ::oPBRepl := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "buttonReplace" ) )
         ::oPBRepl:activate := {|t| t := QLineEdit():configure( ::oRepl:oWidget:lineEdit() ):text() }

         ::oPBClose := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "buttonClose" ) )
         ::oPBClose:activate := {|| ::qFindDlg:hide() }
      ENDIF
   ENDIF

   ::oFind:setFocus()
   ::qFindDlg:show()

   RETURN Self

/*----------------------------------------------------------------------*/

