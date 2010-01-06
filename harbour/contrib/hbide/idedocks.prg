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
 *                               28Dec2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "xbp.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS IdeDockS INHERIT IdeObject

   DATA   nPass                                   INIT   0

   METHOD new()
   METHOD create()
   METHOD destroy()

   METHOD buildDialog()
   METHOD buildStatusBar()

   METHOD buildDockWidgets()

   METHOD buildProjectTree()
   METHOD buildEditorTree()
   METHOD buildFuncList()
   METHOD buildCompileResults()
   METHOD buildLinkResults()
   METHOD buildOutputResults()

   METHOD outputDoubleClicked()
   METHOD toggleLeftDocks()
   METHOD toggleRightDocks()
   METHOD toggleBottomDocks()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeDocks:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDialog()

   #if 1
   LOCAL oUI
   oUI := XbpQtUiLoader():new()
   oUI:file := ::resPath + "mainWindow.ui"
   oUI:create()

   ::oIde:oDlg := XbpDialog():new()
   ::oDlg:icon := ::resPath + "vr.png" // "hbide.png"
   ::oDlg:title := "Harbour-Qt IDE"
   ::oDlg:qtObject := oUI:oWidget
   ::oDlg:create()
   #else
   ::oIde:oDlg := XbpDialog():new( , , {10,10}, {1100,700}, , .f. )
   ::oDlg:icon := ::resPath + "vr.png" // "hbide.png"
   ::oDlg:title := "Harbour-Qt IDE"
   ::oDlg:create()
   #endif

   ::oDlg:setStyleSheet( GetStyleSheet( "QMainWindow" ) )

   ::oDlg:close := {|| MsgBox( "HbIDE is about to be closed!" ), .T. }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )

   ::oIde:oDa := ::oDlg:drawingArea

   SetAppWindow( ::oDlg )

   ::oIde:setPosAndSizeByIni( ::oDlg:oWidget, MainWindowGeometry )
   ::oDlg:Show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDockWidgets()

   ::buildProjectTree()
   ::buildEditorTree()
   ::buildFuncList()
   ::buildCompileResults()
   ::buildLinkResults()
   ::buildOutputResults()

   ::oDlg:oWidget:tabifyDockWidget( ::oDockB:oWidget , ::oDockB1:oWidget )
   ::oDlg:oWidget:tabifyDockWidget( ::oDockB1:oWidget, ::oDockB2:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildProjectTree()
   LOCAL i

   ::oIde:oDockPT := XbpWindow():new( ::oDa )
   ::oDockPT:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockPT:oWidget:setObjectName( "dockProjectTree" )
   ::oDlg:addChild( ::oDockPT )
   ::oDockPT:oWidget:setFeatures( QDockWidget_DockWidgetClosable + QDockWidget_DockWidgetMovable )
   ::oDockPT:oWidget:setAllowedAreas( Qt_LeftDockWidgetArea )
   ::oDockPT:oWidget:setWindowTitle( "Projects" )
   ::oDockPT:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oProjTree := XbpTreeView():new()
   ::oProjTree:hasLines   := .T.
   ::oProjTree:hasButtons := .T.
   ::oProjTree:create( ::oDa, , { 0,0 }, { 10,10 }, , .t. )
   ::oProjTree:setStyleSheet( GetStyleSheet( "QTreeWidget" ) )

   //::oProjTree:itemMarked    := {|oItem| ::manageItemSelected( 0, oItem ), ::oCurProjItem := oItem }
   ::oProjTree:itemMarked    := {|oItem| ::oIde:oCurProjItem := oItem, ::oIde:manageFocusInEditor() }
   ::oProjTree:itemSelected  := {|oItem| ::oIde:manageItemSelected( oItem ) }
   ::oProjTree:hbContextMenu := {|mp1, mp2, oXbp| ::oIde:manageProjectContext( mp1, mp2, oXbp ) }

   ::oIde:oProjRoot := ::oProjTree:rootItem:addItem( "Projects" )

   aadd( ::aProjData, { ::oProjRoot:addItem( "Executables" ), "Executables", ::oProjRoot, NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Libs"        ), "Libs"       , ::oProjRoot, NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Dlls"        ), "Dlls"       , ::oProjRoot, NIL, NIL } )

   ::oProjRoot:expand( .t. )
   //
   FOR i := 1 TO len( ::aProjects )
      ::oIde:updateProjectTree( ::aProjects[ i, 3 ] )
   NEXT

   /* Insert Project Tree Into Dock Widget */
   ::oDockPT:oWidget:setWidget( ::oProjTree:oWidget )

   /* Add dock widget to Main Window */
   ::oDlg:oWidget:addDockWidget_1( Qt_LeftDockWidgetArea, ::oDockPT:oWidget, Qt_Vertical )

   IF ::oIde:aIni[ INI_HBIDE, ProjectTreeVisible ] == "NO"
      ::oIde:lProjTreeVisible := .f.
      ::oDockPT:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildEditorTree()

   ::oIde:oDockED := XbpWindow():new( ::oDa )
   ::oDockED:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockED:oWidget:setObjectName( "dockEditorTabs" )
   ::oDlg:addChild( ::oDockED )
   ::oDockED:oWidget:setFeatures( QDockWidget_DockWidgetClosable + QDockWidget_DockWidgetMovable )
   ::oDockED:oWidget:setAllowedAreas( Qt_LeftDockWidgetArea )
   ::oDockED:oWidget:setWindowTitle( "Editor Tabs" )
   ::oDockED:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oEditTree := XbpTreeView():new()
   ::oEditTree:hasLines   := .T.
   ::oEditTree:hasButtons := .T.
   ::oEditTree:create( ::oDa, , { 0,0 }, { 10,10 }, , .t. )
   ::oEditTree:setStyleSheet( GetStyleSheet( "QTreeWidget" ) )

   //::oEditTree:itemMarked    := {|oItem| ::manageItemSelected( 0, oItem ), ::oCurProjItem := oItem }
   ::oEditTree:itemMarked    := {|oItem| ::oIde:oCurProjItem := oItem, ::oIde:manageFocusInEditor() }
   ::oEditTree:itemSelected  := {|oItem| ::oIde:manageItemSelected( oItem ) }
   ::oEditTree:hbContextMenu := {|mp1, mp2, oXbp| ::oIde:manageProjectContext( mp1, mp2, oXbp ) }

   ::oIde:oOpenedSources := ::oEditTree:rootItem:addItem( "Editor" )

   ::oOpenedSources:expand( .t. )

   /* Insert Project Tree Into Dock Widget */
   ::oDockED:oWidget:setWidget( ::oEditTree:oWidget )

   /* Add dock widget to Main Window */
   ::oDlg:oWidget:addDockWidget_1( Qt_LeftDockWidgetArea, ::oDockED:oWidget, Qt_Vertical )

   IF ::oIde:aIni[ INI_HBIDE, ProjectTreeVisible ] == "NO"
      ::oIde:lProjTreeVisible := .f.
      ::oDockED:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFuncList()

   ::oIde:oDockR := XbpWindow():new( ::oDa )
   ::oDockR:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockR:oWidget:setObjectName( "dockFuncList" )
   ::oDlg:addChild( ::oDockR )
   ::oDockR:oWidget:setFeatures( QDockWidget_DockWidgetClosable + QDockWidget_DockWidgetMovable )
   ::oDockR:oWidget:setAllowedAreas( Qt_RightDockWidgetArea )
   ::oDockR:oWidget:setWindowTitle( "Functions List" )
   ::oDockR:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oFuncList := XbpListBox():new( ::oDockR ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oFuncList:setStyleSheet( GetStyleSheet( "QListView" ) )

   //::oFuncList:ItemMarked := {|mp1, mp2, oXbp| ::gotoFunction( mp1, mp2, oXbp ) }
   ::oFuncList:ItemSelected  := {|mp1, mp2, oXbp| ::oIde:gotoFunction( mp1, mp2, oXbp ) }
   /* Harbour Extension : prefixed with "hb" */
   ::oFuncList:hbContextMenu := {|mp1, mp2, oXbp| ::oIde:manageFuncContext( mp1, mp2, oXbp ) }

   ::oFuncList:oWidget:setEditTriggers( QAbstractItemView_NoEditTriggers )

   ::oDockR:oWidget:setWidget( ::oFuncList:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oDockR:oWidget, Qt_Vertical )

   IF ::oIde:aIni[ INI_HBIDE, FunctionListVisible ] == "YES"
      ::oIde:lDockRVisible := .t.
      //::setSizeAndPosByIni( ::oDockR:oWidget, FunctionListGeometry )
   ELSE
      ::oIde:lDockRVisible := .f.
      ::oDockR:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildCompileResults()

   ::oIde:oDockB := XbpWindow():new( ::oDa )
   ::oDockB:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockB:oWidget:setObjectName( "dockCompileResults" )
   ::oDlg:addChild( ::oDockB )
   ::oDockB:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB:oWidget:setWindowTitle( "Compile Results" )
   ::oDockB:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oCompileResult := XbpMLE():new( ::oDockB ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oDockB:oWidget:setWidget( ::oCompileResult:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB:oWidget, Qt_Horizontal )
   ::oDockB:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildLinkResults()

   ::oIde:oDockB1 := XbpWindow():new( ::oDa )
   ::oDockB1:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockB1:oWidget:setObjectName( "dockLinkResults" )
   ::oDlg:addChild( ::oDockB1 )
   ::oDockB1:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB1:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB1:oWidget:setWindowTitle( "Link Results" )
   ::oDockB1:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oLinkResult := XbpMLE():new( ::oDockB1 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oDockB1:oWidget:setWidget( ::oLinkResult:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB1:oWidget, Qt_Horizontal )
   ::oDockB1:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildOutputResults()

   ::oIde:oDockB2 := XbpWindow():new( ::oDa )
   ::oDockB2:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockB2:oWidget:setObjectName( "dockOutputResults" )
   ::oDlg:addChild( ::oDockB2 )
   ::oDockB2:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB2:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB2:oWidget:setWindowTitle( "Output Console" )
   ::oDockB2:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oOutputResult := XbpRtf():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oOutputResult:oWidget:setAcceptRichText( .t. )
   ::oOutputResult:oWidget:setReadOnly( .T. )

   ::oDockB2:oWidget:setWidget( ::oOutputResult:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB2:oWidget, Qt_Horizontal )
   ::oDockB2:hide()

   Qt_Slots_Connect( ::pSlots, ::oOutputResult:oWidget, "copyAvailable(bool)", {|o,l| ::outputDoubleClicked( l, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:outputDoubleClicked( lSelected )
   LOCAL qCursor, cText
   LOCAL cSource, nLine

   IF lSelected
      ::nPass++
      IF ::nPass == 1
         qCursor := QTextCursor():configure( ::oOutputResult:oWidget:textCursor() )
         cText := QTextBlock():configure( qCursor:block() ):text()

         IF hbide_parseFNfromStatusMsg( cText, @cSource, @nLine, .T. )
            ::oIde:editSource( cSource )
            qCursor := QTextCursor():configure( ::oIde:qCurEdit:textCursor() )
            nLine   := iif( nLine < 1, 0, nLine - 1 )

            qCursor:setPosition( 0 )
            qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine )
            ::oIde:qCurEdit:setTextCursor( qCursor )
            ::oIde:manageFocusInEditor()
         ENDIF
      ENDIF
      IF ::nPass >= 2
         ::nPass := 0
      ENDIF
   ENDIF
   RETURN nLine

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildStatusBar()

   ::oIde:oSBar := XbpStatusBar():new()
   ::oSBar:create( ::oDlg, , { 0,0 }, { ::oDlg:currentSize()[ 1 ], 30 } )
   ::oSBar:oWidget:showMessage( "" )

   ::oSBar:getItem( SB_PNL_MAIN ):autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   ::oSBar:addItem( "", , , , "Ready"    ):oWidget:setMinimumWidth(  80 )
   ::oSBar:addItem( "", , , , "Line"     ):oWidget:setMinimumWidth( 110 )
   ::oSBar:addItem( "", , , , "Column"   ):oWidget:setMinimumWidth(  40 )
   ::oSBar:addItem( "", , , , "Ins"      ):oWidget:setMinimumWidth(  30 )
   ::oSBar:addItem( "", , , , "M_1"      ):oWidget:setMinimumWidth(  30 )
   ::oSBar:addItem( "", , , , "Modified" ):oWidget:setMinimumWidth(  50 )
   ::oSBar:addItem( "", , , , "M_2"      ):oWidget:setMinimumWidth(  30 )
   ::oSBar:addItem( "", , , , "Stream"   ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Edit"     ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Search"   ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Codec"    ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Project"  ):oWidget:setMinimumWidth(  20 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:toggleLeftDocks()

   IF ::lProjTreeVisible
      ::oDockPT:hide()
      ::oDockED:hide()
   ELSE
      ::oDockPT:show()
      ::oDockED:show()
   ENDIF
   ::oIde:lProjTreeVisible := !( ::lProjTreeVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:toggleRightDocks()

   IF ::lDockRVisible
      ::oDockR:hide()
   ELSE
      ::oDockR:show()
   ENDIF
   ::oIde:lDockRVisible := !( ::lDockRVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:toggleBottomDocks()

   IF ::lDockBVisible
      ::oDockB:hide()
      ::oDockB1:hide()
      ::oDockB2:hide()
   ELSEIF ::qTabWidget:count() > 0
      ::oDockB:show()
      ::oDockB1:show()
      ::oDockB2:show()
   ENDIF
   ::oIde:lDockBVisible := !( ::oIde:lDockBVisible )

   RETURN Self

/*----------------------------------------------------------------------*/
