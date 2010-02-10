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

CLASS IdeDocks INHERIT IdeObject

   DATA   nPass                                   INIT   0

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD execEvent( nMode, p )
   METHOD setView( cView )
   METHOD buildHelpWidget()
   METHOD buildDialog()
   METHOD buildViewWidget()
   METHOD buildStackedWidget()
   METHOD buildDockWidgets()
   METHOD buildProjectTree()
   METHOD buildEditorTree()
   METHOD buildFuncList()
   METHOD buildCompileResults()
   METHOD buildLinkResults()
   METHOD buildOutputResults()
   METHOD buildFindInFiles()
   METHOD outputDoubleClicked( lSelected )
   METHOD buildStatusBar()
   METHOD toggleLeftDocks()
   METHOD toggleRightDocks()
   METHOD toggleBottomDocks()
   METHOD setStatusText( nPart, xValue )
   METHOD getMarkWidget( nIndex )
   METHOD dispEnvironment( cEnviron )

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

METHOD IdeDocks:setView( cView )
   LOCAL n

   SWITCH cView

   CASE "New..."
      cView := hbide_fetchAString( ::qViewsCombo, cView, "Name the View", "New View" )
      IF cView != "New..."
         IF ascan( ::aINI[ INI_VIEWS ], {|e| e == cView } ) > 0
            MsgBox( "View: " + cView + ", already exists" )
         ELSE
            aadd( ::aINI[ INI_VIEWS ], cView )
            ::qViewsCombo:addItem( cView )
            ::buildViewWidget()
            ::oStackedWidget:oWidget:setCurrentIndex( len( ::aINI[ INI_VIEWS ] ) )
            ::oIde:cWrkView := cView
         ENDIF
      ENDIF
      EXIT

   CASE "Main"
      ::oIde:nCurView   := 0
      ::oIde:qTabWidget := ::aViews[ ::nCurView + 1 ]:oTabWidget:oWidget
      ::oIde:oTabParent := ::aViews[ ::nCurView + 1 ]
      ::oStackedWidget:oWidget:setCurrentIndex( 0 )
      ::oIde:cWrkView   := "Main"
      EXIT

   OTHERWISE
      IF ( n := ascan( ::aINI[ INI_VIEWS ], cView ) ) > 0
         ::oStackedWidget:oWidget:setCurrentIndex( n )   /* Note: n is always base of zero as main == 1 */
         ::oIde:cWrkView := cView
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeDocks:execEvent( nMode, p )
   LOCAL nIndex

   DO CASE
   CASE nMode == 1
      IF p >= 0 .AND. p <= len( ::aViews )
         ::oIde:nCurView := p

         ::oIde:qTabWidget := ::aViews[ ::nCurView + 1 ]:oTabWidget:oWidget
         ::oIde:oTabParent := ::aViews[ ::nCurView + 1 ]

         nIndex := ::oIde:qTabWidget:currentIndex()
         IF nIndex + 1 == ::oIde:qTabWidget:count()
            ::oIde:qTabWidget:setCurrentIndex( 0 )
            ::oIde:qTabWidget:setCurrentIndex( nIndex )  /* TODO: Must be last saved */
         ENDIF
         ::setStatusText( SB_PNL_VIEW, iif( p == 0, "Main", ::aINI[ INI_VIEWS, ::nCurView ] ) )
      ENDIF
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildStackedWidget()

   /* Its parent will be drawing area and pages will be XbpTabWidgets() */

   ::oIde:oStackedWidget := XbpWindow():new( ::oDa )
   ::oStackedWidget:oWidget := QStackedWidget():new( ::oDa:oWidget )
   ::oStackedWidget:oWidget:setObjectName( "myStackedWidget" )
   ::oDa:addChild( ::oStackedWidget )

   ::oStackedWidget:connect( ::oStackedWidget:oWidget, "currentChanged(int)", {|p| ::execEvent( 1, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDialog()
   LOCAL s

   ::oIde:oDlg := XbpDialog():new()
   ::oDlg:icon := ::resPath + "vr.png"
   ::oDlg:title := "Harbour-Qt IDE"
   #ifdef HBIDE_USE_UIC
      ::oDlg:qtObject := HbQtUI():new( ::resPath + "mainwindow.uic" ):build()
   #else
      ::oDlg:qtObject := HbQtUI():new( ::resPath + "mainwindow.ui" ):create()
   #endif
   ::oDlg:create( , , , , , .f. )

   ::oDlg:setStyleSheet( GetStyleSheet( "QMainWindow" ) )

   ::oDlg:close := {|| hbide_getYesNo( "hbIDE is about to be closed!", "Are you sure?" ) }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )

   ::oIde:oDa := ::oDlg:drawingArea

   SetAppWindow( ::oDlg )

   ::oIde:setPosAndSizeByIni( ::oDlg:oWidget, MainWindowGeometry )
   ::oDlg:Show()

   /* StatusBar */
   ::buildStatusBar()

   /* Attach GRID Layout to Editor Area - Futuristic */
   ::oIde:qLayout := QGridLayout():new()
   ::oIde:qLayout:setContentsMargins( 0,0,0,0 )
   ::oIde:qLayout:setHorizontalSpacing( 0 )
   ::oIde:qLayout:setVerticalSpacing( 0 )
   //
   ::oDa:oWidget:setLayout( ::qLayout )

   ::buildStackedWidget()
   ::qLayout:addWidget_1( ::oStackedWidget:oWidget, 0, 0, 1, 1 )

   /* View Panels */
   ::buildViewWidget()      /* Main */
   FOR EACH s IN ::aINI[ INI_VIEWS ]
      ::buildViewWidget()
   NEXT

   /* Force to populate current widget */
   ::oStackedWidget:oWidget:setCurrentIndex( 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildViewWidget()
   LOCAL n := len( ::aViews ) + 1
   LOCAL oFrame

   oFrame := XbpWindow():new( ::oStackedWidget )
   oFrame:oWidget := QWidget():new( ::oStackedWidget:oWidget )
   oFrame:oWidget:setObjectName( "viewWidget" + hb_ntos( n ) )
   ::oStackedWidget:addChild( oFrame )

   oFrame:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   oFrame:qLayout:setContentsMargins( 2, 2, 2, 2 )

   oFrame:oTabWidget := XbpTabWidget():new():create( oFrame, , {0,0}, {200,200}, , .t. )

   oFrame:oTabWidget:oWidget:setUsesScrollButtons( .f. )
   oFrame:oTabWidget:oWidget:setMovable( .t. )

   aadd( ::oIde:aViews, oFrame )

   ::oStackedWidget:oWidget:addWidget( oFrame:oWidget )

   ::oStackedWidget:oWidget:setCurrentIndex( 0 )

   RETURN oFrame

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDockWidgets()

   ::buildProjectTree()
   ::buildEditorTree()
   ::buildFuncList()
   ::buildCompileResults()
   ::buildLinkResults()
   ::buildOutputResults()
 * ::buildFindInFiles()
   ::buildHelpWidget()

   ::oDlg:oWidget:tabifyDockWidget( ::oDockB:oWidget , ::oDockB1:oWidget )
   ::oDlg:oWidget:tabifyDockWidget( ::oDockB1:oWidget, ::oDockB2:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildProjectTree()
   LOCAL i

   ::oIde:oDockPT := XbpWindow():new()
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
   ::oProjTree:create( ::oDockPT, , { 0,0 }, { 10,10 }, , .t. )

   ::oProjTree:setStyleSheet( GetStyleSheet( "QTreeWidgetHB" ) )

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

   ::oIde:oDockED := XbpWindow():new()
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
   ::oEditTree:create( ::oDockED, , { 0,0 }, { 10,10 }, , .t. )

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

   ::oIde:oDockR := XbpWindow():new()
   ::oDockR:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockR:oWidget:setObjectName( "dockFuncList" )
   ::oDlg:addChild( ::oDockR )
   ::oDockR:oWidget:setFeatures( QDockWidget_DockWidgetClosable + QDockWidget_DockWidgetMovable )
   ::oDockR:oWidget:setAllowedAreas( Qt_RightDockWidgetArea )
   ::oDockR:oWidget:setWindowTitle( "Functions List" )
   ::oDockR:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oFuncList := XbpListBox():new( ::oDockR ):create( , , { 0,0 }, { 100,400 }, , .t. )

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

METHOD IdeDocks:buildHelpWidget()
   STATIC qUrl

   IF empty( qUrl )
      qUrl := QUrl():new( "docs/idemainpage.html" )
   ENDIF

   ::oIde:oHelp := XbpWindow():new()
   ::oHelp:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oHelp:oWidget:setObjectName( "dockHelp" )
   ::oDlg:addChild( ::oHelp )
   ::oHelp:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oHelp:oWidget:setAllowedAreas( Qt_RightDockWidgetArea )
   ::oHelp:oWidget:setWindowTitle( "hbIDE Help" )
   ::oHelp:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:qHelpBrw := QTextBrowser():new( ::oHelp:oWidget )
   ::oIde:qHelpBrw:show()
   ::oIde:qHelpBrw:setOpenExternalLinks( .t. )
   ::qHelpBrw:setSource( qUrl )

   ::oHelp:oWidget:setWidget( ::oIde:qHelpBrw )

   // ::oHelp:connect( ::qHelpBrw )

   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oHelp:oWidget, Qt_Horizontal )
   ::oHelp:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildCompileResults()

   ::oIde:oDockB := XbpWindow():new()
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

   ::oIde:oDockB1 := XbpWindow():new()
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

   ::oIde:oDockB2 := XbpWindow():new()
   ::oDockB2:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockB2:oWidget:setObjectName( "dockOutputResults" )
   ::oDlg:addChild( ::oDockB2 )
   ::oDockB2:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB2:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB2:oWidget:setWindowTitle( "Output Console" )
   ::oDockB2:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oOutputResult := XbpRtf():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .T. )
   ::oOutputResult:oWidget:setAcceptRichText( .T. )
   ::oOutputResult:oWidget:setReadOnly( .T. )

   ::oDockB2:oWidget:setWidget( ::oOutputResult:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB2:oWidget, Qt_Horizontal )
   ::oDockB2:hide()

   ::connect( ::oIde:oOutputResult:oWidget, "copyAvailable(bool)", {|l| ::outputDoubleClicked( l ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:outputDoubleClicked( lSelected )
   LOCAL qCursor, cText
   LOCAL cSource, nLine

   IF lSelected
      qCursor := QTextCursor():configure( ::oOutputResult:oWidget:textCursor() )
      cText := QTextBlock():configure( qCursor:block() ):text()

      IF hbide_parseFNfromStatusMsg( cText, @cSource, @nLine, .T. )
         ::oSM:editSource( cSource, 0, 0, 0, NIL, NIL, .f., .t. )
         qCursor := QTextCursor():configure( ::oIde:qCurEdit:textCursor() )
         nLine   := iif( nLine < 1, 0, nLine - 1 )

         qCursor:setPosition( 0 )
         qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine )
         ::oIde:qCurEdit:setTextCursor( qCursor )
         ::oIde:manageFocusInEditor()
      ENDIF
   ENDIF

   RETURN nLine

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildStatusBar()
   LOCAL i

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
   ::oSBar:addItem( "", , , , "Environ"  ):oWidget:setMinimumWidth(  30 )
   ::oSBar:addItem( "", , , , "Stream"   ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Edit"     ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Search"   ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Codec"    ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Project"  ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "View"     ):oWidget:setMinimumWidth(  20 )

   FOR i := 1 TO 6
      ::oSBar:oWidget:addWidget( ::getMarkWidget( i ) )
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:getMarkWidget( nIndex )
   LOCAL aColors  := { "rgb( 255,255,127 )", "rgb( 175,175,255 )", "rgb( 255,175,175 )", ;
                       "rgb( 175,255,175 )", "rgb( 255,190,125 )", "rgb( 175,255,255 )"  }

   ::oIde:aMarkTBtns[ nIndex ] := QToolButton():new()

   ::oIde:aMarkTBtns[ nIndex ]:setMaximumHeight( 12 )
   ::oIde:aMarkTBtns[ nIndex ]:setMaximumWidth( 12 )
   ::oIde:aMarkTBtns[ nIndex ]:setStyleSheet( "background-color: " + aColors[ nIndex ] + ";" )
   ::oIde:aMarkTBtns[ nIndex ]:hide()

   ::connect( ::oIde:aMarkTBtns[ nIndex ], "clicked()", {|| ::oEM:gotoMark( nIndex ) } )

   RETURN ::oIde:aMarkTBtns[ nIndex ]

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

METHOD IdeDocks:dispEnvironment( cEnviron )
   ::setStatusText( SB_PNL_ENVIRON, cEnviron )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:setStatusText( nPart, xValue )
   LOCAL oPanel := ::oSBar:getItem( nPart )

   SWITCH nPart
   CASE SB_PNL_MAIN
      EXIT
   CASE SB_PNL_READY
      EXIT
   CASE SB_PNL_LINE
      EXIT
   CASE SB_PNL_COLUMN
      EXIT
   CASE SB_PNL_INS
      EXIT
   CASE SB_PNL_SELECTEDCHARS
      oPanel:caption := iif( xValue == 0, "", "Sel: " + hb_ntos( xValue ) )
      EXIT
   CASE SB_PNL_MODIFIED
      oPanel:caption := iif( xValue, "Modified", "" )
      EXIT
   CASE SB_PNL_VIEW
      oPanel:caption := "<font color = blue>View: " + xValue + "</font>"
      EXIT
   CASE SB_PNL_ENVIRON
      oPanel:caption := "Env: " + xValue
      EXIT
   CASE SB_PNL_STREAM
      EXIT
   CASE SB_PNL_EDIT
      EXIT
   CASE SB_PNL_SEARCH
      EXIT
   CASE SB_PNL_CODEC
      EXIT
   CASE SB_PNL_PROJECT
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFindInFiles()
   LOCAL oXbp

   ::oIde:oDockFind := XbpWindow():new( ::oDa )
   ::oDockFind:oWidget := QDockWidget():new( ::oDlg:oWidget )
   ::oDockFind:oWidget:setObjectName( "dockFindInFiles" )
   ::oDlg:addChild( ::oDockFind )
   ::oDockFind:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockFind:oWidget:setAllowedAreas( Qt_RightDockWidgetArea )
   ::oDockFind:oWidget:setWindowTitle( "Find in Files" )
   ::oDockFind:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oFindInFiles  := IdeFindInFiles():new( ::oIde, .t. )
   ::oIde:oFindInFiles:lInDockWindow := .t.
   ::oIde:oFindInFiles:create()

   oXbp := XbpWindow():new( ::oDockFind )
   oXbp:qtObject := IdeFindInFiles():new( ::oIde, .t. ):create() //::oIde:oFindInFiles
   oXbp:oWidget  := oXbp:qtObject:oUI
   //::oIde:oFindInFiles  := XbpWindow():new( ::oDockFind )
   //::oDockFind:qtObject := ::oIde:oFindInFiles:oUI
   //::oDockFind:qtObject := HbQtUI():new( ::resPath + "findform.uic", ::oDockFind:oWidget ):build()
   //::oIde:oFindInFiles:oWidget := ::oDockFind:qtObject

   //::oDockFind:oWidget:setWidget( ::oFindInFiles:oWidget )
   //::oDockFind:oWidget:setWidget( ::oIde:oFindInFiles:oUI )
   ::oDockFind:oWidget:setWidget( oXbp:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oDockFind:oWidget, Qt_Horizontal )
   ::oDockFind:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

