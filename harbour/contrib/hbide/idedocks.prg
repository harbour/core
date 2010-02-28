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

#define docEnvironments_visibilityChanged         301
#define dockFindInFiles_visibilityChanged         302
#define dockThemes_visibilityChanged              303
#define dockProperties_visibilityChanged          304
#define dockDocViewer_visibilityChanged           305

/*----------------------------------------------------------------------*/

CLASS IdeDocks INHERIT IdeObject

   DATA   nPass                                   INIT   0
   DATA   aPanels                                 INIT   {}
   DATA   aBtnLines                               INIT   {}
   DATA   aBtnDocks                               INIT   {}
   DATA   oBtnTabClose

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD execEvent( nMode, p )
   METHOD setView( cView )
   METHOD buildToolBarPanels()
   METHOD buildHelpWidget()
   METHOD buildSkeletonWidget()
   METHOD buildDialog()
   METHOD buildViewWidget()
   METHOD buildStackedWidget()
   METHOD buildSearchReplaceWidget()
   METHOD buildDockWidgets()
   METHOD buildProjectTree()
   METHOD buildEditorTree()
   METHOD buildFuncList()
   METHOD buildCompileResults()
   METHOD buildLinkResults()
   METHOD buildOutputResults()
   METHOD buildFindInFiles()
   METHOD buildThemesDock()
   METHOD buildPropertiesDock()
   METHOD buildEnvironDock()
   METHOD buildDocViewer()
   METHOD outputDoubleClicked( lSelected )
   METHOD buildStatusBar()
   METHOD toggleLeftDocks()
   METHOD toggleRightDocks()
   METHOD toggleBottomDocks()
   METHOD setStatusText( nPart, xValue )
   METHOD getMarkWidget( nIndex )
   METHOD dispEnvironment( cEnviron )
   METHOD execSkeleton( nMode, p )
   METHOD addPanelButton( cPanel )
   METHOD disblePanelButton( qTBtn )
   METHOD getADockWidget( nArea, cObjectName, cWindowTitle, nFlags )

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
   LOCAL oUI := ::oIde:oSkeltnUI
   LOCAL qTBtn

   ::disconnect( oUI:q_buttonNew   , "clicked()" )
   ::disconnect( oUI:q_buttonRename, "clicked()" )
   ::disconnect( oUI:q_buttonDelete, "clicked()" )
   ::disconnect( oUI:q_buttonClear , "clicked()" )
   ::disconnect( oUI:q_buttonGetSel, "clicked()" )
   ::disconnect( oUI:q_buttonUpdate, "clicked()" )
   ::disconnect( oUI:q_listNames   , "itemSelectionChanged()" )

   oUI:destroy()

   FOR EACH qTBtn IN ::aPanels
      ::disconnect( qTBtn, "clicked()" )
      qTBtn := NIL
   NEXT
   FOR EACH qTBtn IN ::aBtnLines
      ::disconnect( qTBtn, "clicked()" )
      qTBtn := NIL
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:getADockWidget( nArea, cObjectName, cWindowTitle, nFlags )
   LOCAL oDock, nBasic

   DEFAULT nFlags TO 0

   nBasic := hb_bitOR( QDockWidget_DockWidgetClosable, nFlags )

   oDock := XbpWindow():new()
   oDock:oWidget := QDockWidget():new( ::oDlg:oWidget )
   oDock:oWidget:setObjectName( cObjectName )
   ::oDlg:addChild( oDock )
   oDock:oWidget:setFeatures( nBasic )
   oDock:oWidget:setAllowedAreas( nArea )
   oDock:oWidget:setWindowTitle( cWindowTitle )
   oDock:oWidget:setFocusPolicy( Qt_NoFocus )
   oDock:oWidget:setStyleSheet( getStyleSheet( "QDockWidget" ) )
   oDock:hide()

   oDock:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   oDock:qLayout:setContentsMargins( 2, 2, 2, 2 )

   RETURN oDock

/*----------------------------------------------------------------------*/

   METHOD IdeDocks:buildDockWidgets()

   ::buildProjectTree()
   ::buildEditorTree()

   ::buildFuncList()

   ::buildHelpWidget()
   ::buildSkeletonWidget()
   ::buildFindInFiles()
   ::buildThemesDock()
   ::buildPropertiesDock()
   ::buildEnvironDock()

   ::buildCompileResults()
   ::buildLinkResults()
   ::buildOutputResults()
   ::buildDocViewer()

   ::oDlg:oWidget:tabifyDockWidget( ::oDockB:oWidget         , ::oDockB1:oWidget         )
   ::oDlg:oWidget:tabifyDockWidget( ::oDockB1:oWidget        , ::oDockB2:oWidget         )

   ::oDlg:oWidget:tabifyDockWidget( ::oHelpDock:oWidget      , ::oSkeltnDock:oWidget     )
   ::oDlg:oWidget:tabifyDockWidget( ::oSkeltnDock:oWidget    , ::oFindDock:oWidget       )
   ::oDlg:oWidget:tabifyDockWidget( ::oFindDock:oWidget      , ::oThemesDock:oWidget     )
   ::oDlg:oWidget:tabifyDockWidget( ::oThemesDock:oWidget    , ::oPropertiesDock:oWidget )
   ::oDlg:oWidget:tabifyDockWidget( ::oPropertiesDock:oWidget, ::oEnvironDock:oWidget    )
   ::oDlg:oWidget:tabifyDockWidget( ::oEnvironDock:oWidget   , ::oFuncDock:oWidget       )

   ::buildToolBarPanels()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:execEvent( nMode, p )
   LOCAL nIndex, aMenu

   DO CASE
   CASE nMode == 1    /* StackedWidget:currentChanged(int) */
      IF p >= 0 .AND. p <= len( ::aViews )
         ::oIde:nCurView := p

         ::oIde:qTabWidget := ::aViews[ ::nCurView + 1 ]:oTabWidget:oWidget
         ::oIde:oTabParent := ::aViews[ ::nCurView + 1 ]

         nIndex := ::oIde:qTabWidget:currentIndex()
         IF nIndex + 1 == ::oIde:qTabWidget:count()
            IF !( ::oIde:lClosing )
               ::oIde:qTabWidget:setCurrentIndex( 0 )
               ::oIde:qTabWidget:setCurrentIndex( nIndex )  /* TODO: Must be last saved */
            ENDIF
         ENDIF
         ::setStatusText( SB_PNL_VIEW, iif( p == 0, "Main", ::aINI[ INI_VIEWS, ::nCurView ] ) )
      ENDIF

   CASE nMode == 2  /* HelpWidget:contextMenuRequested(qPoint) */
      aMenu := {}

      aadd( aMenu, { "Back"      , {|| ::qHelpBrw:backward()  } } )
      aadd( aMenu, { "Forward"   , {|| ::qHelpBrw:forward()   } } )
      aadd( aMenu, { "Home"      , {|| ::qHelpBrw:home()      } } )
      aadd( aMenu, { "" } )
      aadd( aMenu, { "Reload"    , {|| ::qHelpBrw:reload()    } } )
      aadd( aMenu, { "" } )
      aadd( aMenu, { "Select All", {|| ::qHelpBrw:selectAll() } } )
      aadd( aMenu, { "Copy"      , {|| ::qHelpBrw:copy()      } } )

      hbide_execPopup( aMenu, p, ::qHelpBrw )

   CASE nMode == dockDocViewer_visibilityChanged
      IF p
         ::oDocViewDock:qtObject:show()
      ENDIF

   CASE nMode == dockProperties_visibilityChanged
      IF p
         ::oPM:fetchProperties()
      ENDIF

   CASE nMode == docEnvironments_visibilityChanged
      IF p
         ::oPM:manageEnvironments()
      ENDIF

   CASE nMode == dockFindInFiles_visibilityChanged
      IF p
         ::oFindInFiles:show()
      ENDIF

   CASE nMode == dockThemes_visibilityChanged
      IF p
         ::oThemes:show()
      ENDIF

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDialog()
   LOCAL s, aSize

   ::oIde:oDlg := XbpDialog():new()
   ::oDlg:icon := hbide_image( "hbide" )
   ::oDlg:title := "Harbour IDE"
   ::oDlg:qtObject := HbQtUI():new( ::resPath + "mainwindow.uic" ):build()
   ::oDlg:create( , , , , , .f. )

   ::oDlg:setStyleSheet( GetStyleSheet( "QMainWindow" ) )

   ::oDlg:close := {|| hbide_getYesNo( "hbIDE is about to be closed!", "Are you sure?" ) }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )
   ::oDlg:oWidget:setCorner( Qt_BottomLeftCorner, Qt_LeftDockWidgetArea )
   ::oDlg:oWidget:setCorner( Qt_BottomRightCorner, Qt_RightDockWidgetArea )
   ::oDlg:oWidget:resize( 850,430 )

   ::oIde:oDa := ::oDlg:drawingArea

   SetAppWindow( ::oDlg )

   // Center on Desktop and decorate
   aSize := AppDesktop():currentSize()
   ::oDlg:setPos( { ( aSize[ 1 ] - ::oDlg:currentSize()[ 1 ] ) / 2, ;
                    ( aSize[ 2 ] - ::oDlg:currentSize()[ 2 ] ) / 2 } )

   ::oIde:setPosAndSizeByIni( ::oDlg:oWidget, MainWindowGeometry )
   //::oDlg:Show()

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
//   ::buildSearchReplaceWidget()      ////////////////////////////////////
//   ::qLayout:addWidget_1( ::oSearchReplace:oUI, 1, 0, 1, 1 )

   /* View Panels */
   ::buildViewWidget()      /* Main */
   FOR EACH s IN ::aINI[ INI_VIEWS ]
      ::buildViewWidget()
   NEXT

   /* Force to populate current widget */
   ::oStackedWidget:oWidget:setCurrentIndex( 0 )

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

METHOD IdeDocks:buildSearchReplaceWidget()

   ::oIde:oSearchReplace := IdeSearchReplace():new( ::oIde ):create()
   ::oSearchReplace:oUI:hide()

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

METHOD IdeDocks:disblePanelButton( qTBtn )
   LOCAL q

   FOR EACH q IN ::aPanels
      q:setEnabled( !( q == qTBtn ) )
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:addPanelButton( cPanel )
   LOCAL qTBtn, aColors, nIndex, cColor

 * aColors := { "#996633", "#A37547", "#AD855C", "#B89470", "#C2A385", "#CCB299", "#D6C2AD", "#E0D1C2", "#EBE0D6", "#F5F0EB" }
   aColors := { "#98FB98","#20B2AA","#6B8E23","#9ACD32","#FFFF00","#FF00FF","#FFA500","#4169E1","#00FF7F","#FFFF00" }

   IF cPanel == "Main"
      nIndex := 1
      cColor := "#008000"
   ELSE
      nIndex := len( ::aPanels )
      IF nIndex > len( aColors )
         nIndex := nIndex - len( aColors )
      ENDIF
      cColor := aColors[ nIndex ]
   ENDIF

   qTBtn := QToolButton():new()
   qTBtn:setMaximumHeight( 12 )
   qTBtn:setMaximumWidth( 18 )
   qTBtn:setTooltip( "Panel: " + cPanel )
   qTBtn:setStyleSheet( "background-color: " + cColor + " ;" )

   ::connect( qTBtn, "clicked()", {|| ::disblePanelButton( qTBtn ), ::setView( cPanel ) } )
   ::qTBarPanels:addWidget( qTBtn )
   ::qTBarPanels:addSeparator()

   aadd( ::aPanels, qTBtn )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildToolBarPanels()
   LOCAL s, qTBtn, a_, aBtns, qAct

   STATIC qSize

   qSize := QSize():new( 20,20 )

   /* Toolbar Panels */

   ::oIde:qTBarPanels := QToolBar():new()
   ::qTBarPanels:setObjectName( "ToolBar_Panels" )
   ::qTBarPanels:setWindowTitle( "ToolBar: Editor Panels" )
   ::qTBarPanels:setAllowedAreas( Qt_LeftToolBarArea )
   ::qTBarPanels:setOrientation( Qt_Vertical )
   ::qTBarPanels:setIconSize( qSize )
   ::qTBarPanels:setMovable( .f. )
   ::qTBarPanels:setFloatable( .f. )
   ::qTBarPanels:setStyleSheet( "QToolBar { spacing: 1px; color: white; margin-top: 2px; }" )

   ::oDlg:oWidget:addToolBar( Qt_LeftToolBarArea, ::qTBarPanels )

   ::addPanelButton( "Main" )
   FOR EACH s IN ::aINI[ INI_VIEWS ]
      ::addPanelButton( s )
   NEXT

   /* Toolbar Line Actions */

   ::oIde:qTBarLines := QToolBar():new()
   ::qTBarLines:setObjectName( "ToolBar_Lines" )
   ::qTBarLines:setWindowTitle( "ToolBar: Lines and Blocks" )
   ::qTBarLines:setAllowedAreas( Qt_LeftToolBarArea )
   ::qTBarLines:setOrientation( Qt_Vertical )
   ::qTBarLines:setIconSize( qSize )
   ::qTBarLines:setMovable( .f. )
   ::qTBarLines:setFloatable( .f. )

   ::oDlg:oWidget:addToolBar( Qt_LeftToolBarArea, ::qTBarLines )

   aBtns := {}
   aadd( aBtns, { "movelineup"   , "Move Current Line Up"   , {|| ::oEM:moveLine( -1 )  } } )
   aadd( aBtns, { "movelinedown" , "Move Current Line Down" , {|| ::oEM:moveLine(  1 )  } } )
   aadd( aBtns, { "deleteline"   , "Delete Current Line"    , {|| ::oEM:deleteLine()    } } )
   aadd( aBtns, { "duplicateline", "Duplicate Current Line" , {|| ::oEM:duplicateLine() } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "togglelinenumber", "Toggle Line Numbers" , {|| ::oEM:toggleLineNumbers()  } } )
   FOR EACH a_ IN aBtns
      IF empty( a_ )
         ::qTBarLines:addSeparator()
      ELSE
         qTBtn := QToolButton():new()
         qTBtn:setTooltip( a_[ 2 ] )
         qTBtn:setIcon( ::resPath + a_[ 1 ] + ".png" )
         qTBtn:setMaximumWidth( 20 )
         qTBtn:setMaximumHeight( 20 )
         ::connect( qTBtn, "clicked()", a_[ 3 ] )
         ::qTBarLines:addWidget( qTBtn )
         aadd( ::aBtnLines, qTBtn )
      ENDIF
   NEXT
   ::qTBarLines:addSeparator()

   aBtns := {}
   aadd( aBtns, { "toupper"       , "To Upper"               , {|| ::oEM:convertSelection( "ToUpper" ) } } )
   aadd( aBtns, { "tolower"       , "To Lower"               , {|| ::oEM:convertSelection( "ToLower" ) } } )
   aadd( aBtns, { "invertcase"    , "Invert Case"            , {|| ::oEM:convertSelection( "Invert"  ) } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "blockcomment"  , "Block Comment"          , {|| ::oEM:blockComment()   } } )
   aadd( aBtns, { "streamcomment" , "Stream Comment"         , {|| ::oEM:streamComment()  } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "blockindentr"  , "Indent Right"           , {|| ::oEM:indent( 1 )      } } )
   aadd( aBtns, { "blockindentl"  , "Indent Left"            , {|| ::oEM:indent( -1 )     } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "sgl2dblquote"  , "Single to Double Quotes", {|| ::oEM:convertDQuotes() } } )
   aadd( aBtns, { "dbl2sglquote"  , "Double to Single Quotes", {|| ::oEM:convertQuotes()  } } )
   FOR EACH a_ IN aBtns
      IF empty( a_ )
         ::qTBarLines:addSeparator()
      ELSE
         qTBtn := QToolButton():new()
         qTBtn:setTooltip( a_[ 2 ] )
         qTBtn:setIcon( ::resPath + a_[ 1 ] + ".png" )
         qTBtn:setMaximumWidth( 20 )
         qTBtn:setMaximumHeight( 20 )
         ::connect( qTBtn, "clicked()", a_[ 3 ] )
         ::qTBarLines:addWidget( qTBtn )
         aadd( ::aBtnLines, qTBtn )
      ENDIF
   NEXT
   ::qTBarLines:addSeparator()

   /* Right-hand docks toolbar */
   ::oIde:qTBarDocks := QToolBar():new()
   ::qTBarDocks:setObjectName( "ToolBar_Docks" )
   ::qTBarDocks:setWindowTitle( "ToolBar: Dockable Widgets" )
   ::qTBarDocks:setAllowedAreas( Qt_RightToolBarArea )
   ::qTBarDocks:setOrientation( Qt_Vertical )
   ::qTBarDocks:setIconSize( QSize():new( 16,16 ) )
   ::qTBarDocks:setMovable( .f. )
   ::qTBarDocks:setFloatable( .f. )
   ::qTBarDocks:setStyleSheet( "QToolBar { spacing: 1px; color: white; margin-top: 2px; }" )
   ::qTBarDocks:setToolButtonStyle( Qt_ToolButtonIconOnly )

   aBtns := {}
   aadd( aBtns, { ::oHelpDock      , "help"          } )
   aadd( aBtns, { ::oDocViewDock   , "harbourhelp"   } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oDockPT        , "projectstree"  } )
   aadd( aBtns, { ::oDockED        , "tabs"          } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oFuncDock      , "dc_function"   } )
   aadd( aBtns, { ::oPropertiesDock, "properties"    } )
   aadd( aBtns, { ::oEnvironDock   , "envconfig"     } )
   aadd( aBtns, { ::oSkeltnDock    , "codeskeletons" } )
   aadd( aBtns, { ::oThemesDock    , "syntaxhiliter" } )
   aadd( aBtns, { ::oFindDock      , "search"        } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oDockB2        , "builderror"    } )
 * aadd( aBtns, { ::oDockB1        , "builderror"    } )
 * aadd( aBtns, { ::oDockB         , "builderror"    } )

   FOR EACH a_ IN aBtns
      IF empty( a_ )
         ::qTBarDocks:addSeparator()
      ELSE
         qAct := QAction():from( a_[ 1 ]:oWidget:toggleViewAction() )
         qAct:setIcon( hbide_image( a_[ 2 ] ) )
         ::qTBarDocks:addAction( qAct )
         aadd( ::aBtnDocks, qAct )
      ENDIF
   NEXT

   ::oDlg:oWidget:addToolBar( Qt_RightToolBarArea, ::qTBarDocks )

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
   ::oDockPT:oWidget:setWindowTitle( "Projects Tree" )
   ::oDockPT:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oIde:oProjTree := XbpTreeView():new()
   ::oProjTree:hasLines   := .T.
   ::oProjTree:hasButtons := .T.
   ::oProjTree:create( ::oDockPT, , { 0,0 }, { 100,10 }, , .t. )

   ::oProjTree:setStyleSheet( GetStyleSheet( "QTreeWidgetHB" ) )
   ::oProjTree:oWidget:setMinimumWidth( 100 )
   ::oProjTree:oWidget:setSizePolicy_1( QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )

 * ::oProjTree:itemMarked    := {|oItem| ::manageItemSelected( 0, oItem ), ::oCurProjItem := oItem }
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
   ::oEditTree:create( ::oDockED, , { 0,0 }, { 100,10 }, , .t. )

   ::oEditTree:oWidget:setSizePolicy_1( QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )
   ::oEditTree:oWidget:setMinimumWidth( 100 )

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

   ::oIde:oFuncDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockFuncList", "Functions List", QDockWidget_DockWidgetFloatable )

   ::oIde:oFuncList := XbpListBox():new( ::oFuncDock ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oFuncList:oWidget:setEditTriggers( QAbstractItemView_NoEditTriggers )

   //::oFuncList:ItemMarked := {|mp1, mp2, oXbp| ::gotoFunction( mp1, mp2, oXbp ) }
   ::oFuncList:ItemSelected  := {|mp1, mp2, oXbp| ::oIde:gotoFunction( mp1, mp2, oXbp ) }
   /* Harbour Extension : prefixed with "hb" */
   ::oFuncList:hbContextMenu := {|mp1, mp2, oXbp| ::oIde:manageFuncContext( mp1, mp2, oXbp ) }

   ::oFuncDock:oWidget:setWidget( ::oFuncList:oWidget )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oFuncDock:oWidget, Qt_Vertical )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildHelpWidget()
   LOCAL qUrl, qStr

   qUrl := QUrl():new( "idemainpage.html" )
   qStr := QStringList():new()
   qStr:append( hb_dirBase() + "docs" )

   ::oIde:oHelpDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockHelp", "hbIDE Help", QDockWidget_DockWidgetFloatable )

   ::oIde:qHelpBrw := QTextBrowser():new( ::oHelpDock:oWidget )
   ::qHelpBrw:show()
   ::qHelpBrw:setContextMenuPolicy( Qt_CustomContextMenu )
   ::qHelpBrw:setOpenExternalLinks( .t. )

   ::qHelpBrw:setSearchPaths( qStr )
   ::qHelpBrw:setSource( qUrl )

   ::oHelpDock:oWidget:setWidget( ::oIde:qHelpBrw )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oHelpDock:oWidget, Qt_Horizontal )

   ::oHelpDock:connect( ::qHelpBrw, "customContextMenuRequested(QPoint)", {|p| ::execEvent( 2, p ) } )

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
   ::oDockB2:hide()

   ::oIde:oOutputResult := XbpRtf():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .T. )
   ::oOutputResult:oWidget:setAcceptRichText( .T. )
   ::oOutputResult:oWidget:setReadOnly( .T. )

   ::oDockB2:oWidget:setWidget( ::oOutputResult:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB2:oWidget, Qt_Horizontal )

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
   ::oSBar:addItem( "", , , , "Ins"      ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "SelChar"  ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Modified" ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Stream"   ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Edit"     ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Search"   ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Codec"    ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Environ"  ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "View"     ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Project"  ):oWidget:setMinimumWidth(  20 )

   FOR i := 1 TO 6
      ::oSBar:oWidget:addWidget( ::getMarkWidget( i ) )
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildThemesDock()

   ::oIde:oThemesDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockThemes", "Editor Themes", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oThemesDock:oWidget, Qt_Horizontal )

   ::connect( ::oThemesDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( dockThemes_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildPropertiesDock()

   ::oIde:oPropertiesDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockProperties", "Project Properties", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oPropertiesDock:oWidget, Qt_Horizontal )

   ::connect( ::oPropertiesDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( dockProperties_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFindInFiles()

   ::oIde:oFindDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockFindInFiles", "Find in Files", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oFindDock:oWidget, Qt_Horizontal )

   ::connect( ::oFindDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( dockFindInFiles_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDocViewer()

   ::oIde:oDocViewDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockDocViewer", "Harbour Documentation", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oDocViewDock:oWidget, Qt_Horizontal )

   ::oDocViewDock:qtObject := ideHarbourHelp():new():create( ::oIde )

   ::connect( ::oDocViewDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( dockDocViewer_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSkeletonWidget()
   LOCAL oUI

   ::oIde:oSkeltnDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockSkeleton", "Code Skeletons", QDockWidget_DockWidgetFloatable )

   ::oIde:oSkeltnUI := HbQtUI():new( ::oIde:resPath + "skeletons.uic", ::oSkeltnDock:oWidget ):build()

   ::oSkeltnDock:oWidget:setWidget( ::oIde:oSkeltnUI:oWidget )

   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oSkeltnDock:oWidget, Qt_Horizontal )

   oUI := ::oIde:oSkeltnUI

   ::connect( oUI:q_buttonNew   , "clicked()", {|| ::execSkeleton( 1 ) } )
   ::connect( oUI:q_buttonRename, "clicked()", {|| ::execSkeleton( 2 ) } )
   ::connect( oUI:q_buttonDelete, "clicked()", {|| ::execSkeleton( 3 ) } )
   ::connect( oUI:q_buttonClear , "clicked()", {|| ::execSkeleton( 4 ) } )
   ::connect( oUI:q_buttonGetSel, "clicked()", {|| ::execSkeleton( 5 ) } )
   ::connect( oUI:q_buttonUpdate, "clicked()", {|| ::execSkeleton( 6 ) } )
   ::connect( oUI:q_listNames   , "itemSelectionChanged()", {|| ::execSkeleton( 7 ) } )

   //::oSkeltnUI:q_editCode:setFontFamily( "Courier New" )
   //::oSkeltnUI:q_editCode:setFontPointSize( 10 )

   //::oSkeltnUI:q_editCode:setFont( ::oFont:oWidget )
   aeval( ::aSkltns, {|e_| ::oSkeltnUI:q_listNames:addItem( e_[ 1 ] ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:execSkeleton( nMode, p )
   LOCAL cName, cNewName, qItem, cCode, n

   HB_SYMBOL_UNUSED( p )

   SWITCH nMode

   CASE 1
      IF !empty( cName := hbide_fetchAString( ::oSkeltnUI:q_listNames, "", "Name", "New Skeleton" ) )
         ::oSkeltnUI:q_listNames:addItem( cName )
         aadd( ::oIde:aSkltns, { cName, "" } )
         ::oSkeltnUI:q_listNames:setCurrentRow( len( ::aSkltns ) - 1 )
      ENDIF
      EXIT
   CASE 2
      qItem := QListWidgetItem():configure( ::oSkeltnUI:q_listNames:currentItem() )
      cName := qItem:text()
      IF !empty( cNewName := hbide_fetchAString( ::oSkeltnUI:q_listNames, cName, "Name", "Change Skeleton's Name" ) )
         qItem:setText( cNewName )
         n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cName } )
         ::aSkltns[ n, 1 ] := cNewName
      ENDIF
      EXIT
   CASE 3
      qItem := QListWidgetItem():configure( ::oSkeltnUI:q_listNames:currentItem() )
      ::oSkeltnUI:q_listNames:removeItemWidget( qItem )
      EXIT
   CASE 4
      ::oSkeltnUI:q_editCode:clear()
      EXIT
   CASE 5
      IF !empty( cCode := ::oEM:getSelectedText() )
         // TODO: Format cCode
         cCode := strtran( cCode, chr( 0x2029 ), chr( 10 ) )
         ::oSkeltnUI:q_editCode:setPlainText( cCode )
      ENDIF
      EXIT
   CASE 6
      // Update the skeleton code and save the skeleton's buffer | file
      qItem := QListWidgetItem():configure( ::oSkeltnUI:q_listNames:currentItem() )
      cName := qItem:text()
      IF !empty( cName )
         n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cName } )
         ::aSkltns[ n,2 ] := ::oSkeltnUI:q_editCode:toPlainText()

         hbide_saveSkltns( ::oIde )
      ENDIF
      EXIT
   CASE 7
      qItem := QListWidgetItem():configure( ::oSkeltnUI:q_listNames:currentItem() )
      cName := qItem:text()
      n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cName } )
      ::oSkeltnUI:q_editCode:setPlainText( ::aSkltns[ n,2 ] )
      EXIT
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeDocks:setStatusText( nPart, xValue )
   LOCAL oPanel := ::oSBar:getItem( nPart )

   SWITCH nPart

   CASE SB_PNL_MAIN
      oPanel:caption := '<font color="2343212"><b>' + xValue + "</b></font>"
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
      oPanel:caption := xValue
      EXIT
   CASE SB_PNL_STREAM
      EXIT
   CASE SB_PNL_EDIT
      EXIT
   CASE SB_PNL_SEARCH
      oPanel:caption := "Find: " + xValue
      EXIT
   CASE SB_PNL_CODEC
      oPanel:caption := "<font color = brown >Codec: "  + xValue + "</font>"
      EXIT
   CASE SB_PNL_ENVIRON
      oPanel:caption := "<font color = blue  >Env: "    + xValue  + "</font>"
      EXIT
   CASE SB_PNL_VIEW
      oPanel:caption := "<font color = green >View: "   + xValue + "</font>"
      EXIT
   CASE SB_PNL_PROJECT
      oPanel:caption := "<font color = darkred >Proj: "     + xValue + "</font>"
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:dispEnvironment( cEnviron )
   ::setStatusText( SB_PNL_ENVIRON, cEnviron )
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
      ::oFuncDock:hide()
   ELSE
      ::oFuncDock:show()
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

METHOD IdeDocks:buildEnvironDock()

   ::oIde:oEnvironDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockEnvironments", "Compiler Environments", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oEnvironDock:oWidget, Qt_Horizontal )

   ::connect( ::oEnvironDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( docEnvironments_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/
