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
#define docFunctions_visibilityChanged            306
#define dockDocWriter_visibilityChanged           307
#define docSkeletons_visibilityChanged            308
#define dockSkltnsTree_visibilityChanged          309

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
   METHOD buildViewWidget( cObjectName )
   METHOD buildStackedWidget()
   METHOD buildSearchReplaceWidget()
   METHOD buildDockWidgets()
   METHOD buildProjectTree()
   METHOD buildEditorTree()
   METHOD buildFuncList()
   METHOD buildFunctionsDock()
   METHOD buildSkeletonsTree()
   METHOD buildCompileResults()
   METHOD buildLinkResults()
   METHOD buildOutputResults()
   METHOD buildFindInFiles()
   METHOD buildThemesDock()
   METHOD buildPropertiesDock()
   METHOD buildEnvironDock()
   METHOD buildDocViewer()
   METHOD buildDocWriter()
   METHOD outputDoubleClicked( lSelected )
   METHOD buildStatusBar()
   METHOD setStatusText( nPart, xValue )
   METHOD getMarkWidget( nIndex )
   METHOD dispEnvironment( cEnviron )
   METHOD addPanelButton( cPanel )
   METHOD disblePanelButton( qTBtn )
   METHOD getADockWidget( nArea, cObjectName, cWindowTitle, nFlags )
   METHOD getPanelIcon( cView )
   METHOD animateComponents( nMode )

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
   LOCAL qTBtn

   ::disconnect( ::oOutputResult:oWidget  , "copyAvailable(bool)"     )

   ::disconnect( ::oEnvironDock:oWidget   , "visibilityChanged(bool)" )
   ::disconnect( ::oPropertiesDock:oWidget, "visibilityChanged(bool)" )
   ::disconnect( ::oThemesDock:oWidget    , "visibilityChanged(bool)" )
   ::disconnect( ::oDocViewDock:oWidget   , "visibilityChanged(bool)" )
   ::disconnect( ::oDocWriteDock:oWidget  , "visibilityChanged(bool)" )
   ::disconnect( ::oFindDock:oWidget      , "visibilityChanged(bool)" )
   ::disconnect( ::oFunctionsDock:oWidget , "visibilityChanged(bool)" )
   #if 0  /* Not Implemented */
   ::disconnect( ::oHelpDock:oWidget      , "visibilityChanged(bool)" )
   ::disconnect( ::oDockPT:oWidget        , "visibilityChanged(bool)" )
   ::disconnect( ::oDockED:oWidget        , "visibilityChanged(bool)" )
   ::disconnect( ::oDockB2:oWidget        , "visibilityChanged(bool)" )
   ::disconnect( ::oFuncDock:oWidget      , "visibilityChanged(bool)" )
   ::disconnect( ::oSkeltnDock:oWidget    , "visibilityChanged(bool)" )
   #endif

   FOR EACH qTBtn IN ::aPanels
      ::disconnect( qTBtn, "clicked()" )
      qTBtn := NIL
   NEXT
   FOR EACH qTBtn IN ::aBtnLines
      ::disconnect( qTBtn, "clicked()" )
      qTBtn := NIL
   NEXT

   FOR EACH qTBtn IN ::oIde:aMarkTBtns
      ::disconnect( qTBtn, "clicked()" )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDialog()
   LOCAL s, aSize

   ::oIde:oDlg := XbpDialog():new()
   ::oDlg:icon := hbide_image( "hbide" )
   ::oDlg:title := "Harbour IDE"
   ::oDlg:qtObject := HbQtUI():new( ::resPath + "mainwindow.uic" ):build()
   ::oDlg:create( , , , , , .f. )

   ::oDlg:setStyleSheet( GetStyleSheet( "QMainWindow", ::nAnimantionMode ) )

   ::oDlg:close := {|| hbide_getYesNo( "hbIDE is about to be closed!", "Are you sure?" ) }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )
   ::oDlg:oWidget:setCorner( Qt_BottomLeftCorner, Qt_LeftDockWidgetArea )
   ::oDlg:oWidget:setCorner( Qt_BottomRightCorner, Qt_RightDockWidgetArea )
   ::oDlg:oWidget:resize( 868,470 )

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
   ::buildViewWidget( "Stats" )          /* At stayrtup displaying various statistics */
   ::buildViewWidget( "Main"  )          /* Main Panel to hold editor tabs */
   FOR EACH s IN ::aINI[ INI_VIEWS ]
      ::buildViewWidget( s )             /* All other panels user created */
   NEXT

   ::setView( "Stats" )                  /* Always call with name */

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDockWidgets()

   ::buildProjectTree()
   ::buildEditorTree()

   ::buildFuncList()
   ::buildSkeletonsTree()

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
   ::buildDocWriter()
   ::buildFunctionsDock()

   /* Bottom Docks */
   ::oDlg:oWidget:tabifyDockWidget( ::oDockB:oWidget         , ::oDockB1:oWidget         )
   ::oDlg:oWidget:tabifyDockWidget( ::oDockB1:oWidget        , ::oDockB2:oWidget         )

   /* Right Docks */
   ::oDlg:oWidget:tabifyDockWidget( ::oHelpDock:oWidget      , ::oDocViewDock:oWidget    )
   ::oDlg:oWidget:tabifyDockWidget( ::oDocViewDock:oWidget   , ::oFuncDock:oWidget       )
   ::oDlg:oWidget:tabifyDockWidget( ::oFuncDock:oWidget      , ::oFunctionsDock:oWidget  )
   ::oDlg:oWidget:tabifyDockWidget( ::oFunctionsDock:oWidget , ::oPropertiesDock:oWidget )
   ::oDlg:oWidget:tabifyDockWidget( ::oPropertiesDock:oWidget, ::oEnvironDock:oWidget    )
   ::oDlg:oWidget:tabifyDockWidget( ::oEnvironDock:oWidget   , ::oSkeltnDock:oWidget     )
   ::oDlg:oWidget:tabifyDockWidget( ::oSkeltnDock:oWidget    , ::oThemesDock:oWidget     )
   ::oDlg:oWidget:tabifyDockWidget( ::oThemesDock:oWidget    , ::oFindDock:oWidget       )
   ::oDlg:oWidget:tabifyDockWidget( ::oFindDock:oWidget      , ::oDocWriteDock:oWidget   )

   ::buildToolBarPanels()

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
   oDock:oWidget:setStyleSheet( getStyleSheet( "QDockWidget", ::nAnimantionMode ) )
   oDock:hide()

   RETURN oDock

/*----------------------------------------------------------------------*/

METHOD IdeDocks:execEvent( nMode, p )

   DO CASE

   CASE nMode == 2  /* HelpWidget:contextMenuRequested(qPoint) */
      hbide_popupBrwContextMenu( ::qHelpBrw, p )

   CASE nMode == dockSkltnsTree_visibilityChanged
      IF p; ::oSK:showTree(); ENDIF

   CASE nMode == docSkeletons_visibilityChanged
      IF p; ::oSK:show(); ENDIF

   CASE nMode == dockDocWriter_visibilityChanged
      IF p; ::oDW:show(); ENDIF

   CASE nMode == docFunctions_visibilityChanged
      IF p; ::oFN:show(); ENDIF

   CASE nMode == dockDocViewer_visibilityChanged
      IF p; ::oHL:show(); ENDIF

   CASE nMode == dockProperties_visibilityChanged
      IF p; ::oPM:fetchProperties(); ENDIF

   CASE nMode == docEnvironments_visibilityChanged
      IF p; ::oEV:show(); ENDIF

   CASE nMode == dockFindInFiles_visibilityChanged
      IF p; ::oFF:show(); ENDIF

   CASE nMode == dockThemes_visibilityChanged
      IF p; ::oTH:show(); ENDIF

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:setView( cView )
   LOCAL n, nIndex

   SWITCH cView

   CASE "New..."
      cView := hbide_fetchAString( ::qViewsCombo, cView, "Name the View", "New View" )
      IF cView != "New..." .AND. cView != "Stats" .AND. cView != "Main"
         IF ascan( ::aINI[ INI_VIEWS ], {|e| e == cView } ) > 0
            MsgBox( "View: " + cView + ", already exists" )
         ELSE
            aadd( ::aINI[ INI_VIEWS ], cView )
            ::oTM:addPanelsMenu( cView )
            ::buildViewWidget( cView )
            ::addPanelButton( cView )
            ::setView( cView )
         ENDIF
      ENDIF
      EXIT

   OTHERWISE
      IF ( n := ascan( ::aViews, {|o| o:oWidget:objectName() == cView } ) ) > 0
         ::oIde:cWrkView := cView

         IF !( cView == "Stats" )
            ::oIde:qTabWidget := ::aViews[ n ]:oTabWidget:oWidget
            ::oIde:oTabParent := ::aViews[ n ]

            nIndex := ::oIde:qTabWidget:currentIndex()
            IF nIndex + 1 == ::oIde:qTabWidget:count()
               IF !( ::oIde:lClosing )
                  ::oIde:qTabWidget:setCurrentIndex( 0 )
                  ::oIde:qTabWidget:setCurrentIndex( nIndex )  /* TODO: Must be last saved */
               ENDIF
            ENDIF
         ENDIF
         ::oStackedWidget:oWidget:setCurrentIndex( n - 1 )
         ::setStatusText( SB_PNL_VIEW, ::cWrkView )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildStackedWidget()

   /* Its parent will be drawing area and pages will be XbpTabWidgets() */

   ::oIde:oStackedWidget := XbpWindow():new( ::oDa )
   ::oStackedWidget:oWidget := QStackedWidget():new( ::oDa:oWidget )
   ::oStackedWidget:oWidget:setObjectName( "myStackedWidget" )
   ::oDa:addChild( ::oStackedWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildViewWidget( cObjectName )
   LOCAL oFrame

   oFrame := XbpWindow():new( ::oStackedWidget )
   oFrame:oWidget := QWidget():new( ::oStackedWidget:oWidget )
   oFrame:oWidget:setObjectName( cObjectName )       /* This will form the basis of showing at top */
   ::oStackedWidget:addChild( oFrame )

   oFrame:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   oFrame:qLayout:setContentsMargins( 2, 2, 2, 2 )

   oFrame:oTabWidget := XbpTabWidget():new():create( oFrame, , {0,0}, {200,200}, , .t. )
   #if 0
   IF empty( qTBtn )
      qTBtn := QWidget():new() //QToolButton():new( oFrame:oTabWidget:oWidget )
      qTBtn:setTooltip( "Close Tab" )
      qTBtn:setIcon( hbide_image( "closetab" ) )
      qTBtn:setMaximumWidth( 16 )
      qTBtn:setMaximumHeight( 16 )
   ENDIF
   oFrame:oTabWidget:oWidget:setCornerWidget( qTBtn, Qt_TopLeftCorner )
   #endif
   oFrame:oTabWidget:oWidget:setUsesScrollButtons( .f. )
   oFrame:oTabWidget:oWidget:setMovable( .t. )

   aadd( ::oIde:aViews, oFrame )
   oFrame:oWidget:show()
   oFrame:oTabWidget:oWidget:show()

   ::oStackedWidget:oWidget:addWidget( oFrame:oWidget )
   ::setView( cObjectName )

   RETURN oFrame

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSearchReplaceWidget()

   ::oIde:oSearchReplace := IdeSearchReplace():new( ::oIde ):create()
   ::oSearchReplace:oUI:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildToolBarPanels()
   LOCAL s, qTBtn, a_, aBtns, qAct

   STATIC qSize

   qSize := QSize():new( 20,20 )

   /* Toolbar Panels */

   ::oIde:qTBarPanels := QToolBar():new()
   ::qTBarPanels:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qTBarPanels:setObjectName( "ToolBar_Panels" )
   ::qTBarPanels:setWindowTitle( "ToolBar: Editor Panels" )
   ::qTBarPanels:setAllowedAreas( Qt_LeftToolBarArea )
   ::qTBarPanels:setOrientation( Qt_Vertical )
   ::qTBarPanels:setIconSize( qSize )
   ::qTBarPanels:setMovable( .f. )
   ::qTBarPanels:setFloatable( .f. )

   ::oDlg:oWidget:addToolBar( Qt_LeftToolBarArea, ::qTBarPanels )

   ::addPanelButton( "Main" )
   FOR EACH s IN ::aINI[ INI_VIEWS ]
      ::addPanelButton( s )
   NEXT

   /* Toolbar Line Actions */

   ::oIde:qTBarLines := QToolBar():new()
   ::qTBarLines:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qTBarLines:setObjectName( "ToolBar_Lines" )
   ::qTBarLines:setWindowTitle( "ToolBar: Lines and Blocks" )
   ::qTBarLines:setAllowedAreas( Qt_LeftToolBarArea )
   ::qTBarLines:setOrientation( Qt_Vertical )
   ::qTBarLines:setIconSize( qSize )
   ::qTBarLines:setMovable( .f. )
   ::qTBarLines:setFloatable( .f. )

   ::oDlg:oWidget:addToolBar( Qt_LeftToolBarArea, ::qTBarLines )

   aBtns := {}
   aadd( aBtns, { "movelineup"      , "Move Current Line Up"   , {|| ::oEM:moveLine( -1 )  } } )
   aadd( aBtns, { "movelinedown"    , "Move Current Line Down" , {|| ::oEM:moveLine(  1 )  } } )
   aadd( aBtns, { "deleteline"      , "Delete Current Line"    , {|| ::oEM:deleteLine()    } } )
   aadd( aBtns, { "duplicateline"   , "Duplicate Current Line" , {|| ::oEM:duplicateLine() } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "togglelinenumber", "Toggle Line Numbers"    , ;
                {|| ::oIde:lLineNumbersVisible := ! ::lLineNumbersVisible, ::oEM:toggleLineNumbers() } } )
   FOR EACH a_ IN aBtns
      IF empty( a_ )
         ::qTBarLines:addSeparator()
      ELSE
         qTBtn := QToolButton():new()
         qTBtn:setTooltip( a_[ 2 ] )
         qTBtn:setIcon( hbide_image( a_[ 1 ] ) )
         qTBtn:setMaximumWidth( 20 )
         qTBtn:setMaximumHeight( 20 )
         IF a_[ 1 ] == "togglelinenumber"
            qTBtn:setCheckable( .t. )
         ENDIF
         ::connect( qTBtn, "clicked()", a_[ 3 ] )
         ::qTBarLines:addWidget( qTBtn )
         aadd( ::aBtnLines, qTBtn )
      ENDIF
   NEXT
   ::qTBarLines:addSeparator()

   aBtns := {}
   aadd( aBtns, { "toupper"      , "To Upper"               , {|| ::oEM:convertSelection( "ToUpper" ) } } )
   aadd( aBtns, { "tolower"      , "To Lower"               , {|| ::oEM:convertSelection( "ToLower" ) } } )
   aadd( aBtns, { "invertcase"   , "Invert Case"            , {|| ::oEM:convertSelection( "Invert"  ) } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "blockcomment" , "Block Comment"          , {|| ::oEM:blockComment()                } } )
   aadd( aBtns, { "streamcomment", "Stream Comment"         , {|| ::oEM:streamComment()               } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "blockindentr" , "Indent Right"           , {|| ::oEM:indent(  1 )                  } } )
   aadd( aBtns, { "blockindentl" , "Indent Left"            , {|| ::oEM:indent( -1 )                  } } )
   aadd( aBtns, {} )
   aadd( aBtns, { "sgl2dblquote" , "Single to Double Quotes", {|| ::oEM:convertDQuotes()              } } )
   aadd( aBtns, { "dbl2sglquote" , "Double to Single Quotes", {|| ::oEM:convertQuotes()               } } )
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
   ::qTBarDocks:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qTBarDocks:setObjectName( "ToolBar_Docks" )
   ::qTBarDocks:setWindowTitle( "ToolBar: Dockable Widgets" )
   ::qTBarDocks:setAllowedAreas( Qt_RightToolBarArea )
   ::qTBarDocks:setOrientation( Qt_Vertical )
   ::qTBarDocks:setIconSize( QSize():new( 16,16 ) )
   ::qTBarDocks:setMovable( .f. )
   ::qTBarDocks:setFloatable( .f. )
   ::qTBarDocks:setToolButtonStyle( Qt_ToolButtonIconOnly )

   aBtns := {}
   aadd( aBtns, { ::oDockPT        , "projtree"      } )
   aadd( aBtns, { ::oDockED        , "editstree"     } )
   aadd( aBtns, { ::oSkltnsTreeDock, "projtree"      } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oHelpDock      , "help"          } )
   aadd( aBtns, { ::oDocViewDock   , "harbourhelp"   } )
   aadd( aBtns, { ::oDocWriteDock  , "docwriter"     } )
   aadd( aBtns, { ::oFuncDock      , "dc_function"   } )
   aadd( aBtns, { ::oFunctionsDock , "ffn"           } )
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

METHOD IdeDocks:disblePanelButton( qTBtn )
   LOCAL q

   FOR EACH q IN ::aPanels
      q:setEnabled( !( q == qTBtn ) )
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:getPanelIcon( cView )
   LOCAL n

   IF ( n := ascan( ::aPanels, {|q| q:text() == cView } ) ) > 0
      RETURN hbide_image( "panel_" + hb_ntos( n ) )
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD IdeDocks:addPanelButton( cPanel )
   LOCAL qTBtn

   STATIC nIndex := 0
   nIndex++

   qTBtn := QToolButton():new()
   qTBtn:setMaximumHeight( 20 )
   qTBtn:setMaximumWidth( 20 )
   qTBtn:setText( cPanel )
   qTBtn:setTooltip( "Panel: " + cPanel )
   qTBtn:setIcon( hbide_image( "panel_" + hb_ntos( nIndex ) ) )
   aadd( ::aPanels, qTBtn )
   ::qTBarPanels:addWidget( qTBtn )
   ::connect( qTBtn, "clicked()", {|| ::setView( cPanel ) } )

   nIndex := iif( nIndex >= 7, 0, nIndex )

   IF !empty( ::qViewsCombo )
      ::qViewsCombo:setCurrentIndex( len( ::aPanels ) + 1 )
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildProjectTree()
   LOCAL i, oItem

   ::oIde:oDockPT := ::getADockWidget( Qt_LeftDockWidgetArea, "dockProjectTree", "Projects" )
   ::oDlg:oWidget:addDockWidget_1( Qt_LeftDockWidgetArea, ::oDockPT:oWidget, Qt_Vertical )

   ::oIde:oProjTree := XbpTreeView():new()
   ::oProjTree:hasLines   := .T.
   ::oProjTree:hasButtons := .T.
   ::oProjTree:create( ::oDockPT, , { 0,0 }, { 100,10 }, , .t. )

   ::oProjTree:setStyleSheet( GetStyleSheet( "QTreeWidgetHB", ::nAnimantionMode ) )
   ::oProjTree:oWidget:setMinimumWidth( 100 )
   ::oProjTree:oWidget:setSizePolicy_1( QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )
   ::oProjTree:oWidget:setIconSize( QSize():new( 12,12 ) )
   ::oProjTree:oWidget:setIndentation( 12 )

 * ::oProjTree:itemMarked    := {|oItem| ::manageItemSelected( 0, oItem ), ::oCurProjItem := oItem }
   ::oProjTree:itemMarked    := {|oItem| ::oIde:oCurProjItem := oItem } //, ::oIde:manageFocusInEditor() }
   ::oProjTree:itemSelected  := {|oItem| ::oIde:manageItemSelected( oItem ) }
   ::oProjTree:hbContextMenu := {|mp1, mp2, oXbp| ::oIde:manageProjectContext( mp1, mp2, oXbp ) }

   ::oIde:oProjRoot := ::oProjTree:rootItem:addItem( "Projects" )

   oItem := ::oProjRoot:addItem( "Executables" )
   oItem:oWidget:setIcon( 0, hbide_image( "fl_exe" ) )
   aadd( ::aProjData, { oItem, "Executables", ::oProjRoot, NIL, NIL } )
   oItem := ::oProjRoot:addItem( "Libs" )
   oItem:oWidget:setIcon( 0, hbide_image( "fl_lib" ) )
   aadd( ::aProjData, { oItem, "Libs"       , ::oProjRoot, NIL, NIL } )
   oItem := ::oProjRoot:addItem( "Dlls" )
   oItem:oWidget:setIcon( 0, hbide_image( "fl_dll" ) )
   aadd( ::aProjData, { oItem, "Dlls"       , ::oProjRoot, NIL, NIL } )

   ::oProjRoot:expand( .t. )
   //
   FOR i := 1 TO len( ::aProjects )
      ::oIde:updateProjectTree( ::aProjects[ i, 3 ] )
   NEXT

   /* Insert Project Tree Into Dock Widget */
   ::oDockPT:oWidget:setWidget( ::oProjTree:oWidget )

   ::oDockPT:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildEditorTree()

   ::oIde:oDockED := ::getADockWidget( Qt_LeftDockWidgetArea, "dockEditorTabs", "Editors" )
   ::oDlg:oWidget:addDockWidget_1( Qt_LeftDockWidgetArea, ::oDockED:oWidget, Qt_Vertical )

   ::oIde:oEditTree := XbpTreeView():new()
   ::oEditTree:hasLines   := .T.
   ::oEditTree:hasButtons := .T.
   ::oEditTree:create( ::oDockED, , { 0,0 }, { 100,10 }, , .t. )

   ::oEditTree:oWidget:setSizePolicy_1( QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )
   ::oEditTree:oWidget:setMinimumWidth( 100 )
   ::oEditTree:oWidget:setIconSize( QSize():new( 12,12 ) )
   ::oEditTree:oWidget:setIndentation( 12 )
 * ::oEditTree:oWidget:setRootIsDecorated( .f. )

 * ::oEditTree:itemMarked    := {|oItem| ::manageItemSelected( 0, oItem ), ::oCurProjItem := oItem }
   ::oEditTree:itemMarked    := {|oItem| ::oIde:oCurProjItem := oItem }
   ::oEditTree:itemSelected  := {|oItem| ::oIde:manageItemSelected( oItem ) }
   ::oEditTree:hbContextMenu := {|mp1, mp2, oXbp| ::oIde:manageProjectContext( mp1, mp2, oXbp ) }

   ::oIde:oOpenedSources     := ::oEditTree:rootItem:addItem( "Editors" )
   ::oOpenedSources:expand( .t. )

   /* Insert Project Tree Into Dock Widget */
   ::oDockED:oWidget:setWidget( ::oEditTree:oWidget )

   ::oDockED:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSkeletonsTree()

   ::oIde:oSkltnsTreeDock := ::getADockWidget( Qt_LeftDockWidgetArea, "dockSkltnsTree", "Skeletons" )
   ::oDlg:oWidget:addDockWidget_1( Qt_LeftDockWidgetArea, ::oSkltnsTreeDock:oWidget, Qt_Vertical )

   ::connect( ::oSkltnsTreeDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( dockSkltnsTree_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFuncList()

   ::oIde:oFuncDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockFuncList", "Functions List", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oFuncDock:oWidget, Qt_Vertical )

   ::oIde:oFuncList := XbpListBox():new( ::oFuncDock ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oFuncList:oWidget:setEditTriggers( QAbstractItemView_NoEditTriggers )

   //::oFuncList:ItemMarked := {|mp1, mp2, oXbp| ::gotoFunction( mp1, mp2, oXbp ) }
   ::oFuncList:ItemSelected  := {|mp1, mp2, oXbp| ::oIde:gotoFunction( mp1, mp2, oXbp ) }
   /* Harbour Extension : prefixed with "hb" */
   ::oFuncList:hbContextMenu := {|mp1, mp2, oXbp| ::oIde:manageFuncContext( mp1, mp2, oXbp ) }

   ::oFuncDock:oWidget:setWidget( ::oFuncList:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildHelpWidget()
   LOCAL qUrl, qStr

   ::oIde:oHelpDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockHelp", "hbIDE Help", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oHelpDock:oWidget, Qt_Horizontal )

   ::oIde:qHelpBrw := QTextBrowser():new( ::oHelpDock:oWidget )
   ::qHelpBrw:show()
   ::qHelpBrw:setContextMenuPolicy( Qt_CustomContextMenu )
   ::qHelpBrw:setOpenExternalLinks( .t. )

   qUrl := QUrl():new( "idemainpage.html" )
   qStr := QStringList():new()
   qStr:append( hb_dirBase() + "docs" )

   ::qHelpBrw:setSearchPaths( qStr )
   ::qHelpBrw:setSource( qUrl )

   ::oHelpDock:oWidget:setWidget( ::oIde:qHelpBrw )

   ::oHelpDock:connect( ::qHelpBrw, "customContextMenuRequested(QPoint)", {|p| ::execEvent( 2, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildCompileResults()

   ::oIde:oDockB := ::getADockWidget( Qt_BottomDockWidgetArea, "dockCompileResults", "Compile Results" )
   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB:oWidget, Qt_Horizontal )

   ::oIde:oCompileResult := XbpMLE():new( ::oDockB ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oDockB:oWidget:setWidget( ::oCompileResult:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildLinkResults()

   ::oIde:oDockB1 := ::getADockWidget( Qt_BottomDockWidgetArea, "dockLinkResults", "Link Results" )
   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB1:oWidget, Qt_Horizontal )

   ::oIde:oLinkResult := XbpMLE():new( ::oDockB1 ):create( , , { 0,0 }, { 100, 400 }, , .T. )
   ::oDockB1:oWidget:setWidget( ::oLinkResult:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildOutputResults()

   ::oIde:oDockB2 := ::getADockWidget( Qt_BottomDockWidgetArea, "dockOutputResults", "Output Console" )
   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, ::oDockB2:oWidget, Qt_Horizontal )

   ::oIde:oOutputResult := XbpRtf():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .T. )
   ::oOutputResult:oWidget:setAcceptRichText( .T. )
   ::oOutputResult:oWidget:setReadOnly( .T. )

   ::oDockB2:oWidget:setWidget( ::oOutputResult:oWidget )

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
         ::oIde:qCurEdit:centerCursor()
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
   ::oSBar:addItem( "", , , , "Encoding" ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Environ"  ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "View"     ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Project"  ):oWidget:setMinimumWidth(  20 )
   ::oSBar:addItem( "", , , , "Theme"    ):oWidget:setMinimumWidth(  20 )

   FOR i := 1 TO 6
      ::oSBar:oWidget:addWidget( ::getMarkWidget( i ) )
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildThemesDock()

   ::oIde:oThemesDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockThemes", "Theme Manager", QDockWidget_DockWidgetFloatable )
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

   ::connect( ::oDocViewDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( dockDocViewer_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDocWriter()

   ::oIde:oDocWriteDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockDocWriter", "Documentation Writer", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oDocWriteDock:oWidget, Qt_Horizontal )

   ::connect( ::oDocWriteDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( dockDocWriter_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFunctionsDock()

   ::oIde:oFunctionsDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockFunctions", "Projects Functions Lookup", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oFunctionsDock:oWidget, Qt_Horizontal )

   ::connect( ::oFunctionsDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( docFunctions_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildEnvironDock()

   ::oIde:oEnvironDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockEnvironments", "Compiler Environments", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oEnvironDock:oWidget, Qt_Horizontal )

   ::connect( ::oEnvironDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( docEnvironments_visibilityChanged, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSkeletonWidget()

   ::oIde:oSkeltnDock := ::getADockWidget( Qt_RightDockWidgetArea, "dockSkeleton", "Code Skeletons", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oSkeltnDock:oWidget, Qt_Horizontal )

   ::connect( ::oSkeltnDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( docSkeletons_visibilityChanged, p ) } )

   RETURN Self

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
      xValue := iif( empty( xValue ), "default", xValue )
      oPanel:caption := "<font color = brown >Encoding: "  + xValue + "</font>"
      EXIT
   CASE SB_PNL_ENVIRON
      xValue := iif( empty( xValue ), "default", xValue )
      oPanel:caption := "<font color = blue  >Env: "    + xValue + "</font>"
      EXIT
   CASE SB_PNL_VIEW
      oPanel:caption := "<font color = green >View: "   + xValue + "</font>"
      EXIT
   CASE SB_PNL_PROJECT
      xValue := iif( empty( xValue ), "none", xValue )
      oPanel:caption := "<font color = darkred >Proj: " + xValue + "</font>"
      EXIT
   CASE SB_PNL_THEME
      xValue := iif( empty( xValue ), "Bare Minimum", xValue )
      oPanel:caption := "<font color = blue >Theme: " + xValue + "</font>"
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

METHOD IdeDocks:animateComponents( nMode )
   LOCAL cStyle

   ::oDlg:menubar():setStyleSheet( GetStyleSheet( "QMenuBar", nMode ) )

   ::qTBarPanels:setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )
   ::qTBarLines:setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )
   ::qTBarDocks:setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )

   ::oMainToolbar:setStyleSheet( GetStyleSheet( "QToolBar", nMode ) )

   cStyle := GetStyleSheet( "QDockWidget", nMode )

   ::oDockPT:oWidget:setStyleSheet( cStyle )
   ::oDockED:oWidget:setStyleSheet( cStyle )
   ::oSkltnsTreeDock:oWidget:setStyleSheet( cStyle )
   ::oHelpDock:oWidget:setStyleSheet( cStyle )
   ::oDocViewDock:oWidget:setStyleSheet( cStyle )
   ::oDocWriteDock:oWidget:setStyleSheet( cStyle )
   ::oFuncDock:oWidget:setStyleSheet( cStyle )
   ::oFunctionsDock:oWidget:setStyleSheet( cStyle )
   ::oPropertiesDock:oWidget:setStyleSheet( cStyle )
   ::oEnvironDock:oWidget:setStyleSheet( cStyle )
   ::oSkeltnDock:oWidget:setStyleSheet( cStyle )
   ::oThemesDock:oWidget:setStyleSheet( cStyle )
   ::oFindDock:oWidget:setStyleSheet( cStyle )
   ::oDockB2:oWidget:setStyleSheet( cStyle )

   #if 0
   // should be iteration
   ::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget", nMode ) )
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/
