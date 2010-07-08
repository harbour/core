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
 *                               28Dec2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS IdeDocks INHERIT IdeObject

   DATA   nPass                                   INIT   0
   DATA   aPanels                                 INIT   {}
   DATA   aBtnLines                               INIT   {}
   DATA   aBtnDocks                               INIT   {}
   DATA   oBtnTabClose

   DATA   qTBtnClose

   DATA   lChanging                               INIT   .f.

   DATA   qTimer
   DATA   nPrevWindowState
   DATA   lSystemTrayAvailable                    INIT   .f.
   DATA   lMinimizeInSystemTray                   INIT   .t.  // .f.
   DATA   qAct1
   DATA   qAct2

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD execEvent( cEvent, p, p1 )
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
   METHOD getADockWidget( nAreas, cObjectName, cWindowTitle, nFlags, cEventVisibility )
   METHOD getPanelIcon( cView )
   METHOD animateComponents( nMode )
   METHOD buildSourceThumbnail()
   METHOD buildQScintilla()
   METHOD buildUpDownWidget()
   METHOD buildSystemTray()
   METHOD showDlgBySystemTrayIconCommand()
   METHOD setViewInitials( cView )

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

   ::oIde:oProjRoot      := NIL
   ::oIde:oOpenedSources := NIL

   ::disconnect( ::oOutputResult:oWidget  , "copyAvailable(bool)"     )

   ::disconnect( ::oEnvironDock:oWidget   , "visibilityChanged(bool)" )
   ::disconnect( ::oPropertiesDock:oWidget, "visibilityChanged(bool)" )
   ::disconnect( ::oThemesDock:oWidget    , "visibilityChanged(bool)" )
   ::disconnect( ::oDocViewDock:oWidget   , "visibilityChanged(bool)" )
   ::disconnect( ::oDocWriteDock:oWidget  , "visibilityChanged(bool)" )
   ::disconnect( ::oFindDock:oWidget      , "visibilityChanged(bool)" )
   ::disconnect( ::oFunctionsDock:oWidget , "visibilityChanged(bool)" )
   ::disconnect( ::oSkeltnDock:oWidget    , "visibilityChanged(bool)" )
   ::disconnect( ::oHelpDock:oWidget      , "visibilityChanged(bool)" )
   ::disconnect( ::oFuncDock:oWidget      , "visibilityChanged(bool)" )

   ::disconnect( ::oSourceThumbnailDock:oWidget, "visibilityChanged(bool)" )
   ::disconnect( ::oQScintillaDock:oWidget, "visibilityChanged(bool)" )

   #if 0  /* Not Implemented */
   ::disconnect( ::oDockPT:oWidget        , "visibilityChanged(bool)" )
   ::disconnect( ::oDockED:oWidget        , "visibilityChanged(bool)" )
   ::disconnect( ::oDockB2:oWidget        , "visibilityChanged(bool)" )
   #endif

   IF !empty( ::oSys )
      ::disconnect( ::oSys                , "activated(QSystemTrayIcon::ActivationReason)" )
      ::disconnect( ::qAct1               , "triggered(bool)"         )
      ::disconnect( ::qAct2               , "triggered(bool)"         )
   ENDIF

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
   ::oDlg:qtObject := hbide_getUI( "mainwindow" )
   ::oDlg:create( , , , , , .f. )

   ::oDlg:setStyleSheet( GetStyleSheet( "QMainWindow", ::nAnimantionMode ) )

   ::oDlg:close := {|| hbide_getYesNo( "hbIDE is about to be closed!", "Are you sure?" ) }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )
   ::oDlg:oWidget:setCorner( Qt_BottomLeftCorner, Qt_LeftDockWidgetArea )
   ::oDlg:oWidget:setCorner( Qt_BottomRightCorner, Qt_RightDockWidgetArea )
   ::oDlg:oWidget:resize( 900,470 )

   ::oIde:oDa := ::oDlg:drawingArea

   SetAppWindow( ::oDlg )

   // Center on Desktop and decorate
   aSize := AppDesktop():currentSize()
   ::oDlg:setPos( { ( aSize[ 1 ] - ::oDlg:currentSize()[ 1 ] ) / 2, ;
                    ( aSize[ 2 ] - ::oDlg:currentSize()[ 2 ] ) / 2 } )
   ::oIde:setPosAndSizeByIniEx( ::oDlg:oWidget, ::oINI:cMainWindowGeometry )

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
   #if 1
   ::buildSearchReplaceWidget()
   ::qLayout:addWidget_1( ::oSearchReplace:oUI, 1, 0, 1, 1 )
   #endif

   /* View Panels */
   ::buildViewWidget( "Stats" )          /* At stayrtup displaying various statistics */
   ::buildViewWidget( "Main"  )          /* Main Panel to hold editor tabs */
   FOR EACH s IN ::oINI:aViews
      ::buildViewWidget( s )             /* All other panels user created */
   NEXT

   ::setView( "Stats" )                  /* Always call with name */

   ::oDlg:connectEvent( ::oDlg:oWidget, QEvent_WindowStateChange, {|e| ::execEvent( "QEvent_WindowStateChange", e ) } )
   ::oDlg:connectEvent( ::oDlg:oWidget, QEvent_Hide             , {|e| ::execEvent( "QEvent_Hide"             , e ) } )

   #if 1
   ::buildSystemTray()
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSystemTray()

   IF empty( ::oSys )
      ::oIde:oSys := QSystemTrayIcon():new( ::oDlg:oWidget )
      IF ( ::lSystemTrayAvailable := ::oSys:isSystemTrayAvailable() ) .AND. ::lMinimizeInSystemTray
         ::oSys:setIcon( hbide_image( "hbide" ) )
         ::connect( ::oSys, "activated(QSystemTrayIcon::ActivationReason)", {|p| ::execEvent( "qSystemTrayIcon_activated", p ) } )

         ::oIde:oSysMenu := QMenu():new( ::oDlg:oWidget )
         ::qAct1 := ::oSysMenu:addAction_1( hbide_image( "fullscreen" ), "&Show" )
         ::oSysMenu:addSeparator()
         ::qAct2 := ::oSysMenu:addAction_1( hbide_image( "exit" ), "&Exit" )

         ::connect( ::qAct1, "triggered(bool)", {|| ::execEvent( "qSystemTrayIcon_show"  ) } )
         ::connect( ::qAct2, "triggered(bool)", {|| ::execEvent( "qSystemTrayIcon_close" ) } )

         ::oSys:setContextMenu( ::oSysMenu )
         ::oSys:hide()
         ::oSys:setToolTip( "Harbour's Integrated Development Environment (v1.0)" )
      ENDIF
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD IdeDocks:execEvent( cEvent, p, p1 )
   LOCAL qEvent, qMime, qList, qUrl, i

   SWITCH cEvent
   CASE "dockQScintilla_visibilityChanged"
      IF p; ::oBM:show() ; ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockSourceThumbnail_visibilityChanged"
      IF p; ::oEM:showThumbnail(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockSkltnsTree_visibilityChanged"
      IF p; ::oSK:showTree(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockHelpDock_visibilityChanged"
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockDocViewer_visibilityChanged"
      IF p; ::oHL:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockDocWriter_visibilityChanged"
      IF p; ::oDW:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "oFuncDock_visibilityChanged"
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "docFunctions_visibilityChanged"
      IF p; ::oFN:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockProperties_visibilityChanged"
      IF p; ::oPM:fetchProperties(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "docEnvironments_visibilityChanged"
      IF p; ::oEV:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "docSkeletons_visibilityChanged"
      IF p; ::oSK:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockThemes_visibilityChanged"
      IF p; ::oTH:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockFindInFiles_visibilityChanged"
      IF p; ::oFF:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   /* Miscellaneous */
   CASE "qHelpBrw_contextMenuRequested"
      hbide_popupBrwContextMenu( ::qHelpBrw, p )
      EXIT
   CASE "QEvent_WindowStateChange"
      qEvent := QWindowStateChangeEvent():from( p )
      ::nPrevWindowState := qEvent:oldState()
      EXIT

   CASE "QEvent_Hide"
      IF ::lSystemTrayAvailable .AND. ::lMinimizeInSystemTray
         qEvent := QHideEvent():from( p )
         IF ! ::lChanging
            ::lChanging := .t.
            IF qEvent:spontaneous()
               IF empty( ::qTimer )
                  ::qTimer := QTimer():New()
                  ::qTimer:setSingleShot( .t. )
                  ::qTimer:setInterval( 250 )
                  ::connect( ::qTimer, "timeout()", {|| ::execEvent( "qTimer_timeOut" ) } )
               ENDIF
               ::qTimer:start()
               qEvent:ignore()
            ENDIF
            ::lChanging := .f.
         ENDIF
      ENDIF
      EXIT

   CASE "qTimer_timeOut"
      ::oDlg:hide()
      ::oSys:setToolTip( ::oDlg:oWidget:windowTitle() )
      ::oSys:show()
      EXIT

   CASE "qSystemTrayIcon_close"
      PostAppEvent( xbeP_Close, NIL, NIL, ::oDlg )
      EXIT

   CASE "qSystemTrayIcon_show"
      ::showDlgBySystemTrayIconCommand()
      EXIT

   CASE "qSystemTrayIcon_activated"
      IF     p == QSystemTrayIcon_Trigger
         ::showDlgBySystemTrayIconCommand()
      ELSEIF p == QSystemTrayIcon_DoubleClick
      ELSEIF p == QSystemTrayIcon_Context
      ELSEIF p == QSystemTrayIcon_MiddleClick
      ENDIF
      EXIT

   CASE "editWidget_dragEnterEvent"
      qEvent := QDragEnterEvent():from( p )
      qEvent:acceptProposedAction()
      EXIT

   CASE "editWidget_dropEvent"
      qEvent := QDropEvent():from( p )
      qMime := QMimeData():from( qEvent:mimeData() )
      IF qMime:hasUrls()
         qList := QStringList():from( qMime:hbUrlList() )
         FOR i := 0 TO qList:size() - 1
            qUrl := QUrl():new( qList:at( i ) )
            IF hbide_isValidText( qUrl:toLocalFile() )
               ::oSM:editSource( hbide_pathToOSPath( qUrl:toLocalFile() ) )
            ENDIF
         NEXT
      ENDIF
      EXIT

   CASE "projectTree_dragEnterEvent"
HB_TRACE( HB_TR_ALWAYS, "projectTree_dragEnterEvent" )
      QDragEnterEvent():from( p ):acceptProposedAction()
      EXIT

   CASE "projectTree_dropEvent"
HB_TRACE( HB_TR_ALWAYS, "projectTree_dropEvent" )
      qEvent := QDropEvent():from( p )
      qMime := QMimeData():from( qEvent:mimeData() )
      IF qMime:hasUrls()
         qList := QStringList():from( qMime:hbUrlList() )
         FOR i := 0 TO qList:size() - 1
            qUrl := QUrl():new( qList:at( i ) )
            IF hbide_sourceType( qUrl:toLocalFile() ) == ".hbp"
               ::oPM:loadProperties( qUrl:toLocalFile(), .f., .f., .t. )
            ENDIF
         NEXT
      ENDIF
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:showDlgBySystemTrayIconCommand()()

   ::oSys:hide()

   IF hb_bitAnd( ::nPrevWindowState, Qt_WindowMaximized ) == Qt_WindowMaximized
      ::oDlg:oWidget:showMaximized()
   ELSEIF hb_bitAnd( ::nPrevWindowState, Qt_WindowFullScreen ) == Qt_WindowFullScreen
      ::oDlg:oWidget:showFullScreen()
   ELSE
      ::oDlg:oWidget:showNormal()
   ENDIF

   ::oDlg:oWidget:raise()
   ::oDlg:oWidget:activateWindow()

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
   ::buildSourceThumbnail()
   ::buildQScintilla()
   ::buildUpDownWidget()

   /* Bottom Docks */
   ::oDlg:oWidget:tabifyDockWidget( ::oDockB:oWidget              , ::oDockB1:oWidget              )
   ::oDlg:oWidget:tabifyDockWidget( ::oDockB1:oWidget             , ::oDockB2:oWidget              )

   /* Right Docks */
   ::oDlg:oWidget:tabifyDockWidget( ::oHelpDock:oWidget           , ::oDocViewDock:oWidget         )
   ::oDlg:oWidget:tabifyDockWidget( ::oDocViewDock:oWidget        , ::oFuncDock:oWidget            )
   ::oDlg:oWidget:tabifyDockWidget( ::oFuncDock:oWidget           , ::oFunctionsDock:oWidget       )
   ::oDlg:oWidget:tabifyDockWidget( ::oFunctionsDock:oWidget      , ::oPropertiesDock:oWidget      )
   ::oDlg:oWidget:tabifyDockWidget( ::oPropertiesDock:oWidget     , ::oEnvironDock:oWidget         )
   ::oDlg:oWidget:tabifyDockWidget( ::oEnvironDock:oWidget        , ::oSkeltnDock:oWidget          )
   ::oDlg:oWidget:tabifyDockWidget( ::oSkeltnDock:oWidget         , ::oThemesDock:oWidget          )
   ::oDlg:oWidget:tabifyDockWidget( ::oThemesDock:oWidget         , ::oFindDock:oWidget            )
   ::oDlg:oWidget:tabifyDockWidget( ::oFindDock:oWidget           , ::oDocWriteDock:oWidget        )
   ::oDlg:oWidget:tabifyDockWidget( ::oDocWriteDock:oWidget       , ::oSourceThumbnailDock:oWidget )
   ::oDlg:oWidget:tabifyDockWidget( ::oSourceThumbnailDock:oWidget, ::oQScintillaDock:oWidget      )

   ::buildToolBarPanels()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:getADockWidget( nAreas, cObjectName, cWindowTitle, nFlags, cEventVisibility )
   LOCAL oDock, nBasic

   DEFAULT nFlags TO 0

   nBasic := hb_bitOR( QDockWidget_DockWidgetClosable, nFlags )

   oDock := XbpWindow():new()
   oDock:oWidget := QDockWidget():new( ::oDlg:oWidget )
   oDock:oWidget:setObjectName( cObjectName )
   ::oDlg:addChild( oDock )
   oDock:oWidget:setFeatures( nBasic )
   oDock:oWidget:setAllowedAreas( nAreas )
   oDock:oWidget:setWindowTitle( cWindowTitle )
   oDock:oWidget:setFocusPolicy( Qt_NoFocus )
   oDock:oWidget:setStyleSheet( getStyleSheet( "QDockWidget", ::nAnimantionMode ) )
   oDock:hide()

   IF !empty( cEventVisibility )
      ::connect( oDock:oWidget, cEventVisibility, {|p| ::execEvent( cEventVisibility, p, oDock:oWidget ) } )
   ENDIF

   RETURN oDock

/*----------------------------------------------------------------------*/

METHOD IdeDocks:setViewInitials( cView )

   ::setView( cView )

HB_TRACE( HB_TR_ALWAYS, cView, ::qTabWidget:count() )

   IF ::qTabWidget:count() == 1
      ::oEM:setSourceVisibleByIndex( 0 )
   ELSE
      ::qTabWidget:setCurrentIndex( 0 )
      ::qTabWidget:setCurrentIndex( ::qTabWidget:count() - 1 )
      ::qTabWidget:setCurrentIndex( 0 )
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeDocks:setView( cView )
   LOCAL n, nIndex

   SWITCH cView

   CASE "New..."
      cView := hbide_fetchAString( ::qViewsCombo, cView, "Name the View", "New View" )
      IF cView != "New..." .AND. cView != "Stats" .AND. cView != "Main"
         IF ascan( ::oINI:aViews, {|e| e == cView } ) > 0
            MsgBox( "View: " + cView + ", already exists" )
         ELSE
            aadd( ::oIde:oINI:aViews, cView )
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
   LOCAL oFrame, qTBtnClose, qDrop

   oFrame := XbpWindow():new( ::oStackedWidget )
   oFrame:oWidget := QWidget():new( ::oStackedWidget:oWidget )
   oFrame:oWidget:setObjectName( cObjectName )       /* This will form the basis of showing at top */
   ::oStackedWidget:addChild( oFrame )

   oFrame:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   //oFrame:qLayout:setContentsMargins( 2, 2, 2, 2 )
   oFrame:qLayout:setContentsMargins( 0,0,0,0 )

   oFrame:oTabWidget := XbpTabWidget():new():create( oFrame, , {0,0}, {200,200}, , .t. )

   IF !( cObjectName == "Stats" )
      qTBtnClose := QToolButton():new()
      qTBtnClose:setTooltip( "Close Tab" )
      qTBtnClose:setAutoRaise( .t. )
      qTBtnClose:setIcon( hbide_image( "closetab" ) )
      ::connect( qTBtnClose, "clicked()", {|| ::oSM:closeSource() } )
      oFrame:oTabWidget:qCornerWidget := qTBtnClose
      oFrame:oTabWidget:oWidget:setCornerWidget( qTBtnClose, Qt_TopRightCorner )

      qDrop := oFrame:oTabWidget:oWidget

      qDrop:setAcceptDrops( .t. )
      qDrop:installEventFilter( ::pEvents )
      ::connect( qDrop, QEvent_DragEnter, {|p| ::execEvent( "editWidget_dragEnterEvent", p ) } )
      ::connect( qDrop, QEvent_Drop     , {|p| ::execEvent( "editWidget_dropEvent"     , p ) } )

   ENDIF

   oFrame:oTabWidget:oWidget:setUsesScrollButtons( .t. )
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

METHOD IdeDocks:buildUpDownWidget()

   ::oIde:oUpDn := IdeUpDown():new( ::oIde ):create()
   ::oUpDn:oUI:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildToolBarPanels()
   LOCAL s, qTBtn, a_, aBtns, qAct
   LOCAL qSize := QSize():new( 20,20 )

   /* Toolbar Panels */

   ::oIde:qTBarPanels := QToolBar():new()
   ::qTBarPanels:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qTBarPanels:setObjectName( "ToolBar_Panels" )
   ::qTBarPanels:setWindowTitle( "ToolBar: Editor Panels" )
   ::qTBarPanels:setAllowedAreas( Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea )
   ::qTBarPanels:setIconSize( qSize )

   ::oDlg:oWidget:addToolBar( Qt_LeftToolBarArea, ::qTBarPanels )

   ::addPanelButton( "Main" )
   FOR EACH s IN ::oINI:aViews
      ::addPanelButton( s )
   NEXT

   /* Toolbar Line Actions */

   ::oIde:qTBarLines := QToolBar():new()
   ::qTBarLines:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qTBarLines:setObjectName( "ToolBar_Lines" )
   ::qTBarLines:setWindowTitle( "ToolBar: Lines and Blocks" )
   ::qTBarLines:setIconSize( qSize )
   ::qTBarPanels:setAllowedAreas( Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea )

   ::oDlg:oWidget:addToolBar( Qt_LeftToolBarArea, ::qTBarLines )

   aBtns := {}
   aadd( aBtns, { "movelineup"      , "Move Current Line Up"       , {|| ::oEM:moveLine( -1 )                   } } )
   aadd( aBtns, { "movelinedown"    , "Move Current Line Down"     , {|| ::oEM:moveLine(  1 )                   } } )
   aadd( aBtns, { "deleteline"      , "Delete Current Line"        , {|| ::oEM:deleteLine()                     } } )
   aadd( aBtns, { "duplicateline"   , "Duplicate Current Line"     , {|| ::oEM:duplicateLine()                  } } )
   aadd( aBtns, {} )
   #if 0
   aadd( aBtns, { "togglelinenumber", "Toggle Line Numbers"        , {|| ::oEM:toggleLineNumbers()              } } )
   aadd( aBtns, { "curlinehilight"  , "Toggle Current Line Hilight", {|| ::oEM:toggleCurrentLineHighlightMode() } } )
   #endif
   aadd( aBtns, { "togglelinenumber", "Toggle Line Numbers"        , {|| ::oEM:toggleLineNumbers()              } } )
   aadd( aBtns, { "horzruler"       , "Toggle Horizontal Ruler"    , {|| ::oEM:toggleHorzRuler()                } } )
   aadd( aBtns, { "curlinehilight"  , "Toggle Current Line Hilight", {|| ::oEM:toggleCurrentLineHighlightMode() } } )
   FOR EACH a_ IN aBtns
      IF empty( a_ )
         ::qTBarLines:addSeparator()
      ELSE
         qTBtn := QToolButton():new()
         qTBtn:setTooltip( a_[ 2 ] )
         qTBtn:setIcon( hbide_image( a_[ 1 ] ) )
         qTBtn:setMaximumWidth( 20 )
         qTBtn:setMaximumHeight( 20 )
         IF a_[ 1 ] $ "togglelinenumber,curlinehilight,horzruler"
            //qTBtn:setCheckable( .t. )
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
         qTBtn:setIcon( hbide_image( a_[ 1 ] ) )
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
   ::qTBarDocks:setIconSize( qSize )
   ::qTBarDocks:setToolButtonStyle( Qt_ToolButtonIconOnly )
   ::qTBarDocks:setAllowedAreas( Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea )

   aBtns := {}
   aadd( aBtns, { ::oDockPT             , "projtree"      } )
   aadd( aBtns, { ::oDockED             , "editstree"     } )
   aadd( aBtns, { ::oSkltnsTreeDock     , "projtree"      } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oHelpDock           , "help"          } )
   aadd( aBtns, { ::oDocViewDock        , "harbourhelp"   } )
   aadd( aBtns, { ::oDocWriteDock       , "docwriter"     } )
   aadd( aBtns, { ::oFuncDock           , "dc_function"   } )
   aadd( aBtns, { ::oFunctionsDock      , "ffn"           } )
   aadd( aBtns, { ::oPropertiesDock     , "properties"    } )
   aadd( aBtns, { ::oEnvironDock        , "envconfig"     } )
   aadd( aBtns, { ::oSkeltnDock         , "codeskeletons" } )
   aadd( aBtns, { ::oThemesDock         , "syntaxhiliter" } )
   aadd( aBtns, { ::oFindDock           , "search"        } )
   aadd( aBtns, { ::oSourceThumbnailDock, "thumbnail"     } )
   aadd( aBtns, { ::oQScintillaDock     , "browser"       } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oDockB2             , "builderror"    } )

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

   /* User defined toolbars via Tools & Utilities */
   ::oTM:buildUserToolbars()

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
   LOCAL i, oItem, qDrop
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDockPT := ::getADockWidget( nAreas, "dockProjectTree", "Projects", QDockWidget_DockWidgetFloatable )
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

   qDrop := ::oDockPT:oWidget // ::oProjTree:oWidget

   qDrop:setAcceptDrops( .t. )
   qDrop:installEventFilter( ::pEvents )
   ::connect( qDrop, QEvent_DragEnter, {|p| ::execEvent( "projectTree_dragEnterEvent", p ) } )
   ::connect( qDrop, QEvent_Drop     , {|p| ::execEvent( "projectTree_dropEvent"     , p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildEditorTree()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDockED := ::getADockWidget( nAreas, "dockEditorTabs", "Editors", QDockWidget_DockWidgetFloatable )
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
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oSkltnsTreeDock := ::getADockWidget( nAreas, "dockSkltnsTree", "Skeletons", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_LeftDockWidgetArea, ::oSkltnsTreeDock:oWidget, Qt_Vertical )
   ::connect( ::oSkltnsTreeDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockSkltnsTree_visibilityChanged", p, ::oSkltnsTreeDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFuncList()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oFuncDock := ::getADockWidget( nAreas, "dockFuncList", "Functions List", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oFuncDock:oWidget, Qt_Vertical )
   ::connect( ::oFuncDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "oFuncDock_visibilityChanged", p, ::oFuncDock:oWidget ) } )

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

   ::oHelpDock:connect( ::qHelpBrw, "customContextMenuRequested(QPoint)", {|p| ::execEvent( "qHelpBrw_contextMenuRequested", p ) } )

   ::connect( ::oHelpDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockHelpDock_visibilityChanged", p, ::oHelpDock:oWidget ) } )

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
   LOCAL nAreas := Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDockB2 := ::getADockWidget( nAreas, "dockOutputResults", "Output Console", QDockWidget_DockWidgetFloatable )
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
         IF ::oSM:editSource( cSource, 0, 0, 0, NIL, NIL, .f., .t. )
            qCursor := QTextCursor():configure( ::oIde:qCurEdit:textCursor() )
            nLine   := iif( nLine < 1, 0, nLine - 1 )

            qCursor:setPosition( 0 )
            qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nLine )
            ::oIde:qCurEdit:setTextCursor( qCursor )
            ::oIde:qCurEdit:centerCursor()
            ::oIde:manageFocusInEditor()
         ENDIF
      ENDIF
   ENDIF

   RETURN nLine

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildStatusBar()
   LOCAL i

   ::oIde:oSBar := XbpStatusBar():new()
   ::oSBar:create( ::oDlg, , { 0,0 }, { ::oDlg:currentSize()[ 1 ], 30 } )
   ::oSBar:oWidget:showMessage( "" )
   ::oSBar:oWidget:setStyleSheet( GetStyleSheet( "QStatusBar", ::nAnimantionMode ) )

   ::oSBar:getItem( SB_PNL_MAIN ):autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   ::oSBar:addItem( "", , , , "Ready"    ):oWidget:setMinimumWidth(  40 )
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
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oThemesDock := ::getADockWidget( nAreas, "dockThemes", "Theme Manager", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oThemesDock:oWidget, Qt_Horizontal )
   ::connect( ::oThemesDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockThemes_visibilityChanged", p, ::oThemesDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildPropertiesDock()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oPropertiesDock := ::getADockWidget( nAreas, "dockProperties", "Project Properties", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oPropertiesDock:oWidget, Qt_Horizontal )
   ::connect( ::oPropertiesDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockProperties_visibilityChanged", p, ::oPropertiesDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFindInFiles()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oFindDock := ::getADockWidget( nAreas, "dockFindInFiles", "Find in Files", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oFindDock:oWidget, Qt_Horizontal )
   ::connect( ::oFindDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockFindInFiles_visibilityChanged", p, ::oFindDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDocViewer()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDocViewDock := ::getADockWidget( nAreas, "dockDocViewer", "Harbour Documentation", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oDocViewDock:oWidget, Qt_Horizontal )
   ::connect( ::oDocViewDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockDocViewer_visibilityChanged", p, ::oDocViewDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDocWriter()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDocWriteDock := ::getADockWidget( nAreas, "dockDocWriter", "Documentation Writer", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oDocWriteDock:oWidget, Qt_Horizontal )
   ::connect( ::oDocWriteDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockDocWriter_visibilityChanged", p, ::oDocWriteDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFunctionsDock()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oFunctionsDock := ::getADockWidget( nAreas, "dockFunctions", "Projects Functions Lookup", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oFunctionsDock:oWidget, Qt_Horizontal )
   ::connect( ::oFunctionsDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "docFunctions_visibilityChanged", p, ::oFunctionsDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildEnvironDock()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oEnvironDock := ::getADockWidget( nAreas, "dockEnvironments", "Compiler Environments", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oEnvironDock:oWidget, Qt_Horizontal )
   ::connect( ::oEnvironDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "docEnvironments_visibilityChanged", p, ::oEnvironDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSkeletonWidget()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oSkeltnDock := ::getADockWidget( nAreas, "dockSkeleton", "Code Skeletons", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oSkeltnDock:oWidget, Qt_Horizontal )
   ::connect( ::oSkeltnDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "docSkeletons_visibilityChanged", p, ::oSkeltnDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSourceThumbnail()
   LOCAL oDock
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   oDock := ::getADockWidget( nAreas, "dockSourceThumbnail", "Source Thumbnail", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, oDock:oWidget, Qt_Horizontal )
   ::connect( oDock:oWidget, "visibilityChanged(bool)", {|p| ::execEvent( "dockSourceThumbnail_visibilityChanged", p, oDock:oWidget ) } )
   ::oIde:oSourceThumbnailDock := oDock

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildQScintilla()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oQScintillaDock := ::getADockWidget( nAreas, "dockQScintilla", "ideDBU", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, ::oQScintillaDock:oWidget, Qt_Horizontal )
   ::connect( ::oQScintillaDock:oWidget, "visibilityChanged(bool)"  , {|p| ::execEvent( "dockQScintilla_visibilityChanged", p, ::oQScintillaDock:oWidget ) } )

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
      oPanel:caption := iif( empty( xValue ), "St", xValue )
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
   LOCAL cStyle, oView

   IF nMode == NIL
      ::oIde:nAnimantionMode := iif( ::nAnimantionMode == HBIDE_ANIMATION_NONE, HBIDE_ANIMATION_GRADIENT, HBIDE_ANIMATION_NONE )
      nMode := ::nAnimantionMode
   ENDIF
   ::oIde:nAnimantionMode := nMode
   ::oIde:oINI:cIdeAnimated := hb_ntos( ::nAnimantionMode )

   /* Main Window */
   ::oDlg:setStyleSheet( GetStyleSheet( "QMainWindow", ::nAnimantionMode ) )

   /* Main Menu Bar with all its submenus */
   ::oDlg:menubar():setStyleSheet( GetStyleSheet( "QMenuBar", nMode ), GetStyleSheet( "QMenuPop", nMode ) )

   /* Toolbars */
   ::oMainToolbar:setStyleSheet( GetStyleSheet( "QToolBar"   , nMode ) )
   ::qTBarPanels :setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )
   ::qTBarLines  :setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )
   ::qTBarDocks  :setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )

   /* User defined toolbars */
   ::oTM:setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )

   ::oEM:setStyleSheet( nMode )
   ::oBM:setStyleSheet( nMode )

   /* Statusbar */
   ::oSBar:oWidget:setStyleSheet( GetStyleSheet( "QStatusBar", nMode ) )

   /* Docking Widgets */
   cStyle := GetStyleSheet( "QDockWidget", nMode )
   //
   ::oDockPT:oWidget              : setStyleSheet( cStyle )
   ::oDockED:oWidget              : setStyleSheet( cStyle )
   ::oSkltnsTreeDock:oWidget      : setStyleSheet( cStyle )
   ::oHelpDock:oWidget            : setStyleSheet( cStyle )
   ::oDocViewDock:oWidget         : setStyleSheet( cStyle )
   ::oDocWriteDock:oWidget        : setStyleSheet( cStyle )
   ::oFuncDock:oWidget            : setStyleSheet( cStyle )
   ::oFunctionsDock:oWidget       : setStyleSheet( cStyle )
   ::oPropertiesDock:oWidget      : setStyleSheet( cStyle )
   ::oEnvironDock:oWidget         : setStyleSheet( cStyle )
   ::oSkeltnDock:oWidget          : setStyleSheet( cStyle )
   ::oThemesDock:oWidget          : setStyleSheet( cStyle )
   ::oFindDock:oWidget            : setStyleSheet( cStyle )
   ::oDockB2:oWidget              : setStyleSheet( cStyle )
   ::oQScintillaDock:oWidget      : setStyleSheet( cStyle )
   ::oSourceThumbnailDock:oWidget : setStyleSheet( cStyle )

   ::oProjTree:setStyleSheet( GetStyleSheet( "QTreeWidgetHB", ::nAnimantionMode ) )

   /* Edior Tab Widget */
   FOR EACH oView IN ::aViews
      oView:oTabWidget:oWidget:setStyleSheet( GetStyleSheet( "QTabWidget", nMode ) )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

