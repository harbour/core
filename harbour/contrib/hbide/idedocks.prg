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
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

CLASS IdeDocks INHERIT IdeObject

   DATA   nPass                                   INIT   0
   DATA   aPanels                                 INIT   {}
   DATA   aMdiBtns                                INIT   {}
   DATA   aBtnLines                               INIT   {}
   DATA   aBtnDocks                               INIT   {}
   DATA   oBtnTabClose

   DATA   qMdiToolBar
   DATA   qMdiToolBarL
   DATA   aViewsInfo                              INIT   {}

   DATA   qTBtnClose

   DATA   lChanging                               INIT   .f.

   DATA   qTimer
   DATA   nPrevWindowState                        INIT   Qt_WindowNoState
   DATA   lSystemTrayAvailable                    INIT   .f.
   DATA   lMinimizeInSystemTray                   INIT   .f.  /* TODO: make it user definable */
   DATA   qAct1
   DATA   qAct2
   DATA   cOldView                                INIT   ""


   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD execEvent( cEvent, p, p1 )
   METHOD setView( cView )
   METHOD buildToolBarPanels()
   METHOD buildHelpWidget()
   METHOD buildSkeletonWidget()
   METHOD buildDialog()
   METHOD buildViewWidget( cView )
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
   METHOD buildReportsDesignerWidget()
   METHOD buildSystemTray()
   METHOD showDlgBySystemTrayIconCommand()
   METHOD setViewInitials()
   METHOD buildMdiToolbar()
   METHOD buildMdiToolbarLeft()
   METHOD getEditorPanelsInfo()
   METHOD restPanelsGeometry()
   METHOD savePanelsGeometry()
   METHOD stackVertically()
   METHOD stackHorizontally()
   METHOD stackMaximized()
   METHOD stackZoom( nMode )
   METHOD restState( nMode )
   METHOD setButtonState( cButton, lChecked )
   METHOD buildFormatWidget()
   METHOD hideAllDocks()
   METHOD setToolbarSize( nSize )
   METHOD buildCuiEdWidget()

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

METHOD IdeDocks:hideAllDocks()

   // Left
   ::oDockPT                  : hide()
   ::oDockED                  : hide()
   ::oSkltnsTreeDock          : hide()

   // Right
   ::oEnvironDock             : hide()
   ::oPropertiesDock          : hide()
   ::oThemesDock              : hide()
   ::oDocViewDock             : hide()
   ::oDocWriteDock            : hide()
   ::oFindDock                : hide()
   ::oFunctionsDock           : hide()
   ::oSkeltnDock              : hide()
   ::oHelpDock                : hide()
   ::oFuncDock                : hide()
   ::oSourceThumbnailDock     : hide()
   ::oQScintillaDock          : hide()
   ::oReportsManagerDock      : hide()
   ::oFormatDock              : hide()
   ::oCuiEdDock               : hide()

   // Bottom
   ::oDockB2                  : hide()
   ::oDockB1                  : hide()
   ::oDockB                   : hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:destroy()
   LOCAL qTmp

   FOR EACH qTmp IN ::oIde:aViews
      qTmp:oTabWidget:oWidget:disconnect( QEvent_DragEnter )
      qTmp:oTabWidget:oWidget:disconnect( QEvent_DragMove )
      qTmp:oTabWidget:oWidget:disconnect( QEvent_Drop      )
   NEXT

   ::oDlg:oWidget                : disconnect( QEvent_WindowStateChange  )
   ::oDlg:oWidget                : disconnect( QEvent_Hide               )

   ::oIde:oProjRoot              := NIL
   ::oIde:oOpenedSources         := NIL

   ::oOutputResult:oWidget       : disconnect( "copyAvailable(bool)"     )
   ::oEnvironDock:oWidget        : disconnect( "visibilityChanged(bool)" )
   ::oPropertiesDock:oWidget     : disconnect( "visibilityChanged(bool)" )
   ::oThemesDock:oWidget         : disconnect( "visibilityChanged(bool)" )
   ::oDocViewDock:oWidget        : disconnect( "visibilityChanged(bool)" )
   ::oDocWriteDock:oWidget       : disconnect( "visibilityChanged(bool)" )
   ::oFindDock:oWidget           : disconnect( "visibilityChanged(bool)" )
   ::oFunctionsDock:oWidget      : disconnect( "visibilityChanged(bool)" )
   ::oSkeltnDock:oWidget         : disconnect( "visibilityChanged(bool)" )
   ::oHelpDock:oWidget           : disconnect( "visibilityChanged(bool)" )
   ::oFuncDock:oWidget           : disconnect( "visibilityChanged(bool)" )

   ::oSourceThumbnailDock:oWidget: disconnect( "visibilityChanged(bool)" )
   ::oQScintillaDock:oWidget     : disconnect( "visibilityChanged(bool)" )
   ::oReportsManagerDock:oWidget : disconnect( "visibilityChanged(bool)" )
   ::oFormatDock:oWidget         : disconnect( "visibilityChanged(bool)" )
   ::oCuiEdDock:oWidget          : disconnect( "visibilityChanged(bool)" )

   #if 0  /* Not Implemented */
   ::oDockPT:oWidget             : disconnect( "visibilityChanged(bool)" )
   ::oDockED:oWidget             : disconnect( "visibilityChanged(bool)" )
   ::oDockB2:oWidget             : disconnect( "visibilityChanged(bool)" )
   #endif

   IF !empty( ::oSys )
      ::oIde:oSys                : disconnect( "activated(QSystemTrayIcon::ActivationReason)" )
      IF hb_isObject( ::qAct1 )
         ::qAct1                 : disconnect( "triggered(bool)"         )
         ::qAct2                 : disconnect( "triggered(bool)"         )
      ENDIF

      ::oIde:oSys := NIL
      ::qAct1     := NIL
      ::qAct2     := NIL
   ENDIF

   IF !empty( ::qTimer )
      ::qTimer:disconnect( "timeout()" )
      ::qTimer:stop()
      ::qTimer := NIL
   ENDIF

   ::oIde:oOutputResult          := NIL
   ::oIde:oEnvironDock           := NIL
   ::oIde:oPropertiesDock        := NIL
   ::oIde:oThemesDock            := NIL
   ::oIde:oDocViewDock           := NIL
   ::oIde:oDocWriteDock          := NIL
   ::oIde:oFindDock              := NIL
   ::oIde:oFunctionsDock         := NIL
   ::oIde:oSkeltnDock            := NIL
   ::oIde:oHelpDock              := NIL
   ::oIde:oFuncDock              := NIL

   ::oIde:oSourceThumbnailDock   := NIL
   ::oIde:oQScintillaDock        := NIL
   ::oIde:oReportsManagerDock    := NIL
   ::oIde:oFormatDock            := NIL
   ::oIde:oCuiEdDock             := NIL

   ::oIde:oDockPT                := NIL
   ::oIde:oDockED                := NIL
   ::oIde:oDockB2                := NIL

   ::oIde:oDockB1                := NIL
   ::oIde:oLinkResult            := NIL

   ::oIde:oSys                   := NIL
   ::qAct1                       := NIL
   ::qAct2                       := NIL

   ::oIde:oDockB                 := NIL
   ::oIde:oCompileResult         := NIL

   FOR EACH qTmp IN ::aPanels
      qTmp:disconnect( "clicked()" )
      qTmp := NIL
   NEXT
   FOR EACH qTmp IN ::aMdiBtns
      qTmp:disconnect( "clicked()" )
      qTmp := NIL
   NEXT

   FOR EACH qTmp IN ::oIde:aMarkTBtns
      qTmp:disconnect( "clicked()" )
      qTmp := NIL
   NEXT

   FOR EACH qTmp IN ::oIde:aMdies
      qTmp:disconnect( "windowStateChanged(Qt::WindowStates,Qt::WindowStates)" )
      qTmp := NIL
   NEXT

   IF hb_isObject( ::qMdiToolBar )
      ::qMdiToolBar:destroy()
      ::qMdiToolBar := NIL
   ENDIF
   IF hb_isObject( ::qMdiToolBarL )
      ::qMdiToolBarL:destroy()
      ::qMdiToolBarL := NIL
   ENDIF
   ::nPass                       := NIL
   ::aPanels                     := NIL
   ::aMdiBtns                    := NIL
   ::aBtnLines                   := NIL
   ::aBtnDocks                   := NIL
   ::oBtnTabClose                := NIL
   ::aViewsInfo                  := NIL
   ::qTBtnClose                  := NIL
   ::qTimer                      := NIL
   ::nPrevWindowState            := NIL

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeDocks:getEditorPanelsInfo()
   LOCAL b_, a_:= {}
   FOR EACH b_ IN ::aViewsInfo
      aadd( a_, b_[ 1 ] + "," + ;
                iif( empty( b_[ 2 ] ), "",  hbide_nArray2String( { b_[ 2 ]:x(), b_[ 2 ]:y(), b_[ 2 ]:width(), b_[ 2 ]:height() } ) ) + "," + ;
                hb_ntos( b_[ 3 ] ) + "," + hb_ntos( b_[ 4 ] ) + "," + ;
                hb_ntos( ::oStackedWidget:oWidget:viewMode() ) + "," + hb_ntos( ::oINI:nEditsViewStyle ) + ","   ;
          )
   NEXT
   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDialog()
   LOCAL s, aSize, a_, x_, lNew := .f.

   ::oIde:oDlg     := XbpDialog():new()
   ::oDlg:icon     := hbide_image( "hbide" )
   ::oDlg:title    := "Harbour IDE"
   ::oDlg:qtObject := hbide_getUI( "mainwindow" )
   ::oDlg:create( , , , , , .f. )

   ::oDlg:oWidget:setStyleSheet( GetStyleSheet( "QMainWindow", ::nAnimantionMode ) )

   ::oDlg:close := {|| hbide_setClose( hbide_getYesNo( "hbIDE is about to be closed!", "Are you sure?" ) ), ;
                                                                      PostAppEvent( xbeP_Close, , , ::oDlg ) }
   //::oDlg:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:setTabShape( ::oINI:nDocksTabShape )
   ::oDlg:setTabPosition( Qt_RightDockWidgetArea , ::oINI:nDocksRightTabPos  )
   ::oDlg:setTabPosition( Qt_BottomDockWidgetArea, ::oINI:nDocksBottomTabPos )
   ::oDlg:setTabPosition( Qt_LeftDockWidgetArea  , ::oINI:nDocksLeftTabPos   )
   ::oDlg:setTabPosition( Qt_TopDockWidgetArea   , ::oINI:nDocksTopTabPos    )

   ::oDlg:setCorner( Qt_BottomLeftCorner, Qt_LeftDockWidgetArea )
   ::oDlg:setCorner( Qt_BottomRightCorner, Qt_RightDockWidgetArea )
   ::oDlg:oWidget:resize( 1000,570 )

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
   ::oIde:qLayout := QGridLayout()
   ::oIde:qLayout:setContentsMargins( 0,0,0,0 )
   ::oIde:qLayout:setHorizontalSpacing( 0 )
   ::oIde:qLayout:setVerticalSpacing( 0 )
   //
   ::oDa:setLayout( ::qLayout )
   ::buildMdiToolbar()
   ::qLayout:addWidget( ::qMdiToolbar:oWidget   , 0, 0, 1, 2 )
   ::buildMdiToolbarLeft()
   ::qLayout:addWidget( ::qMdiToolbarL:oWidget  , 1, 0, 1, 1 )
   ::buildStackedWidget()
   ::qLayout:addWidget( ::oStackedWidget:oWidget, 1, 1, 1, 1 )
   ::buildSearchReplaceWidget()
   ::qLayout:addWidget( ::oSearchReplace:oUI    , 2, 0, 1, 2 )

   /* Normalize Views */
   FOR EACH s IN ::oINI:aViews
      a_:= hb_aTokens( s, "," )
      asize( a_, 6 )
      IF ! empty( a_[ 2 ] )
         a_[ 2 ] := hbide_array2Rect( hbide_string2nArray( a_[ 2 ] ) )
      ENDIF
      DEFAULT a_[ 3 ] TO "0"
      DEFAULT a_[ 4 ] TO "0"
      DEFAULT a_[ 5 ] TO hb_ntos( QMdiArea_TabbedView )
      DEFAULT a_[ 6 ] TO "0"
      a_[ 3 ] := val( a_[ 3 ] )
      a_[ 4 ] := val( a_[ 4 ] )
      a_[ 5 ] := val( a_[ 5 ] )
      a_[ 6 ] := val( a_[ 6 ] )
      aadd( ::aViewsInfo, a_ )
   NEXT
   IF ascan( ::aViewsInfo, {|e_| e_[ 1 ] == "Main" } ) == 0
      lNew := .t.
      hb_ains( ::aViewsInfo, 1, { "Main", NIL, 0, 0, QMdiArea_TabbedView, 0 }, .t. )
   ENDIF

   /* View Panels */
   x_:= aclone( ::aViewsInfo )
   FOR EACH a_ IN ::aViewsInfo
      ::buildViewWidget( a_[ 1 ] )
   NEXT

   ::setView( "Main" )

   IF lNew
      ::oStackedWidget:setViewMode( QMdiArea_TabbedView )
      ::oINI:nEditsViewStyle  := HBPMDI_STYLE_MAXIMIZED
      ::stackMaximized()

   ELSE
      IF x_[ 1,5 ] == QMdiArea_TabbedView
         ::oStackedWidget:setViewMode( QMdiArea_TabbedView )
      ENDIF

      IF     x_[ 1,6 ] == HBPMDI_STYLE_TILED
         ::oStackedWidget:tileSubWindows()
      ELSEIF x_[ 1,6 ] == HBPMDI_STYLE_CASCADED
         ::oStackedWidget:cascadeSubWindows()
      ELSEIF x_[ 1,6 ] == HBPMDI_STYLE_MAXIMIZED
         ::oINI:nEditsViewStyle  := HBPMDI_STYLE_MAXIMIZED
         ::stackMaximized()
      ELSEIF x_[ 1,6 ] == HBPMDI_STYLE_TILEDVERT
         ::oINI:nEditsViewStyle  := HBPMDI_STYLE_TILEDVERT
         ::stackVertically()
      ELSEIF x_[ 1,6 ] == HBPMDI_STYLE_TILEDHORZ
         ::oINI:nEditsViewStyle  := HBPMDI_STYLE_TILEDHORZ
         ::stackHorizontally()
      ELSE
         FOR EACH a_ IN x_
            IF !empty( a_[ 2 ] )
               ::oIde:aMdies[ a_:__enumIndex() ]:setGeometry( a_[ 2 ] )
            ENDIF
            ::oIde:aMdies[ a_:__enumIndex() ]:setWindowState( a_[ 4 ] )
         NEXT
      ENDIF
   ENDIF

   ::oDlg:oWidget:connect( QEvent_WindowStateChange, {|e| ::execEvent( "QEvent_WindowStateChange", e ) } )
   ::oDlg:oWidget:connect( QEvent_Hide             , {|e| ::execEvent( "QEvent_Hide"             , e ) } )

   ::buildSystemTray()

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
   ::buildReportsDesignerWidget()
   ::buildFormatWidget()
   ::buildCuiEdWidget()

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
   ::oDlg:oWidget:tabifyDockWidget( ::oQScintillaDock:oWidget     , ::oReportsManagerDock:oWidget  )
   ::oDlg:oWidget:tabifyDockWidget( ::oReportsManagerDock:oWidget , ::oFormatDock:oWidget          )
   ::oDlg:oWidget:tabifyDockWidget( ::oFormatDock:oWidget         , ::oCuiEdDock:oWidget           )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSystemTray()

   IF empty( ::oSys )
      ::oIde:oSys := QSystemTrayIcon( ::oDlg:oWidget )
      IF ( ::lSystemTrayAvailable := ::oSys:isSystemTrayAvailable() ) .AND. ::lMinimizeInSystemTray
         ::oSys:setIcon( hbide_image( "hbide" ) )
         ::oSys:connect( "activated(QSystemTrayIcon::ActivationReason)", {|p| ::execEvent( "qSystemTrayIcon_activated", p ) } )

         ::oIde:oSysMenu := QMenu()
         ::qAct1 := ::oSysMenu:addAction( hbide_image( "fullscreen" ), "&Show" )
         ::oSysMenu:addSeparator()
         ::qAct2 := ::oSysMenu:addAction( hbide_image( "exit" ), "&Exit" )

         ::qAct1:connect( "triggered(bool)", {|| ::execEvent( "qSystemTrayIcon_show"  ) } )
         ::qAct2:connect( "triggered(bool)", {|| ::execEvent( "qSystemTrayIcon_close" ) } )

         ::oSys:setContextMenu( ::oSysMenu )
         ::oSys:hide()
         ::oSys:setToolTip( "Harbour's Integrated Development Environment (v1.0)" )
      ENDIF
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD IdeDocks:execEvent( cEvent, p, p1 )
   LOCAL qEvent, qMime, qList, qUrl, i, n, oEdit, aMenu

   IF ::lQuitting
      RETURN Self 
   ENDIF 
   
   SWITCH cEvent
   CASE "dockCuiEd_visibilityChanged"
      IF p; ::oCUI:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT

   CASE "dockFormat_visibilityChanged"
      IF p; ::oFmt:show(); ENDIF
      IF ! p .AND. ! p1:isVisible()
         p1:raise()
      ENDIF
      EXIT
   CASE "dockReportsManager_visibilityChanged"
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockQScintilla_visibilityChanged"
      IF p; ::oBM:show() ; ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockSourceThumbnail_visibilityChanged"
      IF p; ::oEM:showThumbnail(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockSkltnsTree_visibilityChanged"
      IF p; ::oSK:showTree(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockHelpDock_visibilityChanged"
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockDocViewer_visibilityChanged"
      IF p; ::oHL:show(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockDocWriter_visibilityChanged"
      IF p; ::oDW:show(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "oFuncDock_visibilityChanged"
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "docFunctions_visibilityChanged"
      IF p; ::oFN:show(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockProperties_visibilityChanged"
      IF p; ::oPM:fetchProperties(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "docEnvironments_visibilityChanged"
      IF p; ::oEV:show(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "docSkeletons_visibilityChanged"
      IF p; ::oSK:show(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockThemes_visibilityChanged"
      IF p; ::oTH:show(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   CASE "dockFindInFiles_visibilityChanged"
      IF p; ::oFF:show(); ENDIF
      IF !empty( p1 )
         IF ! p .AND. ! p1:isVisible()
            p1:raise()
         ENDIF
      ENDIF
      EXIT
   /* Miscellaneous */
   CASE "qHelpBrw_contextMenuRequested"
      hbide_popupBrwContextMenu( ::qHelpBrw, p )
      EXIT
   CASE "outputConsole_contextMenuRequested"
      aMenu := {}
      aadd( aMenu, { "Clear"     , {|| ::oOutputResult:oWidget:clear()     } } )
      aadd( aMenu, { "" } )
      aadd( aMenu, { "Select All", {|| ::oOutputResult:oWidget:selectAll() } } )
      aadd( aMenu, { "Copy"      , {|| ::oOutputResult:oWidget:copy()      } } )
      hbide_execPopup( aMenu, p, ::oOutputResult:oWidget )
      EXIT

   CASE "QEvent_WindowStateChange"
      ::nPrevWindowState := p:oldState()
      EXIT

   CASE "QEvent_Hide"
      IF ::lSystemTrayAvailable .AND. ::lMinimizeInSystemTray
         qEvent := p
         IF ! ::lChanging
            ::lChanging := .t.
            IF qEvent:spontaneous()
               IF empty( ::qTimer )
                  ::qTimer := QTimer()
                  ::qTimer:setSingleShot( .t. )
                  ::qTimer:setInterval( 250 )
                  ::qTimer:connect( "timeout()", {|| ::execEvent( "qTimer_timeOut" ) } )
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

   CASE "editWidget_dragMoveEvent"
   CASE "editWidget_dragEnterEvent"
HB_TRACE( HB_TR_DEBUG, "editWidget_dragEnterEvent", 0 )
      p:acceptProposedAction()
      EXIT

   CASE "editWidget_dropEvent"
HB_TRACE( HB_TR_DEBUG, "editWidget_dropEvent", 0 )
      qMime := p:mimeData()
      IF qMime:hasUrls()
         qList := qMime:urls()
         FOR i := 0 TO qList:size() - 1
            qUrl := qList:at( i )
            IF hbide_isValidText( qUrl:toLocalFile() )
               ::oSM:editSource( hbide_pathToOSPath( qUrl:toLocalFile() ) )
            ENDIF
         NEXT
         p:setDropAction( Qt_CopyAction )
         p:accept()
         qList := NIL
      ENDIF
      qMime := NIL
      EXIT

   CASE "projectTree_dragEnterEvent"
HB_TRACE( HB_TR_DEBUG, "projectTree_dragEnterEvent" )
      p:acceptProposedAction()
      EXIT

   CASE "projectTree_dropEvent"
HB_TRACE( HB_TR_DEBUG, "projectTree_dropEvent" )
      qMime := p:mimeData()
      IF qMime:hasUrls()
         qList := qMime:urls()
         FOR i := 0 TO qList:size() - 1
            qUrl := qList:at( i )
            IF hbide_sourceType( qUrl:toLocalFile() ) == ".hbp"
               ::oPM:loadProperties( qUrl:toLocalFile(), .f., .f., .t. )
            ENDIF
         NEXT
      ENDIF
      EXIT

   CASE "x_mdiArea_subWindowActivated"
      IF ! empty( ::oIde:aMdies )
         IF ( n := ascan( ::oIde:aMdies, {|e| hbqt_IsEqual( e, p ) } ) )  > 0

            ::setView( ::oIde:aMdies[ n ]:objectName() )

            IF ! ::oIde:aMdies[ n ]:objectName() == "Stats" .AND. ! empty( ::oEM ) .AND. ! empty( oEdit := ::oEM:getEditorCurrent() )
               oEdit:setDocumentProperties()
               oEdit:qCoEdit:relayMarkButtons()
               oEdit:qCoEdit:toggleLineNumbers()
               oEdit:qCoEdit:toggleHorzRuler()
               oEdit:qCoEdit:toggleCurrentLineHighlightMode()
               oEdit:qCoEdit:dispStatusInfo()
               ::oUpDn:show()
               oEdit:changeThumbnail()
            ENDIF

         ENDIF
      ENDIF
      EXIT

   /* Left-toolbar actions */
   CASE "buttonViewTabbed_clicked"
      ::oStackedWidget:setViewMode( iif( ::oStackedWidget:viewMode() == QMdiArea_TabbedView, QMdiArea_SubWindowView, QMdiArea_TabbedView ) )
      EXIT
   CASE "buttonViewOrganized_clicked"
      ::oINI:nEditsViewStyle  := HBPMDI_STYLE_ORGANIZED
      ::restState()
      EXIT
   CASE "buttonSaveLayout_clicked"
      IF ::oINI:nEditsViewStyle == HBPMDI_STYLE_ORGANIZED
         ::savePanelsGeometry()
      ENDIF
      EXIT
   CASE "buttonViewTiled_clicked"
      ::oStackedWidget:tileSubWindows()
      ::oINI:nEditsViewStyle  := HBPMDI_STYLE_TILED
      EXIT
   CASE "buttonViewCascaded_clicked"
      ::oStackedWidget:cascadeSubWindows()
      ::oINI:nEditsViewStyle  := HBPMDI_STYLE_CASCADED
      EXIT
   CASE "buttonViewMaximized_clicked"
      ::oINI:nEditsViewStyle  := HBPMDI_STYLE_MAXIMIZED
      ::stackMaximized()
      EXIT
   CASE "buttonViewStackedVert_clicked"
      ::oINI:nEditsViewStyle  := HBPMDI_STYLE_TILEDVERT
      ::stackVertically()
      EXIT
   CASE "buttonViewStackedHorz_clicked"
      ::oINI:nEditsViewStyle  := HBPMDI_STYLE_TILEDHORZ
      ::stackHorizontally()
      EXIT
   CASE "buttonViewZoomedIn_clicked"
      ::stackZoom( +1 )
      EXIT
   CASE "buttonViewZoomedOut_clicked"
      ::stackZoom( -1 )
      EXIT
   /* Ends: MDI actions */

   CASE "mdiSubWindow_windowStateChanged"
      IF ! empty( ::oIde:aMdies )
         IF ( n := ascan( ::oIde:aMdies, {|o| o == p } ) )  > 0
            ::aViewsInfo[ n, 3 ] := p1[ 1 ]
            ::aViewsInfo[ n, 4 ] := p1[ 2 ]
         ENDIF
         IF p1[ 2 ] >= 8 .AND. !( ::cWrkView == p:objectName() )
            ::setView( p:objectName() )
            IF ! empty( ::oEM ) .AND. ! empty( oEdit := ::oEM:getEditorCurrent() )
               oEdit:setDocumentProperties()
               oEdit:qCoEdit:relayMarkButtons()
               oEdit:qCoEdit:toggleLineNumbers()
               oEdit:qCoEdit:toggleHorzRuler()
               oEdit:qCoEdit:toggleCurrentLineHighlightMode()
               oEdit:qCoEdit:dispStatusInfo()
               ::oUpDn:show()
               oEdit:changeThumbnail()
            ENDIF
         ENDIF
      ENDIF
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:restState( nMode )
   LOCAL qMdi
   HB_SYMBOL_UNUSED( nMode )
   FOR EACH qMdi IN ::oIde:aMdies
      qMdi:setWindowState( Qt_WindowNoState )
   NEXT
   ::restPanelsGeometry()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:stackMaximized()
   LOCAL qObj, qMdi
   qObj := ::oStackedWidget:activeSubWindow()
   FOR EACH qMdi IN ::oIde:aMdies
      qMdi:setWindowState( Qt_WindowMaximized )
   NEXT
   ::oStackedWidget:setActiveSubWindow( qObj )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:stackZoom( nMode )
   LOCAL qMdi, nT, nL, nH, nW, qRect

   HB_SYMBOL_UNUSED( nMode )

   IF ::oINI:nEditsViewStyle == 4 .OR. ::oINI:nEditsViewStyle == 5
      IF ::oINI:nEditsViewStyle == 4
         nT := 0
         FOR EACH qMdi IN ::oIde:aMdies
            qRect := qMdi:geometry()
            nH := qRect:height() + ( nMode * ( qRect:height() / 4 ) )
            qMdi:setGeometry( QRect( 0, nT, qRect:width(), nH ) )
            nT += nH
         NEXT
      ELSE
         nL := 0
         FOR EACH qMdi IN ::oIde:aMdies
            qRect := qMdi:geometry()
            nW := qRect:width() + ( nMode * ( qRect:width() / 4 ) )
            qMdi:setGeometry( QRect( nL, 0, nW, qRect:height() ) )
            nL += nW
         NEXT
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:stackHorizontally()
   LOCAL qArea, qObj, qVPort, nH, nT, nW, qMdi, nL

   ::restState( 0 )

   qArea  := ::oStackedWidget
   qObj   := qArea:activeSubWindow()
   qVPort := qArea:viewport()
   nH     := qVPort:height()
   nW     := qVPort:width() / len( ::oIde:aMdies )
   nT     := 0
   nL     := 0

   FOR EACH qMdi IN ::oIde:aMdies
      qMdi:setGeometry( QRect( nL, nT, nW, nH ) )
      nL += nW
   NEXT

   ::oStackedWidget:setActiveSubWindow( qObj )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:stackVertically()
   LOCAL qArea, qObj, qVPort, nH, nT, nW, qMdi

   ::restState( 0 )

   qArea  := ::oStackedWidget
   qObj   := qArea:activeSubWindow()
   qVPort := qArea:viewport()
   nH     := qVPort:height() / len( ::oIde:aMdies )
   nW     := qVPort:width()
   nT     := 0

   FOR EACH qMdi IN ::oIde:aMdies
      qMdi:setGeometry( QRect( 0, nT, nW, nH ) )
      nT += nH
   NEXT

   ::oStackedWidget:setActiveSubWindow( qObj )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:restPanelsGeometry()
   LOCAL a_, n
   FOR EACH a_ IN ::aViewsInfo
      IF ( n := ascan( ::oIde:aMdies, {|o| o:objectName() == a_[ 1 ] } ) ) > 0
         IF hb_isObject( a_[ 2 ] )
            ::oIde:aMdies[ n ]:setGeometry( a_[ 2 ] )
         ENDIF
      ENDIF
   NEXT
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeDocks:savePanelsGeometry()
   LOCAL a_, n
   FOR EACH a_ IN ::aViewsInfo
      IF ( n := ascan( ::oIde:aMdies, {|o| o:objectName() == a_[ 1 ] } ) ) > 0
         a_[ 2 ] := ::oIde:aMdies[ n ]:geometry()
      ENDIF
   NEXT
   RETURN Self

/*------------------------------------------------------------------------*/

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

METHOD IdeDocks:getADockWidget( nAreas, cObjectName, cWindowTitle, nFlags, cEventVisibility )
   LOCAL oDock, nBasic

   DEFAULT nFlags TO 0

   nBasic := hb_bitOR( QDockWidget_DockWidgetClosable, nFlags )

   oDock := XbpWindow():new()
   oDock:oWidget := QDockWidget( ::oDlg:oWidget )
   oDock:oWidget:setObjectName( cObjectName )
   ::oDlg:addChild( oDock )
   oDock:oWidget:setFeatures( nBasic )
   oDock:oWidget:setAllowedAreas( nAreas )
   oDock:oWidget:setWindowTitle( cWindowTitle )
   oDock:oWidget:setFocusPolicy( Qt_NoFocus )
   oDock:oWidget:setStyleSheet( getStyleSheet( "QDockWidget", ::nAnimantionMode ) )
   oDock:hide()

   IF !empty( cEventVisibility )
      oDock:oWidget:connect( cEventVisibility, {|p| ::execEvent( cEventVisibility, p, oDock:oWidget ) } )
   ENDIF

   RETURN oDock

/*----------------------------------------------------------------------*/

METHOD IdeDocks:setViewInitials()
   LOCAL a_

   FOR EACH a_ IN ::aViewsInfo
      ::setView( a_[ 1 ] )

      IF ::qTabWidget:count() == 1
         ::oEM:setSourceVisibleByIndex( 0 )
      ELSE
#if 0         
         ::qTabWidget:setCurrentIndex( 0 )
         ::qTabWidget:setCurrentIndex( ::qTabWidget:count() - 1 )
         ::qTabWidget:setCurrentIndex( 0 )
#endif          
      ENDIF
   NEXT

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeDocks:setView( cView )
   LOCAL n, nIndex

   ::cOldView := ::oIde:cWrkView

   SWITCH cView

   CASE "New..."
      cView := hbide_fetchAString( ::qViewsCombo, cView, "Name the View", "New View" )
      IF !( cView == "New..." ) .AND. !( cView == "Stats" ) .AND. !( cView == "Main" )
         IF ascan( ::aViewsInfo, {|e_| e_[ 1 ] == cView } ) > 0
            MsgBox( "View: " + cView + ", already exists" )
         ELSE
            aadd( ::aViewsInfo, { cView, NIL, 0, 0, 0, 0 } )
            ::oTM:addPanelsMenu( cView )
            ::buildViewWidget( cView )
            ::setView( cView )
         ENDIF
      ENDIF
      EXIT

   OTHERWISE
      IF ( n := ascan( ::aViews, {|o| iif( hb_isChar( o:oWidget:objectName() ), o:oWidget:objectName() == cView, .f. ) } ) ) > 0
         ::oIde:cWrkView := cView
         ::oIde:qTabWidget := ::aViews[ n ]:oTabWidget:oWidget
         ::oIde:oTabParent := ::aViews[ n ]

         nIndex := ::oIde:qTabWidget:currentIndex()
         IF nIndex + 1 == ::oIde:qTabWidget:count()
            IF !( ::oIde:lClosing )
               ::oIde:qTabWidget:setCurrentIndex( 0 )
               ::oIde:qTabWidget:setCurrentIndex( nIndex )  /* TODO: Must be last saved */
            ENDIF
         ENDIF

         ::oStackedWidget:oWidget:setActiveSubWindow( ::oIde:aMdies[ n ] )
         ::setStatusText( SB_PNL_VIEW, ::cWrkView )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN NIL

/*------------------------------------------------------------------------*/

METHOD IdeDocks:setToolbarSize( nSize )

   ::qMdiToolbarL:size := QSize( nSize,nSize )
   ::qMdiToolbarL:setIconSize( ::qMdiToolbarL:size )
   ::qMdiToolbarL:setMaximumWidth( nSize + 8 )

   ::qMdiToolbar:size := QSize( nSize,nSize )
   ::qMdiToolbar:setIconSize( ::qMdiToolbar:size )
   ::qMdiToolbar:setMaximumHeight( nSize + 8 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildMdiToolbarLeft()

   ::qMdiToolbarL := HbqToolbar():new()
   ::qMdiToolbarL:orientation := Qt_Vertical
   ::qMdiToolbarL:size := QSize(  val( ::oINI:cToolbarSize ), val( ::oINI:cToolbarSize ) )
   ::qMdiToolbarL:create( "EditsManager_Left_Toolbar" )
   ::qMdiToolbarL:setWindowTitle( "Toolbar: Editing Area's Left" )
   ::qMdiToolbarL:setObjectName( "ToolbarEditingAreaLeft" )
   ::qMdiToolbarL:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )

   ::qMdiToolbarL:addToolButton( "ViewTabbed"     , "Toggle tabbed view"         , hbide_image( "view_tabbed"      ), {|| ::execEvent( "buttonViewTabbed_clicked"      ) }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ViewArranged"   , "View as arranged"           , hbide_image( "view_organized"   ), {|| ::execEvent( "buttonViewOrganized_clicked"   ) }, .f. )
   ::qMdiToolbarL:addToolButton( "SaveLayout"     , "Save layout"                , hbide_image( "save3"            ), {|| ::execEvent( "buttonSaveLayout_clicked"      ) }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ViewCascaded"   , "View as cascaded"           , hbide_image( "view_cascaded"    ), {|| ::execEvent( "buttonViewCascaded_clicked"    ) }, .f. )
   ::qMdiToolbarL:addToolButton( "viewTiled"      , "View as tiled"              , hbide_image( "view_tiled"       ), {|| ::execEvent( "buttonViewTiled_clicked"       ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewMaximized"  , "View Maximized"             , hbide_image( "fullscreen"       ), {|| ::execEvent( "buttonViewMaximized_clicked"   ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewTiledVert"  , "View Vertically Tiled"      , hbide_image( "view_vertstacked" ), {|| ::execEvent( "buttonViewStackedVert_clicked" ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewTiledHorz"  , "View Horizontally Tiled"    , hbide_image( "view_horzstacked" ), {|| ::execEvent( "buttonViewStackedHorz_clicked" ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewZoomedIn"   , "View Zoom In"               , hbide_image( "view_zoomin"      ), {|| ::execEvent( "buttonViewZoomedIn_clicked"    ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewZoomedOut"  , "View Zoom Out"              , hbide_image( "view_zoomout"     ), {|| ::execEvent( "buttonViewZoomedOut_clicked"   ) }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ToggleLineNos"  , "Toggle Line Numbers"        , hbide_image( "togglelinenumber" ), {|| ::oEM:toggleLineNumbers()                      }, .f. )
   ::qMdiToolbarL:addToolButton( "ToggleHorzRuler", "Toggle Horizontal Ruler"    , hbide_image( "horzruler"        ), {|| ::oEM:toggleHorzRuler()                        }, .f. )
   ::qMdiToolbarL:addToolButton( "ToggleCurLine"  , "Toggle Current Line Hilight", hbide_image( "curlinehilight"   ), {|| ::oEM:toggleCurrentLineHighlightMode()         }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ToggleCodeComp" , "Toggle Code Completion"     , hbide_image( "help1"            ), {|| ::oEM:toggleCodeCompetion()                    }, .f. )
   ::qMdiToolbarL:addToolButton( "ToggleCompTips" , "Toggle Completion Tips"     , hbide_image( "infotips"         ), {|| ::oEM:toggleCompetionTips()                    }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ZoomIn"         , "Zoom In"                    , hbide_image( "zoomin3"          ), {|| ::oEM:zoom( +1 )                               }, .f. )
   ::qMdiToolbarL:addToolButton( "ZoomOut"        , "Zoom Out"                   , hbide_image( "zoomout3"         ), {|| ::oEM:zoom( -1 )                               }, .f. )
   ::qMdiToolbarL:addSeparator()

   IF ! ::oINI:lShowEditsLeftToolbar
      ::qMdiToolbarL:hide()
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeDocks:buildMdiToolbar()
   LOCAL qTBar, nW := 25

   STATIC sp0,sp1,sp2,sp3
   IF empty( sp0 )
      sp0 := QLabel(); sp0:setMinimumWidth( nW )
      sp1 := QLabel(); sp1:setMinimumWidth( nW )
      sp2 := QLabel(); sp2:setMinimumWidth( nW )
      sp3 := QLabel(); sp3:setMinimumWidth( nW )
   ENDIF

   ::qMdiToolbar := HbqToolbar():new()
   ::qMdiToolbar:orientation := Qt_Horizontal
   ::qMdiToolbar:size := QSize(  val( ::oINI:cToolbarSize ), val( ::oINI:cToolbarSize ) )
   ::qMdiToolbar:create( "EditsManager_Top_Toolbar" )
   ::qMdiToolbar:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )
   ::qMdiToolbar:setObjectName( "ToolbarEditingAreaTop" )
   ::qMdiToolbar:setWindowTitle( "Toolbar: Editing Area's Top" )

   qTBar := ::qMdiToolbar

   qTBar:addWidget( "Panels", ::oIde:oTM:buildPanelsButton() )
   qTBar:addWidget( "Label0", sp0 )
   qTBar:addToolButton( "Undo"      , "Undo"                       , hbide_image( "undo"          ), {|| ::oEM:undo()                        }, .f. )
   qTBar:addToolButton( "Redo"      , "Redo"                       , hbide_image( "redo"          ), {|| ::oEM:redo()                        }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "Cut"       , "Cut"                        , hbide_image( "cut"           ), {|| ::oEM:cut()                         }, .f. )
   qTBar:addToolButton( "Copy"      , "Copy"                       , hbide_image( "copy"          ), {|| ::oEM:copy()                        }, .f. )
   qTBar:addToolButton( "Paste"     , "Paste"                      , hbide_image( "paste"         ), {|| ::oEM:paste()                       }, .f. )
   qTBar:addToolButton( "SelectAll" , "Select all"                 , hbide_image( "selectall"     ), {|| ::oEM:selectAll()                   }, .f. )
   qTBar:addToolButton( "SelectionMode", "Selection mode"          , hbide_image( "stream"        ), {|| ::oEM:toggleSelectionMode(), ::oIDE:manageFocusInEditor() }, .t. )
   qTBar:addWidget( "Label1", sp1 )
   qTBar:addToolButton( "Find"      , "Find / Replace"             , hbide_image( "find"          ), {|| ::oEM:find()                        }, .f. )
   qTBar:addToolButton( "BookMark"  , "Toggle Mark"                , hbide_image( "bookmark"      ), {|| ::oEM:setMark()                     }, .f. )
   qTBar:addToolButton( "GotoLine"  , "Goto Line"                  , hbide_image( "gotoline3"     ), {|| ::oEM:goTo()                        }, .f. )
   qTBar:addToolButton( "Reload"    , "Reload Source"              , hbide_image( "view_refresh"  ), {|| ::oEM:reload()                      }, .f. )
   qTBar:addWidget( "Label2", sp2 )
   qTBar:addToolButton( "MoveUp"    , "Move Current Line Up"       , hbide_image( "movelineup"    ), {|| ::oEM:moveLine( -1 )                }, .f. )
   qTBar:addToolButton( "MoveDn"    , "Move Current Line Down"     , hbide_image( "movelinedown"  ), {|| ::oEM:moveLine(  1 )                }, .f. )
   qTBar:addToolButton( "DelLine"   , "Delete Current Line"        , hbide_image( "deleteline"    ), {|| ::oEM:deleteLine()                  }, .f. )
   qTBar:addToolButton( "Duplicate" , "Duplicate Current Line"     , hbide_image( "duplicateline" ), {|| ::oEM:duplicateLine()               }, .f. )
   qTBar:addWidget( "Label3", sp3 )
   qTBar:addToolButton( "ToUpper"   , "To Upper"                   , hbide_image( "toupper"       ), {|| ::oEM:convertSelection( "ToUpper" ) }, .f. )
   qTBar:addToolButton( "ToLower"   , "To Lower"                   , hbide_image( "tolower"       ), {|| ::oEM:convertSelection( "ToLower" ) }, .f. )
   qTBar:addToolButton( "InvertCase", "Invert Case"                , hbide_image( "invertcase"    ), {|| ::oEM:convertSelection( "Invert"  ) }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "BlockCmnt" , "Block Comment"              , hbide_image( "blockcomment"  ), {|| ::oEM:blockComment()                }, .f. )
   qTBar:addToolButton( "StreamCmnt", "Stream Comment"             , hbide_image( "streamcomment" ), {|| ::oEM:streamComment()               }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "IndentR"   , "Indent Right"               , hbide_image( "blockindentr"  ), {|| ::oEM:indent(  1 )                  }, .f. )
   qTBar:addToolButton( "IndentL"   , "Indent Left"                , hbide_image( "blockindentl"  ), {|| ::oEM:indent( -1 )                  }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "Sgl2Dbl"   , "Single to Double Quotes"    , hbide_image( "sgl2dblquote"  ), {|| ::oEM:convertDQuotes()              }, .f. )
   qTBar:addToolButton( "Dbl2Sgl"   , "Double to Single Quotes"    , hbide_image( "dbl2sglquote"  ), {|| ::oEM:convertQuotes()               }, .f. )

   IF ! ::oINI:lShowEditsTopToolbar
      ::qMdiToolbar:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:setButtonState( cButton, lChecked )
   IF ::qMdiToolbar:contains( cButton )
      RETURN ::qMdiToolbar:setItemChecked( cButton, lChecked )
   ELSEIF ::qMdiToolbarL:contains( cButton )
      RETURN ::qMdiToolbarL:setItemChecked( cButton, lChecked )
   ENDIF
   RETURN .f.

/*------------------------------------------------------------------------*/

METHOD IdeDocks:buildStackedWidget()

   ::oIde:oStackedWidget := XbpWindow():new( ::oDa )
   ::oStackedWidget:oWidget := QMdiArea( ::oDa:oWidget )
   ::oStackedWidget:oWidget:setObjectName( "editMdiArea" )
   ::oStackedWidget:oWidget:setDocumentMode( .t. )
   ::oStackedWidget:oWidget:setTabShape( QTabWidget_Triangular )
   ::oStackedWidget:oWidget:setOption( QMdiArea_DontMaximizeSubWindowOnActivation, .t. )
   ::oStackedWidget:oWidget:setVerticalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::oStackedWidget:oWidget:setHorizontalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::oStackedWidget:oWidget:setActivationOrder( QMdiArea_CreationOrder )

   ::oDa:addChild( ::oStackedWidget )

   ::oStackedWidget:oWidget:connect( "subWindowActivated(QMdiSubWindow*)", {|p| ::execEvent( "mdiArea_subWindowActivated", p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildViewWidget( cView )
   LOCAL oFrame, qTBtnClose, qDrop, qMdi, n

   qMdi := QMdiSubWindow()
   qMdi:setWindowTitle( cView )
   qMdi:setObjectName( cView )
   qMdi:setWindowIcon( ::getPanelIcon( cView ) )

   oFrame := XbpWindow():new( ::oStackedWidget )
   oFrame:oWidget := QWidget( ::oStackedWidget:oWidget )
   oFrame:oWidget:setObjectName( cView )
   ::oStackedWidget:addChild( oFrame )

   oFrame:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   oFrame:qLayout:setContentsMargins( 0,0,0,0 )

   oFrame:oTabWidget := XbpTabWidget():new():create( oFrame, , {0,0}, {200,200}, , .t. )

   qTBtnClose := QToolButton()
   qTBtnClose:setTooltip( "Close Tab" )
   qTBtnClose:setAutoRaise( .t. )
   qTBtnClose:setIcon( hbide_image( "closetab" ) )
   qTBtnClose:connect( "clicked()", {|| ::oSM:closeSource() } )
   oFrame:oTabWidget:qCornerWidget := qTBtnClose
   oFrame:oTabWidget:oWidget:setCornerWidget( qTBtnClose, Qt_TopRightCorner )

   qDrop := oFrame:oTabWidget:oWidget

   qDrop:setAcceptDrops( .t. )
   qDrop:connect( QEvent_DragEnter, {|p| ::execEvent( "editWidget_dragEnterEvent", p ) } )
   qDrop:connect( QEvent_DragMove , {|p| ::execEvent( "editWidget_dragMoveEvent" , p ) } )
   qDrop:connect( QEvent_Drop     , {|p| ::execEvent( "editWidget_dropEvent"     , p ) } )

   oFrame:oTabWidget:oWidget:setUsesScrollButtons( .t. )
   oFrame:oTabWidget:oWidget:setMovable( .t. )

   oFrame:oWidget:show()
   oFrame:oTabWidget:oWidget:show()

   aadd( ::oIde:aViews, oFrame )
   aadd( ::oIde:aMdies, qMdi   )

   IF ( n := ascan( ::aViewsInfo, {|e_| e_[ 1 ] == cView } ) ) > 0
      IF !empty( ::aViewsInfo[ n, 2 ] )
         qMdi:setGeometry( ::aViewsInfo[ n, 2 ] )
      ELSE
         qMdi:resize( 300, 200 )
      ENDIF
   ENDIF
   qMdi:setWidget( oFrame:oWidget )

   ::oStackedWidget:oWidget:addSubWindow( qMdi )
   qMdi:connect( "windowStateChanged(Qt::WindowStates,Qt::WindowStates)", ;
                              {|p,p1| ::execEvent( "mdiSubWindow_windowStateChanged", qMdi, { p, p1 } ) } )
   ::setView( cView )

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

   ::oUpDn:oUI:setParent( ::qMdiToolbarL:oWidget )
   ::oUpDn:oUI:show()
   ::qMdiToolbarL:addWidget( "UpDown", ::oUpDn:oUI:oWidget )
   ::oUpDn:oUI:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildToolBarPanels()
   LOCAL a_, aBtns, qAct
   LOCAL qSize := QSize( 20,20 )

   /* Right-hand docks toolbar */
   ::oIde:qTBarDocks := QToolBar()
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
   aadd( aBtns, { ::oReportsManagerDock , "designer"      } )
   aadd( aBtns, { ::oCuiEdDock          , "cuied"         } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oDockB2             , "builderror"    } )

   FOR EACH a_ IN aBtns
      IF empty( a_ )
         ::qTBarDocks:addSeparator()
      ELSE
         qAct := a_[ 1 ]:oWidget:toggleViewAction()
         qAct:setIcon( hbide_image( a_[ 2 ] ) )
         ::qTBarDocks:addAction( qAct )
         aadd( ::aBtnDocks, qAct )
      ENDIF
   NEXT

   ::oDlg:oWidget:addToolBar( Qt_TopToolBarArea, ::qTBarDocks )

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

   IF ( n := ascan( ::aViewsInfo, {|e_| e_[ 1 ] == cView } ) ) > 0
      RETURN hbide_image( "panel_" + hb_ntos( n ) )
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD IdeDocks:addPanelButton( cPanel )
   LOCAL qTBtn

   STATIC nIndex := 0
   nIndex++

   qTBtn := QToolButton()
   qTBtn:setMaximumHeight( 20 )
   qTBtn:setMaximumWidth( 20 )
   qTBtn:setText( cPanel )
   qTBtn:setTooltip( "Panel: " + cPanel )
   qTBtn:setIcon( hbide_image( "panel_" + hb_ntos( nIndex ) ) )
   aadd( ::aPanels, qTBtn )
   ::qTBarPanels:addWidget( qTBtn )
   qTBtn:connect( "clicked()", {|| ::setView( cPanel ) } )

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
   ::oDlg:oWidget:addDockWidget( Qt_LeftDockWidgetArea, ::oDockPT:oWidget, Qt_Vertical )

   ::oIde:oProjTree := XbpTreeView():new()
   ::oProjTree:hasLines   := .T.
   ::oProjTree:hasButtons := .T.
   ::oProjTree:create( ::oDockPT, , { 0,0 }, { 100,10 }, , .t. )

   ::oProjTree:oWidget:setStyleSheet( GetStyleSheet( "QTreeWidgetHB", ::nAnimantionMode ) )
   ::oProjTree:oWidget:setMinimumWidth( 100 )
   ::oProjTree:oWidget:setSizePolicy( QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )
   ::oProjTree:oWidget:setIconSize( QSize( 12,12 ) )
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
   qDrop:connect( QEvent_DragEnter, {|p| ::execEvent( "projectTree_dragEnterEvent", p ) } )
   qDrop:connect( QEvent_Drop     , {|p| ::execEvent( "projectTree_dropEvent"     , p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildEditorTree()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDockED := ::getADockWidget( nAreas, "dockEditorTabs", "Editors", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_LeftDockWidgetArea, ::oDockED:oWidget, Qt_Vertical )

   ::oIde:oEditTree := XbpTreeView():new()
   ::oEditTree:hasLines   := .T.
   ::oEditTree:hasButtons := .T.
   ::oEditTree:create( ::oDockED, , { 0,0 }, { 100,10 }, , .t. )

   ::oEditTree:oWidget:setSizePolicy( QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )
   ::oEditTree:oWidget:setMinimumWidth( 100 )
   ::oEditTree:oWidget:setIconSize( QSize( 12,12 ) )
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
   ::oDlg:oWidget:addDockWidget( Qt_LeftDockWidgetArea, ::oSkltnsTreeDock:oWidget, Qt_Vertical )
   ::oSkltnsTreeDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockSkltnsTree_visibilityChanged", p, ::oSkltnsTreeDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFuncList()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oFuncDock := ::getADockWidget( nAreas, "dockFuncList", "Functions List", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oFuncDock:oWidget, Qt_Vertical )
   ::oFuncDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "oFuncDock_visibilityChanged", p, ::oFuncDock:oWidget ) } )

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
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oHelpDock:oWidget, Qt_Horizontal )

   ::oIde:qHelpBrw := QTextBrowser( ::oHelpDock:oWidget )
   ::qHelpBrw:show()
   ::qHelpBrw:setContextMenuPolicy( Qt_CustomContextMenu )
   ::qHelpBrw:setOpenExternalLinks( .t. )

   qUrl := QUrl( "idemainpage.html" )
   qStr := QStringList()
   qStr:append( hb_dirBase() + "docs" )

   ::qHelpBrw:setSearchPaths( qStr )
   ::qHelpBrw:setSource( qUrl )

   ::oHelpDock:oWidget:setWidget( ::oIde:qHelpBrw )

   //::oHelpDock:connect( ::qHelpBrw, "customContextMenuRequested(QPoint)", {|p| ::execEvent( "qHelpBrw_contextMenuRequested", p ) } )
   ::qHelpBrw:connect( "customContextMenuRequested(QPoint)", {|p| ::execEvent( "qHelpBrw_contextMenuRequested", p ) } )

   ::oHelpDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockHelpDock_visibilityChanged", p, ::oHelpDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildCompileResults()

   ::oIde:oDockB := ::getADockWidget( Qt_BottomDockWidgetArea, "dockCompileResults", "Compile Results" )
   ::oDlg:oWidget:addDockWidget( Qt_BottomDockWidgetArea, ::oDockB:oWidget, Qt_Horizontal )

   ::oIde:oCompileResult := XbpMLE():new( ::oDockB ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oDockB:oWidget:setWidget( ::oCompileResult:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildLinkResults()

   ::oIde:oDockB1 := ::getADockWidget( Qt_BottomDockWidgetArea, "dockLinkResults", "Link Results" )
   ::oDlg:oWidget:addDockWidget( Qt_BottomDockWidgetArea, ::oDockB1:oWidget, Qt_Horizontal )

   ::oIde:oLinkResult := XbpMLE():new( ::oDockB1 ):create( , , { 0,0 }, { 100, 400 }, , .T. )
   ::oDockB1:oWidget:setWidget( ::oLinkResult:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildOutputResults()
   LOCAL nAreas := Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDockB2 := ::getADockWidget( nAreas, "dockOutputResults", "Output Console" )//, QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_BottomDockWidgetArea, ::oDockB2:oWidget, Qt_Horizontal )

   ::oIde:oOutputResult := XbpRtf():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .T. )
   ::oOutputResult:oWidget:setAcceptRichText( .T. )
   ::oOutputResult:oWidget:setReadOnly( .T. )
   ::oOutputResult:setContextMenuPolicy( Qt_CustomContextMenu )

   ::oDockB2:oWidget:setWidget( ::oOutputResult:oWidget )

   ::oOutputResult:oWidget:connect( "customContextMenuRequested(QPoint)", {|p| ::execEvent( "outputConsole_contextMenuRequested", p ) } )
   ::oOutputResult:oWidget:connect( "copyAvailable(bool)", {|l| ::outputDoubleClicked( l ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:outputDoubleClicked( lSelected )
   LOCAL qCursor, cText
   LOCAL cSource, nLine

   IF lSelected
      qCursor := ::oOutputResult:oWidget:textCursor()
      cText := qCursor:block():text()

      IF hbide_parseFNfromStatusMsg( cText, @cSource, @nLine, .T. )
         IF ::oSM:editSource( cSource, 0, 0, 0, NIL, NIL, .f., .t. )
            qCursor := ::oIde:qCurEdit:textCursor()
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

METHOD IdeDocks:setStatusText( nPart, xValue )
   LOCAL oPanel

   IF ! hb_isObject( ::oSBar )
      RETURN Self
   ENDIF

   oPanel := ::oSBar:getItem( nPart )

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
      oPanel:caption := "<font color = brown >Enc: "  + xValue + "</font>"
      EXIT
   CASE SB_PNL_ENVIRON
      xValue := iif( empty( xValue ), "default", xValue )
      oPanel:caption := "<font color = blue  >Env: "    + xValue + "</font>"
      EXIT
   CASE SB_PNL_VIEW
      oPanel:caption := "<font color = green >Panel: "   + xValue + "</font>"
      EXIT
   CASE SB_PNL_PROJECT
      xValue := iif( empty( xValue ), "none", xValue )
      oPanel:caption := "<font color = darkred >Prj: " + xValue + "</font>"
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

   ::oIde:aMarkTBtns[ nIndex ] := QToolButton()

   ::oIde:aMarkTBtns[ nIndex ]:setMaximumHeight( 12 )
   ::oIde:aMarkTBtns[ nIndex ]:setMaximumWidth( 12 )
   ::oIde:aMarkTBtns[ nIndex ]:setStyleSheet( "background-color: " + aColors[ nIndex ] + ";" )
   ::oIde:aMarkTBtns[ nIndex ]:hide()
   ::oIde:aMarkTBtns[ nIndex ]:connect( "clicked()", {|| ::oEM:gotoMark( nIndex ) } )

   RETURN ::oIde:aMarkTBtns[ nIndex ]

/*----------------------------------------------------------------------*/

METHOD IdeDocks:animateComponents( nMode )
   LOCAL cStyle, oView, oMenu

   IF nMode == NIL
      ::oIde:nAnimantionMode := iif( ::nAnimantionMode == HBIDE_ANIMATION_NONE, HBIDE_ANIMATION_GRADIENT, HBIDE_ANIMATION_NONE )
      nMode := ::nAnimantionMode
   ENDIF
   ::oIde:nAnimantionMode := nMode
   ::oIde:oINI:cIdeAnimated := hb_ntos( ::nAnimantionMode )

   ::qAnimateAction:setChecked( ::nAnimantionMode != HBIDE_ANIMATION_NONE )

   /* Main Window */
   ::oDlg:oWidget:setStyleSheet( GetStyleSheet( "QMainWindow", ::nAnimantionMode ) )

   /* Main Menu Bar with all its submenus */
   ::oDlg:menubar():oWidget:setStyleSheet( GetStyleSheet( "QMenuBar", nMode ) )
   FOR EACH oMenu IN ::oDlg:menubar():childList()
      oMenu:oWidget:setStyleSheet( GetStyleSheet( "QMenuPop", nMode ) )
   NEXT

   /* Toolbars */
   ::oMainToolbar:oWidget:setStyleSheet( GetStyleSheet( "QToolBar", nMode ) )
   ::qTBarDocks  :setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )

   ::qMdiToolbar:setStyleSheet( GetStyleSheet( "QToolBar", nMode ) )
   ::qMdiToolbarL:setStyleSheet( GetStyleSheet( "QToolBarLR5", nMode ) )

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

   ::oProjTree:oWidget:setStyleSheet( GetStyleSheet( "QTreeWidgetHB", ::nAnimantionMode ) )

   /* Edior Tab Widget */
   FOR EACH oView IN ::aViews
      oView:oTabWidget:oWidget:setStyleSheet( GetStyleSheet( "QTabWidget", nMode ) )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildThemesDock()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oThemesDock := ::getADockWidget( nAreas, "dockThemes", "Theme Manager", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oThemesDock:oWidget, Qt_Horizontal )
   ::oThemesDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockThemes_visibilityChanged", p, ::oThemesDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildPropertiesDock()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oPropertiesDock := ::getADockWidget( nAreas, "dockProperties", "Project Properties", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oPropertiesDock:oWidget, Qt_Horizontal )
   ::oPropertiesDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockProperties_visibilityChanged", p, ::oPropertiesDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFindInFiles()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oFindDock := ::getADockWidget( nAreas, "dockFindInFiles", "Find in Files", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oFindDock:oWidget, Qt_Horizontal )
   ::oFindDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockFindInFiles_visibilityChanged", p, ::oFindDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDocViewer()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDocViewDock := ::getADockWidget( nAreas, "dockDocViewer", "Harbour Documentation", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oDocViewDock:oWidget, Qt_Horizontal )
   ::oDocViewDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockDocViewer_visibilityChanged", p, ::oDocViewDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildDocWriter()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oDocWriteDock := ::getADockWidget( nAreas, "dockDocWriter", "Documentation Writer", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oDocWriteDock:oWidget, Qt_Horizontal )
   ::oDocWriteDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockDocWriter_visibilityChanged", p, ::oDocWriteDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFunctionsDock()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oFunctionsDock := ::getADockWidget( nAreas, "dockFunctions", "Projects Functions Lookup", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oFunctionsDock:oWidget, Qt_Horizontal )
   ::oFunctionsDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "docFunctions_visibilityChanged", p, ::oFunctionsDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildEnvironDock()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oEnvironDock := ::getADockWidget( nAreas, "dockEnvironments", "Compiler Environments", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oEnvironDock:oWidget, Qt_Horizontal )
   ::oEnvironDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "docEnvironments_visibilityChanged", p, ::oEnvironDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSkeletonWidget()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oSkeltnDock := ::getADockWidget( nAreas, "dockSkeleton", "Code Skeletons", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oSkeltnDock:oWidget, Qt_Horizontal )
   ::oSkeltnDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "docSkeletons_visibilityChanged", p, ::oSkeltnDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildSourceThumbnail()
   LOCAL oDock
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   oDock := ::getADockWidget( nAreas, "dockSourceThumbnail", "Source Thumbnail", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, oDock:oWidget, Qt_Horizontal )
   oDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockSourceThumbnail_visibilityChanged", p, oDock:oWidget ) } )
   ::oIde:oSourceThumbnailDock := oDock

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildQScintilla()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oQScintillaDock := ::getADockWidget( nAreas, "dockQScintilla", "ideDBU", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oQScintillaDock:oWidget, Qt_Horizontal )
   ::oQScintillaDock:oWidget:connect( "visibilityChanged(bool)"  , {|p| ::execEvent( "dockQScintilla_visibilityChanged", p, ::oQScintillaDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildReportsDesignerWidget()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oReportsManagerDock := ::getADockWidget( nAreas, "dockReportDesigner", "HBReportsManager", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oReportsManagerDock:oWidget, Qt_Horizontal )
   ::oReportsManagerDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockReportsManager_visibilityChanged", p, ::oReportsManagerDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildFormatWidget()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oFormatDock := ::getADockWidget( nAreas, "dockFormat", "Format Source", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oFormatDock:oWidget, Qt_Horizontal )
   ::oFormatDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockFormat_visibilityChanged", p, ::oFormatDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocks:buildCuiEdWidget()
   LOCAL nAreas := Qt_LeftDockWidgetArea + Qt_RightDockWidgetArea + Qt_TopDockWidgetArea + Qt_BottomDockWidgetArea

   ::oIde:oCuiEdDock := ::getADockWidget( nAreas, "dockCuiEd", "CUI Screen Designer", QDockWidget_DockWidgetFloatable )
   ::oDlg:oWidget:addDockWidget( Qt_RightDockWidgetArea, ::oCuiEdDock:oWidget, Qt_Horizontal )
   ::oCuiEdDock:oWidget:connect( "visibilityChanged(bool)", {|p| ::execEvent( "dockCuiEd_visibilityChanged", p, ::oCuiEdDock:oWidget ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

