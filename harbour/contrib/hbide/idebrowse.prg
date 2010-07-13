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
 *                               27Jun2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"
#include "hbide.ch"
#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

#define  BRW_TYPE_DBF                             1
#define  BRW_TYPE_ARRAY                           2

/*------------------------------------------------------------------------*/

#define  TBL_PANEL                                1
#define  TBL_NAME                                 2
#define  TBL_ALIAS                                3
#define  TBL_DRIVER                               4
#define  TBL_INDEX                                5
#define  TBL_RECORD                               6
#define  TBL_CURSOR                               7
#define  TBL_GEOMETRY                             8
#define  TBL_NEXT                                 9

#define  TBL_VRBLS                                9

#define  SUB_ID                                   1
#define  SUB_WINDOW                               2
#define  SUB_GEOMETRY                             3
#define  SUB_BROWSER                              4
#define  SUB_NIL                                  5

/*----------------------------------------------------------------------*/

CLASS IdeBrowseManager INHERIT IdeObject

   DATA   qDbu
   DATA   qStack
   DATA   qLayout
   DATA   qVSplitter
   DATA   qToolBar

   DATA   aPanels                                 INIT  {}
   DATA   aToolBtns                               INIT  {}
   DATA   aButtons                                INIT  {}

   DATA   oCurBrw
   DATA   oCurPanel

   DATA   qPanelsMenu
   DATA   qPanelsButton
   DATA   aPanelsAct                              INIT  {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD show()
   METHOD destroy()
   METHOD buildToolbar()
   METHOD execEvent( cEvent, p, p1 )
   METHOD addTable( cFileDBF, cAlias, aInfo )
   METHOD addArray( aData, aAttr )
   METHOD getPanelNames()
   METHOD getPanelsInfo()
   METHOD addPanels()
   METHOD addPanel( cPanel )
   METHOD setPanel( cPanel )
   METHOD isPanel( cPanel )
   METHOD loadTables()
   METHOD buildPanelsButton()
   METHOD addPanelsMenu( cPanel )
   METHOD setStyleSheet( nMode )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:getPanelNames()
   LOCAL oPanel, aNames := {}, aAttr

   FOR EACH oPanel IN ::aPanels
      aAttr := {}

      aadd( aAttr, oPanel:cPanel )
      aadd( aAttr, hb_ntos( oPanel:viewMode() ) )
      aadd( aAttr, hb_ntos( oPanel:nViewStyle ) )

      aadd( aNames,  hbide_array2String( aAttr, "," ) )
   NEXT
   RETURN aNames

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:getPanelsInfo()
   LOCAL oBrw, oPanel, aSub
   LOCAL aInfo := {}, aAttr

   FOR EACH oPanel IN ::aPanels
      FOR EACH aSub IN oPanel:subWindows()
         aAttr := array( TBL_VRBLS )
         aAttr[ TBL_PANEL ] := oPanel:cPanel

         oBrw := aSub[ 4 ]

         IF oBrw:nType == BRW_TYPE_DBF
            aAttr[ TBL_NAME   ] := oBrw:cTable
            aAttr[ TBL_ALIAS  ] := oBrw:cAlias
            aAttr[ TBL_DRIVER ] := oBrw:cDriver
            aAttr[ TBL_INDEX  ] := hb_ntos( oBrw:indexOrd()  )
            aAttr[ TBL_RECORD ] := hb_ntos( oBrw:recNo()     )
            aAttr[ TBL_CURSOR ] := hb_ntos( oBrw:nCursorType )
            IF !hb_isObject( aSub[ 3 ] )
               aSub[ 3 ] := QRect():from( aSub[ 2 ]:geometry() )
            ENDIF
            aAttr[ TBL_GEOMETRY ] := hb_ntos( aSub[ 3 ]:x() )     + " " + hb_ntos( aSub[ 3 ]:y() ) + " " + ;
                                  hb_ntos( aSub[ 3 ]:width() ) + " " + hb_ntos( aSub[ 3 ]:height() )
            aAttr[ TBL_NEXT ] := ""

         ELSEIF oBrw:nType == BRW_TYPE_ARRAY
            //
         ENDIF

         aadd( aInfo, hbide_array2String( aAttr, "," ) )
      NEXT
   NEXT

   RETURN aInfo

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:setStyleSheet( nMode )

   ::qToolbar:setStyleSheet( GetStyleSheet( "QToolBar", nMode ) )
   ::qPanelsMenu:setStyleSheet( GetStyleSheet( "QMenuPop", nMode ) )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeBrowseManager:destroy()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:show()

   ::qToolbar:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )
   ::oQScintillaDock:oWidget:raise()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:create( oIde )
   LOCAL qDock

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   qDock := ::oIde:oEM:oQScintillaDock:oWidget

   ::qDbu := QWidget():new()

   qDock:setWidget( ::qDbu )

   qDock:setAcceptDrops( .t. )
   qDock:installEventFilter( ::pEvents )
   ::connect( qDock, QEvent_DragEnter, {|p| ::execEvent( "dockDbu_dragEnterEvent", p ) } )
   ::connect( qDock, QEvent_Drop     , {|p| ::execEvent( "dockDbu_dropEvent"     , p ) } )

   /* Layout applied to dbu widget */
   ::qLayout := QVBoxLayout():new()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setSpacing( 2 )

   ::qDbu:setLayout( ::qLayout )

   /* Toolbar */
   ::buildToolbar()
   ::qLayout:addWidget( ::qToolbar )

   /* Stacked widget */
   ::qStack := QStackedWidget():new()
   ::qLayout:addWidget( ::qStack )

   /* Panels on the stacked widget */
   ::addPanels()

   /* Spread tables onto panels */
   ::loadTables()

   /* Switch to the default panel */
   ::setPanel( "Main" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:buildToolbar()
   LOCAL qBtn, aBtn

   ::qToolbar := QToolbar():new()
   ::qToolbar:setIconSize( QSize():new( 16,16 ) )
   ::qToolbar:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )

   aadd( ::aButtons, { "Open a table"       , "dc_plus"       , "clicked()", {|| ::execEvent( "buttonOpen_clicked"          ) }, .f. } )
   aadd( ::aButtons, {} )
   aadd( ::aButtons, { "Show/hide form view", "formview"      , "clicked()", {|| ::execEvent( "buttonShowForm_clicked"      ) }, .t. } )
   aadd( ::aButtons, {} )
   aadd( ::aButtons, { "Toggle View"        , "view_tabbed"   , "clicked()", {|| ::execEvent( "buttonViewTabbed_clicked"    ) }, .f. } )
   aadd( ::aButtons, {} )
   aadd( ::aButtons, { "View as arranged"   , "view_organized", "clicked()", {|| ::execEvent( "buttonViewOrganized_clicked" ) }, .f. } )
   aadd( ::aButtons, { "View as cascaded"   , "view_cascaded" , "clicked()", {|| ::execEvent( "buttonViewCascaded_clicked"  ) }, .f. } )
   aadd( ::aButtons, { "View as tiled"      , "view_tiled"    , "clicked()", {|| ::execEvent( "buttonViewTiled_clicked"     ) }, .f. } )
   aadd( ::aButtons, {} )
   aadd( ::aButtons, { "Close current table", "dc_delete"     , "clicked()", {|| ::execEvent( "buttonClose_clicked"         ) }, .f. } )
   aadd( ::aButtons, {} )

   FOR EACH aBtn IN ::aButtons
      IF empty( aBtn )
         ::qToolbar:addSeparator()
      ELSE
         qBtn := QToolButton():new()
         qBtn:setTooltip( aBtn[ 1 ] )
         qBtn:setAutoRaise( .t. )
         qBtn:setIcon( hbide_image( aBtn[ 2 ] ) )
         IF aBtn[ 5 ]
            qBtn:setCheckable( .t. )
         ENDIF
         ::connect( qBtn, aBtn[ 3 ],  aBtn[ 4 ] )
         ::qToolBar:addWidget( qBtn )
         aadd( ::aToolBtns, qBtn )
      ENDIF
   NEXT

   ::buildPanelsButton()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addPanels()
   LOCAL cPanel, aPnl

   ::addPanel( "Main", .t. )           /* The default one */

   FOR EACH cPanel IN ::oINI:aDbuPanelNames
      aPnl := hb_aTokens( cPanel, "," )
      aSize( aPnl, 2 )
      IF empty( aPnl[ 2 ] )
         aPnl[ 2 ] := "NO"
      ENDIF
      IF aPnl[ 1 ] != "Main"
         ::addPanel( aPnl[ 1 ], aPnl[ 2 ] == "YES" )
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addPanel( cPanel )
   LOCAL qPanel

   qPanel := IdeBrowsePanel():new( ::oIde, cPanel )

   ::qStack:addWidget( qPanel:qWidget )

   aadd( ::aPanels, qPanel )

   ::addPanelsMenu( cPanel )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addPanelsMenu( cPanel )
   LOCAL qAct

   ( qAct := QAction():from( ::qPanelsMenu:addAction( cPanel ) ) ):setIcon( hbide_image( "panel_7" ) )
   ::connect( qAct, "triggered(bool)", {|| ::setPanel( cPanel ) } )
   aadd( ::aPanelsAct, qAct )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:isPanel( cPanel )
   RETURN ascan( ::aPanels, {|o| o:qWidget:objectName() == cPanel } ) > 0

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:setPanel( cPanel )
   LOCAL n

   IF ( n := ascan( ::aPanels, {|o| o:qWidget:objectName() == cPanel } ) ) > 0
      ::qStack:setCurrentWidget( ::aPanels[ n ]:qWidget )
      ::oCurPanel := ::aPanels[ n ]

      ::oCurPanel:prepare()

      #if 0
      ::oCurPanel:nViewStyle  := 0

      ::oCurPanel:saveGeometry()
      ::oCurPanel:tileSubWindows()
      ::oCurPanel:nViewStyle  := 1
      ::oCurPanel:prepare()

      ::oCurPanel:nViewStyle  := 0
      ::oCurPanel:restGeometry()
      ::oCurPanel:prepare()
      #endif
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:execEvent( cEvent, p, p1 )
   LOCAL cTable, cPath, cPanel, qEvent, qMime, qList, i, cExt, qUrl

   HB_SYMBOL_UNUSED( p )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH cEvent
   CASE "dockDbu_dragEnterEvent"
      qEvent := QDragEnterEvent():from( p )
      qEvent:acceptProposedAction()
      EXIT

   CASE "dockDbu_dropEvent"
      qEvent := QDropEvent():from( p )
      qMime := QMimeData():from( qEvent:mimeData() )
      IF qMime:hasUrls()
         qList := QStringList():from( qMime:hbUrlList() )
         FOR i := 0 TO qList:size() - 1
            qUrl := QUrl():new( qList:at( i ) )
            hb_fNameSplit( qUrl:toLocalFile(), @cPath, @cTable, @cExt )
            IF lower( cExt ) == ".dbf"
               ::addTable( hbide_pathToOSPath( cPath + cTable + cExt ) )
            ENDIF
            HB_TRACE( HB_TR_ALWAYS, cPath, cTable, cExt )
         NEXT
      ENDIF
      EXIT

   CASE "buttonShowForm_clicked"
      IF !empty( ::oCurBrw )
         IF ::oCurBrw:qForm:isHidden()
            ::oCurBrw:qForm:show()
            ::aToolBtns[ 3 ]:setChecked( .t. )
         ELSE
            ::oCurBrw:qForm:hide()
            ::aToolBtns[ 3 ]:setChecked( .f. )
         ENDIF
      ENDIF
      EXIT

   CASE "buttonCloseX_clicked"
      ::oCurPanel:destroyByX( p )
      EXIT

   CASE "buttonClose_clicked"
      IF !empty( ::oCurBrw )
         ::oCurPanel:destroy( ::oCurBrw )
      ENDIF
      EXIT

   CASE "buttonOpen_clicked"
      IF !empty( cTable := hbide_fetchAFile( ::oIde:oDlg, "Select a Table", { { "Database File", "*.dbf" } }, ::oIde:cWrkFolderLast ) )
         hb_fNameSplit( cTable, @cPath )
         ::oIde:cWrkFolderLast := cPath
         ::addTable( cTable )
      ENDIF
      EXIT

   CASE "qPanelsButton_clicked"
      cPanel := hbide_fetchAString( ::qToolbar, "New...", "Name the Panel", "New Panel" )
      IF cPanel != "New..." .AND. cPanel != "Main"
         IF ::isPanel( cPanel )
            MsgBox( "Panel: " + cPanel + ", already exists" )
         ELSE
            ::addPanel( cPanel )
            ::setPanel( cPanel )
         ENDIF
      ENDIF
      EXIT

   CASE "mdiSubWindow_aboutToActivate"
//      ::oCurPanel:prepare( p )
      EXIT

   CASE "mdiSubWindow_windowStateChanged"
//HB_TRACE( HB_TR_ALWAYS, "mdiSubWindow_windowStateChanged", p, p1 )
      EXIT

   CASE "buttonViewOrganized_clicked"
      ::oCurPanel:nViewStyle  := 0
      ::oCurPanel:restGeometry()
      ::oCurPanel:prepare()
      EXIT

   CASE "buttonViewTiled_clicked"
      ::oCurPanel:saveGeometry()
      ::oCurPanel:tileSubWindows()
      ::oCurPanel:nViewStyle  := 1
      ::oCurPanel:prepare()
      EXIT

   CASE "buttonViewCascaded_clicked"
      ::oCurPanel:saveGeometry()
      ::oCurPanel:cascadeSubWindows()
      ::oCurPanel:nViewStyle  := 2
      ::oCurPanel:prepare()
      EXIT

   CASE "buttonViewTabbed_clicked"
      ::oCurPanel:setViewMode( iif( ::oCurPanel:viewMode() == QMdiArea_TabbedView, QMdiArea_SubWindowView, QMdiArea_TabbedView ) )
      EXIT

   ENDSWITCH

   #if 0
   activateNextSubWindow()
   activatePreviousSubWindow()
   closeActiveSubWindow()
   closeAllSubWindows()
   setActiveSubWindow( QMdiSubWindow )
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:loadTables()
   LOCAL cInfo, aPanel, oCurPanel, cTable

   oCurPanel := ::oCurPanel

   FOR EACH cInfo IN ::oINI:aDbuPanelsInfo
      aPanel := hb_aTokens( cInfo, "," )

      aSize( aPanel, TBL_VRBLS )

      DEFAULT aPanel[ TBL_PANEL    ] TO ""
      DEFAULT aPanel[ TBL_NAME     ] TO ""
      DEFAULT aPanel[ TBL_ALIAS    ] TO ""
      DEFAULT aPanel[ TBL_DRIVER   ] TO ""
      DEFAULT aPanel[ TBL_INDEX    ] TO 0
      DEFAULT aPanel[ TBL_RECORD   ] TO 0
      DEFAULT aPanel[ TBL_CURSOR   ] TO 0
      DEFAULT aPanel[ TBL_GEOMETRY ] TO ""
      DEFAULT aPanel[ TBL_NEXT     ] TO ""

      IF ::isPanel( aPanel[ 1 ] )
         IF hb_fileExists( cTable := hbide_pathToOSPath( aPanel[ 2 ] ) )
            ::setPanel( aPanel[ 1 ] )
            ::addTable( cTable, , aPanel )
         ENDIF
      ENDIF
   NEXT

   ::qStack:setCurrentWidget( oCurPanel )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addTable( cFileDBF, cAlias, aInfo )
   LOCAL oBrw, qSubWindow

   oBrw := IdeBrowse():new()
   oBrw:cTable   := cFileDBF
   oBrw:cAlias   := cAlias
   oBrw:oManager := Self

   oBrw:create()

   IF empty( oBrw:oBrw )
      RETURN Self
   ENDIF

   qSubWindow := ::oCurPanel:addBrowser( oBrw, aInfo )

   ::connect( qSubWindow, "aboutToActivate()", {|| ::execEvent( "mdiSubWindow_aboutToActivate", qSubWindow ) } )
   ::connect( qSubWindow, "windowStateChanged(Qt::WindowStates,Qt::WindowStates)", ;
                                 {|p,p1| ::execEvent( "mdiSubWindow_windowStateChanged", p, p1, qSubWindow ) } )

   qSubWindow:installEventFilter( ::pEvents )
   ::connect( qSubWindow, QEvent_Close, {|| ::execEvent( "buttonCloseX_clicked", qSubWindow ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addArray( aData, aAttr )

   HB_SYMBOL_UNUSED( aData )
   HB_SYMBOL_UNUSED( aAttr )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:buildPanelsButton()

   ::qPanelsMenu := QMenu():new()
   ::qPanelsMenu:setStyleSheet( GetStyleSheet( "QMenuPop", ::nAnimantionMode ) )

   ::qPanelsButton := QToolButton():new()
   ::qPanelsButton:setTooltip( "ideDBU Panels" )
   ::qPanelsButton:setIcon( hbide_image( "panel_8" ) )
   ::qPanelsButton:setPopupMode( QToolButton_MenuButtonPopup )
   ::qPanelsButton:setMenu( ::qPanelsMenu )

   ::connect( ::qPanelsButton, "clicked()", {|| ::execEvent( "qPanelsButton_clicked" ) } )

   ::qToolbar:addWidget( ::qPanelsButton )

   RETURN Self

/*----------------------------------------------------------------------*/
//
//                         Class IdeBrowsePanel
//
/*----------------------------------------------------------------------*/

CLASS IdeBrowsePanel INHERIT IdeObject

   DATA   qWidget
   DATA   qMenuWindows

   DATA   cPanel                                  INIT  ""
   DATA   nViewStyle                              INIT  0    /* 0=asWindows 1=tabbed */
   DATA   lLayoutLocked                           INIT  .f.

   DATA   aSubWindows                             INIT  {}
   ACCESS subWindows()                            INLINE ::aSubWindows

   METHOD new( oIde, cPanel )
   METHOD destroy( oBrw )
   METHOD destroyByX( qSubWindow )

   METHOD addBrowser( oBrw, aInfo )
   METHOD prepare()
   METHOD saveGeometry()
   METHOD restGeometry()

   METHOD viewMode()                              INLINE ::qWidget:viewMode()
   METHOD setViewMode( nMode )                    INLINE ::qWidget:setViewMode( nMode )
   METHOD tileSubWindows()                        INLINE ::qWidget:tileSubWindows()
   METHOD cascadeSubWindows()                     INLINE ::qWidget:cascadeSubWindows()
   METHOD activateNextSubWindow()                 INLINE ::qWidget:activateNextSubWindow()
   METHOD activatePreviousSubWindow()             INLINE ::qWidget:activatePreviousSubWindow()

   #if 0
   closeActiveSubWindow()
   closeAllSubWindows()
   setActiveSubWindow( QMdiSubWindow )
   #endif

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeBrowsePanel:new( oIde, cPanel )

   ::oIde  := oIde
   ::cPanel := cPanel

   ::qWidget := QMdiArea():new()
   ::qWidget:setObjectName( ::cPanel )
   ::qWidget:setDocumentMode( .t. )
   ::qWidget:setOption( QMdiArea_DontMaximizeSubWindowOnActivation, .t. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowsePanel:destroy( oBrw )
   LOCAL n, oSub

   IF ( n := ascan( ::aSubWindows, {|e_| e_[ 4 ] == oBrw } ) )  > 0
      oSub := ::aSubWindows[ n, 2 ]

      ::qWidget:removeSubWindow( oSub )
      oBrw:destroy()
      oSub := NIL

      hb_adel( ::aSubWindows, n, .t. )
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeBrowsePanel:destroyByX( qSubWindow )
   LOCAL n, oSub

   IF ( n := ascan( ::aSubWindows, {|e_| e_[ 2 ] == qSubWindow } ) )  > 0
      oSub := ::aSubWindows[ n, 2 ]

      ::qWidget:removeSubWindow( oSub )
      ::aSubWindows[ n, 4 ]:destroy()
      oSub := NIL

      hb_adel( ::aSubWindows, n, .t. )
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeBrowsePanel:prepare()
   LOCAL aSub

   FOR EACH aSub IN ::aSubWindows
      aSub[ SUB_BROWSER ]:configure()
   NEXT

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeBrowsePanel:addBrowser( oBrw, aInfo )
   LOCAL qSubWindow, qRect, cR

   STATIC nID := 0

   nID++                 /* Unique for current run */

   qSubWindow := QMdiSubWindow():new( ::oDlg:oWidget )

   qSubWindow:setWidget( oBrw:oWnd:oWidget )
   qSubWindow:setWindowTitle( oBrw:cTable )
   qSubWindow:setObjectName( hb_ntos( nID ) )

   IF !empty( aInfo ) .AND. !empty( aInfo[ TBL_GEOMETRY ] )
      qRect := hb_aTokens( aInfo[ TBL_GEOMETRY ], " " )
      FOR EACH cR IN qRect
         cR := val( cR )
      NEXT
      qRect := QRect():new( qRect[ 1 ], qRect[ 2 ], qRect[ 3 ], qRect[ 4 ] )
      qSubWindow:setGeometry( qRect )
   ELSE
      //qSubWindow:resize( 300, 200 )
   ENDIF

   ::qWidget:addSubWindow( qSubWindow )

   oBrw:oWnd:oWidget:show()
   qSubWindow:show()

   aadd( ::aSubWindows, { nId, qSubWindow, qRect, oBrw, NIL } )

   RETURN qSubWindow

/*------------------------------------------------------------------------*/

METHOD IdeBrowsePanel:saveGeometry()
   LOCAL aSubWindow

   IF ::nViewStyle == 0                 /* Only if in self organized mode */
      FOR EACH aSubWindow IN ::aSubWindows
         aSubWindow[ 3 ] := QRect():from( aSubWindow[ 2 ]:geometry() )
      NEXT
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeBrowsePanel:restGeometry()
   LOCAL aSubWindow

   IF ::nViewStyle == 0
      FOR EACH aSubWindow IN ::aSubWindows
         IF hb_isObject( aSubWindow[ 3 ] )
            aSubWindow[ 2 ]:setGeometry( aSubWindow[ 3 ] )
         ENDIF
      NEXT
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//
//                            Class IdeBrowse
//
/*----------------------------------------------------------------------*/

CLASS IdeBrowse INHERIT IdeObject

   DATA   oWnd
   DATA   oBrw
   DATA   qLayout
   DATA   qForm
   DATA   qFLayout
   DATA   qSplitter
   DATA   aForm                                   INIT  {}
   DATA   oManager

   DATA   nType                                   INIT  BRW_TYPE_DBF
   DATA   cAlias                                  INIT  ""
   DATA   cTable                                  INIT  ""
   DATA   aData                                   INIT  {}
   DATA   aStruct                                 INIT  {}
   DATA   aAttr                                   INIT  {}
   DATA   nIndex                                  INIT  0
   DATA   cDriver                                 INIT  "DBFCDX"
   DATA   cIndex                                  INIT  ""
   DATA   nOrder                                  INIT  0
   DATA   nArea                                   INIT  0
   DATA   nCursorType                             INIT  XBPBRW_CURSOR_CELL
   DATA   lOpened                                 INIT  .f.

   DATA   qVerSpl
   DATA   qClose

   ACCESS indexOrd()                              INLINE ::nOrder

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD configure()
   METHOD destroy()
   METHOD execEvent( cEvent, p, p1 )
   METHOD buildBrowser()
   METHOD buildColumns()
   METHOD dataLink( nField )
   METHOD getPP( aStruct )

   METHOD skipBlock( nHowMany )
   METHOD goTop()
   METHOD goBottom()
   METHOD goTo( nRec )
   METHOD recNo()
   METHOD lastRec()
   METHOD next()
   METHOD previous()
   METHOD buildForm()
   METHOD populateForm()
   METHOD fetchAlias( cTable )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:fetchAlias( cTable )
   LOCAL cFile

   STATIC n := 0
   n++

   hb_fNameSplit( cTable, , @cFile )

   RETURN upper( "C" + cFile + hb_ntos( n ) )

/*------------------------------------------------------------------------*/

METHOD IdeBrowse:create( oIde )
   LOCAL xVrb, cT, cAlias, bError, oErr
   LOCAL lMissing := .t.
   LOCAL lErr := .f.

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   IF !empty( ::aData )
      ::nType := BRW_TYPE_ARRAY
   ENDIF

   #if 0
   IF empty( ::cAlias )
      ::cAlias := ::fetchAlias( ::cTable )
   ENDIF
   #endif

   IF ::nType == BRW_TYPE_DBF
      IF !empty( ::cAlias ) .AND. empty( ::cTable )
         IF select( ::cAlias ) > 0
            lMissing := .f.
         ENDIF
      ENDIF

      IF lMissing .AND. !empty( ::cTable )
         bError := ErrorBlock( {|o| break( o ) } )
         BEGIN SEQUENCE
            IF empty( ::cAlias )
               USE ( ::cTable ) SHARED NEW VIA ( ::cDriver )
            ELSE
               cAlias := ::cAlias
               USE ( ::cTable ) ALIAS ( cAlias ) SHARED NEW VIA ( ::cDriver )
            ENDIF
            IF NetErr()
               MsgBox( ::cTable, "Could not been opened!" )
               lErr := .t.
            ENDIF
         RECOVER USING oErr
            MsgBox( oErr:description, "Error Opening Table" )
            RETURN Self
         ENDSEQUENCE
         ErrorBlock( bError )
         IF lErr
            RETURN Self
         ENDIF
         IF empty( ::cAlias )
            ::cAlias := alias()
         ENDIF
         ::lOpened := .t.
      ENDIF

      ::aStruct := DbStruct()
   ELSE
      FOR EACH xVrb IN ::aData[ 1 ]
         cT := valtype( xVrb )
         aadd( ::aStruct, "Fld_" + hb_ntos( xVrb:__enumIndex() ) )
         aadd( ::aStruct, cT )
         IF cT == "N"
            aadd( ::aStruct, 12 )
            aadd( ::aStruct,  2 )
         ELSEIF cT == "D"
            aadd( ::aStruct,  8 )
            aadd( ::aStruct,  0 )
         ELSEIF cT == "L"
            aadd( ::aStruct,  1 )
            aadd( ::aStruct,  0 )
         ELSE
            aadd( ::aStruct, len( xVrb ) )
            aadd( ::aStruct,  0 )
         ENDIF
      NEXT
   ENDIF

   ::buildBrowser()
   ::buildColumns()
   ::buildForm()

   ::oBrw:configure()
   ::oBrw:forceStable()

   ::oBrw:navigate := {|mp1,mp2| ::execEvent( "browse_navigate", mp1, mp2 ) }

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:destroy()
   IF !empty( ::oWnd )
      ::qLayout:removeWidget( ::qSplitter )
      ::oWnd:destroy()
      ::qForm := NIL
      IF ::lOpened
         ( ::cAlias )->( dbCloseArea() )
      ENDIF
      ::oManager:oCurBrw := NIL
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:configure()
   LOCAL nOff
   LOCAL nRowPos := ::oBrw:rowPos()
   LOCAL nColPos := ::oBrw:colPos()

   ::oBrw:configure()

   IF nRowPos > ::oBrw:rowCount()
      nOff := nRowPos - ::oBrw:rowCount()
      ::oBrw:rowPos := ::oBrw:rowCount()
   ELSE
      nOff := 0
   ENDIF
   ::oBrw:colPos := nColPos

   ::oBrw:refreshAll()
   ::oBrw:forceStable()
   ::oBrw:setCurrentIndex( nRowPos > ::oBrw:rowCount() )
   IF nOff > 0
      SetAppEvent( xbeBRW_Navigate, XBPBRW_Navigate_Skip, nOff, ::oBrw )
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeBrowse:buildBrowser()
   LOCAL qLayout, oWnd, oXbpBrowse

   oWnd := XbpWindow():new()
   oWnd:oWidget := QWidget():new()

   qLayout := QHBoxLayout():new()
   oWnd:oWidget:setLayout( qLayout )
   qLayout:setContentsMargins( 0,0,0,0 )
   qLayout:setSpacing( 2 )

   ::qSplitter := QSplitter():new()
   ::qSplitter:setOrientation( Qt_Horizontal )

   qLayout:addWidget( ::qSplitter )

   /* Browse View */
   oXbpBrowse := XbpBrowse():new():create( oWnd, , { 0,0 }, oWnd:currentSize() )
   oXbpBrowse:setFontCompoundName( "10.Courier" )

   ::qSplitter:addWidget( oXbpBrowse:oWidget )

   oXbpBrowse:cursorMode    := ::nCursorType

   oXbpBrowse:skipBlock     := {|n| ::skipBlock( n ) }
   oXbpBrowse:goTopBlock    := {| | ::goTop()        }
   oXbpBrowse:goBottomBlock := {| | ::goBottom()     }
   //
   oXbpBrowse:firstPosBlock := {| | 1                }
   oXbpBrowse:lastPosBlock  := {| | ::lastRec()      }

   oXbpBrowse:posBlock      := {| | ::recNo()        }
   oXbpBrowse:goPosBlock    := {|n| ::goto( n )      }
   oXbpBrowse:phyPosBlock   := {| | ::recNo()        }

   /* Form View */
   ::qForm := QWidget():new()
   ::qFLayout := QFormLayout():new()
   ::qForm:setLayout( ::qFLayout )

   //::qSplitter:addWidget( ::qForm )

   ::qForm:hide()  /* Form view defaults to hidden */

   ::qLayout := qLayout
   ::oWnd    := oWnd
   ::oBrw    := oXbpBrowse

   ::qVerSpl := QSplitter():new( Qt_Vertical )
   ::qSplitter:addWidget( ::qVerSpl )

   ::qVerSpl:addWidget( ::qForm )

   ::qClose := QToolButton():new()
   ::qClose:setIcon( hbide_image( "closetab" ) )
   ::qClose:hide()

   ::qVerSpl:addWidget( ::qClose )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:execEvent( cEvent, p, p1 )

   HB_SYMBOL_UNUSED( p  )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH cEvent
   CASE "browse_navigate"
      ::populateForm()
      ::oManager:oCurBrw := Self
      ::oManager:aToolBtns[ 3 ]:setChecked( ! ::qForm:isHidden() )
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_xtosForForm( xVrb )
   LOCAL cType := valtype( xVrb )

   DO CASE
   CASE cType == "N" ; RETURN ltrim( str( xVrb ) )
   CASE cType == "L" ; RETURN iif( xVrb, "YES", "NO" )
   CASE cType == "D" ; RETURN dtos( xVrb )
   CASE cType == "C" ; RETURN trim( xVrb )
   ENDCASE

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:populateForm()
   LOCAL a_, oCol

   IF ::nType == BRW_TYPE_DBF
      FOR EACH a_ IN ::aForm
         oCol := ::oBrw:getColumn( a_:__enumIndex() )
         ::aForm[ a_:__enumIndex(), 2 ]:setText( hbide_xtosForForm( eval( oCol:block ) ) )
      NEXT
   ELSE

   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:buildForm()
   LOCAL a_, qLbl, qEdit

   IF ::nType == BRW_TYPE_DBF
      FOR EACH a_ IN ::aStruct
         qLbl := QLabel():new(); qLbl:setText( a_[ 1 ] )
         qEdit := QLineEdit():new()
         ::qFLayout:addRow( qLbl, qEdit )
         aadd( ::aForm, { qLbl, qEdit } )
      NEXT
   ELSE

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:dataLink( nField )
   LOCAL bBlock

   IF ::nType == BRW_TYPE_DBF
      bBlock := {|| ( ::cAlias )->( fieldget( nField ) ) }
   ELSE
      bBlock := {|| ::aData[ ::nIndex, nField ] }
   ENDIF

   RETURN bBlock

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:buildColumns()
   LOCAL oXbpColumn, aPresParam, a_

   IF ::nType == BRW_TYPE_DBF
      FOR EACH a_ IN ::aStruct
         aPresParam := ::getPP( a_ )

         oXbpColumn          := XbpColumn():new()
         oXbpColumn:dataLink := ::dataLink( a_:__enumIndex() )
         oXbpColumn:create( , , , , aPresParam )

         ::oBrw:addColumn( oXbpColumn )
      NEXT
   ELSE
      FOR EACH a_ IN ::aStruct
         ::getPP( a_, a_:__enumIndex() )

         oXbpColumn          := XbpColumn():new()
         oXbpColumn:dataLink := ::dataLink( a_:__enumIndex() )
         oXbpColumn:create( , , , , aPresParam )

         ::oBrw:addColumn( oXbpColumn )
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:getPP( aStruct )
   LOCAL aPresParam := {}

   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , aStruct[ 1 ]  } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20            } )

   RETURN aPresParam

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:skipBlock( nHowMany )
   LOCAL nRecs, nCurPos
   LOCAL nSkipped := 0

   IF ::nType == BRW_TYPE_DBF
      IF nHowMany == 0
         ( ::cAlias )->( DBSkip( 0 ) )

      ELSEIF nHowMany > 0
         DO WHILE nSkipped != nHowMany .AND. ::next()
            nSkipped++
         ENDDO
      ELSE
         DO WHILE nSkipped != nHowMany .AND. ::previous()
            nSkipped--
         ENDDO
      ENDIF

   ELSE
      nRecs    := len( ::aData )
      nCurPos  := ::nIndex

      IF nHowMany >= 0
         IF ( nCurpos + nHowMany ) > nRecs
            nSkipped := nRecs - nCurpos
            ::nIndex := nRecs
         ELSE
            nSkipped := nHowMany
            ::nIndex += nHowMany
         ENDIF

      ELSE
         IF ( nCurpos + nHowMany ) < 1
            nSkipped := 1 - nCurpos
            ::nIndex := 1
         ELSE
            nSkipped := nHowMany
            ::nIndex += nHowMany
         ENDIF

      ENDIF

   ENDIF

   RETURN nSkipped

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goTop()

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGotop() )
   ELSE
      ::nIndex := 1
   ENDIF
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goBottom()

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGoBottom() )
   ELSE
      ::nIndex := len( ::aData )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:recNo()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( RecNo() )
   ELSE
      RETURN ::nIndex
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:lastRec()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( LastRec() )
   ELSE
      RETURN len( ::aData )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goTo( nRec )

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGoto( nRec ) )
   ELSE
      ::nIndex := nRec
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:next()
   LOCAL nSaveRecNum := ( ::cAlias )->( recno() )
   LOCAL lMoved := .T.

   IF ( ::cAlias )->( Eof() )
      lMoved := .F.
   ELSE
      ( ::cAlias )->( DBSkip( 1 ) )
      IF ( ::cAlias )->( Eof() )
         lMoved := .F.
         ( ::cAlias )->( DBGoTo( nSaveRecNum ) )
      ENDIF
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:previous()
   LOCAL nSaveRecNum := ( ::cAlias )->( recno() )
   LOCAL lMoved := .T.

   ( ::cAlias )->( DBSkip( -1 ) )

   IF ( ::cAlias )->( Bof() )
      ( ::cAlias )->( DBGoTo( nSaveRecNum ) )
      lMoved := .F.
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/

