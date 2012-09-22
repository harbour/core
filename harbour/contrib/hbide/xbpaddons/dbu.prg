/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the HBIDE/DBU
 *
 * Copyright 2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                        Harbour HbpDBU Class
 *
 *                             Pritpal Bedi
 *                              13Sep2012
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

#define __dbu_dragEnterEvent__                    2011
#define __dbu_dropEvent__                         2012
#define __dbStruct_closeEvent__                   2013
#define __fieldsTable_itemSelectionChanged__      2014
#define __buttonCopyStruct_clicked__              2015
#define __buttonOpen_clicked__                    2016
#define __buttonShowForm_clicked__                2017
#define __buttonDbStruct_clicked__                2018
#define __buttonFind_clicked__                    2019
#define __buttonGoto_clicked__                    2020
#define __buttonClose_clicked__                   2021
#define __buttonViewTabbed_clicked__              2022
#define __buttonViewOrganized_clicked__           2023
#define __buttonSaveLayout_clicked__              2024
#define __buttonViewCascaded_clicked__            2025
#define __buttonViewTiled_clicked__               2026
#define __buttonViewMaximized_clicked__           2027
#define __buttonViewStackedVert_clicked__         2028
#define __buttonViewStackedHorz_clicked__         2029
#define __buttonViewZoomedIn_clicked__            2030
#define __buttonViewZoomedOut_clicked__           2031
#define __buttonAppendRecord_clicked__            2032
#define __buttonDelRecord_clicked__               2033
#define __buttonLockRecord_clicked__              2034
#define __buttonGoTop_clicked__                   2035
#define __buttonGoBottom_clicked__                2036
#define __buttonScrollToFirst_clicked__           2037
#define __buttonScrollToLast_clicked__            2038
#define __buttonSearchInTable_clicked__           2039
#define __buttonZaptable_clicked__                2040
#define __qPanelsButton_clicked__                 2041
#define __buttonTables_clicked__                  2042
#define __buttonIndex_clicked__                   2043
#define __mdiArea_subWindowActivated__            2044
#define __browse_navigate__                       2045
#define __browse_keyboard__                       2046
#define __timer_timeout__                         2047
#define __browser_contextMenu__                   2048
#define __mdiSubWindow_windowStateChanged__       2049
#define __mdiSubWindow_buttonXclicked__           2050

/*----------------------------------------------------------------------*/

#define  BRW_TYPE_DBF                             1
#define  BRW_TYPE_ARRAY                           2

#define  TBL_PANEL                                1
#define  TBL_NAME                                 2
#define  TBL_ALIAS                                3
#define  TBL_DRIVER                               4
#define  TBL_INDEX                                5
#define  TBL_RECORD                               6
#define  TBL_CURSOR                               7
#define  TBL_GEOMETRY                             8
#define  TBL_ROWPOS                               9
#define  TBL_COLPOS                               10
#define  TBL_HZSCROLL                             11
#define  TBL_CONXN                                12
#define  TBL_NEXT                                 13

#define  TBL_VRBLS                                13

#define  SUB_ID                                   1
#define  SUB_WINDOW                               2
#define  SUB_GEOMETRY                             3
#define  SUB_BROWSER                              4
#define  SUB_NIL                                  5

#define  PNL_PANELS                               1
#define  PNL_TABLES                               2
#define  PNL_MISC                                 3
#define  PNL_READY                                4

/*----------------------------------------------------------------------*/

CLASS HbpDBU INHERIT XbpWindow

   CLASSDATA lRegistered                          INIT .F.

   METHOD init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD execSlot( nSlot, p )
   METHOD handleEvent( nEvent, mp1, mp2 )
   METHOD setStyleSheet( ... )                    VIRTUAL

   DATA   sl_brush
   METHOD background( oBrush )                    SETGET

   DATA   qStack
   DATA   qLayout
   DATA   qVSplitter
   DATA   qToolBar
   DATA   qToolBarL
   DATA   qStruct
   DATA   qRddCombo
   DATA   qConxnCombo
   DATA   qStatus
   DATA   qTimer

   DATA   cWrkFolderLast                          INIT  ""
   DATA   cDbStructDialogGeometry                 INIT  ""

   DATA   aStatusPnls                             INIT  {}
   DATA   aPanels                                 INIT  {}
   DATA   aIndexAct                               INIT  {}
   DATA   aRdds                                   INIT  { "DBFCDX", "DBFNTX", "DBFNSX" }
   DATA   aConxns                                 INIT  {}

   DATA   oCurBrw
   DATA   oCurPanel

   DATA   qPanelsMenu
   DATA   qIndexMenu
   DATA   qTablesMenu
   DATA   qPanelsButton
   DATA   qIndexButton
   DATA   qTablesButton
   DATA   aPanelsAct                              INIT  {}

   DATA   lStructOpen                             INIT  .f.
   DATA   lDeletedOn                              INIT  .t.
   DATA   qComboAction
   DATA   sp0,sp1,sp2,sp3

   DATA   nPrevMode                               INIT  0

   METHOD open( aDbfs )
   METHOD buildToolbar()
   METHOD execEvent( nEvent, p, p1 )
   METHOD addArray( aData, aAttr )
   METHOD getPanelNames()
   METHOD getPanelsInfo()
   METHOD addPanels()
   METHOD addPanel( cPanel )
   METHOD setPanel( cPanel )
   METHOD isPanel( cPanel )
   METHOD loadTables()
   METHOD buildPanelsButton()
   METHOD buildIndexButton()
   METHOD addPanelsMenu( cPanel )
   METHOD showStruct()
   METHOD buildUiStruct()
   METHOD populateUiStruct()
   METHOD populateFieldData()
   METHOD updateIndexMenu( oBrw )
   METHOD buildRddsCombo()
   METHOD buildConxnCombo()
   METHOD loadConxnCombo( cDriver )
   ACCESS currentDriver()                         INLINE ::qRddCombo:currentText()
   ACCESS currentConxn()                          INLINE ::qConxnCombo:currentText()
   METHOD buildStatusPanels()
   METHOD dispStatusInfo()
   METHOD buildLeftToolbar()
   METHOD buildTablesButton()
   METHOD showTablesTree()
   METHOD fetchFldsList( cAlias )
   METHOD getBrowserByAlias( cAlias )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbpDBU:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ! ::lRegistered
      ::lRegistered := .T.
      QResource():registerResource_1( hbqtres_xbp() )
   ENDIF

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL oLayout

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF HB_ISOBJECT( ::qtObject )
      ::oWidget := QWidget( ::qtObject )
      oLayout := ::qtObject:layout()
      SWITCH __objGetClsName( oLayout )
      CASE "QVBOXLAYOUT"
      CASE "QHBOXLAYOUT"
         oLayout:addWidget( ::oWidget )
         EXIT
      CASE "QGRIDLAYOUT"
         oLayout:addWidget( ::oWidget, 0, 0, 1, 1 )
         EXIT
      ENDSWITCH
   ELSE
      ::oWidget := QWidget()
   ENDIF

   ::oWidget:setAcceptDrops( .t. )
   ::oWidget:connect( QEvent_DragEnter, {|p| ::execEvent( __dbu_dragEnterEvent__, p ) } )
   ::oWidget:connect( QEvent_Drop     , {|p| ::execEvent( __dbu_dropEvent__     , p ) } )
   ::oWidget:hide()

   /* Layout applied to dbu widget */
   ::qLayout := QGridLayout()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setSpacing( 0 )

   ::oWidget:setLayout( ::qLayout )

   /* Toolbar */
   ::buildToolbar()
   ::qLayout:addWidget( ::qToolbar:oWidget, 0, 0, 1, 2 )

   /* Toolbar left */
   ::buildLeftToolbar()
   ::qLayout:addWidget( ::qToolbarL:oWidget, 1, 0, 1, 1 )

   /* Stacked widget */
   ::qStack := QStackedWidget()
   ::qLayout:addWidget( ::qStack   , 1, 1, 1, 1 )

   /* StatusBar */
   ::qStatus := QStatusBar()
   ::qStatus:setSizeGripEnabled( .f. )
   ::qLayout:addWidget( ::qStatus  , 2, 0, 1, 2 )

   /* */
   ::buildStatusPanels()
   /* Panels on the stacked widget */
   ::addPanels()

   /* Spread tables onto panels */
   ::loadTables()

   /* Switch to the default panel */
   ::setPanel( "Main" )

   /* Timer to update ststus bar */
   ::qTimer := QTimer()
   ::qTimer:setInterval( 2000 )
   ::qTimer:connect( "timeout()", {|| ::dispStatusInfo() } )
   ::qTimer:start()

   IF ::visible
      ::show()
   ENDIF

   IF HB_ISOBJECT( oParent )
      ::oParent:AddChild( SELF )
      ::postCreate()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:execSlot( nSlot, p )
   HB_SYMBOL_UNUSED( nSlot )
   HB_SYMBOL_UNUSED( p )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:handleEvent( nEvent, mp1, mp2 )
   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )
   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD HbpDBU:destroy()
   ::oWidget:setParent( QWidget() )
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbpDBU:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:background( oBrush )
   LOCAL oOldBrush := ::sl_brush
   LOCAL oPanel

   IF HB_ISOBJECT( oBrush )
      ::sl_brush := oBrush
      FOR EACH oPanel IN ::aPanels
         oPanel:setBackground( ::sl_brush )
      NEXT
   ENDIF

   RETURN oOldBrush

/*----------------------------------------------------------------------*/

METHOD HbpDBU:getPanelNames()
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

METHOD HbpDBU:getPanelsInfo()
   LOCAL oBrw, oPanel, aSub
   LOCAL aInfo := {}, aAttr

   FOR EACH oPanel IN ::aPanels
      FOR EACH aSub IN oPanel:subWindows()
         aAttr := array( TBL_VRBLS )
         aAttr[ TBL_PANEL ] := oPanel:cPanel

         oBrw := aSub[ 4 ]

         IF oBrw:nType == BRW_TYPE_DBF
            aAttr[ TBL_NAME     ] := oBrw:cTable
            aAttr[ TBL_ALIAS    ] := oBrw:cAlias
            aAttr[ TBL_DRIVER   ] := oBrw:cDriver
            aAttr[ TBL_INDEX    ] := hb_ntos( oBrw:indexOrd()  )
            aAttr[ TBL_RECORD   ] := hb_ntos( oBrw:recNo()     )
            aAttr[ TBL_CURSOR   ] := hb_ntos( oBrw:nCursorType )
            IF !HB_ISOBJECT( aSub[ SUB_GEOMETRY ] )
               aSub[ SUB_GEOMETRY ] := aSub[ SUB_WINDOW ]:geometry()
            ENDIF
            aAttr[ TBL_GEOMETRY ] := hb_ntos( aSub[ SUB_GEOMETRY ]:x() )     + " " + hb_ntos( aSub[ SUB_GEOMETRY ]:y() ) + " " + ;
                                     hb_ntos( aSub[ SUB_GEOMETRY ]:width() ) + " " + hb_ntos( aSub[ SUB_GEOMETRY ]:height() )
            aAttr[ TBL_ROWPOS   ] := hb_ntos( oBrw:oBrw:rowPos() )
            aAttr[ TBL_COLPOS   ] := hb_ntos( oBrw:oBrw:colPos() )
            aAttr[ TBL_HZSCROLL ] := ""
            aAttr[ TBL_CONXN    ] := oBrw:cConxnFull
            aAttr[ TBL_NEXT     ] := ""

         ELSEIF oBrw:nType == BRW_TYPE_ARRAY
            //
         ENDIF

         aadd( aInfo, hbide_array2String( aAttr, "," ) )
      NEXT
   NEXT

   RETURN aInfo

/*----------------------------------------------------------------------*/

METHOD HbpDBU:fetchFldsList( cAlias )
   LOCAL aFlds := {}, cA, oBrw, a_, oPanel, aBrw

   cA := upper( cAlias )

   SWITCH cA
   CASE "FIELD"
      FOR EACH oPanel IN ::aPanels
         FOR EACH aBrw IN oPanel:aBrowsers
            oBrw := aBrw[ SUB_BROWSER ]
            FOR EACH a_ IN oBrw:aStruct
               aadd( aFlds, pad( a_[ 1 ], 10 ) + " (" + padc( oBrw:cTableOnly, 12 ) + ")" + str( a_:__enumIndex(),3,0 ) + ", " + a_[ 2 ] + ", " + str( a_[ 3 ],3,0 ) + ", " + hb_ntos( a_[ 4 ] ) + " [f]" )
            NEXT
         NEXT
      NEXT
      EXIT
   OTHERWISE
      IF ! empty( oBrw := ::getBrowserByAlias( cA ) )
         FOR EACH a_ IN oBrw:aStruct
            aadd( aFlds, pad( a_[ 1 ], 10 ) + " ( " + str( a_:__enumIndex(),3,0 ) + ", " + a_[ 2 ] + ", " + str( a_[ 3 ],3,0 ) + ", " + hb_ntos( a_[ 4 ] ) + " )"  + " [f]" )
         NEXT
      ENDIF
      EXIT
   ENDSWITCH

   RETURN aFlds

/*------------------------------------------------------------------------*/

METHOD HbpDBU:getBrowserByAlias( cAlias )
   LOCAL oPanel, aBrw

   FOR EACH oPanel IN ::aPanels
      FOR EACH aBrw IN oPanel:aBrowsers
         IF aBrw[ SUB_BROWSER ]:cAlias == cAlias
            RETURN aBrw[ SUB_BROWSER ]
         ENDIF
      NEXT
   NEXT
   RETURN NIL

/*------------------------------------------------------------------------*/

METHOD HbpDBU:dispStatusInfo()

   ::aStatusPnls[ PNL_PANELS ]:setText( "Panels: " + hb_ntos( Len( ::aPanels ) ) + ":" + ::oCurPanel:cPanel )
   ::aStatusPnls[ PNL_TABLES ]:setText( "Tables: " + hb_ntos( Len( ::oCurPanel:aBrowsers ) ) )

   ::aStatusPnls[ PNL_MISC   ]:setText( "M:"    )
   ::aStatusPnls[ PNL_READY  ]:setText( "Ready" )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpDBU:buildStatusPanels()
   LOCAL qLabel

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 1 )
   aadd( ::aStatusPnls, qLabel )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpDBU:addPanels()
   //LOCAL cPanel, aPnl

   ::addPanel( "Main", .T. )           /* The default one */
#if 0  /* Later */
   FOR EACH cPanel IN ::oINI:aDbuPanelNames
      aPnl := hb_aTokens( cPanel, "," )
      aSize( aPnl, 2 )
      IF empty( aPnl[ 2 ] )
         aPnl[ 2 ] := "NO"
      ENDIF
      IF !( aPnl[ 1 ] == "Main" )
         ::addPanel( aPnl[ 1 ], aPnl[ 2 ] == "YES" )
      ENDIF
   NEXT
#endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:addPanel( cPanel )
   LOCAL qPanel

   qPanel := HbpBrowsePanel():new( cPanel, self )
   ::qStack:addWidget( qPanel:qWidget )
   aadd( ::aPanels, qPanel )
   ::addPanelsMenu( cPanel )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:addPanelsMenu( cPanel )
   LOCAL qAct
IF HB_ISOBJECT( ::qPanelsMenu )
   qAct := ::qPanelsMenu:addAction( cPanel )
   qAct:setIcon( QIcon( dbu_Image( "panel_7" ) ) )
   qAct:connect( "triggered(bool)", {|| ::setPanel( cPanel ) } )
   aadd( ::aPanelsAct, qAct )
ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:isPanel( cPanel )
   RETURN ascan( ::aPanels, {|o| o:qWidget:objectName() == cPanel } ) > 0

/*----------------------------------------------------------------------*/

METHOD HbpDBU:setPanel( cPanel )
   LOCAL n

   IF ( n := ascan( ::aPanels, {|o| o:qWidget:objectName() == cPanel } ) ) > 0
      ::qStack:setCurrentWidget( ::aPanels[ n ]:qWidget )
      ::oCurPanel := ::aPanels[ n ]
      ::oCurPanel:prepare()
      ::oCurPanel:activateBrowser()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:execEvent( nEvent, p, p1 )
   LOCAL cTable, cPath, cPanel, qMime, qList, i, cExt, qUrl, aStruct, cTmp

   HB_SYMBOL_UNUSED( p )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH nEvent
   CASE __dbu_dragEnterEvent__
      p:acceptProposedAction()
      EXIT

   CASE __dbu_dropEvent__
      qMime := p:mimeData()
      IF qMime:hasUrls()
         qList := qMime:urls()
         FOR i := 0 TO qList:size() - 1
            qUrl := qList:at( i )
            hb_fNameSplit( qUrl:toLocalFile(), @cPath, @cTable, @cExt )
            IF lower( cExt ) == ".dbf"
               ::oCurPanel:addBrowser( { NIL, hbide_pathToOSPath( cPath + cTable + cExt ), NIL, ;
                             iif( ! ( ::qRddCombo:currentText() $ "DBFCDX.DBFNTX,DBFNSX,ADS" ), "DBFCDX", ::qRddCombo:currentText() ) } )
            ENDIF
         NEXT
      ENDIF
      EXIT

   CASE __buttonShowForm_clicked__
      IF !empty( ::oCurBrw )
         IF ::oCurBrw:qScrollArea:isHidden()
            ::oCurBrw:qScrollArea:show()
            ::qToolBar:setItemChecked( "Toggle", .t. )
         ELSE
            ::oCurBrw:qScrollArea:hide()
            ::qToolBar:setItemChecked( "Toggle", .f. )
         ENDIF
      ENDIF
      EXIT

   CASE __buttonClose_clicked__
      IF !empty( ::oCurBrw )
         ::oCurPanel:destroyBrw( ::oCurBrw )
      ENDIF
      EXIT

   CASE __buttonOpen_clicked__
      IF .T. //::currentDriver() $ "DBFCDX,DBFNTX,DBFNSX,ADS"
         IF !empty( cTable := hbide_fetchAFile( SetAppWindow(), "Select a Table", { { "Database File", "*.dbf" } }, ::cWrkFolderLast ) )
            hb_fNameSplit( cTable, @cPath )
            ::cWrkFolderLast := cPath
            ::oCurPanel:addBrowser( { NIL, cTable } )
         ENDIF
      ELSE
         #if 0
         IF ! empty( cTable := hbide_execScriptFunction( "tableSelect", ::currentDriver(), ::currentConxn() ) )
            ::oCurPanel:addBrowser( { NIL, cTable } )
         ENDIF
         #endif
      ENDIF
      EXIT

   CASE __qPanelsButton_clicked__
      cPanel := hbide_fetchAString( ::qToolbar:oWidget, "New...", "Name the Panel", "New Panel" )
      IF !( cPanel == "New..." ) .AND. !( cPanel == "Main" )
         IF ::isPanel( cPanel )
            MsgBox( "Panel: " + cPanel + ", already exists" )
         ELSE
            ::addPanel( cPanel )
            ::setPanel( cPanel )
         ENDIF
      ENDIF
      EXIT

   /* Left-toolbar actions */
   CASE __buttonViewTabbed_clicked__
      ::oCurPanel:setViewMode( iif( ::oCurPanel:viewMode() == QMdiArea_TabbedView, QMdiArea_SubWindowView, QMdiArea_TabbedView ) )
      EXIT
   CASE __buttonViewOrganized_clicked__
      ::oCurPanel:setViewStyle( HBPMDI_STYLE_ORGANIZED )
      EXIT
   CASE __buttonSaveLayout_clicked__
      ::oCurPanel:saveGeometry()
      EXIT
   CASE __buttonViewCascaded_clicked__
      ::oCurPanel:setViewStyle( HBPMDI_STYLE_CASCADED )
      EXIT
   CASE __buttonViewTiled_clicked__
      ::oCurPanel:setViewStyle( HBPMDI_STYLE_TILED )
      EXIT
   CASE __buttonViewMaximized_clicked__
      ::oCurPanel:setViewStyle( HBPMDI_STYLE_MAXIMIZED )
      EXIT
   CASE __buttonViewStackedVert_clicked__
      ::oCurPanel:setViewStyle( HBPMDI_STYLE_TILEDVERT )
      EXIT
   CASE __buttonViewStackedHorz_clicked__
      ::oCurPanel:setViewStyle( HBPMDI_STYLE_TILEDHORZ )
      EXIT
   CASE __buttonViewZoomedIn_clicked__
      ::oCurPanel:tilesZoom( +1 )
      EXIT
   CASE __buttonViewZoomedOut_clicked__
      ::oCurPanel:tilesZoom( -1 )
      EXIT

   /* Left-toolbar Table Manipulation Actions */
   CASE __buttonDbStruct_clicked__
      IF !empty( ::oCurBrw )
         ::showStruct()
      ENDIF
      EXIT

   CASE __buttonTables_clicked__
      ::showTablesTree()
      EXIT

   CASE __buttonIndex_clicked__
      EXIT

   CASE __dbStruct_closeEvent__
      ::cDbStructDialogGeometry := hbide_posAndSize( ::qStruct:oWidget )
      ::qStruct:close()
      ::lStructOpen := .f.
      EXIT

   CASE __fieldsTable_itemSelectionChanged__
      ::populateFieldData()
      EXIT

   CASE __buttonFind_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:searchAsk()
      ENDIF
      EXIT

   CASE __buttonGoto_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:gotoAsk()
      ENDIF
      EXIT

   CASE __buttonAppendRecord_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:append()
      ENDIF
      EXIT
   CASE __buttonDelRecord_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:delete( .t. )
      ENDIF
      EXIT
   CASE __buttonLockRecord_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:lock()
      ENDIF
      EXIT
   CASE __buttonGoTop_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:goTop()
      ENDIF
      EXIT
   CASE __buttonGoBottom_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:goBottom()
      ENDIF
      EXIT
   CASE __buttonScrollToFirst_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:toColumn( 1 )
      ENDIF
      EXIT
   CASE __buttonScrollToLast_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:toColumn( Len( ::oCurBrw:aStruct ) )
      ENDIF
      EXIT
   CASE __buttonSearchInTable_clicked__
      IF !empty( ::oCurBrw )
         ::oCurBrw:searchAsk()
      ENDIF
      EXIT
   CASE __buttonZaptable_clicked__
      EXIT
   CASE __buttonCopyStruct_clicked__
      IF !empty( aStruct := ::oCurBrw:dbStruct() )
         i := 0
         aeval( aStruct, {|e_| iif( Len( e_[ 1 ] ) > i, i := len( e_[ 1 ] ), NIL ) } )
         i += 2

         cTmp := "   LOCAL aStruct := {"
         aeval( aStruct, {|e_,n| cTmp += iif( n == 1, ' { ', space( 20 ) + '  { ' ) + ;
                                    pad( '"' + e_[ 1 ] + '"', i ) + ', "' + e_[ 2 ] + '", ' + ;
                                        str( e_[ 3 ], 4, 0 ) + ', ' + ;
                                            str( e_[ 4 ], 2, 0 ) + ' }' + ;
                                                iif( Len( aStruct ) == n, " }", ",;" ) + hb_eol() } )

         QClipboard():setText( cTmp )
      ENDIF
      EXIT
   /*  End - left-toolbar actions */

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:showTablesTree()
   LOCAL oUI, qTree, qParent, oPanel, qItm, aBrowser, q, aFld, qFont, nMax, nSz, oBrw
   LOCAL a_:={}

   oUI := hbqtui_xbpTables( ::oCurPanel:qWidget )

   qFont := QFont( "Courier New", 8 )
   qTree := oUI:treeTables
   qTree:setFont( qFont )

   FOR EACH oPanel IN ::aPanels
      qParent := QTreeWidgetItem()
      qParent:setText( 0, oPanel:cPanel )
      qTree:addTopLevelItem( qParent )
      aadd( a_, qParent )
      FOR EACH aBrowser IN oPanel:aBrowsers
         oBrw := aBrowser[ SUB_BROWSER ]

         qItm := QTreeWidgetItem()
         qItm:setText( 0, oBrw:cTable )

         qItm:setToolTip( 0, oBrw:cTableOnly + " [ " + oBrw:cDriver + "  " + ;
                          hb_ntos( oBrw:indexOrd() ) + "/" + hb_ntos( oBrw:numIndexes() ) + iif( oBrw:indexOrd() > 0, ":" + oBrw:ordName(), "" ) + ;
                          "  " + hb_ntos( oBrw:recno() ) + "/" + hb_ntos( oBrw:lastRec() ) + " ]  " )

         qParent:addChild( qItm )
         nSz := 0 ; aeval( aBrowser[ SUB_BROWSER ]:aStruct, {|e_| nSz += e_[ 3 ] } )
         nMax := 12
         FOR EACH aFld IN aBrowser[ SUB_BROWSER ]:aStruct
            q := QTreeWidgetItem()
            q:setText( 0, pad( aFld[ 1 ], nMax ) + aFld[ 2 ] + str( aFld[ 3 ], 4, 0 ) + str( aFld[ 4 ], 2, 0 ) )
            q:setToolTip( 0, "" )
            qItm:addChild( q )
         NEXT
         q := QTreeWidgetItem()
         q:setText( 0, pad( "T", nMax - 2 ) + str( nSz, 7, 0 ) )
         qItm:addChild( q )
     NEXT
      qParent:setExpanded( .t. )
   NEXT
//   ::oIde:setPosAndSizeByIniEx( oUI:oWidget, ::oINI:cTablesDialogGeometry )
   oUI:buttonOk:connect( "clicked()", {|| oUI:done( 1 ) } )
   oUI:exec()
   oUI:buttonOk:disconnect( "clicked()" )
//   ::oIde:oINI:cTablesDialogGeometry := hbide_posAndSize( oUI:oWidget )
   oUI:destroy()

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpDBU:showStruct()

   IF empty( ::qStruct )
      ::buildUiStruct()
   ENDIF

   IF ! ::lStructOpen
      ::lStructOpen := .t.
      ::populateUiStruct()
//      ::oIde:setPosAndSizeByIniEx( ::qStruct:oWidget, ::oINI:cDbStructDialogGeometry )
      ::qStruct:show()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:populateFieldData()
   LOCAL nRow, qItm

   IF ( nRow := ::qStruct:tableFields:currentRow() ) >= 0
      qItm := ::qStruct:tableFields:item( nRow, 1 )
      ::qStruct:editName:setText( qItm:text() )
      qItm := ::qStruct:tableFields:item( nRow, 2 )
      ::qStruct:comboType:setCurrentIndex( ascan( { "Character", "Numeric", "Date", "Logical" }, qItm:text() ) - 1 )
      qItm := ::qStruct:tableFields:item( nRow, 3 )
      ::qStruct:editSize:setText( qItm:text() )
      qItm := ::qStruct:tableFields:item( nRow, 4 )
      ::qStruct:editDec:setText( qItm:text() )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:populateUiStruct()
   LOCAL qItm, fld_, n
   LOCAL oTbl := ::qStruct:tableFields
   LOCAL aStruct := ::oCurBrw:dbStruct()

   ::qStruct:tableFields:clearContents()

   oTbl:setRowCount( Len( aStruct ) )

   n := 0
   FOR EACH fld_ IN aStruct
      qItm := QTableWidgetItem()
      qItm:setText( hb_ntos( n+1 ) )
      oTbl:setItem( n, 0, qItm )

      qItm := QTableWidgetItem()
      qItm:setText( fld_[ 1 ] )
      oTbl:setItem( n, 1, qItm )

      qItm := QTableWidgetItem()
      qItm:setText( hbide_fldType2Desc( fld_[ 2 ] )  )
      oTbl:setItem( n, 2, qItm )

      qItm := QTableWidgetItem()
      qItm:setText( hb_ntos( fld_[ 3 ] ) )
      oTbl:setItem( n, 3, qItm )

      qItm := QTableWidgetItem()
      qItm:setText( hb_ntos( fld_[ 4 ] ) )
      oTbl:setItem( n, 4, qItm )

      oTbl:setRowHeight( n, 20 )
      n++
   NEXT

   n := 0
   aeval( aStruct, {|e_| n += e_[ 3 ] } )

   ::qStruct:labelRecSize:setText( hb_ntos( n + 1 ) )

   oTbl:setCurrentCell( 0,0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:buildUiStruct()
   LOCAL oTbl, n, qItm
   LOCAL hdr_:= { { "", 50 }, { "Field Name",200 }, { "Type", 100 }, { "Len", 50 }, { "Dec", 70 } }

   ::qStruct := hbqtui_xbpDbStruct( ::oWidget )

   ::qStruct:setWindowFlags( Qt_Dialog )
   ::qStruct:setMaximumHeight( ::qStruct:height() )
   ::qStruct:setMinimumHeight( ::qStruct:height() )
   ::qStruct:setMinimumWidth( ::qStruct:width() )
   ::qStruct:setMaximumWidth( ::qStruct:width() )

   ::qStruct:connect( QEvent_Close, {|| ::execEvent( __dbStruct_closeEvent__ ) } )

   oTbl := ::qStruct:tableFields
   oTbl:verticalHeader():hide()
   oTbl:horizontalHeader():setStretchLastSection( .t. )
   oTbl:setAlternatingRowColors( .t. )
   oTbl:setColumnCount( Len( hdr_ ) )
   oTbl:setShowGrid( .t. )
   oTbl:setSelectionMode( QAbstractItemView_SingleSelection )
   oTbl:setSelectionBehavior( QAbstractItemView_SelectRows )
   FOR n := 1 TO Len( hdr_ )
      qItm := QTableWidgetItem()
      qItm:setText( hdr_[ n,1 ] )
      oTbl:setHorizontalHeaderItem( n-1, qItm )
      oTbl:setColumnWidth( n-1, hdr_[ n,2 ] )
   NEXT

   ::qStruct:comboType:addItem( "Character" )
   ::qStruct:comboType:addItem( "Numeric"   )
   ::qStruct:comboType:addItem( "Date"      )
   ::qStruct:comboType:addItem( "Logical"   )

   oTbl:connect( "itemSelectionChanged()", {|| ::execEvent( __fieldsTable_itemSelectionChanged__ ) } )

   ::qStruct:buttonCopyStruct:connect( "clicked()", {|| ::execEvent( __buttonCopyStruct_clicked__ ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:open( aDbfs )
   LOCAL aInfo, cTable
   LOCAL nX := 0, nY := 0

   FOR EACH cTable IN aDbfs
      nX += 20; nY += 20
      // Main,C:\harbour\tests\test.dbf,TEST,DBFCDX,0,500,2,0 0 300 504,21,1,,,,
      aInfo := array( TBL_VRBLS )
      aInfo[ TBL_PANEL ] := "Main"
      aInfo[ TBL_NAME  ] := cTable
      aInfo[ TBL_GEOMETRY ] := hb_ntos( nX ) + " " + hb_ntos( nY ) + " 500 300"

      ::oCurPanel:addBrowser( aInfo )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:loadTables()
   //LOCAL cInfo, aInfo, oCurPanel
   LOCAL oCurPanel

   oCurPanel := ::oCurPanel
#if 0
   FOR EACH cInfo IN ::oINI:aDbuPanelsInfo
      aInfo := hb_aTokens( cInfo, "," )
      IF ::isPanel( aInfo[ 1 ] )
         ::setPanel( aInfo[ 1 ] )
         ::oCurPanel:addBrowser( aInfo )
      ENDIF
   NEXT
#endif
   IF HB_ISOBJECT( oCurPanel )
      ::qStack:setCurrentWidget( oCurPanel )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:addArray( aData, aAttr )

   HB_SYMBOL_UNUSED( aData )
   HB_SYMBOL_UNUSED( aAttr )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:buildToolbar()
   LOCAL nW := 25
   LOCAL qTBar

   ::sp0 := QLabel(); ::sp0:setMinimumWidth( nW )
   ::sp1 := QLabel(); ::sp1:setMinimumWidth( nW )
   ::sp2 := QLabel(); ::sp2:setMinimumWidth( nW )
   ::sp3 := QLabel(); ::sp3:setMinimumWidth( nW )

   qTBar := XbpToolbar():new()
   qTBar:imageWidth  := 16
   qTBar:imageHeight := 16
   qTBar:create()

   qTBar:buttonClick := {|oButton| iif( HB_ISBLOCK( oButton:key ), Eval( oButton:key ), NIL ) }

   ::qToolbar := qTBar

   ::buildPanelsButton()
   qTBar:addItem( ::sp0 )
   ::buildRddsCombo()
   ::buildConxnCombo()
   qTBar:addItem( { "Open"     , "Open a Table"       , QIcon( dbu_Image( "open3"     ) ), {|| ::execEvent( __buttonOpen_clicked__     ) }, .F. } )
   qTBar:addItem( ::sp1 )
   qTBar:addItem( { "Toggle"   , "Show/Hide Form View", QIcon( dbu_Image( "form"      ) ), {|| ::execEvent( __buttonShowForm_clicked__ ) }, .T. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "Structure", "Table Structure"    , QIcon( dbu_Image( "dbstruct"  ) ), {|| ::execEvent( __buttonDbStruct_clicked__ ) }, .F. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::buildIndexButton()
   qTBar:addItem( { "Search"   , "Search in Table"    , QIcon( dbu_Image( "find"      ) ), {|| ::execEvent( __buttonFind_clicked__     ) }, .F. } )
   qTBar:addItem( { "Goto"     , "Goto Record"        , QIcon( dbu_Image( "gotoline3" ) ), {|| ::execEvent( __buttonGoto_clicked__     ) }, .F. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "Close"    , "Close Current Table", QIcon( dbu_Image( "dc_delete" ) ), {|| ::execEvent( __buttonClose_clicked__    ) }, .F. } )
   qTBar:addItem( ::sp2 )
   ::buildTablesButton()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:buildLeftToolbar()
   LOCAL qTBar

   qTBar := XbpToolbar():new()
   qTBar:orientation := Qt_Vertical
   qTBar:imageWidth  := 16
   qTBar:imageHeight := 16
   qTBar:create()
   qTBar:buttonClick := {|oButton| iif( HB_ISBLOCK( oButton:key ), Eval( oButton:key ), NIL ) }

   ::qToolbarL := qTBar

   qTBar:addItem( { "view_tabbed"      , "Toggle Tabbed View"      , QIcon( dbu_Image( "view_tabbed"       ) ), {|| ::execEvent( __buttonViewTabbed_clicked__      ) }, .f. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "view_organized"   , "View as Arranged"        , QIcon( dbu_Image( "view_organized"    ) ), {|| ::execEvent( __buttonViewOrganized_clicked__   ) }, .f. } )
   qTBar:addItem( { "save3"            , "Save Layout"             , QIcon( dbu_Image( "save3"             ) ), {|| ::execEvent( __buttonSaveLayout_clicked__      ) }, .f. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "view_cascaded"    , "View as Cascaded"        , QIcon( dbu_Image( "view_cascaded"     ) ), {|| ::execEvent( __buttonViewCascaded_clicked__    ) }, .f. } )
   qTBar:addItem( { "view_tiled"       , "View as Tiled"           , QIcon( dbu_Image( "view_tiled"        ) ), {|| ::execEvent( __buttonViewTiled_clicked__       ) }, .f. } )
   qTBar:addItem( { "fullscreen"       , "View Maximized"          , QIcon( dbu_Image( "fullscreen"        ) ), {|| ::execEvent( __buttonViewMaximized_clicked__   ) }, .f. } )
   qTBar:addItem( { "view_vertstacked" , "View Vertically Tiled"   , QIcon( dbu_Image( "view_vertstacked"  ) ), {|| ::execEvent( __buttonViewStackedVert_clicked__ ) }, .f. } )
   qTBar:addItem( { "view_horzstacked" , "View Horizontally Tiled" , QIcon( dbu_Image( "view_horzstacked"  ) ), {|| ::execEvent( __buttonViewStackedHorz_clicked__ ) }, .f. } )
   qTBar:addItem( { "view_zoomin"      , "View Zoom In"            , QIcon( dbu_Image( "view_zoomin"       ) ), {|| ::execEvent( __buttonViewZoomedIn_clicked__    ) }, .f. } )
   qTBar:addItem( { "view_zoomout"     , "View Zoom Out"           , QIcon( dbu_Image( "view_zoomout"      ) ), {|| ::execEvent( __buttonViewZoomedOut_clicked__   ) }, .f. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "database_add"     , "Append Record"           , QIcon( dbu_Image( "database_add"      ) ), {|| ::execEvent( __buttonAppendRecord_clicked__    ) }, .f. } )
   qTBar:addItem( { "database_remove"  , "Delete Record"           , QIcon( dbu_Image( "database_remove"   ) ), {|| ::execEvent( __buttonDelRecord_clicked__       ) }, .f. } )
   qTBar:addItem( { "database_lock"    , "Lock/Unlock Record"      , QIcon( dbu_Image( "database_lock"     ) ), {|| ::execEvent( __buttonLockRecord_clicked__      ) }, .f. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "database_up"      , "Goto Top"                , QIcon( dbu_Image( "database_up"       ) ), {|| ::execEvent( __buttonGoTop_clicked__           ) }, .f. } )
   qTBar:addItem( { "database_down"    , "Goto Bottom"             , QIcon( dbu_Image( "database_down"     ) ), {|| ::execEvent( __buttonGoBottom_clicked__        ) }, .f. } )
   qTBar:addItem( { "database_previous", "Scroll to First Column"  , QIcon( dbu_Image( "database_previous" ) ), {|| ::execEvent( __buttonScrollToFirst_clicked__   ) }, .f. } )
   qTBar:addItem( { "database_next"    , "Scroll to Last Column"   , QIcon( dbu_Image( "database_next"     ) ), {|| ::execEvent( __buttonScrollToLast_clicked__    ) }, .f. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "database_search"  , "Search in Table"         , QIcon( dbu_Image( "database_search"   ) ), {|| ::execEvent( __buttonSearchInTable_clicked__   ) }, .f. } )
   qTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   qTBar:addItem( { "database_process" , "Zap Table"               , QIcon( dbu_Image( "database_process"  ) ), {|| ::execEvent( __buttonZaptable_clicked__        ) }, .f. } )

   RETURN NIL

/*------------------------------------------------------------------------*/

METHOD HbpDBU:buildPanelsButton()

   ::qPanelsMenu := QMenu()

   ::qPanelsButton := QToolButton()
   ::qPanelsButton:setTooltip( "HbpDBU Panels" )
   ::qPanelsButton:setIcon( QIcon( dbu_Image( "panel_8" ) ) )
   ::qPanelsButton:setPopupMode( QToolButton_MenuButtonPopup )
   ::qPanelsButton:setMenu( ::qPanelsMenu )

   ::qPanelsButton:connect( "clicked()", {|| ::execEvent( __qPanelsButton_clicked__ ) } )

   ::qToolbar:addItem( ::qPanelsButton )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:buildConxnCombo()

   ::qConxnCombo := QComboBox()
   ::qConxnCombo:setToolTip( "Connection to open next table" )
   ::qToolBar:addItem( ::qConxnCombo )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:buildRddsCombo()
   LOCAL cRdd
   #if 0
   IF !empty( aRdds := hbide_execScriptFunction( "rdds" ) )
      aeval( aRdds, {|e| aadd( ::aRdds, e ) } )
   ENDIF
   #endif
   ::qRddCombo := QComboBox()
   ::qRddCombo:setToolTip( "Rdd to open next table" )
   FOR EACH cRdd IN ::aRdds
      cRdd := alltrim( cRdd )
      ::qRddCombo:addItem( cRdd )
   NEXT
   ::qRddCombo:connect( "currentIndexChanged(QString)", {|p| ::loadConxnCombo( p ) } )
   ::qToolBar:addItem( ::qRddCombo )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:buildTablesButton()

   ::qTablesMenu := QMenu()

   ::qTablesButton := QToolButton()
   ::qTablesButton:setTooltip( "Tables" )
   ::qTablesButton:setIcon( QIcon( dbu_Image( "table" ) ) )
   ::qTablesButton:setPopupMode( QToolButton_MenuButtonPopup )
   ::qTablesButton:setMenu( ::qTablesMenu )

   ::qTablesButton:connect( "clicked()", {|| ::execEvent( __buttonTables_clicked__ ) } )

   ::qToolbar:addItem( ::qTablesButton )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:buildIndexButton()

   ::qIndexMenu := QMenu()

   ::qIndexButton := QToolButton()
   ::qIndexButton:setTooltip( "Indexes" )
   ::qIndexButton:setIcon( QIcon( dbu_Image( "sort" ) ) )
   ::qIndexButton:setPopupMode( QToolButton_MenuButtonPopup )
   ::qIndexButton:setMenu( ::qIndexMenu )

   ::qIndexButton:connect( "clicked()", {|| ::execEvent( __buttonIndex_clicked__ ) } )

   ::qToolbar:addItem( ::qIndexButton )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpDBU:loadConxnCombo( cDriver )
   LOCAL cConxn, a_

   DEFAULT cDriver TO ::currentDriver()

   ::aConxns := {}
   #if 0
   IF !empty( aConxns := hbide_execScriptFunction( "connections", cDriver ) )
      aeval( aConxns, {|e| aadd( ::aConxns, e ) } )
   ENDIF
   #endif
   ::qConxnCombo:clear()
   FOR EACH cConxn IN ::aConxns
      a_:= hb_aTokens( cConxn, ";" )
      ::qConxnCombo:addItem( alltrim( a_[ 1 ] ) )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_getMenuBlock( oPanel, oBrw, cIndex )
   RETURN {|| oPanel:setIndex( oBrw, cIndex ) }

/*----------------------------------------------------------------------*/

METHOD HbpDBU:updateIndexMenu( oBrw )
   LOCAL qAct, aIndex, cIndex

   FOR EACH qAct IN ::aIndexAct
      qAct:disconnect( "triggered(bool)" )
      qAct := NIL
   NEXT
   ::aIndexAct := {}

   ::qIndexMenu:clear()

   aIndex := ::oCurPanel:getIndexInfo( oBrw )
   FOR EACH cIndex IN aIndex
      qAct := ::qIndexMenu:addAction( cIndex )
      qAct:connect( "triggered(bool)", hbide_getMenuBlock( ::oCurPanel, oBrw, cIndex ) )
      aadd( ::aIndexAct, qAct )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/
//
//                         Class HbpBrowsePanel
//
/*----------------------------------------------------------------------*/

CLASS HbpBrowsePanel

   DATA   oManager

   DATA   qWidget
   DATA   qMenuWindows
   DATA   qStruct

   DATA   cPanel                                  INIT  ""
   DATA   nViewStyle                              INIT  0    /* 0=asWindows 1=tabbed */
   DATA   lLayoutLocked                           INIT  .f.

   DATA   aBrowsers                               INIT  {}
   ACCESS subWindows()                            INLINE ::aBrowsers

   METHOD new( cPanel, oManager )
   METHOD destroy()
   METHOD destroyBrw( oBrw )
   METHOD execEvent( nEvent, p )
   METHOD setCurrentBrowser( oBrw )
   METHOD getIndexInfo( oBrw )
   METHOD setIndex( oBrw, cIndex )

   METHOD addBrowser( aInfo )
   METHOD prepare()
   METHOD saveGeometry()
   METHOD restGeometry()
   METHOD activateBrowser()
   METHOD setViewStyle( nStyle )
   METHOD tileVertically()
   METHOD tileHorizontally()
   METHOD tilesZoom( nMode )

   ERROR HANDLER OnError( ... )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:new( cPanel, oManager )

   ::cPanel := cPanel
   ::oManager := oManager

   ::qWidget := QMdiArea()
   ::qWidget:setObjectName( ::cPanel )
   ::qWidget:setDocumentMode( .t. )
   ::qWidget:setOption( QMdiArea_DontMaximizeSubWindowOnActivation, .t. )
   ::qWidget:setVerticalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::qWidget:setHorizontalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::qWidget:setDocumentMode( .t. )
   ::qWidget:setTabShape( QTabWidget_Triangular )
   ::qWidget:setViewMode( QMdiArea_TabbedView )

   ::qWidget:connect( "subWindowActivated(QMdiSubWindow*)", {|p| ::execEvent( __mdiArea_subWindowActivated__, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:destroy()
   LOCAL aBrw, oSub

   ::qWidget:disconnect( "subWindowActivated(QMdiSubWindow*)" )

   FOR EACH aBrw IN ::aBrowsers
      oSub := aBrw[ SUB_WINDOW ]
      ::qWidget:removeSubWindow( oSub )
      aBrw[ SUB_BROWSER ]:destroy()
      oSub := NIL
      aBrw := NIL
   NEXT
   ::aBrowsers    := NIL

   ::qMenuWindows := NIL
   ::qStruct      := NIL
   ::qWidget      := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:onError( ... )
   LOCAL cMsg

   cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF

   RETURN ::qWidget:&cMsg( ... )

/*------------------------------------------------------------------------*/

METHOD HbpBrowsePanel:setViewStyle( nStyle )
   LOCAL qObj, a_
   LOCAL nOldStyle := ::nViewStyle

   IF HB_ISNUMERIC( nStyle )
      IF nStyle != ::nViewStyle
         IF ::nViewStyle == HBPMDI_STYLE_ORGANIZED
            ::saveGeometry()
         ENDIF

         IF ::nViewStyle == HBPMDI_STYLE_MAXIMIZED
            qObj := ::qWidget:activeSubWindow()
            FOR EACH a_ IN ::aBrowsers
               a_[ 2 ]:setWindowState( Qt_WindowNoState )
            NEXT
            ::qWidget:setActiveSubWindow( qObj )
         ENDIF

         SWITCH nStyle
         CASE HBPMDI_STYLE_ORGANIZED
            ::restGeometry()
            EXIT
         CASE HBPMDI_STYLE_CASCADED
            ::qWidget:cascadeSubWindows()
            EXIT
         CASE HBPMDI_STYLE_TILED
            ::qWidget:tileSubWindows()
            EXIT
         CASE HBPMDI_STYLE_MAXIMIZED
            qObj := ::activeSubWindow()
            FOR EACH a_ IN ::aBrowsers
               a_[ 2 ]:setWindowState( Qt_WindowMaximized )
            NEXT
            ::setActiveSubWindow( qObj )
            EXIT
         CASE HBPMDI_STYLE_TILEDVERT
            ::tileVertically()
            EXIT
         CASE HBPMDI_STYLE_TILEDHORZ
            ::tileHorizontally()
            EXIT
         ENDSWITCH

         ::nViewStyle := nStyle
         ::prepare()
      ENDIF
   ENDIF
   RETURN nOldStyle

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:tileVertically()
   LOCAL qObj, qVPort, nH, nT, nW, a_

   qObj   := ::activeSubWindow()
   qVPort := ::viewport()
   nH     := qVPort:height() / Len( ::aBrowsers )
   nW     := qVPort:width()
   nT     := 0
   FOR EACH a_ IN ::aBrowsers
      a_[ 2 ]:setGeometry( QRect( 0, nT, nW, nH ) )
      nT += nH
   NEXT
   ::setActiveSubWindow( qObj )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:tileHorizontally()
   LOCAL qObj, qVPort, nH, nT, nW, nL, a_

   qObj   := ::activeSubWindow()
   qVPort := ::viewport()
   nH     := qVPort:height()
   nW     := qVPort:width() / Len( ::aBrowsers )
   nT     := 0
   nL     := 0
   FOR EACH a_ IN ::aBrowsers
      a_[ 2 ]:setGeometry( QRect( nL, nT, nW, nH ) )
      nL += nW
   NEXT
   ::setActiveSubWindow( qObj )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:tilesZoom( nMode )
   LOCAL qMdi, nT, nL, nH, nW, qRect, a_

   IF ::nViewStyle == HBPMDI_STYLE_TILEDVERT .OR. ::nViewStyle == HBPMDI_STYLE_TILEDHORZ
      IF ::nViewStyle == HBPMDI_STYLE_TILEDVERT
         nT := 0
         FOR EACH a_ IN ::aBrowsers
            qMdi  := a_[ 2 ]
            qRect := qMdi:geometry()
            nH    := qRect:height() + ( nMode * ( qRect:height() / 4 ) )
            qMdi:setGeometry( QRect( 0, nT, qRect:width(), nH ) )
            nT    += nH
         NEXT
      ELSE
         nL := 0
         FOR EACH a_ IN ::aBrowsers
            qMdi  := a_[ 2 ]
            qRect := qMdi:geometry()
            nW    := qRect:width() + ( nMode * ( qRect:width() / 4 ) )
            qMdi:setGeometry( QRect( nL, 0, nW, qRect:height() ) )
            nL    += nW
         NEXT
      ENDIF

      ::prepare()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:saveGeometry()
   LOCAL a_
   IF ::nViewStyle == HBPMDI_STYLE_ORGANIZED
      FOR EACH a_ IN ::aBrowsers
         a_[ SUB_GEOMETRY ] := a_[ SUB_WINDOW ]:geometry()
      NEXT
   ENDIF
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowsePanel:restGeometry()
   LOCAL a_
   FOR EACH a_ IN ::aBrowsers
      IF HB_ISOBJECT( a_[ SUB_GEOMETRY ] )
         a_[ SUB_WINDOW ]:setGeometry( a_[ SUB_GEOMETRY ] )
      ENDIF
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:destroyBrw( oBrw )
   LOCAL n, oSub

   IF ( n := ascan( ::aBrowsers, {|e_| e_[ SUB_BROWSER ] == oBrw } ) )  > 0
      oSub := ::aBrowsers[ n, SUB_WINDOW ]
      hb_adel( ::aBrowsers, n, .t. )

      ::qWidget:removeSubWindow( oSub )
#if 1
      oSub:setParent( QWidget() ) /* This alone releases all Windows down its hirarchy, right at this line */
                                  /* Without it GPFing when a single browser was being closed via X button */
#endif
      oBrw:destroy()  /* this is almost non-effective */
      oBrw := NIL
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowsePanel:execEvent( nEvent, p )
   LOCAL n, oBrw

   SWITCH nEvent
   CASE __mdiArea_subWindowActivated__
      IF ! empty( ::aBrowsers )
         IF ( n := ascan( ::aBrowsers, {|e_| hbqt_IsEqual( e_[ SUB_WINDOW ], p ) } ) )  > 0
            oBrw := ::aBrowsers[ n, SUB_BROWSER ]

            oBrw:configure()
            oBrw:oBrw:setCurrentIndex( .t. )
            oBrw:oBrw:setFocus()

            ::oManager:updateIndexMenu( oBrw )
         ENDIF
      ENDIF
      EXIT
   ENDSWITCH

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowsePanel:setIndex( oBrw, cIndex )
   IF ascan( ::aBrowsers, {|e_| e_[ SUB_BROWSER ] == oBrw } ) > 0
      RETURN oBrw:setIndex( cIndex )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:getIndexInfo( oBrw )
   IF ascan( ::aBrowsers, {|e_| e_[ SUB_BROWSER ] == oBrw } )  > 0
      RETURN oBrw:getIndexInfo()
   ENDIF
   RETURN {}

/*----------------------------------------------------------------------*/

METHOD HbpBrowsePanel:setCurrentBrowser( oBrw )
   IF ascan( ::aBrowsers, {|e_| e_[ SUB_BROWSER ] == oBrw } )  > 0
      ::oManager:oCurBrw := oBrw
   ENDIF
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowsePanel:prepare()
   LOCAL aSub
   FOR EACH aSub IN ::aBrowsers
      aSub[ SUB_BROWSER ]:configure()
   NEXT
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowsePanel:addBrowser( aInfo )
   LOCAL oBrw
   oBrw := HbpBrowse():new( ::oManager, Self, aInfo ):create()
   IF empty( oBrw:oBrw )
      RETURN Self
   ENDIF
   aadd( ::aBrowsers, { oBrw:nID, oBrw:qMdi, oBrw:qMdi:geometry(), oBrw, NIL } )
   ::oManager:updateIndexMenu( oBrw )
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowsePanel:activateBrowser()
   IF Len( ::aBrowsers ) > 0
      ::qWidget:setActiveSubWindow( ::aBrowsers[ 1, SUB_WINDOW ] )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
//
//                            Class HbpBrowse
//
/*----------------------------------------------------------------------*/

CLASS HbpBrowse

   DATA   oWnd
   DATA   oBrw
   DATA   qLayout
   DATA   qForm
   DATA   qFLayout
   DATA   qSplitter
   DATA   qTimer
   DATA   qScrollArea

   DATA   nID                                     INIT  0

   DATA   aForm                                   INIT  {}
   DATA   oManager
   DATA   oPanel
   DATA   qMDI
   DATA   aInfo                                   INIT  {}

   DATA   nType                                   INIT  BRW_TYPE_DBF
   DATA   cAlias                                  INIT  ""
   DATA   cTable                                  INIT  ""
   DATA   cTableOnly                              INIT  ""
   DATA   aData                                   INIT  {}
   DATA   aStruct                                 INIT  {}
   DATA   aAttr                                   INIT  {}
   DATA   nIndex                                  INIT  0
   DATA   cDriver                                 INIT  "DBFCDX"
   DATA   cConxn                                  INIT  ""
   DATA   cConxnFull                              INIT  ""
   DATA   cIndex                                  INIT  ""
   DATA   nOrder                                  INIT  0
   DATA   nArea                                   INIT  0
   DATA   nCursorType                             INIT  XBPBRW_CURSOR_CELL
   DATA   lOpened                                 INIT  .f.

   DATA   qVerSpl
   DATA   qClose
   DATA   aIndex                                  INIT  {}

   DATA   xSearch
   DATA   lInSearch                               INIT  .f.

   DATA   aMenu                                   INIT  {}
   DATA   aIdx                                    INIT  {}
   DATA   aFlds                                   INIT  {}
   DATA   aSeek                                   INIT  {}
   DATA   aToFld                                  INIT  {}

   CLASSDATA  nIdentity                           INIT  0

   METHOD new( oManager, oPanel, aInfo )
   METHOD create( oManager, oPanel, aInfo )
   METHOD configure()
   METHOD destroy()
   METHOD execEvent( nEvent, p, p1 )
   METHOD buildBrowser()
   METHOD buildColumns()
   METHOD buildMdiWindow()
   METHOD dataLink( nField )
   METHOD getPP( aStruct )

   METHOD skipBlock( nHowMany )

   METHOD use()
   METHOD exists()
   METHOD goTop()
   METHOD goBottom()
   METHOD goTo( nRec )
   METHOD lock()
   METHOD goToAsk()
   METHOD append()
   METHOD delete( lAsk )
   METHOD recall()
   METHOD recNo()
   METHOD lastRec()
   METHOD ordKeyCount()
   METHOD ordKeyNo()
   METHOD ordKeyGoto( nRec )
   ACCESS dbStruct()                              INLINE ::aStruct
   METHOD indexOrd()
   METHOD ordName( nOrder )
   METHOD IndexKey( nOrder )
   METHOD IndexKeyValue( nOrder )
   METHOD refreshAll()
   METHOD getIndexInfo()
   METHOD setIndex( cIndex )
   METHOD setOrder( nOrder )
   ACCESS numIndexes()                            INLINE Len( ::aIndex )

   METHOD dispInfo()
   METHOD search( cSearch, lSoft, lLast, nMode )
   METHOD searchAsk( nMode )
   METHOD seekAsk( nMode )
   METHOD next()
   METHOD previous()
   METHOD buildForm()
   METHOD populateForm()
   METHOD fetchAlias( cTable )
   METHOD saveField( nField, x )
   METHOD toColumn( ncIndex )
   METHOD getSome( cType, cFor )
   METHOD buildContextMenu()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:new( oManager, oPanel, aInfo )

   ::oManager := oManager
   ::oPanel   := oPanel
   ::aInfo    := aInfo

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:destroy()

   IF ::lOpened
      ( ::cAlias )->( dbCloseArea() )
   ENDIF

   IF !empty( ::qTimer )
      ::qTimer:disconnect( "timeout()" )
   ENDIF

   ::QTimer := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:create( oManager, oPanel, aInfo )
   LOCAL xVrb, cT, cName, n
   LOCAL lMissing := .t.

   DEFAULT oManager TO ::oManager
   DEFAULT oPanel   TO ::oPanel
   DEFAULT aInfo    TO ::aInfo

   ::oManager := oManager
   ::oPanel   := oPanel
   ::aInfo    := aInfo

   aSize( ::aInfo, TBL_VRBLS )

   DEFAULT ::aInfo[ TBL_PANEL    ] TO ::oPanel:cPanel
   DEFAULT ::aInfo[ TBL_NAME     ] TO ""
   DEFAULT ::aInfo[ TBL_ALIAS    ] TO ""
   DEFAULT ::aInfo[ TBL_DRIVER   ] TO ::oManager:currentDriver()
   DEFAULT ::aInfo[ TBL_INDEX    ] TO ""
   DEFAULT ::aInfo[ TBL_RECORD   ] TO ""
   DEFAULT ::aInfo[ TBL_CURSOR   ] TO ""
   DEFAULT ::aInfo[ TBL_GEOMETRY ] TO ""
   DEFAULT ::aInfo[ TBL_ROWPOS   ] TO "1"
   DEFAULT ::aInfo[ TBL_COLPOS   ] TO "1"
   DEFAULT ::aInfo[ TBL_HZSCROLL ] TO ""
   DEFAULT ::aInfo[ TBL_CONXN    ] TO ::oManager:currentConxn()
   DEFAULT ::aInfo[ TBL_NEXT     ] TO ""

   ::cTable := hbide_pathToOSPath( ::aInfo[ TBL_NAME ] )
   hb_fNameSplit( ::cTable, , @cName )
   ::cTableOnly := cName
   ::cAlias     := ::aInfo[ TBL_ALIAS  ]
   ::cDriver    := ::aInfo[ TBL_DRIVER ]
   ::cConxn     := ::aInfo[ TBL_CONXN  ]

   IF ! ::exists()
      RETURN Self
   ENDIF

   IF !empty( ::oManager:aConxns )
      n := ascan( ::oManager:aConxns, {|e| e == ::cConxn } )
      ::cConxnFull := ::oManager:aConxns[ n ]
   ENDIF

   IF ::nType == BRW_TYPE_DBF
      IF !empty( ::cAlias ) .AND. empty( ::cTable )
         IF select( ::cAlias ) > 0
            lMissing := .f.
         ENDIF
      ENDIF

      IF lMissing .AND. !empty( ::cTable )
         IF ! ( ::lOpened := ::use() )
            RETURN Self
         ENDIF
      ENDIF

      ::aStruct := ( ::cAlias )->( DbStruct() )
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
            aadd( ::aStruct, Len( xVrb ) )
            aadd( ::aStruct,  0 )
         ENDIF
      NEXT
   ENDIF

   ::buildBrowser()
   ::buildColumns()
   ::buildForm()
   ::buildMdiWindow()

   ::oManager:oCurBrw := Self

   ::oBrw:configure()
   ::oBrw:forceStable()
   ::oBrw:rowPos := max( 1, val( aInfo[ TBL_ROWPOS ] ) )
   ::oBrw:colPos := max( 1, val( aInfo[ TBL_COLPOS ] ) )
   ::oBrw:forceStable()
   ::setOrder( val( aInfo[ TBL_INDEX ] ) )
   ::goto( max( 1, val( aInfo[ TBL_RECORD ] ) ) )
   ::oBrw:setCurrentIndex( .t. )

   ::oBrw:navigate := {|mp1,mp2| ::execEvent( __browse_navigate__, mp1, mp2 ) }
   ::oBrw:keyboard := {|mp1,mp2| ::execEvent( __browse_keyboard__, mp1, mp2 ) }

   ::qTimer := QTimer()
   ::qTimer:setInterval( 5 )
   ::qTimer:connect( "timeout()",  {|| ::execEvent( __timer_timeout__ ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:buildBrowser()
   LOCAL qLayout, oWnd, oXbpBrowse

   oWnd := XbpWindow():new()
   oWnd:oWidget := QWidget()

   qLayout := QHBoxLayout()
   oWnd:oWidget:setLayout( qLayout )
   qLayout:setContentsMargins( 0,0,0,0 )
   qLayout:setSpacing( 2 )

   ::qSplitter := QSplitter()
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
   #if 0
   oXbpBrowse:lastPosBlock  := {| | ::lastRec()      }
   oXbpBrowse:posBlock      := {| | ::recNo()        }
   oXbpBrowse:goPosBlock    := {|n| ::goto( n )      }
   oXbpBrowse:phyPosBlock   := {| | ::recNo()        }
   #endif
   oXbpBrowse:lastPosBlock  := {| | ::ordKeyCount()   }
   oXbpBrowse:posBlock      := {| | ::ordKeyNo()      }
   oXbpBrowse:goPosBlock    := {|n| ::ordKeyGoto( n ) }
   oXbpBrowse:phyPosBlock   := {| | ::ordKeyNo()      }


   oXbpBrowse:hbContextMenu := {|mp1| ::execEvent( __browser_contextMenu__, mp1 ) }

   /* Form View */
   ::qForm := QWidget()
   ::qForm:setMinimumSize( QSize( 300  , Len( ::aStruct ) * 34 ) )
   ::qForm:setMaximumSize( QSize( 12000, 48000 ) )

   ::qFLayout := QFormLayout()
   ::qForm:setLayout( ::qFLayout )

   ::qScrollArea := QScrollArea()
   ::qScrollArea:setWidget( ::qForm )
   ::qScrollArea:hide()

   ::qSplitter:addWidget( ::qScrollArea )

   ::qLayout := qLayout
   ::oWnd    := oWnd
   ::oBrw    := oXbpBrowse

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:buildColumns()
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

METHOD HbpBrowse:buildForm()
   LOCAL a_, qLbl, qEdit

   IF ::nType == BRW_TYPE_DBF
      FOR EACH a_ IN ::aStruct
         qLbl := QLabel(); qLbl:setText( a_[ 1 ] )
         qEdit := QLineEdit()
         ::qFLayout:addRow( qLbl, qEdit )
         aadd( ::aForm, { qLbl, qEdit } )
      NEXT
   ELSE
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:buildMdiWindow()
   LOCAL qRect, cR

   STATIC nID := 0

   ::nID := ++nID

   ::qMdi := QMdiSubWindow()
   //
   ::qMdi:setWidget( ::oWnd:oWidget )
   ::oPanel:qWidget:addSubWindow( ::qMdi )

   ::oWnd:oWidget:show()
   ::qMdi:show()

   ::qMdi:setWindowTitle( ::cTable )
   ::qMdi:setObjectName( hb_ntos( nID ) )
   ::qMdi:setWindowIcon( QIcon( dbu_Image( "dbf_p" + hb_ntos( ::nID ) ) ) )

   IF ! empty( ::aInfo[ TBL_GEOMETRY ] )
      qRect := hb_aTokens( ::aInfo[ TBL_GEOMETRY ], " " )
      FOR EACH cR IN qRect
         cR := val( cR )
      NEXT
      qRect := QRect( qRect[ 1 ], qRect[ 2 ], qRect[ 3 ], qRect[ 4 ] )
      ::qMdi:setGeometry( qRect )
      ::qMdi:resize( ::qMdi:width()+1, ::qMdi:height()+1 )
      ::qMdi:resize( ::qMdi:width()-1, ::qMdi:height()-1 )
   ELSE
      ::qMdi:resize( 300, 200 )
   ENDIF
   ::dispInfo()

 * ::qMdi:connect( "aboutToActivate()", {|| ::execEvent( "mdiSubWindow_aboutToActivate" ) } )
   ::qMdi:connect( "windowStateChanged(Qt::WindowStates,Qt::WindowStates)", ;
                                 {|p,p1| ::execEvent( __mdiSubWindow_windowStateChanged__, p, p1 ) } )
   ::qMdi:connect( QEvent_Close, {|oEvent| oEvent:accept(), ::execEvent( __mdiSubWindow_buttonXclicked__ ) } )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowse:configure()
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

METHOD HbpBrowse:execEvent( nEvent, p, p1 )

   HB_SYMBOL_UNUSED( p  )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH nEvent
   CASE __browse_navigate__
      ::dispInfo()
      ::populateForm()
      ::oManager:oCurBrw := Self
      ::oManager:qToolbar:setItemChecked( "Toggle", ! ::qForm:isHidden() )
      EXIT
   CASE __browse_keyboard__
      IF p == xbeK_CTRL_F
         ::searchAsk()
      ELSEIF p == xbeK_CTRL_G
         ::gotoAsk()
      ELSEIF p == xbeK_CTRL_E
         ::oBrw:oTableView:edit( ::oBrw:getCurrentIndex() )
      ENDIF
      EXIT
   CASE __timer_timeout__
      ::oBrw:down()
      IF ::oBrw:hitBottom
         ::qTimer:stop()
         ::dispInfo()
      ELSEIF Left( eval( ::oBrw:getColumn( ::oBrw:colPos ):block ), Len( ::xSearch ) ) == ::xSearch
         ::qTimer:stop()
         ::dispInfo()
      ENDIF
      EXIT
   CASE __mdiSubWindow_buttonXclicked__
      ::oPanel:destroyBrw( Self )
      EXIT
#if 0
   CASE __browser_ScrollToColumn__
   CASE __mdiSubWindow_aboutToActivate__
      ::oBrw:configure()
      ::oBrw:setCurrentIndex( .t. )
      EXIT
#endif
   CASE __mdiSubWindow_windowStateChanged__
      IF p1 == 8
         ::oPanel:setCurrentBrowser( Self )
      ENDIF
      EXIT
   CASE __browser_contextMenu__
      IF empty( ::aMenu )
         ::buildContextMenu()
      ENDIF
      hbide_execPopup( ::aMenu, p, ::qMdi )
      EXIT
   ENDSWITCH

   #if 0
   activateNextSubWindow()
   activatePreviousSubWindow()
   closeActiveSubWindow()
   closeAllSubWindows()
   setActiveSubWindow( QMdiSubWindow * )
   #endif

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:buildContextMenu()
   LOCAL a_, cPmt, nZeros, cIndex

   ::qMdi:setFocus( 0 )

   IF Len( ::aIndex ) > 0
      aadd( ::aMenu, { "Set to Natural Order", {|| ::setOrder( 0 ) } } )
      aadd( ::aMenu, { "" } )
   ENDIF

   /* Indexed Order */
   ::getIndexInfo()
   FOR EACH cIndex IN ::aIndex
      aadd( ::aIdx,  hbide_indexArray( Self, cIndex, cIndex:__enumIndex() ) )
   NEXT
   IF ! empty( ::aIdx )
      aadd( ::aMenu, { ::aIdx, "Set to Indexed Order" } )
      aadd( ::aMenu, { "" } )
   ENDIF

   /* Column Scrolling */
   nZeros := iif( Len( ::aStruct ) < 10, 1, iif( len( ::aStruct ) < 100, 2, 3 ) )
   FOR EACH a_ IN ::aStruct
      cPmt := strzero( a_:__enumIndex(), nZeros ) + " " + a_[ 2 ] + " . " + a_[ 1 ]
      aadd( ::aFlds, hbide_fieldsArray( Self, cPmt, a_:__enumIndex() ) )
   NEXT
   aadd( ::aMenu, { ::aFlds, "Scroll to Column" } )
   aadd( ::aMenu, { "Scroll to ..."  , {|v| v := QInputDialog():getText( ::qMdi, "Field Name", "" ), ::toColumn( v ) } } )
   aadd( ::aMenu, { "" } )

   /* Seeks */
   aadd( ::aSeek, { "Seek"           , {|| ::seekAsk( 0 )   } } )
   aadd( ::aSeek, { "Seek Soft"      , {|| ::seekAsk( 1 )   } } )
   aadd( ::aSeek, { "Seek Last"      , {|| ::seekAsk( 2 )   } } )
   aadd( ::aMenu, { ::aSeek          , "Seek..." } )
   aadd( ::aMenu, { "Search in Field", {|| ::searchAsk( 1 ) } } )
   aadd( ::aMenu, { "" } )

   /* Navigation */
   aadd( ::aMenu, { "Go Top"         , {|| ::goTop()        } } )
   aadd( ::aMenu, { "Go Bottom"      , {|| ::goBottom()     } } )
   aadd( ::aMenu, { "Goto Record"    , {|| ::gotoAsk()      } } )
   aadd( ::aMenu, { "" } )

   /* Manipulation */
   aadd( ::aMenu, { "Append Blank"   , {|| ::append()       } } )
   aadd( ::aMenu, { "Delete Record"  , {|| ::delete( .t. )  } } )
   aadd( ::aMenu, { "Recall Deleted" , {|| ::recall()       } } )
   aadd( ::aMenu, { "" } )

   /* Miscellaneous */
   aadd( ::aMenu, { "Form View"      , {|| ::oManager:execEvent( __buttonShowForm_clicked__  ) } } )


   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_fieldsArray( obj, cPmt, nIndex )
   RETURN { cPmt, {|| obj:toColumn( nIndex ) } }

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_indexArray( obj, cIndex, nOrder )
   RETURN { cIndex, {|| obj:setOrder( nOrder ) } }

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:dispInfo()
   LOCAL cTitle

   IF !empty( ::qMdi )
      ::qMdi:setTooltip( ::cTable )
#if 1
      cTitle := "[ " + ::cDriver + "  " + ;
                             hb_ntos( ::indexOrd() ) + "/" + hb_ntos( ::numIndexes() ) + iif( ::indexOrd() > 0, ":" + ::ordName(), "" ) + ;
                             "  " + hb_ntos( ::recNo() ) + "/" + hb_ntos( ::lastRec() ) + " ]  " + ;
                             ::cTableOnly
#else
      //cTitle := HBQString( hb_ntos( ::recNo() ) )
      cTitle := hb_ntos( ::recNo() )
#endif

      ::qMdi:setWindowTitle( cTitle )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:fetchAlias( cTable )
   LOCAL cFile

   STATIC n := 0
   n++

   hb_fNameSplit( cTable, , @cFile )

   RETURN upper( "C" + cFile + hb_ntos( n ) )

/*------------------------------------------------------------------------*/

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

METHOD HbpBrowse:populateForm()
   LOCAL a_, oCol

   IF ::nType == BRW_TYPE_DBF
      IF ::qForm:isVisible()
         FOR EACH a_ IN ::aForm
            oCol := ::oBrw:getColumn( a_:__enumIndex() )
            ::aForm[ a_:__enumIndex(), 2 ]:setText( hbide_xtosForForm( eval( oCol:block ) ) )
         NEXT
      ENDIF
   ELSE

   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:saveField( nField, x )
   IF ( ::cAlias )->( DbrLock() )
      ( ::cAlias )->( FieldPut( nField, x ) )
      ( ::cAlias )->( DbCommit() )
      ( ::cAlias )->( DbrUnlock() )
      ::oBrw:refreshCurrent()
      ::oBrw:forceStable()
      ::oBrw:SetCurrentIndex( .f. )
   ENDIF
   RETURN x

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:dataLink( nField )
   LOCAL bBlock

   IF ::nType == BRW_TYPE_DBF
      bBlock := {|x| iif( x == NIL, ( ::cAlias )->( fieldget( nField ) ), ::saveField( nField, x ) ) }
   ELSE
      bBlock := {|| ::aData[ ::nIndex, nField ] }
   ENDIF

   RETURN bBlock

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:getPP( aStruct )
   LOCAL aPresParam := {}

   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , aStruct[ 1 ]  } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20            } )

   RETURN aPresParam

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:skipBlock( nHowMany )
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
      nRecs    := Len( ::aData )
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

METHOD HbpBrowse:next()
   LOCAL nSaveRecNum := ( ::cAlias )->( recno() )
   LOCAL lMoved := .T.

   IF ( ::cAlias )->( Eof() )
      lMoved := .F.
   ELSE
      ( ::cAlias )->( DbSkip( 1 ) )
      IF ( ::cAlias )->( Eof() )
         lMoved := .F.
         ( ::cAlias )->( DbGoTo( nSaveRecNum ) )
      ENDIF
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:previous()
   LOCAL nSaveRecNum := ( ::cAlias )->( recno() )
   LOCAL lMoved := .T.

   ( ::cAlias )->( DbSkip( -1 ) )

   IF ( ::cAlias )->( Bof() )
      ( ::cAlias )->( DbGoTo( nSaveRecNum ) )
      lMoved := .F.
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:getSome( cType, cFor )
   LOCAL nOrd := ::indexOrd()
   LOCAL qWidget := QApplication():focusWidget() // ::oWnd:oWidget

   SWITCH cType
   CASE "N"
      RETURN QInputDialog():getDouble( qWidget, "Search for?", cFor, ;
                         0, -2147483647, 2147483647, iif( nOrd > 0, 3, ::aStruct[ ::oBrw:colPos, 4 ] ) )
   CASE "D"
      RETURN hbide_fetchADate( qWidget, "Search for?", cFor )
   CASE "C"
      DEFAULT cFor TO ""
      RETURN QInputDialog():getText( qWidget, "Search for?", cFor )
   ENDSWITCH

   RETURN ""

/*------------------------------------------------------------------------*/

METHOD HbpBrowse:seekAsk( nMode )

   IF ::indexOrd() == 0
      RETURN Self
   ENDIF

   ::search( ::getSome( valtype( ::indexKeyValue() ) ), nMode == 1, nMode == 2 )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowse:searchAsk( nMode )
   LOCAL xValue, cFor

   DEFAULT nMode TO 0

   IF nMode == 0
      xValue := iif( ::indexOrd() > 0, ::indexKeyValue(), eval( ::oBrw:getColumn( ::oBrw:colPos ):block ) )
      cFor   := iif( ::indexOrd() > 0, "Indexed: " + ::indexKey(), ::aStruct[ ::oBrw:colPos, 1 ] )
   ELSEIF nMode == 1
      xValue := eval( ::oBrw:getColumn( ::oBrw:colPos ):block )
      cFor   := ::aStruct[ ::oBrw:colPos, 1 ]
   ENDIF

   ::search( ::getSome( valtype( xValue ), cFor ), .f., .f., nMode )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:search( cSearch, lSoft, lLast, nMode )
   LOCAL nRec

   DEFAULT nMode TO 0

   IF ::lInSearch
      ::qTimer:stop()
      ::lInSearch := .f.
   ENDIF

   IF ::nType == BRW_TYPE_DBF
      IF nMode == 0
         IF ( ::cAlias )->( IndexOrd() ) > 0

            DEFAULT lLast TO .f.
            DEFAULT lSoft TO .f.

            nRec := ::recNo()
            IF ( ::cAlias )->( DbSeek( cSearch, lSoft, lLast ) )
               ::refreshAll()
               ::dispInfo()
            ELSEIF ! lSoft
               ::goto( nRec )
               MsgBox( "Could not find: " + cSearch )
            ENDIF
         ELSE
            ::xSearch   := cSearch
            ::lInSearch := .t.
            ::qTimer:start()
         ENDIF
      ELSE
         ::xSearch   := cSearch
         ::lInSearch := .t.
         ::qTimer:start()
      ENDIF
   ELSE
      // Ascan
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:refreshAll()
   LOCAL qRect

   ::oBrw:refreshAll()
   ::oBrw:forceStable()
   ::oBrw:setCurrentIndex( .t. )
   qRect := ::qMdi:geometry()
   qRect:setHeight( qRect:height() + 3 )
   ::qMdi:setGeometry( qRect)
   qRect:setHeight( qRect:height() - 3 )
   ::qMdi:setGeometry( qRect)

   ::dispInfo()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:toColumn( ncIndex )
   LOCAL nIndex

   IF valtype( ncIndex ) == "C"
      ncIndex := upper( ncIndex )
      nIndex := ascan( ::aStruct, {|e_| Left( e_[ 1 ], Len( ncIndex ) ) == ncIndex } )
   ELSE
      nIndex := ncIndex
   ENDIF

   IF empty( nIndex )
      RETURN Self
   ENDIF

   ::oBrw:colPos := nIndex
   ::oBrw:refreshAll()
   ::oBrw:forceStable()
   ::oBrw:setCurrentIndex( .t. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:lock()

   IF ::nType == BRW_TYPE_DBF
      IF ! ( ::cAlias )->( DbrLock() )
         MsgBox( "Record could not been locked" )
      ENDIF
   ELSE
      MsgBox( "Record can not be locked" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:goToAsk()
   LOCAL nRec

   IF ! empty( nRec := ( QInputDialog() ):getInt( ::qMdi, "Goto", "Record_# ?", ::recno(), 1, ::lastrec() ) )
      ::goto( nRec )
      ::refreshAll()
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:ordKeyGoto( nRec )

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( OrdKeyGoto( nRec ) )
      ::refreshAll()
   ELSE
      IF nRec > 0 .AND. nRec <= Len( ::aData )
         ::nIndex := nRec
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:goto( nRec )

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGoto( nRec ) )
      ::refreshAll()
   ELSE
      IF nRec > 0 .AND. nRec <= Len( ::aData )
         ::nIndex := nRec
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:goTop()

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGotop() )
      ::refreshAll()
   ELSE
      ::nIndex := 1
   ENDIF
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:goBottom()

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGoBottom() )
      ::refreshAll()
   ELSE
      ::nIndex := Len( ::aData )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:setOrder( nOrder )

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbSetOrder( nOrder ) )
      ::refreshAll()
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:indexOrd()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( IndexOrd() )
   ENDIF

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:ordKeyNo()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( OrdKeyNo() )
   ELSE
      RETURN ::nIndex
   ENDIF

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:recNo()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( RecNo() )
   ELSE
      RETURN ::nIndex
   ENDIF

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:ordKeyCount()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( ordKeyCount() )
   ELSE
      RETURN Len( ::aData )
   ENDIF

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:lastRec()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( LastRec() )
   ELSE
      RETURN Len( ::aData )
   ENDIF

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:setIndex( cIndex )
   LOCAL n

   IF ( n := ascan( ::aIndex, cIndex ) ) > 0
      ( ::cAlias )->( DbSetOrder( n ) )
      ::oBrw:refreshAll()
      ::oBrw:forceStable()
      ::oBrw:setCurrentIndex( .t. )

      ::dispInfo()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:getIndexInfo()
   LOCAL a_:= {}, i, cKey

   IF ::nType == BRW_TYPE_DBF
      FOR i := 1 to 50
         IF ( cKey := ( ::cAlias )->( IndexKey( i ) ) ) == ''
            EXIT
         ENDIF
         aadd( a_, ( ::cAlias )->( OrdName( i ) ) + ' : ' + cKey )
      NEXT
   ENDIF

   ::aIndex := a_

   RETURN ::aIndex

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:ordName( nOrder )
   DEFAULT nOrder TO ::indexOrd()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( OrdName( nOrder ) )
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:indexKeyValue( nOrder )
   LOCAL xValue

   IF ::nType == BRW_TYPE_DBF
      xValue := ( ::cAlias )->( &( IndexKey( nOrder ) ) )
   ENDIF

   RETURN xValue

/*------------------------------------------------------------------------*/

METHOD HbpBrowse:indexKey( nOrder )
   DEFAULT nOrder TO ::indexOrd()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( IndexKey( nOrder ) )
   ENDIF

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:append()

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbAppend() )
      IF ! NetErr()
         ( ::cAlias )->( DbCommit() )
         ( ::cAlias )->( DbrUnlock() )
         ::refreshAll()
      ENDIF
   ELSE

   ENDIF
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowse:delete( lAsk )

   DEFAULT lAsk TO .t.

   IF lAsk
      IF ! hbide_getYesNo( "Delete Record ?", , "Deletion Process" )
         RETURN Self
      ENDIF
   ENDIF

   IF ::nType == BRW_TYPE_DBF
      IF ( ::cAlias )->( DbRLock() )
         ( ::cAlias )->( DbDelete() )
         ( ::cAlias )->( DbCommit() )
         ( ::cAlias )->( DbRUnlock() )
         ::refreshAll()
      ENDIF
   ELSE

   ENDIF
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowse:recall()

   IF ::nType == BRW_TYPE_DBF
      IF ( ::cAlias )->( Deleted() )
         IF ( ::cAlias )->( DbRLock() )
            ( ::cAlias )->( DbRecall() )
            ( ::cAlias )->( DbCommit() )
            ( ::cAlias )->( DbRUnlock() )
            ::refreshAll()
         ENDIF
      ENDIF
   ELSE

   ENDIF
   RETURN Self

/*------------------------------------------------------------------------*/

METHOD HbpBrowse:use()
   LOCAL bError, oErr
   LOCAL lErr := .f.

   SWITCH ::cDriver
   CASE "DBFCDX"
   CASE "DBFNTX"
   CASE "DBFNSX"
   CASE "ADS"
      bError := ErrorBlock( {|o| break( o ) } )
      BEGIN SEQUENCE
         IF empty( ::cAlias )
            USE ( ::cTable ) SHARED NEW VIA ( ::cDriver )
         ELSE
            USE ( ::cTable ) ALIAS ( ::cAlias ) SHARED NEW VIA ( ::cDriver )
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

      EXIT
   OTHERWISE
      //lErr := hbide_execScriptFunction( "tableUse", ::cTable, ::cAlias, ::cDriver, ::cConxn ) /* cTable holds the information about connection */
      EXIT
   ENDSWITCH

   IF lErr
      RETURN .f.
   ENDIF

   IF empty( ::cAlias )
      ::cAlias := alias()
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD HbpBrowse:exists()

   SWITCH ::cDriver
   CASE "DBFCDX"
   CASE "DBFNSX"
   CASE "DBFNTX"
   CASE "ADS"
      RETURN hb_fileExists( ::cTable )
   OTHERWISE
      //RETURN hbide_execScriptFunction( "tableExists", ::cTable, ::cDriver, ::cConxn )
   ENDSWITCH

   RETURN .f.

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_pathToOSPath( cPath )
   LOCAL n

   cPath := strtran( cPath, "//", hb_ps() )
   cPath := strtran( cPath, "/" , hb_ps() )
   cPath := strtran( cPath, "\\", hb_ps() )
   cPath := strtran( cPath, "\" , hb_ps() )

   IF ( n := at( ":", cPath ) ) > 0
      cPath := substr( cPath, 1, n - 1 ) + substr( cPath, n )
   ENDIF

   RETURN cPath

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_array2string( a_, cDlm )
   LOCAL s := ""

   aeval( a_, {|e| s += e + cDlm } )

   RETURN s

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_fetchAFile( oWnd, cTitle, aFlt, cDftDir, cDftSuffix, lAllowMulti )
   LOCAL oDlg

   DEFAULT cTitle      TO "Please Select a File"
   DEFAULT aFlt        TO { { "All Files", "*" } }
   DEFAULT cDftDir     TO hb_dirBase()
   DEFAULT lAllowMulti TO .f.

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )

   oDlg:title       := cTitle
   oDlg:center      := .t.
   oDlg:fileFilters := aFlt
   IF HB_ISSTRING( cDftSuffix )
      oDlg:oWidget:setDefaultSuffix( cDftSuffix )
   ENDIF

   RETURN oDlg:open( cDftDir, , lAllowMulti )

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_fetchAString( qParent, cDefault, cWhat, cTitle )
   LOCAL qGo, cText

   DEFAULT cDefault TO ""
   DEFAULT cWhat    TO ""
   DEFAULT cTitle   TO "A String Value"

   qGo := QInputDialog( qParent )
   qGo:setTextValue( cDefault )
   qGo:setLabelText( cWhat )
   qGo:setWindowTitle( cTitle )

   qGo:exec()
   cText := qGo:textValue()
   qGo:setParent( QWidget() )

   RETURN cText

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_fetchADate( qParent, cTitle, cPrompt, dDefault )
   LOCAL qDate, oUI, nRet

   DEFAULT cTitle  TO "A Date Value"
   DEFAULT cPrompt TO "What"

   oUI := hbqtui_xbpFetchDate( qParent )

   oUI:setWindowTitle( cTitle )
   oUI:labelPrompt:setText( cPrompt )
   IF dDefault != NIL
      qDate := QDate()
      qDate:setYear( year( dDefault ) )
      qDate:setMonth( month( dDefault ) )
      qDate:setDay( day( dDefault ) )
      oUI:editDate:setDate( qDate )
   ENDIF

   oUI:buttonOk:connect( "clicked()", {|| oUI:done( 1 ) } )
   oUI:buttonCancel:connect( "clicked()", {|| oUI:done( 0 ) } )

   nRet := oUI:exec()

   oUI:buttonOk:disconnect( "clicked()" )
   oUI:buttonCancel:disconnect( "clicked()" )

   IF nRet == 1
      qDate := oUI:editDate:date()
      RETURN stod( strzero( qDate:year(), 4 ) + strzero( qDate:month(),2 ) + strzero( qDate:day(), 2 ) )
   ENDIF

   RETURN NIL

/*------------------------------------------------------------------------*/

STATIC FUNCTION hbide_getYesNo( cMsg, cInfo, cTitle, qParent )
   LOCAL oMB, nRet

   DEFAULT cTitle  TO "Option Please!"
   DEFAULT qParent TO SetAppWindow():oWidget

   oMB := QMessageBox( qParent )
   oMB:setText( "<b>"+ cMsg +"</b>" )
   IF !empty( cInfo )
      oMB:setInformativeText( cInfo )
   ENDIF
   oMB:setIcon( QMessageBox_Information )
   oMB:setWindowTitle( cTitle )
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setStandardButtons( QMessageBox_Yes + QMessageBox_No )

   nRet := oMB:exec()

   oMB:setParent( QWidget() )

   RETURN nRet == QMessageBox_Yes

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_posAndSize( qWidget )

   RETURN hb_ntos( qWidget:x() )     + "," + hb_ntos( qWidget:y() )      + "," + ;
          hb_ntos( qWidget:width() ) + "," + hb_ntos( qWidget:height() ) + ","

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_fldType2Desc( cType )

   SWITCH cType
   CASE "C" ; RETURN "Character"
   CASE "N" ; RETURN "Numeric"
   CASE "D" ; RETURN "Date"
   CASE "L" ; RETURN "Logical"
   ENDSWITCH

   RETURN ""

/*----------------------------------------------------------------------*/

FUNCTION dbu_Image( cName )

   RETURN ":/xbp/resources" + "/" + cName + ".png"

/*----------------------------------------------------------------------*/
