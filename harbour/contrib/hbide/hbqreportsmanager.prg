/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               07Aug2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

#define  UNIT  0.1

#define  INI_KEY( cKey, n )     cKey + "_" + hb_ntos( n ) + "="

#define hbqt_screen_heightMM ( QDesktopWidget():new():height() / QDesktopWidget():new():physicalDpiY() * 25.4 )
#define hbqt_screen_widthMM  ( QDesktopWidget():new():width()  / QDesktopWidget():new():physicalDpiX() * 25.4 )

#define HBQT_GRAPHICSVIEW_ZOOM_IN                 1
#define HBQT_GRAPHICSVIEW_ZOOM_OUT                2
#define HBQT_GRAPHICSVIEW_ZOOM_WYSIWYG            3
#define HBQT_GRAPHICSVIEW_ZOOM_ORIGINAL           4

STATIC hIDs := {=>}

/*----------------------------------------------------------------------*/

CLASS HbqReportsManager

   DATA   qParent

   DATA   oWidget
   DATA   qLayout
   DATA   qToolbar
   DATA   qToolbarL
   DATA   qToolbarAlign
   DATA   qStack
   DATA   qStatus
   DATA   qTabBar
   DATA   qWidget1
   DATA   qWidget2
   DATA   qWidget3

   DATA   qPaper

   DATA   qSpliter
   DATA   qLayoutD
   DATA   qFrameL
   DATA   qScroll
   DATA   qFrameR

   DATA   qLayL
   DATA   qLayR
   DATA   qSplL
   DATA   qSplR

   DATA   qTabL0
   DATA   qPageL01
   DATA   qPageL02
   DATA   qPageL01Lay
   DATA   qTreeObjects

   DATA   qTabL1
   DATA   qPageL11
   DATA   qPageL12
   DATA   qPageL11Lay
   DATA   qTreeProp

   DATA   qEditDesc

   DATA   qTabR1
   DATA   qPageR11
   DATA   qPageR11Lay
   DATA   qTreeData
   DATA   qPageR12
   DATA   qPageR13

   DATA   qDesign
   DATA   qHRuler
   DATA   qVRuler
   DATA   qPort
   DATA   qView
   DATA   qScene
   DATA   qDrag
   DATA   qMime

   DATA   aStatusPnls                             INIT {}
   DATA   aItems                                  INIT {}
   DATA   hItems                                  INIT {=>}
   DATA   hObjTree                                INIT {=>}
   DATA   qCurGraphicsItem

   DATA   aPages                                  INIT {}
   DATA   aSources                                INIT {}
   DATA   aObjects                                INIT {}
   DATA   aRptPages                               INIT {}
   DATA   aRptSources                             INIT {}
   DATA   aRptObjects                             INIT {}

   DATA   lNew                                    INIT .t.
   DATA   cSaved                                  INIT ""

   /* Report's Properties */
   DATA   symposis                                INIT "HBReports Designer"
   DATA   version                                 INIT 0.1
   DATA   title                                   INIT "Report"
   DATA   author                                  INIT "hbIDE"
   DATA   created                                 INIT date()
   DATA   modified                                INIT date()

   DATA   xData

   METHOD new( qParent )
   METHOD create( qParent )
   METHOD destroy()
   METHOD execEvent( cEvent, p, p1, p2 )
   METHOD buildToolbar()
   METHOD buildToolbarAlign()
   METHOD buildToolbarLeft()
   METHOD buildStacks()
   METHOD buildStatusBar()
   METHOD buildTabBar()
   METHOD buildDesignReport()
   METHOD addField( cAlias, cField, qGeo, qPos )
   METHOD addObject( cType, qGeo, qPos )
   METHOD fetchBarString( cCode, lCheck )
   METHOD loadReport( xData )
   METHOD saveReport( lSaveAs )
   METHOD prepareReport()
   METHOD getNextID( cType )
   METHOD getImageOfType( cType )
   METHOD updateObjectsTree( cType, cParent, cName, cSubType )
   METHOD contextMenuItem( p1, p2 )
   METHOD contextMenuScene( p1 )
   METHOD addSource( cAlias, aStruct )
   METHOD clear()
   METHOD buildReportStream()
   METHOD toString()
   METHOD openReport()
   METHOD parseBuffer( cBuffer )
   METHOD presentBlankPage()
   METHOD printReport( qPrinter )
   METHOD printPreview( qPrinter )
   METHOD paintRequested( pPrinter )
   METHOD zoom( nMode )
   METHOD drawObject( pPainter, aInfo )
   METHOD drawBarcode( qPainter, qRect, aObj, qObj )
   METHOD drawImage( qPainter, qRect, aObj, qObj )
   METHOD drawChart( qPainter, qRect, aObj, qObj )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:new( qParent )
   ::qParent := qParent
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:create( qParent )

   DEFAULT qParent TO ::qParent
   ::qParent := qParent

   ::oWidget := QWidget():new( ::qParent )

   /* Layout applied to RM widget */
   ::qLayout := QGridLayout():new()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setSpacing( 0 )

   ::oWidget:setLayout( ::qLayout )

   /* Toolbar */
   ::buildToolbar()
   ::qLayout:addWidget_1( ::qToolbar:oWidget      , 0, 0, 1, 2 )
   ::buildToolbarAlign()
   ::qLayout:addWidget_1( ::qToolbarAlign:oWidget , 1, 0, 1, 2 )

   /* Toolbar left */
   ::buildToolbarLeft()
   ::qLayout:addWidget_1( ::qToolbarL:oWidget     , 2, 0, 2, 1 )

   /* ::qTabBar */
   ::buildTabBar()
   ::qLayout:addWidget_1( ::qTabBar               , 2, 1, 1, 1 )

   /* Stacked widget */
   ::buildStacks()
   ::qLayout:addWidget_1( ::qStack                , 3, 1, 1, 1 )

   /* StatusBar */
   ::buildStatusBar()
   ::qLayout:addWidget_1( ::qStatus               , 4, 0, 1, 2 )

   /* Document manipulation interface */
   ::buildDesignReport()

   ::qTabBar:setCurrentIndex( 2 )
   ::oWidget:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:destroy()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildDesignReport()

   ::qLayoutD := QHBoxLayout():new()
   ::qLayoutD:setContentsMargins( 0,0,0,0 )
   ::qLayoutD:setSpacing( 1 )
   ::qWidget3:setLayout( ::qLayoutD )

   ::qSpliter := QSplitter():new()
   ::qSpliter:setOrientation( Qt_Horizontal )

   ::qLayoutD:addWidget( ::qSpliter )

   ::qFrameL := QFrame():new()
   ::qSpliter:addWidget( ::qFrameL )

   ::qScene := HBQGraphicsScene():new()
   ::qScene:hbSetBlock( {|p,p1,p2| ::execEvent( "graphicsScene_block", p, p1, p2 ) } )

   ::qView := QGraphicsView():new( ::qDesign )
   ::qView:setMouseTracking( .t. )
   ::qView:setScene( ::qScene )
   //
   ::qSpliter:addWidget( ::qView )

   ::qFrameR := QFrame():new()
   ::qSpliter:addWidget( ::qFrameR )

   ::qLayL := QVBoxLayout():new()
   ::qLayL:setContentsMargins( 0,0,0,0 )
   ::qLayL:setSpacing( 1 )
   ::qFrameL:setLayout( ::qLayL )
   ::qSplL := QSplitter():new()
   ::qSplL:setOrientation( Qt_Vertical )
   ::qLayL:addWidget( ::qSplL )

   ::qLayR := QVBoxLayout():new()
   ::qLayR:setContentsMargins( 0,0,0,0 )
   ::qLayR:setSpacing( 1 )
   ::qFrameR:setLayout( ::qLayR )
   ::qSplR := QSplitter():new()
   ::qSplR:setOrientation( Qt_Vertical )
   ::qLayR:addWidget( ::qSplR )

   ::qFrameL:setMinimumWidth( 100 )
   ::qFrameR:setMinimumWidth( 100 )


   ::qTabL0 := QTabWidget():new()
   ::qSplL:addWidget( ::qTabL0 )
   /* Left Pane Objects Page */
   ::qPageL01 := QWidget():new()
   ::qTabL0:addTab( ::qPageL01, "Objects" )
   ::qPageL01Lay := QVBoxLayout():new()
   ::qPageL01:setLayout( ::qPageL01Lay )
   ::qPageL01Lay:setContentsMargins( 0,0,0,0 )
   /* Left Pane Events page */
   ::qPageL02 := QWidget():new()
   ::qTabL0:addTab( ::qPageL02, "Else" )
   /* Left pane Properties Treeview */
   ::qTreeObjects := QTreeWidget():new()
   ::qPageL01Lay:addWidget( ::qTreeObjects )
   ::qTreeObjects:setHeaderHidden( .t. )
   ::qTreeObjects:setObjectName( "ObjectsTree" )
   ::qTreeObjects:setIconSize( QSize():new( 12,12 ) )
   ::qTreeObjects:setIndentation( 12 )
   ::qTreeObjects:connect( "itemClicked(QTWItem)", {|p,p1| ::execEvent( "treeObjects_clicked", p, p1 ) } )

   ::qTabL1 := QTabWidget():new()
   ::qSplL:addWidget( ::qTabL1 )
   /* Left Pane Properties Page */
   ::qPageL11 := QWidget():new()
   ::qTabL1:addTab( ::qPageL11, "Props" )
   ::qPageL11Lay := QVBoxLayout():new()
   ::qPageL11:setLayout( ::qPageL11Lay )
   ::qPageL11Lay:setContentsMargins( 0,0,0,0 )
   /* Left Pane Events page */
   ::qPageL12 := QWidget():new()
   ::qTabL1:addTab( ::qPageL12, "Events" )
   /* Left pane Properties Treeview */
   ::qTreeProp := QTreeWidget():new()
   ::qPageL11Lay:addWidget( ::qTreeProp )
   ::qTreeProp:setHeaderHidden( .t. )
   ::qTreeProp:setObjectName( "PropertiesTree" )


   ::qEditDesc := QTextEdit():new()
   ::qSplL:addWidget( ::qEditDesc )
   ::qEditDesc:setPlainText( "Interface implemented is just a proof of concept, no promises yet, please." )
   ::qEditDesc:setMaximumHeight( 120 )

   ::qTabR1 := QTabWidget():new()
   ::qSplR:addWidget( ::qTabR1 )
   ::qPageR11 := QWidget():new()
   ::qTabR1:addTab( ::qPageR11, "Data" )
   ::qPageR12 := QWidget():new()
   ::qTabR1:addTab( ::qPageR12, "Variables" )
   ::qPageR13 := QWidget():new()
   ::qTabR1:addTab( ::qPageR13, "Functions" )

   ::qPageR11Lay := QVBoxLayout():new()
   ::qPageR11:setLayout( ::qPageR11Lay )
   ::qPageR11Lay:setContentsMargins( 0,0,0,0 )

   ::qTreeData := QTreeWidget():new()
   ::qPageR11Lay:addWidget( ::qTreeData )
   ::qTreeData:setHeaderHidden( .t. )
   ::qTreeData:setObjectName( "DataTree" )
   ::qTreeData:setDragEnabled( .t. )

   ::loadReport()

   ::qScene:setPageSize( QPrinter_Letter )
   ::qScene:zoomWYSIWYG()
   ::qToolbarAlign:setItemChecked( "Grid", ::qScene:showGrid() )
   //
   ::qScene:setLeftMagnet( .t. )
   ::qScene:setTopMagnet( .t. )
   ::qScene:setRightMagnet( .t. )
   ::qScene:setBottomMagnet( .t. )

   ::qLayoutD:setStretch( 1,1 )

   ::qWidget1:show()
   ::qWidget2:show()
   ::qWidget3:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:execEvent( cEvent, p, p1, p2 )
   LOCAL qEvent, qMime, qItem, i, qList, cFile, nArea, aStruct, cAlias, cPath

   SWITCH cEvent
   CASE "graphicsScene_block"
      qEvent := QGraphicsSceneDragDropEvent():from( p1 )

      DO CASE
      CASE p == QEvent_GraphicsSceneContextMenu
         ::contextMenuScene( p1 )

      CASE p == 21107    // Left button pressed nowhere on an item
         IF ! empty( ::qCurGraphicsItem )
            ::qCurGraphicsItem := NIL
            ::qTreeObjects:setCurrentItem( QTreeWidgetItem():new() )
         ENDIF

      CASE p == QEvent_GraphicsSceneDragEnter
         qEvent:acceptProposedAction()

      CASE p == QEvent_GraphicsSceneDragMove
         qEvent:acceptProposedAction()

      CASE p == QEvent_GraphicsSceneDragLeave

      CASE p == QEvent_GraphicsSceneDrop
         qMime := QMimeData():from( qEvent:mimeData() )
         IF qMime:hasFormat( "application/x-qabstractitemmodeldatalist" )
            IF p2[ 1 ] == "DataTree"
               IF p2[ 2 ] != p2[ 3 ]
                  ::addField( p2[ 2 ], p2[ 3 ], NIL, QPoint():from( qEvent:scenePos() ) )
               ENDIF
            ENDIF

         ELSEIF qMime:hasFormat( "application/x-toolbaricon"  )
            ::addObject( qMime:html(), NIL, QPoint():from( qEvent:scenePos() ) )

         ELSEIF qMime:hasUrls()
            qList := QStringList():from( qMime:hbUrlList() )
            FOR i := 0 TO qList:size() - 1
               cFile := ( QUrl():new( qList:at( i ) ) ):toLocalFile()

               IF ".dbf" == right( lower( cFile ), 4 )
                  hb_fNameSplit( cFile, @cPath, @cAlias )

                  BEGIN SEQUENCE
                     nArea := select()
                     USE ( cFile ) ALIAS "RPTDUMMY" NEW SHARED VIA "DBFCDX"
                     IF ! neterr()
                        aStruct := DbStruct()
                        DbCloseArea()
                        ::addSource( upper( substr( cAlias, 1, 1 ) ) + lower( substr( cAlias, 2 ) ), aStruct )
                     ENDIF
                     select( nArea )
                  END SEQUENCE
               ENDIF
            NEXT
         ENDIF
      ENDCASE

      EXIT

   CASE "graphicsItem_block"
      DO CASE
      CASE p == 21101 // Object selected
         IF hb_hHasKey( ::hObjTree, p1 )
            ::qCurGraphicsItem := ::hItems[ p1 ]
            ::qTreeObjects:setCurrentItem( ::hObjTree[ p1 ] )
         ELSE
            ::qCurGraphicsItem := NIL
         ENDIF
      CASE p == 21017 // Paint Object
         ::drawObject( p1, p2 )

      CASE p == QEvent_GraphicsSceneContextMenu
         ::contextMenuItem( p1, p2 )

      ENDCASE
      EXIT

   CASE "treeObjects_clicked"
      qItem := QTreeWidgetItem():from( p )
      IF hb_hHasKey( ::hItems, qItem:text( 0 ) )
         ::qScene:clearSelection()
         ::hItems[ qItem:text( 0 ) ]:setSelected( .t. )
      ENDIF
      EXIT

   CASE "tabBar_currentChanged"
      IF !empty( ::qStack ) .AND. p < ::qStack:count()
         ::qStack:setCurrentIndex( p )
      ENDIF
      EXIT

   CASE "dataTree_dropEvent"
   CASE "dataTree_mouseReleseEvent"
   CASE "dataTree_dragMoveEvent"
   CASE "dataTree_dragEnterEvent"
      EXIT

   CASE "buttonLandscape_clicked"
      ::qScene:setOrientation( QPrinter_Landscape )
      EXIT
   CASE "buttonPortrait_clicked"
      ::qScene:setOrientation( QPrinter_Portrait )
      EXIT
   CASE "buttonRotateL_clicked"
      IF !empty( ::qCurGraphicsItem )
         ::qCurGraphicsItem:rotate( -10 )
      ENDIF
      EXIT
   CASE "buttonRotateR_clicked"
      IF !empty( ::qCurGraphicsItem )
         ::qCurGraphicsItem:rotate( 10 )
      ENDIF
      EXIT
   CASE "buttonToBack_clicked"
      EXIT
   CASE "buttonToFront_clicked"
      EXIT
   CASE "buttonNew_clicked"
      EXIT
   CASE "buttonOpen_clicked"
      ::openReport()
      EXIT
   CASE "buttonSave_clicked"
      ::saveReport()
      EXIT
   CASE "buttonClose_clicked"
      EXIT
   CASE "buttonPrint_clicked"
      ::printPreview()
      //::printReport()
      EXIT
   CASE "buttonGrid_clicked"
      ::qScene:setShowGrid( ::qToolbarAlign:setItemChecked( "Grid" ) )
      EXIT
   CASE "buttonZoom_clicked"
      DO CASE
      CASE p == 1
         //::qScene:zoomIn()
         ::zoom( HBQT_GRAPHICSVIEW_ZOOM_IN )
      CASE p == 2
         //::qScene:zoomOut()
         ::zoom( HBQT_GRAPHICSVIEW_ZOOM_OUT )
      CASE p == 3
         ::qScene:zoomWYSIWYG()
         //::zoom( HBQT_GRAPHICSVIEW_ZOOM_WYSIWYG )
      CASE p == 4
         ::qScene:zoomOriginal()
         //::zoom( HBQT_GRAPHICSVIEW_ZOOM_ORIGINAL )
      ENDCASE
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:presentBlankPage()

   aadd( ::aPages, { "Page_1" } )

   ::qScene:setPageSize( QPrinter_A4 )
   ::qScene:setOrientation( QPrinter_Portrait )

   ::updateObjectsTree( "ReportName", NIL, "Report" )
   ::updateObjectsTree( "Page", "Report", "Page_1" )

   ::addSource( "Customer", { { "Title" ,"C",35,0 }, { "Street","C",20,0 }, { "Revenue","N",12,2 } } )
   ::addSource( "Invoice" , { { "Number","C",10,0 }, { "Date"  ,"D",08,0 }, { "Amount" ,"N",12,2 } } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:openReport()
   LOCAL qFileDlg, qList, cFile

   qFileDlg := QFileDialog():new( ::oWidget )

   qFileDlg:setAcceptMode( QFileDialog_AcceptOpen )
   qFileDlg:setFileMode( QFileDialog_AnyFile )
   qFileDlg:setViewMode( QFileDialog_List )
   qFileDlg:setNameFilter( "HB Reports (*.hqr)" )

   IF qFileDlg:exec() == 1
      qList := QStringList():from( qFileDlg:selectedFiles() )
      cFile := qList:at( 0 )
      IF !empty( cFile ) .AND. lower( right( cFile, 4 ) ) == ".hqr"
         ::loadReport( cFile )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:saveReport( lSaveAs )
   LOCAL cFile, cBuffer, qFileDlg, qList, cExt
   LOCAL lSave := .t.

   DEFAULT lSaveAs TO .f.

   IF lSaveAs .OR. ::lNew .OR. empty( ::cSaved )
      qFileDlg := QFileDialog():new( ::oWidget )

      qFileDlg:setAcceptMode( QFileDialog_AcceptSave )
      qFileDlg:setFileMode( QFileDialog_AnyFile )
      qFileDlg:setViewMode( QFileDialog_List )
      qFileDlg:setNameFilter( "HB Reports (*.hqr)" )

      IF qFileDlg:exec() == 1
         qList := QStringList():from( qFileDlg:selectedFiles() )
         cFile := qList:at( 0 )
         hb_fNameSplit( cFile, , , @cExt )
         IF empty( cExt )
            cFile += ".hqr"
         ENDIF

         ::cSaved := cFile
      ELSE
         lSave := .f.
      ENDIF
   ENDIF

   IF lSave .AND. !empty( ::cSaved )
      cBuffer  := ::buildReportStream()
      hb_memowrit( ::cSaved, hb_strtoutf8( cBuffer ) )

      RETURN hb_fileExists( ::cSaved )
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:prepareReport()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:toString()
   RETURN ::buildReportStream()

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildReportStream()
   LOCAL txt_:= {}, n, a_, s, oWidget, qPos, qTran

   aadd( txt_, "[GENERAL]" )
   aadd( txt_, "" )
   aadd( txt_, "Symposis"     + "=" + "HBReportsManager"   )
   aadd( txt_, "Version"      + "=" + hb_ntos( ::version ) )
   aadd( txt_, "Title"        + "=" + ::title              )
   aadd( txt_, "Author"       + "=" + ::author             )
   aadd( txt_, "DateCreated"  + "=" + dtos( ::created )    )
   aadd( txt_, "DateModified" + "=" + dtos( ::modified )   )
   aadd( txt_, "Properties"   + "=" + ""                   )
   aadd( txt_, "" )
   aadd( txt_, "[SOURCES]" )
   aadd( txt_, "" )
   FOR EACH a_ IN ::aSources
      n := a_:__enumIndex()
      aadd( txt_, INI_KEY( "source", n ) + a_[ 1 ] + "," + rmgr_a2arrayStr( a_[ 2 ] ) )
   NEXT
   aadd( txt_, "" )
   aadd( txt_, "[PAGES]" )
   aadd( txt_, "" )
   FOR EACH a_ IN ::aPages
      n := a_:__enumIndex()
      aadd( txt_, INI_KEY( "page", n ) + rmgr_a2arrayStr( a_ ) )
   NEXT
   aadd( txt_, "" )
   aadd( txt_, "[OBJECTS]" )
   aadd( txt_, "" )
   FOR EACH a_ IN ::aObjects
      n := a_:__enumIndex()
      IF hb_hHasKey( ::hItems, a_[ 3 ] )
         oWidget := ::hItems[ a_[ 3 ] ]
         qPos    := QPointF():from( oWidget:scenePos() )
         qTran   := QTransform():from( oWidget:transform() )

         a_[ 5 ] := { { 0, 0, oWidget:width(), oWidget:height() }, ;
                      { qPos:x(), qPos:y() }, ;
                      { qTran:m11(), qTran:m12(), qTran:m13(), qTran:m21(), qTran:m22(), qTran:m23(), qTran:m31(), qTran:m32(), qTran:m33() }, ;
                    }

         aadd( txt_, INI_KEY( "object", n ) + rmgr_a2arrayStr( a_ ) )
      ENDIF
   NEXT
   aadd( txt_, "" )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 ) + chr( 10 ) } )

   RETURN s

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:parseBuffer( cBuffer )
   LOCAL aTxt, s, nPart, cKey, cVal

   aTxt := hb_ATokens( StrTran( cBuffer, Chr( 13 ) ), Chr( 10 ) )

   FOR EACH s IN aTxt
      s := alltrim( s )
      IF empty( s )
         LOOP
      ENDIF

      SWITCH Upper( s )
      CASE "[GENERAL]" ; nPart := "HQR_GENERAL" ; EXIT
      CASE "[SOURCES]" ; nPart := "HQR_SOURCES" ; EXIT
      CASE "[PAGES]"   ; nPart := "HQR_PAGES"   ; EXIT
      CASE "[OBJECTS]" ; nPart := "HQR_OBJECTS" ; EXIT
      OTHERWISE
         DO CASE
         CASE nPart == "HQR_GENERAL"
         CASE nPart == "HQR_SOURCES"
            IF rmgr_keyValuePair( s, @cKey, @cVal, "=" )
               IF rmgr_keyValuePair( cVal, @cKey, @cVal, "," )
                  aadd( ::aRptSources, { "Source", rmgr_evalAsArray( cVal ) } )
               ENDIF
            ENDIF
         CASE nPart == "HQR_PAGES"
            IF rmgr_keyValuePair( s, @cKey, @cVal, "=" )
               aadd( ::aRptPages, { "Page", rmgr_evalAsArray( cVal ) } )
            ENDIF
         CASE nPart == "HQR_OBJECTS"
            IF rmgr_keyValuePair( s, @cKey, @cVal, "=" )
               aadd( ::aRptObjects, rmgr_evalAsArray( cVal ) )
            ENDIF
         ENDCASE
      ENDSWITCH
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:loadReport( xData )
   LOCAL cBuffer, a_, d_, n, cName, cAlias, cField, oWidget
   LOCAL aGeo, aPt, aTran
   LOCAL qGeo, qPt, qTran

   ::clear()

   IF empty( xData )
      ::presentBlankPage()
      ::lNew := .t.

   ELSE
      ::lNew := .f.

      IF len( xData ) <= 300 .AND. hb_fileExists( xData )
         ::cSaved := xData
         cBuffer  := hb_utf8tostr( hb_memoread( xData ) )

         IF !empty( ::qParent )
            ::qParent:setWindowTitle( "HBReportsManager : " + ::cSaved )
         ENDIF
      ELSE
         ::cSaved := ""
         cBuffer  := xData
      ENDIF

      ::parseBuffer( cBuffer )

      ::qScene:setPageSize( QPrinter_A4 )
      ::qScene:setOrientation( QPrinter_Portrait )

      ::updateObjectsTree( "ReportName", NIL, "Report" )
      ::updateObjectsTree( "Page", "Report", "Page_1" )

      FOR EACH a_ IN ::aRptSources
         ::addSource( a_[ 1 ], a_[ 2 ] )
      NEXT

      FOR EACH a_ IN ::aRptObjects
         d_:= a_[ 5 ] ; aGeo := d_[ 1 ] ; aPt := d_[ 2 ] ; aTran := d_[ 3 ]

         qGeo := QRectF():new( aGeo[ 1 ], aGeo[ 2 ], aGeo[ 3 ], aGeo[ 4 ] )
         qPt  := QPointF():new( aPt[ 1 ], aPt[ 2 ] )

         SWITCH a_[ 1 ]
         CASE "Object"
            oWidget := ::addObject( a_[ 4 ], qGeo, qPt )
            EXIT
         CASE "Field"
            cName   := a_[ 3 ] ; n := at( "...", cName ) ; cAlias := substr( cName, 1, n-1 )
            cField  := substr( cName, n + 3 ) ; n := at( "_", cField ) ; cField := substr( cField, 1, n-1 )
            oWidget := ::addField( cAlias, cField, qGeo, qPt )
            EXIT
         ENDSWITCH

         qTran   := QTransform():new()
         qTran   :  setMatrix( aTran[ 1 ], aTran[ 2 ], aTran[ 3 ], aTran[ 4 ], aTran[ 5 ], aTran[ 6 ], aTran[ 7 ], aTran[ 8 ], aTran[ 9 ] )
         oWidget :  setTransform( qTran )
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:addObject( cType, qGeo, qPos )
   LOCAL oWidget, cName, nW, nH, qGrad, cCode, qStrList, i

   cName := cType + "_" + hb_ntos( ::getNextID( cType ) )

   SWITCH cType
   CASE "Image"
      nW := 300 ;  nH := 300
      oWidget := HBQGraphicsItem():new( HBQT_GRAPHICSITEM_PICTURE )
      oWidget:setPixmap( QPixmap():new( app_image( "hbide" ) ) )
      oWidget:setBorderWidth( 2 )
      EXIT
   CASE "Chart"
      nW := 400 ;  nH := 250
      oWidget := HBQGraphicsItem():new( HBQT_GRAPHICSITEM_CHART )
      EXIT
   CASE "Gradient"
      nW := 300 ;  nH := 50
      qGrad := QLinearGradient():new()// 0, 0, 1, 1 )
      qGrad:setColorAt( 0, QColor():new( 195,225,255 ) )
      //qGrad:setColorAt( 1, ( QColor():new( 195,225,255 ) ):darker( 150 ) )
      qGrad:setColorAt( 1, ( QColor():new( Qt_darkBlue ) ):darker( 150 ) )
      qGrad:setCoordinateMode( QGradient_StretchToDeviceMode )

      oWidget := HBQGraphicsItem():new( HBQT_GRAPHICSITEM_RECT )
      oWidget:setBrush( QBrush():new( "QGradient", qGrad ) )
      oWidget:setPen( QPen():new( Qt_NoPen ) )
      EXIT
   CASE "Barcode"
      nW := 300 ;  nH := 200
      cCode := ::fetchBarString( "51550621" )
      qStrList := QStringList():new()
      FOR i := 1 TO len( cCode )
         IF substr( cCode, i, 1 ) == "1"
            qStrList:append( "-" )
         ELSE
            qStrList:append( "." )
         ENDIF
      NEXT
      oWidget := HBQGraphicsItem():new( HBQT_GRAPHICSITEM_BARCODE )
      oWidget:setBarValues( qStrList )
      EXIT
   CASE "Text"
      nW := 300 ;  nH := 50
      oWidget := HBQGraphicsItem():new( HBQT_GRAPHICSITEM_SIMPLETEXT )
      oWidget:setBrush( QBrush():new( "QColor", QColor():new( 200,200,245 ) ) )
      oWidget:setText( "Harbour" )
      EXIT
   ENDSWITCH

   oWidget:setObjectType( cType )
   oWidget:setObjectName( cName )
   oWidget:setTooltip( cName )
   oWidget:hbSetBlock( {|p,p1,p2| ::execEvent( "graphicsItem_block", p, p1, p2 ) } )

   ::qScene:addItem( oWidget )

   oWidget:setGeometry( iif( empty( qGeo ), QRectF():new( 0, 0, nW, nH ), qGeo ) )
   IF !empty( qPos )
      oWidget:setPos( qPos )
   ENDIF
   ::hItems[ cName ] := oWidget
   ::updateObjectsTree( "Object", "Page_1", cName, cType )
   aadd( ::aObjects, { "Object", "Page_1", cName, cType, {} } )

   RETURN oWidget

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:addField( cAlias, cField, qGeo, qPos )
   LOCAL oWidget, nW := 300, nH := 50
   LOCAL cName := cAlias + "..." + cField

   cName := cName + "_" + hb_ntos( ::getNextID( cName ) )

   oWidget := HBQGraphicsItem():new( HBQT_GRAPHICSITEM_SIMPLETEXT )
   oWidget:setText( cName )
   oWidget:hbSetBlock( {|p,p1,p2| ::execEvent( "graphicsItem_block", p, p1, p2 ) } )
   oWidget:setTooltip( cName )
   oWidget:setObjectType( "Field" )
   oWidget:setObjectName( cName )

   ::qScene:addItem( oWidget )

   oWidget:setGeometry( iif( empty( qGeo ), QRectF():new( 0, 0, nW, nH ), qGeo ) )
   IF !empty( qPos )
      oWidget:setPos( qPos )
   ENDIF
   ::hItems[ cName ] := oWidget
   ::updateObjectsTree( "Field", "Page_1", cName, "Field" )
   aadd( ::aObjects, { "Field", "Page_1", cName, "Field", {} } )

   RETURN oWidget

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:addSource( cAlias, aStruct )
   LOCAL qItem, qItmC, b_

   qItem := QTreeWidgetItem():new()
   qItem:setText( 0, cAlias )
   ::qTreeData:addTopLevelItem( qItem )

   FOR EACH b_ IN aStruct
      qItmC := QTreeWidgetItem():new()
      qItmC:setText( 0, b_[ 1 ] )
      qItem:addChild( qItmC )
      qItem:setExpanded( .t. )
   NEXT

   aadd( ::aSources, { cAlias, aStruct } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:clear()
   LOCAL qObj

   FOR EACH qObj IN ::hItems
      ::qScene:removeItem( qObj )
      qObj := NIL
   NEXT
   ::hItems   := {=>}

   ::qTreeObjects:clear()
   ::qTreeData:clear()

   ::aObjects := {}
   ::aPages   := {}
   ::aSources := {}

   ::aRptObjects := {}
   ::aRptPages   := {}
   ::aRptSources := {}

   hIDs := {=>}

   ::qScene:invalidate()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:updateObjectsTree( cType, cParent, cName, cSubType )
   LOCAL qParent, qItem

   DO CASE
   CASE cType == "ReportName"
      qItem := QTreeWidgetItem():new() ; qItem:setText( 0, cName )
      qItem:setIcon( 0, app_image( "r-report" ) )
      ::qTreeObjects:addTopLevelItem( qItem )
      ::hObjTree[ cName ] := qItem
      qItem:setExpanded( .t. )

   CASE cType == "Page" .OR. cType == "Object" .OR. cType == "Field"
      IF hb_hHasKey( ::hObjTree, cParent )
         qParent := ::hObjTree[ cParent ]
      ENDIF
      IF !empty( qParent )
         IF hb_hHasKey( ::hObjTree, cName )
            //
         ENDIF
         qItem := QTreeWidgetItem():new() ; qItem:setText( 0, cName )
         qParent:addChild( qItem )
         ::hObjTree[ cName ] := qItem

         IF cType == "Page"
            qItem:setIcon( 0, app_image( "r-page" ) )
         ELSEIF cType == "Object"
            qItem:setIcon( 0, ::getImageOfType( cSubType ) )
         ELSEIF cType == "Field"
            qItem:setIcon( 0, ::getImageOfType( "Field" ) )
         ENDIF

         qParent:setExpanded( .t. )
      ENDIF
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:zoom( nMode )
   LOCAL qWidget

   SWITCH nMode
   CASE HBQT_GRAPHICSVIEW_ZOOM_IN
      ::qView:scale( 1.1, 1.1 )
      EXIT
   CASE HBQT_GRAPHICSVIEW_ZOOM_OUT
      ::qView:scale( 0.9, 0.9 )
      EXIT
   CASE HBQT_GRAPHICSVIEW_ZOOM_WYSIWYG
      qWidget := QDesktopWidget():new()
      //qWidget := QWidget():from( qWidget:screen() )

HB_TRACE( HB_TR_ALWAYS, qWidget:width(), qWidget:physicalDpiX(), hbqt_screen_widthMM, qWidget:height(), hbqt_screen_heightMM  )

      ::qView:resetMatrix()
      ::qView:scale( qWidget:width() / hbqt_screen_widthMM  * 10, qWidget:height() / hbqt_screen_heightMM * 10 )
      ::qView:centerOn( 0, 0 )
      EXIT
   CASE HBQT_GRAPHICSVIEW_ZOOM_ORIGINAL
      ::qView:resetMatrix()
      ::qView:scale( 0,0 )
      EXIT
   ENDSWITCH

   RETURN sELF

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:contextMenuScene( p1 )
   LOCAL qMenu, qEvent, pAct

   qEvent := QGraphicsSceneContextMenuEvent():from( p1 )

   qMenu := QMenu():new( ::qView )
   qMenu:addAction( "Refresh"  )
   qMenu:addAction( "Zoom+" )

   pAct := qMenu:exec_1( qEvent:screenPos() )
   IF ! hbqt_isEmptyQtPointer( pAct )
      SWITCH ( QAction():configure( pAct ) ):text()
      CASE "Refresh"
         EXIT
      CASE "Zoom+"
         EXIT
      ENDSWITCH
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:contextMenuItem( p1, p2 )
   LOCAL qMenu, qEvent, pAct

   HB_SYMBOL_UNUSED( p2 )

   qEvent := QGraphicsSceneContextMenuEvent():from( p1 )

   qMenu := QMenu():new()
   qMenu:addAction( "Cut"  )
   qMenu:addAction( "Copy" )

   pAct := qMenu:exec_1( qEvent:screenPos() )
   IF ! hbqt_isEmptyQtPointer( pAct )
      SWITCH ( QAction():configure( pAct ) ):text()
      CASE "Cut"
         EXIT
      CASE "Copy"
         EXIT
      ENDSWITCH
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildTabBar()

   ::qTabBar := QTabBar():new()
   ::qTabBar:setShape( QTabBar_TriangularNorth )

   ::qTabBar:addTab( "Code"    )
   ::qTabBar:addTab( "Dialogs" )
   ::qTabBar:addTab( "Page_1"  )

   ::qTabBar:connect( "currentChanged(int)", {|p| ::execEvent( "tabBar_currentChanged", p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildStacks()

   ::qStack := QStackedWidget():new()

   ::qWidget1 := QWidget():new()
   ::qStack:addWidget( ::qWidget1 )

   ::qWidget2 := QWidget():new()
   ::qStack:addWidget( ::qWidget2 )

   ::qWidget3 := QWidget():new()
   ::qStack:addWidget( ::qWidget3 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildStatusBar()
   LOCAL qLabel

   ::qStatus := QStatusBar():new()
   ::qStatus:setSizeGripEnabled( .f. )

   qLabel := QLabel():new(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )
   qLabel:setText( "Ready" )

   qLabel := QLabel():new(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel():new(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel():new(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 1 )
   aadd( ::aStatusPnls, qLabel )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:getImageOfType( cType )
   LOCAL cImage

   DO CASE
   CASE cType == "Image"
      cImage := "f-image"
   CASE cType == "Barcode"
      cImage := "f_barcode"
   CASE cType == "Chart"
      cImage := "f_chart"
   CASE cType == "Gradient"
      cImage := "f_gradient"
   CASE cType == "Text"
      cImage := "text"
   CASE cType == "Field"
      cImage := "text"
   ENDCASE

   RETURN app_image( cImage )

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:getNextID( cType )

   IF ! hb_hHasKey( hIDs, cType )
      hIDs[ cType ] := 0
   ENDIF

   RETURN ++hIDs[ cType ]

/*----------------------------------------------------------------------*/
/*                                                                      */
/*   NOTE: the code below is works of someone else I do not remmeber    */
/*         the name. Please let me know who that is so due credits be   */
/*         given to him. I had downloaded this code many years back     */
/*         and adopted to Vouch32 library and Vouch32 Active-X Server.  */

METHOD HbqReportsManager:fetchBarString( cCode, lCheck )
   STATIC cCars   := '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%'
   STATIC aBarras := {  '1110100010101110',;  // 1
                        '1011100010101110',;  // 2
                        '1110111000101010',;  // 3
                        '1010001110101110',;  // 4
                        '1110100011101010',;  // 5
                        '1011100011101010',;  // 6
                        '1010001011101110',;  // 7
                        '1110100010111010',;  // 8
                        '1011100010111010',;  // 9
                        '1010001110111010',;  // 0
                        '1110101000101110',;  // A
                        '1011101000101110',;  // B
                        '1110111010001010',;  // C
                        '1010111000101110',;  // D
                        '1110101110001010',;  // E
                        '1011101110001010',;
                        '1010100011101110',;
                        '1110101000111010',;
                        '1011101000111010',;
                        '1010111000111010',;
                        '1110101010001110',;  // K
                        '1011101010001110',;
                        '1110111010100010',;
                        '1010111010001110',;
                        '1110101110100010',;
                        '1011101110100010',;  // p
                        '1010101110001110',;
                        '1110101011100010',;
                        '1011101011100010',;
                        '1010111011100010',;
                        '1110001010101110',;
                        '1000111010101110',;
                        '1110001110101010',;
                        '1000101110101110',;
                        '1110001011101010',;
                        '1000111011101010',;  // Z
                        '1000101011101110',;  // -
                        '1110001010111010',;  // .
                        '1000111010111010',;  // ' '
                        '1000101110111010',;  // *
                        '1000100010100010',;
                        '1000100010100010',;
                        '1000101000100010',;
                        '1010001000100010' }


   LOCAL cCar, m, n, cBarra := '',  nCheck := 0

   DEFAULT lCheck TO .f.

   cCode := upper( cCode )
   IF len( cCode ) > 32
      cCode := left( cCode,32 )
   ENDIF

   cCode := '*' + cCode + '*'
   FOR n := 1 TO len( cCode )
      cCar := substr( cCode,n,1 )
      m    := at( cCar, cCars )
      IF m > 0
         cBarra := cBarra + aBarras[ m ]
         nCheck += ( m-1 )
      ENDIF
   NEXT

   IF lCheck
      cBarra += aBarras[ nCheck % 43 + 1 ]
   ENDIF

   RETURN cBarra

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildToolbar()

   ::qToolbar := HbqToolbar():new()
   ::qToolbar:orientation := Qt_Horizontal
   ::qToolbar:create( "ReportManager_Top_Toolbar" )

   ::qToolbar:addToolButton( "New"      , "New Report"            , app_image( "new"         ), {|| ::execEvent( "buttonNew_clicked"     ) } )
   ::qToolbar:addToolButton( "Open"     , "Open Report"           , app_image( "open3"       ), {|| ::execEvent( "buttonOpen_clicked"    ) } )
   ::qToolbar:addToolButton( "Save"     , "Save Report"           , app_image( "save3"       ), {|| ::execEvent( "buttonSave_clicked"    ) } )
   ::qToolbar:addToolButton( "Close"    , "Close Report"          , app_image( "close3"      ), {|| ::execEvent( "buttonClose_clicked"   ) } )
   ::qToolbar:addToolButton( "Print"    , "Print Report"          , app_image( "print"       ), {|| ::execEvent( "buttonPrint_clicked"   ) } )
   ::qToolbar:addSeparator()
   ::qToolbar:addToolButton( "ToBack"   , "Push to back"          , app_image( "toback"      ), {|| ::execEvent( "buttonToBack_clicked"  ) }, .f., .f. )
   ::qToolbar:addToolButton( "ToFront"  , "Bring to front"        , app_image( "tofront"     ), {|| ::execEvent( "buttonToFront_clicked" ) }, .f., .f. )
   ::qToolbar:addSeparator()
   ::qToolbar:addToolButton( "RotateL"  , "Rotate anti-clock wise", app_image( "unload_1"    ), {|| ::execEvent( "buttonRotateL_clicked" ) }, .f., .f. )
   ::qToolbar:addToolButton( "RotateR"  , "Rotate clock wise"     , app_image( "load_1"      ), {|| ::execEvent( "buttonRotateR_clicked" ) }, .f., .f. )
   ::qToolbar:addSeparator()
   ::qToolbar:addToolButton( "Portrait" , "Portrait orientation"  , app_image( "r-portrait"  ), {|| ::execEvent( "buttonPortrait_clicked" ) }, .f., .f. )
   ::qToolbar:addToolButton( "Landscape", "Landscape orientation" , app_image( "r-landscape" ), {|| ::execEvent( "buttonLandscape_clicked" ) }, .f., .f. )
   ::qToolbar:addSeparator()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildToolbarAlign()

   ::qToolbarAlign := HbqToolbar():new()
   ::qToolbarAlign:orientation := Qt_Horizontal
   ::qToolbarAlign:create( "ReportManager_Top_Toolbar_Align" )

   ::qToolbarAlign:addToolButton( "FontG"  , "Font"              , app_image( "f-generic"       ), {|| ::execEvent( "button_clicked" ) }, .f., .f. )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "FontB"  , "Text Bold"         , app_image( "f-bold-1"        ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "FontI"  , "Text Italic"       , app_image( "f-italic-1"      ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "FontU"  , "Text Underlined"   , app_image( "f-underline-1"   ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "FontS"  , "Text Strikethrough", app_image( "f-strike-1"      ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "JustL"  , "Align left"        , app_image( "f_align_left"    ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustC"  , "Align center"      , app_image( "f_align_center"  ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustR"  , "Align right"       , app_image( "f_align_right"   ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustJ"  , "Align justify"     , app_image( "f_align_justify" ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "JustT"  , "Align top"         , app_image( "f_align_top"     ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustM"  , "Align middle"      , app_image( "f_align_middle"  ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustB"  , "Align bottom"      , app_image( "f_align_bottom"  ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "BoxT"   , "Box-frame top"     , app_image( "f_box_top"       ), {|| ::execEvent( "button_clicked" ) }, .t., .f. )
   ::qToolbarAlign:addToolButton( "BoxL"   , "Box-frame left"    , app_image( "f_box_left"      ), {|| ::execEvent( "button_clicked" ) }, .t., .f. )
   ::qToolbarAlign:addToolButton( "BoxB"   , "Box-frame bottom"  , app_image( "f_box_bottom"    ), {|| ::execEvent( "button_clicked" ) }, .t., .f. )
   ::qToolbarAlign:addToolButton( "BoxR"   , "Box-frame right"   , app_image( "f_box_right"     ), {|| ::execEvent( "button_clicked" ) }, .t., .f. )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "BoxA"   , "Box-frame all"     , app_image( "f_box_all"       ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "BoxP"   , "No box-frame"      , app_image( "f_box_plain"     ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "BoxS"   , "Box shadowed"      , app_image( "f_box_shadow"    ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "ZoomIn" , "Zoom In"           , app_image( "zoomin3"         ), {|| ::execEvent( "buttonZoom_clicked", 1 ) } )
   ::qToolbarAlign:addToolButton( "ZoomOut", "Zoom Out"          , app_image( "zoomout3"        ), {|| ::execEvent( "buttonZoom_clicked", 2 ) } )
   ::qToolbarAlign:addToolButton( "ZoomWYS", "Zoom WYSIWYG"      , app_image( "zoomin"          ), {|| ::execEvent( "buttonZoom_clicked", 3 ) } )
   ::qToolbarAlign:addToolButton( "ZoomOrg", "Zoom Original"     , app_image( "zoomout"         ), {|| ::execEvent( "buttonZoom_clicked", 4 ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "Grid"   , "Show Grid"         , app_image( "grid"            ), {|| ::execEvent( "buttonGrid_clicked", 4 ) }, .t., .f. )
   ::qToolbarAlign:addSeparator()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:buildToolbarLeft()

   ::qToolbarL := HbqToolbar():new()
   ::qToolbarL:orientation := Qt_Vertical
   ::qToolbarL:create( "ReportManager_Left_Toolbar" )

   ::qToolbarL:addToolButton( "Image"   , "Image"   , app_image( "f-image"    ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Chart"   , "Chart"   , app_image( "f_chart"    ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Gradient", "Gradient", app_image( "f_gradient" ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Barcode" , "Barcode" , app_image( "f_barcode"  ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Text"    , "Text"    , app_image( "text"       ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION rmgr_xtos( x )
   SWITCH valtype( x )
   CASE "C" ; RETURN x
   CASE "D" ; RETURN dtos( x )
   CASE "L" ; RETURN iif( x, "YES", "NO" )
   CASE "N" ; RETURN hb_ntos( x )
   ENDSWITCH
   RETURN ""

STATIC FUNCTION rmgr_array2String( aArray )
   LOCAL a_, s, x

   s := ""
   FOR EACH a_ IN aArray
      FOR EACH x IN a_
         s += rmgr_xtos( x ) + " "
      NEXT
      s := trim( s ) + ","
   NEXT

   RETURN s

STATIC FUNCTION rmgr_a2arrayStr( aArray )
   LOCAL s, x

   s := "{"
   FOR EACH x IN aArray
      SWITCH valtype( x )
      CASE "C"
         s += '"' + x + '"'               ; EXIT
      CASE "N"
         s += hb_ntos( x )                ; EXIT
      CASE "D"
         s += "stod(" + dtos( x ) + ")"   ; EXIT
      CASE "L"
         s += iif( x, ".t.", ".f." )      ; EXIT
      CASE "A"
         s += rmgr_a2arrayStr( x )        ; EXIT
      OTHERWISE
         s += "NIL"                       ; EXIT
      ENDSWITCH
      s += ","
   NEXT
   s := iif( len( s ) == 1, s, substr( s, 1, len( s ) - 1 ) ) + "}"

   RETURN s

/*----------------------------------------------------------------------*/

STATIC FUNCTION rmgr_generateNextColor()
   RETURN QColor():new( hb_random( 0,255 ), hb_random( 0,255 ), hb_random( 0,255 ), 255 )

/*----------------------------------------------------------------------*/

STATIC FUNCTION rmgr_keyValuePair( s, cKey, cVal, cDlm )
   LOCAL n

   DEFAULT cDlm TO "="

   IF ( n := at( cDlm, s ) ) > 0
      cKey := alltrim( substr( s, 1, n - 1 ) )
      cVal := alltrim( substr( s, n + 1 ) )
      RETURN .t.
   ENDIF
   RETURN .f.

/*----------------------------------------------------------------------*/

STATIC FUNCTION rmgr_evalAsArray( cStr )
   LOCAL a_, bErr := ErrorBlock( {|| break() } )

   BEGIN SEQUENCE
      a_:= eval( &( "{|| " + cStr + "}" ) )
   RECOVER
      a_:= {}
   ENDSEQUENCE

   ErrorBlock( bErr )
   RETURN a_

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:printReport( qPrinter )
   LOCAL qPainter, qObj, qOption, a_, qRectF

   DEFAULT qPrinter TO QPrinter():new()

   qPrinter:setOutputFormat( QPrinter_PdfFormat )
   qPrinter:setOrientation( ::qScene:orientation() )
   //qPrinter:setPaperSize( ::qScene:pageSize() )
   //m_printer->setPaperSize(dynamic_cast<PageInterface*>(obj)->paperRect().size());

   qPrinter:setPaperSize( QRectF():from( ::qScene:paperRect() ):size() )
   // qPrinter:setFullPage( .t. )

   qPainter := QPainter():new()

   qOption := QStyleOptionGraphicsItem():new()
   //qOption:type := 17

   qPainter:begin( qPrinter )
   //
   FOR EACH a_ IN ::aObjects
      IF hb_hHasKey( ::hItems, a_[ 3 ] )
         qObj   := ::hItems[ a_[ 3 ] ]
         qRectF := QRectF():from( qObj:geometry() )
         qRectF := QRectF():new( qRectF:x()*10/25.4, qRectF:y()*10/25.4, qRectF:width()*10/25.4, qRectF:height()*10/25.4 )
         SWITCH a_[ 4 ]
         CASE "Image"
            ::drawImage( qPainter, qRectF, a_, qObj )
            EXIT
         CASE "Barcode"
            ::drawBarcode( qPainter, qRectF, a_, qObj )
            EXIT
         CASE "Chart"
            ::drawChart( qPainter, qRectF, a_, qObj )
            EXIT
         CASE "Gradient"
            EXIT
         OTHERWISE
            IF a_[ 1 ] == "Field"

            ENDIF
         ENDSWITCH
      ENDIF
   NEXT
   //
   qPainter:end()
   qPainter := NIL
   IF empty( qPrinter )
      ::printPreview( qPrinter )
   ENDIF

   RETURN qOption

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:printPreview( qPrinter )
   LOCAL qDlg

   IF !empty( qPrinter )
      qDlg := QPrintPreviewDialog():new( qPrinter, ::qView )
   ELSE
      qDlg := QPrintPreviewDialog():new( ::qView )
      qDlg:connect( "paintRequested(QPrinter)", {|p| ::paintRequested( p ) } )
   ENDIF

   qDlg:setWindowTitle( "HBReportGenerator : " + iif( !empty( ::cSaved ), ::cSaved, "Untitled" ) )
   qDlg:move( 20, 20 )
   qDlg:resize( 300, 400 )
   qDlg:exec()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:paintRequested( pPrinter )
   LOCAL qPrinter := QPrinter():from( pPrinter )

   ::printReport( qPrinter )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:drawObject( pPainter, aInfo )
   LOCAL qPainter := QPainter():from( pPainter )
   LOCAL qRect    := QRectF():from( aInfo[ 1 ] )
   LOCAL cName    := aInfo[ 2 ]
   LOCAL qObj, aObj, n

   IF hb_hHasKey( ::hItems, cName )
      IF ( n := ascan( ::aObjects, {|e_| e_[ 3 ] == cName } ) ) > 0
         aObj := ::aObjects[ n ]
         qObj := ::hItems[ cName ]

         DO CASE
         CASE aObj[ 1 ] == "Object" .AND. aObj[ 4 ] == "Barcode"
            ::drawBarcode( qPainter, qRect, aObj, qObj )
         CASE aObj[ 1 ] == "Object" .AND. aObj[ 4 ] == "Image"
            ::drawImage( qPainter, qRect, aObj, qObj )
         CASE aObj[ 1 ] == "Object" .AND. aObj[ 4 ] == "Chart"
            //::drawChart( qPainter, qRect, aObj, qObj )
         ENDCASE
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:drawBarcode( qPainter, qRect, aObj, qObj )
   LOCAL fl, clr, rc, w, x, i, cCode

   cCode := ::fetchBarString( "Harbour" )

   rc    := QRectF():from( qRect:adjusted( 5, 5, -10, -10 ) )

   fl    := QColor():new( Qt_white )
   clr   := QColor():new( Qt_black )
   w     := rc:width() / len( cCode )
   x     := 0.0

   FOR i := 1 TO len( cCode )
      IF substr( cCode, i, 1 ) == "1"
         qPainter:fillRect_6( QRectF():new( rc:x() + x, rc:y(), w, rc:height() ), clr )
      ELSE
         qPainter:fillRect_6( QRectF():new( rc:x() + x, rc:y(), w, rc:height() ), fl )
      ENDIF
      x += w
   NEXT

   RETURN { aObj, qObj }

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:drawImage( qPainter, qRect, aObj, qObj )
   LOCAL qPix, image, rc, img, point
   LOCAL drawTextType := HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE
   LOCAL paintType    := HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO
   LOCAL borderWidth  := 0
   LOCAL borderColor  := 0, pen, textH, sw, sh, cx, cy, cw, ch, textColor := 0
   LOCAL cText := "Picture"

   rc    := QRectF():from( qRect:adjusted( 5, 5, -10, -10 ) )

   textH := 0
   sw    := 0
   sh    := 0

   IF ( drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE .OR.  ::drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW )
      textH = QFontMetricsF():from( qPainter:font() ):height()
   ENDIF

   qPix  := QPixmap():from( qObj:pixmap() )
   image := QImage():from( qPix:toImage() )

   IF image:isNull()
      qPainter:drawRect( qRect )
   ELSE
      img   := QImage():new( 0, 0 )
      point := QPointF():from( qRect:topLeft() )
      cx    := 0; cy := 0; cw := qPix:width(); ch := qPix:height()

      SWITCH paintType
      CASE HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO
         img := QImage():from( image:scaled_1( rc:width(), rc:height() - textH, Qt_KeepAspectRatio, Qt_SmoothTransformation ) )
         EXIT

      CASE HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_IGNORE_ASPECT_RATIO
         img := QImage():from( image:scaled_1( rc:width(), rc:height() - textH, Qt_IgnoreAspectRatio, Qt_SmoothTransformation ) )
         EXIT

      CASE HBQT_GRAPHICSITEM_CENTER_PICTURE_TO_ITEM
         point:setX( point:x() + ( rc:width() - image:width() ) / 2 )
         point:setY( point:y() + ( rc:height() - image:height() - textH ) / 2 )
         IF ( point:x() < 0 )
            cx := abs( point:x() )
            cw -= 2 * cx
            point:setX( 0 )
         ENDIF
         IF ( point:y() < 0 )
            cy = abs( point:y() )
            ch -= 2 * cy
            point:setY( 0 )
         ENDIF
         img := QImage():from( image:copy_1( cx, cy, cw, ch ) )
         EXIT

      CASE HBQT_GRAPHICSITEM_RESIZE_ITEM_TO_PICTURE
         img := image
         sw := img:width() - qObj:width()
         sh := img:height() - ( qObj:height() - textH )
         EXIT

      ENDSWITCH

      IF drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE
         point:setY( point:y() + textH )
      ENDIF

      qPainter:drawImage_2( point, img )
   ENDIF
   qPainter:setPen( QPen():new( textColor ) )

   SWITCH drawTextType

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_TOP
      qPainter:drawText( rc, Qt_AlignTop + Qt_AlignHCenter, cText )
      EXIT

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_BOTTOM
      qPainter:drawText( rc, Qt_AlignBottom + Qt_AlignHCenter, cText )
      EXIT

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE
      qPainter:drawText( rc, Qt_AlignTop + Qt_AlignHCenter, cText )
      EXIT

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW
      qPainter:drawText( rc, Qt_AlignBottom + Qt_AlignHCenter, cText )
      EXIT

   ENDSWITCH

   IF !empty( sw ) .OR. !empty( sh )
      qObj:setWidth( qObj:width() + sw )
      qObj:setHeight( qObj:height() + sh )
   ENDIF

   IF borderWidth > 0
      pen := QPen():new()
      pen:setWidth( borderWidth )
      pen:setColor( borderColor )
      pen:setJoinStyle( Qt_MiterJoin )
      qPainter:setPen( pen )
      qPainter:setBrush( QBrush():new( Qt_NoBrush ) )
      qPainter:drawRect( rc:x() + borderWidth / 2, rc:y() + borderWidth / 2, ;
                         rc:width() - borderWidth, rc:height() - borderWidth )
   ENDIF

   RETURN { aObj, qObj }

/*----------------------------------------------------------------------*/

METHOD HbqReportsManager:drawChart( qPainter, qRect, aObj, qObj )
   LOCAL qFMetrix, maxpv, minnv, absMaxVal, powVal, chartStep, powStep, maxHeight, valstep, maxLabelWidth, minpv
   LOCAL pw, rc, maxval, y, i, x, cv, barWidth, lg, py, f, cMaxVal, nDec, nFHeight
   LOCAL m_drawBorder      := .t.
   LOCAL m_showLabels      := .t.
   LOCAL m_barsIdentation  := 1
   LOCAL m_showGrid        := .t.
   LOCAL m_toColorFactor   := 1.7

   HB_SYMBOL_UNUSED( aObj )
   HB_SYMBOL_UNUSED( qObj )

   qFMetrix := QFontMetrics():from( qPainter:fontMetrics() )
   nFHeight := qFMetrix:height()

   IF empty( ::xData )
      ::xData := {}

      aadd( ::xData, { "Bananas", 040.0, rmgr_generateNextColor() } )
      aadd( ::xData, { "Oranges", 150.0, rmgr_generateNextColor() } )
      aadd( ::xData, { "Mangoes", 120.0, rmgr_generateNextColor() } )
   ENDIF

   maxpv := 0
   minnv := 0
   aeval( ::xData, {|e_| iif( e_[ 2 ] < 0, minpv := min( minpv, e_[ 2 ] ), NIL ), iif( e_[ 2 ] < 0, maxpv := max( maxpv, e_[ 2 ] ), NIL ) } )

   absMaxVal := maxpv - minnv
   cMaxVal   := hb_ntos( absMaxVal )
   nDec      := at( ".", cMaxVal )

   // = ( absMaxVal < 1 ) ? pow( 10.0, QString::number( absMaxVal ).right( QString::number( absMaxVal ).indexOf( '.' ) ).length() + 1 ) : 1;
   powVal    := iif( absMaxVal < 1,  10.0 ^ ( len( substr( cMaxVal, nDec+1 ) ) + 1 ), 1 )
   maxpv     *= powVal
   minnv     *= powVal

   maxpv     := maxpv
   minnv     := -minnv
   minnv     := -minnv

   qPainter:fillRect( qRect, ::brush() )

   IF m_drawBorder
      qPainter:drawqRect( qRect )
   ENDIF

   pw := iif( abs( ::pen():widthF() ) > 0, abs( ::pen():widthF() ), 1 )
   rc := QRectF():from( qRect:adjusted( pw / 2, pw / 2, -pw, -pw ) )

   f  := 2
   chartStep := ( 10.0 ^ ( len( substr( cMaxVal, 1, nDec - 1 ) ) - 1 ) ) / f
   powStep   := iif( chartStep < 1, 10, 1 )
   chartStep *= powStep
   maxpv     *= powStep
   minnv     *= powStep
   powVal    *= powStep

   maxpv  := maxpv + ( iif( (   maxpv % chartStep ) != 0, ( chartStep - (   maxpv % chartStep ) ), 0 ) ) / powVal
   minnv  := minnv - ( iif( ( - minnv % chartStep ) != 0, ( chartStep - ( - minnv % chartStep ) ), 0 ) ) / powVal
   maxVal := maxpv - minnv

   maxHeight := rc:height() - nFHeight
   valstep := maxHeight / ( maxVal / chartStep )

   IF ( valstep < nFHeight )
      chartStep *= ( (  ( nFHeight / valstep ) ) + 1 )
      valstep := ( (  ( nFHeight / valstep ) ) + 1 ) * valstep
   ENDIF

   IF m_showLabels
      maxLabelWidth := 0
      FOR i := 0 TO maxVal / chartStep + 1 + iif( ( maxVal %  chartStep ) != 0, 1, 0 )
         IF ( maxLabelWidth < qFMetrix:width( hb_ntos( ( maxVal * i - chartStep * i ) / powVal ) ) )
            maxLabelWidth := qFMetrix:width( hb_ntos( ( maxVal * i - chartStep * i ) / powVal ) )
         ENDIF
      NEXT
      y := 0
      FOR i := 0 TO maxVal / chartStep + 1 + iif( ( maxVal % chartStep ) != 0, 1, 0 )
         qPainter:drawText( QRectF( rc:x(), rc:y() + y, maxLabelWidth, nFHeight ), ;
                         Qt_AlignRight + Qt_AlignVCenter, hb_ntos( ( maxpv - chartStep * i ) / powVal ) )
         y += valstep
      NEXT

      qPainter:drawLine( rc:x() + maxLabelWidth + 1 / UNIT / 4, rc:y(), rc:x() + maxLabelWidth + 1 / UNIT / 4, rc:y() + qRect:height() )
      rc := QRectF():from( rc:adjusted( maxLabelWidth + 1 / UNIT / 4, 0, 0, 0 ) )
   ENDIF

   IF ( m_showGrid )
      y :=  nFHeight / 2
      FOR i := 0 TO maxVal / chartStep + 1 + ( iif( maxVal %  chartStep !=0, 1, 0 ) )
         qPainter:drawLine( rc:x(), rc:y() + y, rc:x() + rc:width(), rc:y() + y )
         y += valstep
      NEXT
   ENDIF

   rc := rc:adjusted( 0,  nFHeight / 2, 0, 0 )
   x := m_barsIdentation
   barWidth := ( rc:width() - m_barsIdentation * ( len( ::aData ) + 1 ) ) / len( ::aData )
   py := maxHeight / maxVal

   FOR EACH cv IN ::xData
      lg := QLinearGradient():new( QPointF():new( x + barWidth / 2, 0 ), QPointF():new( x + barWidth , 0 ) )
      lg:setSpread( QGradient_ReflectSpread )
      lg:setColorAt( 0, cv[ 3 ] )
      lg:setColorAt( 1, QColor( cv[ 3 ]:red() * m_toColorFactor, cv[ 3 ]:green() * m_toColorFactor, cv[ 3 ]:blue() * m_toColorFactor, cv[ 3 ]:alpha() ) )
      qPainter:fillqRect( QRectF( rc:x() + x, rc:y() + py * maxpv - py * cv[ 2 ] * powVal, barWidth, py * cv[ 2 ] * powVal ), QBrush():new( "QGradient", lg ) )
      IF ( m_showLabels )
         qPainter:drawText( QRectF( rc:x() + x - m_barsIdentation / 2, rc:y() + py * maxpv - iif( cv[ 2 ] >= 0, nFHeight, 0 ), ;
                                      barWidth + m_barsIdentation, nFHeight ), Qt_AlignCenter, hb_ntos( cv[ 2 ] ) )
      ENDIF
      x += barWidth + m_barsIdentation
   NEXT

   #if 0  /* Legend */
   qPainter:fillqRect( qRect, ::brush() )
   qPainter:drawqRect( qRect )
   qPainter:translate( qRect:topLeft() )
   qreal y := 1 / UNIT
   qreal vstep := ( qRect:height() - y - 1 / UNIT * val:size() ) / len( ::aData )
   FOR EACH cv IN ::aData
   {
      qPainter:fillqRect( QRectF( 1 / UNIT / 2, y, m_legendColorqRectWidth, vstep ), QBrush():new( cv[ 3 ] ) )
      qPainter:drawText( QRectF( 1 / UNIT + m_legendColorqRectWidth, y, qRect:width() - ( 1 / UNIT + m_legendColorqRectWidth ), vstep ),
                                                                            Qt_AlignVCenter + Qt_AlignLeft, cv[ 1 ] )
      y += vstep + 1 / UNIT
   }
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/
//                       HqrGraphicsItem() Class
/*----------------------------------------------------------------------*/

CLASS HqrGraphicsItem

   DATA   oWidget
   DATA   cParent

   DATA   nOperation

   DATA   cType                                   INIT ""
   DATA   cName                                   INIT ""
   DATA   cText                                   INIT ""

   DATA   nX                                      INIT 0
   DATA   nY                                      INIT 0
   DATA   nWidth                                  INIT 200
   DATA   nHeight                                 INIT 100

   DATA   qPen
   DATA   qBrush
   DATA   qBgBrush

   DATA   aPos                                    INIT {}
   DATA   aGeometry                               INIT {}

   DATA   xData                                   INIT NIL

   METHOD new( oRM, cParent, cType, cName, aPos, aGeometry )

   METHOD text( ... )                             SETGET
   METHOD pen( ... )                              SETGET
   METHOD brush( ... )                            SETGET
   METHOD bgBrush( ... )                          SETGET
   METHOD draw( ... )
   METHOD drawBarcode( qPainter, qRect )
   METHOD drawImage( qPainter, qRect )
   METHOD drawChart( qPainter, qRect )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:new( oRM, cParent, cType, cName, aPos, aGeometry )

   ::oRM       := oRM
   ::cParent   := cParent
   ::cType     := cType
   ::cName     := cName
   ::aPos      := aPos
   ::aGeometry := aGeometry



   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:text( ... )
   LOCAL a_:= hb_aParams()
   IF empty( a_ )
      RETURN ::cText
   ENDIF

   RETURN Self
/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:pen( ... )
   LOCAL a_:= hb_aParams()
   IF empty( a_ )
      RETURN ::qPen
   ENDIF

   RETURN Self
/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:brush( ... )
   LOCAL a_:= hb_aParams()
   IF empty( a_ )
      RETURN ::qBrush
   ENDIF

   RETURN Self
/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:bgBrush( ... )
   LOCAL a_:= hb_aParams()
   IF empty( a_ )
      RETURN ::qBgBrush
   ENDIF

   RETURN Self
/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:draw( ... )
   LOCAL a_:= hb_aParams()
   IF empty( a_ )
      RETURN Self
   ENDIF

   RETURN Self
/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:drawBarcode( qPainter, qRect )
   LOCAL fl, clr, rc, w, x, i, cCode

   rc    := QRectF():from( qRect:adjusted( 5, 5, -10, -10 ) )

   cCode := ::fetchBarString( "Harbour" )

   fl    := QColor():new( Qt_white )
   clr   := QColor():new( Qt_black )
   w     := rc:width() / len( cCode )
   x     := 0.0

   FOR i := 1 TO len( cCode )
      IF substr( cCode, i, 1 ) == "1"
         qPainter:fillRect_6( QRectF():new( rc:x() + x, rc:y(), w, rc:height() ), clr )
      ELSE
         qPainter:fillRect_6( QRectF():new( rc:x() + x, rc:y(), w, rc:height() ), fl )
      ENDIF
      x += w
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:drawImage( qPainter, qRect )
   LOCAL qPix, image, rc, img, point
   LOCAL drawTextType := HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE
   LOCAL paintType    := HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO
   LOCAL borderWidth  := 0
   LOCAL borderColor  := 0, pen, textH, sw, sh, cx, cy, cw, ch, textColor := 0
   LOCAL cText        := "Picture"
   LOCAL qObj         := ::oWidget

   rc    := QRectF():from( qRect:adjusted( 1, 1, -2, -2 ) )

   textH := 0
   sw    := 0
   sh    := 0

   IF ( drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE .OR.  ::drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW )
      textH = QFontMetricsF():from( qPainter:font() ):height()
   ENDIF

   qPix  := QPixmap():from( qObj:pixmap() )
   image := QImage():from( qPix:toImage() )

   IF image:isNull()
      qPainter:drawRect( qRect )
   ELSE
      img   := QImage():new( 0, 0 )
      point := QPointF():from( qRect:topLeft() )
      cx    := 0; cy := 0; cw := qPix:width(); ch := qPix:height()

      SWITCH paintType
      CASE HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO
         img := QImage():from( image:scaled_1( rc:width(), rc:height() - textH, Qt_KeepAspectRatio, Qt_SmoothTransformation ) )
         EXIT

      CASE HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_IGNORE_ASPECT_RATIO
         img := QImage():from( image:scaled_1( rc:width(), rc:height() - textH, Qt_IgnoreAspectRatio, Qt_SmoothTransformation ) )
         EXIT

      CASE HBQT_GRAPHICSITEM_CENTER_PICTURE_TO_ITEM
         point:setX( point:x() + ( rc:width() - image:width() ) / 2 )
         point:setY( point:y() + ( rc:height() - image:height() - textH ) / 2 )
         IF ( point:x() < 0 )
            cx := abs( point:x() )
            cw -= 2 * cx
            point:setX( 0 )
         ENDIF
         IF ( point:y() < 0 )
            cy = abs( point:y() )
            ch -= 2 * cy
            point:setY( 0 )
         ENDIF
         img := QImage():from( image:copy_1( cx, cy, cw, ch ) )
         EXIT

      CASE HBQT_GRAPHICSITEM_RESIZE_ITEM_TO_PICTURE
         img := image
         sw := img:width() - qObj:width()
         sh := img:height() - ( qObj:height() - textH )
         EXIT

      ENDSWITCH

      IF drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE
         point:setY( point:y() + textH )
      ENDIF

      qPainter:drawImage_2( point, img )
   ENDIF
   qPainter:setPen( QPen():new( textColor ) )

   SWITCH drawTextType

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_TOP
      qPainter:drawText( rc, Qt_AlignTop + Qt_AlignHCenter, cText )
      EXIT

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_BOTTOM
      qPainter:drawText( rc, Qt_AlignBottom + Qt_AlignHCenter, cText )
      EXIT

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE
      qPainter:drawText( rc, Qt_AlignTop + Qt_AlignHCenter, cText )
      EXIT

   CASE HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW
      qPainter:drawText( rc, Qt_AlignBottom + Qt_AlignHCenter, cText )
      EXIT

   ENDSWITCH

   IF !empty( sw ) .OR. !empty( sh )
      qObj:setWidth( qObj:width() + sw )
      qObj:setHeight( qObj:height() + sh )
   ENDIF

   IF borderWidth > 0
      pen := QPen():new()
      pen:setWidth( borderWidth )
      pen:setColor( borderColor )
      pen:setJoinStyle( Qt_MiterJoin )
      qPainter:setPen( pen )
      qPainter:setBrush( QBrush():new( Qt_NoBrush ) )
      qPainter:drawRect( rc:x() + borderWidth / 2, rc:y() + borderWidth / 2, ;
                         rc:width() - borderWidth, rc:height() - borderWidth )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HqrGraphicsItem:drawChart( qPainter, qRect )
   LOCAL qFMetrix, maxpv, minnv, absMaxVal, powVal, chartStep, powStep, maxHeight, valstep, maxLabelWidth, minpv
   LOCAL pw, rc, maxval, y, i, x, cv, barWidth, lg, py, f, cMaxVal, nDec, nFHeight
   LOCAL m_drawBorder      := .t.
   LOCAL m_showLabels      := .t.
   LOCAL m_barsIdentation  := 1
   LOCAL m_showGrid        := .t.
   LOCAL m_toColorFactor   := 1.7

   HB_SYMBOL_UNUSED( qPainter )
   HB_SYMBOL_UNUSED( qRect )

   qFMetrix := QFontMetrics():from( qPainter:fontMetrics() )
   nFHeight := qFMetrix:height()

   IF empty( ::xData )
      ::xData := {}

      aadd( ::xData, { "Bananas", 040.0, rmgr_generateNextColor() } )
      aadd( ::xData, { "Oranges", 150.0, rmgr_generateNextColor() } )
      aadd( ::xData, { "Mangoes", 120.0, rmgr_generateNextColor() } )
   ENDIF

   maxpv := 0
   minnv := 0
   aeval( ::xData, {|e_| iif( e_[ 2 ] < 0, minpv := min( minpv, e_[ 2 ] ), NIL ), iif( e_[ 2 ] < 0, maxpv := max( maxpv, e_[ 2 ] ), NIL ) } )

   absMaxVal := maxpv - minnv
   cMaxVal   := hb_ntos( absMaxVal )
   nDec      := at( ".", cMaxVal )

   // = ( absMaxVal < 1 ) ? pow( 10.0, QString::number( absMaxVal ).right( QString::number( absMaxVal ).indexOf( '.' ) ).length() + 1 ) : 1;
   powVal    := iif( absMaxVal < 1,  10.0 ^ ( len( substr( cMaxVal, nDec+1 ) ) + 1 ), 1 )
   maxpv     *= powVal
   minnv     *= powVal

   maxpv     := maxpv
   minnv     := -minnv
   minnv     := -minnv

   qPainter:fillRect( qRect, ::brush() )

   IF m_drawBorder
      qPainter:drawqRect( qRect )
   ENDIF

   pw := iif( abs( ::pen():widthF() ) > 0, abs( ::pen():widthF() ), 1 )
   rc := QRectF():from( qRect:adjusted( pw / 2, pw / 2, -pw, -pw ) )

   f  := 2
   chartStep := ( 10.0 ^ ( len( substr( cMaxVal, 1, nDec - 1 ) ) - 1 ) ) / f
   powStep   := iif( chartStep < 1, 10, 1 )
   chartStep *= powStep
   maxpv     *= powStep
   minnv     *= powStep
   powVal    *= powStep

   maxpv  := maxpv + ( iif( (   maxpv % chartStep ) != 0, ( chartStep - (   maxpv % chartStep ) ), 0 ) ) / powVal
   minnv  := minnv - ( iif( ( - minnv % chartStep ) != 0, ( chartStep - ( - minnv % chartStep ) ), 0 ) ) / powVal
   maxVal := maxpv - minnv

   maxHeight := rc:height() - nFHeight
   valstep := maxHeight / ( maxVal / chartStep )

   IF ( valstep < nFHeight )
      chartStep *= ( (  ( nFHeight / valstep ) ) + 1 )
      valstep := ( (  ( nFHeight / valstep ) ) + 1 ) * valstep
   ENDIF

   IF m_showLabels
      maxLabelWidth := 0
      FOR i := 0 TO maxVal / chartStep + 1 + iif( ( maxVal %  chartStep ) != 0, 1, 0 )
         IF ( maxLabelWidth < qFMetrix:width( hb_ntos( ( maxVal * i - chartStep * i ) / powVal ) ) )
            maxLabelWidth := qFMetrix:width( hb_ntos( ( maxVal * i - chartStep * i ) / powVal ) )
         ENDIF
      NEXT
      y := 0
      FOR i := 0 TO maxVal / chartStep + 1 + iif( ( maxVal % chartStep ) != 0, 1, 0 )
         qPainter:drawText( QRectF( rc:x(), rc:y() + y, maxLabelWidth, nFHeight ), ;
                         Qt_AlignRight + Qt_AlignVCenter, hb_ntos( ( maxpv - chartStep * i ) / powVal ) )
         y += valstep
      NEXT

      qPainter:drawLine( rc:x() + maxLabelWidth + 1 / UNIT / 4, rc:y(), rc:x() + maxLabelWidth + 1 / UNIT / 4, rc:y() + qRect:height() )
      rc := QRectF():from( rc:adjusted( maxLabelWidth + 1 / UNIT / 4, 0, 0, 0 ) )
   ENDIF

   IF ( m_showGrid )
      y :=  nFHeight / 2
      FOR i := 0 TO maxVal / chartStep + 1 + ( iif( maxVal %  chartStep !=0, 1, 0 ) )
         qPainter:drawLine( rc:x(), rc:y() + y, rc:x() + rc:width(), rc:y() + y )
         y += valstep
      NEXT
   ENDIF

   rc := rc:adjusted( 0,  nFHeight / 2, 0, 0 )
   x := m_barsIdentation
   barWidth := ( rc:width() - m_barsIdentation * ( len( ::aData ) + 1 ) ) / len( ::aData )
   py := maxHeight / maxVal

   FOR EACH cv IN ::xData
      lg := QLinearGradient():new( QPointF():new( x + barWidth / 2, 0 ), QPointF():new( x + barWidth , 0 ) )
      lg:setSpread( QGradient_ReflectSpread )
      lg:setColorAt( 0, cv[ 3 ] )
      lg:setColorAt( 1, QColor( cv[ 3 ]:red() * m_toColorFactor, cv[ 3 ]:green() * m_toColorFactor, cv[ 3 ]:blue() * m_toColorFactor, cv[ 3 ]:alpha() ) )
      qPainter:fillqRect( QRectF( rc:x() + x, rc:y() + py * maxpv - py * cv[ 2 ] * powVal, barWidth, py * cv[ 2 ] * powVal ), QBrush():new( "QGradient", lg ) )
      IF ( m_showLabels )
         qPainter:drawText( QRectF( rc:x() + x - m_barsIdentation / 2, rc:y() + py * maxpv - iif( cv[ 2 ] >= 0, nFHeight, 0 ), ;
                                      barWidth + m_barsIdentation, nFHeight ), Qt_AlignCenter, hb_ntos( cv[ 2 ] ) )
      ENDIF
      x += barWidth + m_barsIdentation
   NEXT

   #if 0  /* Legend */
   qPainter:fillqRect( qRect, ::brush() )
   qPainter:drawqRect( qRect )
   qPainter:translate( qRect:topLeft() )
   qreal y := 1 / UNIT
   qreal vstep := ( qRect:height() - y - 1 / UNIT * val:size() ) / len( ::aData )
   FOR EACH cv IN ::aData
   {
      qPainter:fillqRect( QRectF( 1 / UNIT / 2, y, m_legendColorqRectWidth, vstep ), QBrush():new( cv[ 3 ] ) )
      qPainter:drawText( QRectF( 1 / UNIT + m_legendColorqRectWidth, y, qRect:width() - ( 1 / UNIT + m_legendColorqRectWidth ), vstep ),
                                                                            Qt_AlignVCenter + Qt_AlignLeft, cv[ 1 ] )
      y += vstep + 1 / UNIT
   }
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

