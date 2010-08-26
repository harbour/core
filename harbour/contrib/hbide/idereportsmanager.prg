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

/*----------------------------------------------------------------------*/

CLASS HbpReportsManager

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

   DATA   symposis                                INIT "hbReports Designer"
   DATA   version                                 INIT 0.1
   DATA   author                                  INIT ""
   DATA   dateCreated                             INIT date()
   DATA   dateModified                            INIT date()
   DATA   aPages                                  INIT {}
   DATA   aSources                                INIT {}
   DATA   aObjects                                INIT {}

   DATA   hDoc                                    INIT {=>}
   DATA   lNew                                    INIT .t.
   DATA   cSaved                                  INIT ""

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
   METHOD addField( qPos, cAlias, cField )
   METHOD addObject( qPos, cType )
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

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:new( qParent )
   ::qParent := qParent
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:create( qParent )

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

METHOD HbpReportsManager:destroy()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:buildDesignReport()

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

   ::qScene:zoomWYSIWYG()
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

METHOD HbpReportsManager:clear()

   /* Cleanup the environments */

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:loadReport( xData )

   ::clear()

   IF empty( xData )
      IF empty( ::aPages )
         aadd( ::aPages, { "Page_1" } )
      ENDIF

      ::updateObjectsTree( "ReportName", NIL, "Report" )
      ::updateObjectsTree( "Page", "Report", "Page_1" )

      ::addSource( "Customer", { { "Title" ,"C",35,0 }, { "Street","C",20,0 }, { "Revenue","N",12,2 } } )
      ::addSource( "Invoice" , { { "Number","C",10,0 }, { "Date"  ,"D",08,0 }, { "Amount" ,"N",12,2 } } )

   ELSE
      // read from - buffer, file

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:addSource( cAlias, aStruct )
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

#define INI_KEY( cKey, n )    cKey + "_" + hb_ntos( n ) + "="

STATIC FUNCTION rmgr_iniKey( cKey, n )
   RETURN cKey + "_" + hb_ntos( n ) + "="

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
         s += '"' + x + '"'
         EXIT
      CASE "N"
         s += hb_ntos( x )
         EXIT
      CASE "D"
         s += "stod(" + dtos( x ) + ")"
         EXIT
      CASE "L"
         s += iif( x, ".t.", ".f." )
         EXIT
      CASE "A"
         s += rmgr_a2arrayStr( x )
         EXIT
      OTHERWISE
         s += "NIL"
      ENDSWITCH
      s += ","
   NEXT
   s := iif( len( s ) == 1, s, substr( s, 1, len( s ) - 1 ) ) + "}"

   RETURN s

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:saveReport( lSaveAs )
   LOCAL cFile, cBuffer, qFileDlg, qList, cExt
   LOCAL lSave := .t.

   DEFAULT lSaveAs TO .f.

   IF lSaveAs .OR. ::lNew .OR. empty( ::cSaved )
      qFileDlg := QFileDialog():new( ::oWidget )

      qFileDlg:setAcceptMode( QFileDialog_AcceptSave )
      qFileDlg:setFileMode( QFileDialog_AnyFile )
      qFileDlg:setViewMode( QFileDialog_List )
      qFileDlg:setNameFilter( "HB Reports (*.hrp)" )

      IF qFileDlg:exec() == 1
         qList := QStringList():from( qFileDlg:selectedFiles() )
         cFile := qList:at( 0 )
         hb_fNameSplit( cFile, , , @cExt )
         IF empty( cExt )
            cFile += ".hrp"
         ENDIF

         ::cSaved := cFile
      ELSE
         lSave := .f.
      ENDIF
   ENDIF

   IF lSave .AND. !empty( ::cSaved )
      cBuffer  := ::buildReportStream()
      hb_memowrit( ::cSaved, cBuffer )

      RETURN hb_fileExists( ::cSaved )
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:toString()

   RETURN ::buildReportStream()

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:buildReportStream()
   LOCAL txt_:= {}, n, a_, s

   aadd( txt_, "[GENERAL]" )
   aadd( txt_, "" )
   aadd( txt_, "Symposis"     + "=" + "HBReportDesigner - (C) Harbour-Project <http://harbour-project.org>  (C) 2010 Pritpal Bedi <bedipritpal@hotmail.com>" )
   aadd( txt_, "Version"      + "=" + "0.1"            )
   aadd( txt_, "Title"        + "=" + "Report"         )
   aadd( txt_, "Author"       + "=" + "hbIDE"          )
   aadd( txt_, "DateCreated"  + "=" + dtos( date() )   )
   aadd( txt_, "DateModified" + "=" + dtos( date() )   )
   aadd( txt_, "Properties"   + "=" + ""               )
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
      aadd( txt_, INI_KEY( "object", n ) + rmgr_a2arrayStr( a_ ) )
   NEXT
   aadd( txt_, "" )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 ) + chr( 10 ) } )

   RETURN s

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:prepareReport()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:execEvent( cEvent, p, p1, p2 )
   LOCAL qEvent, qMime, qItem, i, qList, cFile, nArea, aStruct, cAlias, cPath

   SWITCH cEvent
   CASE "graphicsScene_block"
      qEvent := QGraphicsSceneDragDropEvent():from( p1 )

      DO CASE
      CASE p == 21105    // Context Menu Event
         ::contextMenuScene( p1 )

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
                  ::addField( QPoint():from( qEvent:scenePos() ), p2[ 2 ], p2[ 3 ] )
               ENDIF
            ENDIF

         ELSEIF qMime:hasFormat( "application/x-toolbaricon"  )
            ::addObject( QPoint():from( qEvent:scenePos() ), qMime:html() )

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
      CASE p == 21105 // Context Menu Event
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
      EXIT
   CASE "buttonSave_clicked"
      ::saveReport()
      EXIT
   CASE "buttonClose_clicked"
      EXIT
   CASE "buttonPrint_clicked"
      EXIT
   CASE "buttonGrid_clicked"
      ::qScene:setShowGrid( ::qToolbarAlign:setItemChecked( "Grid" ) )
      EXIT
   CASE "buttonZoom_clicked"
      DO CASE
      CASE p == 1
         ::qScene:zoomIn()
      CASE p == 2
         ::qScene:zoomOut()
      CASE p == 3
         ::qScene:zoomWYSIWYG()
      CASE p == 4
         ::qScene:zoomOriginal()
      ENDCASE
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:addObject( qPos, cType )
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

   oWidget:setGeometry( QRectF():new( 0, 0, nW, nH ) )
   IF !empty( qPos )
      oWidget:setPos( qPos )
   ENDIF
   ::hItems[ cName ] := oWidget
   ::updateObjectsTree( "Object", "Page_1", cName, cType )
   aadd( ::aObjects, { "Object", "Page_1", cName, cType, {} } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:addField( qPos, cAlias, cField )
   LOCAL oWidget, nW := 300, nH := 50
   LOCAL cName := cAlias + "..." + cField

   cName := cName + "_" + hb_ntos( ::getNextID( cName ) )

   oWidget := HBQGraphicsItem():new( HBQT_GRAPHICSITEM_SIMPLETEXT )
   oWidget:setText( cName )
   oWidget:hbSetBlock( {|p,p1,p2| ::execEvent( "graphicsItem_block", p, p1, p2 ) } )
   oWidget:setGeometry( QRectF():new( 0, 0, nW, nH ) )
   oWidget:setTooltip( cName )
   oWidget:setObjectType( "Field" )
   oWidget:setObjectName( cName )

   ::qScene:addItem( oWidget )

   IF !empty( qPos )
      oWidget:setPos( qPos )
   ENDIF
   ::hItems[ cName ] := oWidget
   ::updateObjectsTree( "Field", "Page_1", cName, NIL )
   aadd( ::aObjects, { "Field", "Page_1", cName, NIL, {} } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:updateObjectsTree( cType, cParent, cName, cSubType )
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

METHOD HbpReportsManager:contextMenuScene( p1 )
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

METHOD HbpReportsManager:contextMenuItem( p1, p2 )
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

METHOD HbpReportsManager:buildTabBar()

   ::qTabBar := QTabBar():new()
   ::qTabBar:setShape( QTabBar_TriangularNorth )

   ::qTabBar:addTab( "Code"    )
   ::qTabBar:addTab( "Dialogs" )
   ::qTabBar:addTab( "Page_1"  )

   ::qTabBar:connect( "currentChanged(int)", {|p| ::execEvent( "tabBar_currentChanged", p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:buildStacks()

   ::qStack := QStackedWidget():new()

   ::qWidget1 := QWidget():new()
   ::qStack:addWidget( ::qWidget1 )

   ::qWidget2 := QWidget():new()
   ::qStack:addWidget( ::qWidget2 )

   ::qWidget3 := QWidget():new()
   ::qStack:addWidget( ::qWidget3 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:buildStatusBar()
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

METHOD HbpReportsManager:getImageOfType( cType )
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

METHOD HbpReportsManager:getNextID( cType )

   STATIC hIDs := {=>}

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

METHOD HbpReportsManager:fetchBarString( cCode, lCheck )
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

METHOD HbpReportsManager:buildToolbar()

   ::qToolbar := IdeToolbar():new()
   ::qToolbar:orientation := Qt_Horizontal
   ::qToolbar:create( "ReportManager_Top_Toolbar" )

   ::qToolbar:addToolButton( "New"    , "New Report"    , app_image( "new"      ), {|| ::execEvent( "buttonNew_clicked"     ) } )
   ::qToolbar:addToolButton( "Open"   , "Open Report"   , app_image( "open3"    ), {|| ::execEvent( "buttonOpen_clicked"    ) } )
   ::qToolbar:addToolButton( "Save"   , "Save Report"   , app_image( "save3"    ), {|| ::execEvent( "buttonSave_clicked"    ) } )
   ::qToolbar:addToolButton( "Close"  , "Close Report"  , app_image( "close3"   ), {|| ::execEvent( "buttonClose_clicked"   ) } )
   ::qToolbar:addToolButton( "Print"  , "Print Report"  , app_image( "print"    ), {|| ::execEvent( "buttonPrint_clicked"   ) } )
   ::qToolbar:addSeparator()
   ::qToolbar:addToolButton( "ToBack" , "Push to back"  , app_image( "toback"   ), {|| ::execEvent( "buttonToBack_clicked"  ) }, .f., .f. )
   ::qToolbar:addToolButton( "ToFront", "Bring to front", app_image( "tofront"  ), {|| ::execEvent( "buttonToFront_clicked" ) }, .f., .f. )
   ::qToolbar:addSeparator()
   ::qToolbar:addToolButton( "RotateL", "Rotate anti-clock wise", app_image( "unload_1" ), {|| ::execEvent( "buttonRotateL_clicked" ) }, .f., .f. )
   ::qToolbar:addToolButton( "RotateR", "Rotate clock wise"     , app_image( "load_1"   ), {|| ::execEvent( "buttonRotateR_clicked" ) }, .f., .f. )
   ::qToolbar:addSeparator()
   ::qToolbar:addToolButton( "Portrait" , "Portrait orientation" , app_image( "r-portrait"  ), {|| ::execEvent( "buttonPortrait_clicked" ) }, .f., .f. )
   ::qToolbar:addToolButton( "Landscape", "Landscape orientation", app_image( "r-landscape" ), {|| ::execEvent( "buttonLandscape_clicked" ) }, .f., .f. )
   ::qToolbar:addSeparator()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:buildToolbarAlign()

   ::qToolbarAlign := IdeToolbar():new()
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

   ::qToolbarAlign:setItemChecked( "Grid", .t. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpReportsManager:buildToolbarLeft()

   ::qToolbarL := IdeToolbar():new()
   ::qToolbarL:orientation := Qt_Vertical
   ::qToolbarL:create( "ReportManager_Left_Toolbar" )

   ::qToolbarL:addToolButton( "Image"   , "Image"   , app_image( "f-image"    ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Chart"   , "Chart"   , app_image( "f_chart"    ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Gradient", "Gradient", app_image( "f_gradient" ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Barcode" , "Barcode" , app_image( "f_barcode"  ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )
   ::qToolbarL:addToolButton( "Text"    , "Text"    , app_image( "text"       ), {|| ::execEvent( "buttonNew_clicked"   ) }, .t., .t. )

   RETURN Self

/*----------------------------------------------------------------------*/

