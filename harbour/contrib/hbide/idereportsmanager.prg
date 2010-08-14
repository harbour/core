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

/*----------------------------------------------------------------------*/

CLASS IdeReportsManager INHERIT IdeObject

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


   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( cEvent, p )
   METHOD buildToolbar()
   METHOD buildToolbarAlign()
   METHOD buildToolbarLeft()
   METHOD buildStacks()
   METHOD buildStatusBar()
   METHOD buildTabBar()
   METHOD buildDesignReport()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:create( oIde )
   LOCAL qDock

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   qDock := ::oIde:oReportsManagerDock:oWidget

   ::oWidget := QWidget():new()

   qDock:setWidget( ::oWidget )

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

METHOD IdeReportsManager:destroy()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:show()
   ::oReportsManagerDock:raise()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:execEvent( cEvent, p )
   LOCAL qEvent, qMime

   SWITCH cEvent
   CASE "tabBar_currentChanged"
      IF !empty( ::qStack ) .AND. p < ::qStack:count()
         ::qStack:setCurrentIndex( p )
      ENDIF
      EXIT
   CASE "buttonNew_clicked"
      EXIT
   CASE "buttonOpen_clicked"
      EXIT
   CASE "buttonSave_clicked"
      EXIT
   CASE "buttonClose_clicked"
      EXIT
   CASE "buttonPrint_clicked"
      EXIT
   CASE "viewWidget_dropEvent"
      qEvent := QDropEvent():from( p )
      qMime := QMimeData():from( qEvent:mimeData() )
HB_TRACE( HB_TR_ALWAYS,  "viewWidget_dropEvent", qMime:hasText(), qMime:hasImage(), qMime:hasColor() )
      EXIT
   CASE "viewWidget_dragEnterEvent"
      qEvent := QDragEnterEvent():from( p )
      qEvent:acceptProposedAction()
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:buildDesignReport()
   STATIC qItem

   ::qLayoutD := QHBoxLayout():new()
   ::qLayoutD:setContentsMargins( 0,0,0,0 )
   ::qLayoutD:setSpacing( 1 )
   ::qWidget3:setLayout( ::qLayoutD )

   ::qSpliter := QSplitter():new()
   ::qSpliter:setOrientation( Qt_Horizontal )

   ::qLayoutD:addWidget( ::qSpliter )

   ::qFrameL := QFrame():new()
   ::qSpliter:addWidget( ::qFrameL )

   ::qScroll := QScrollArea():new()
   ::qScroll:setVerticalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::qScroll:setHorizontalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::qScroll:setWidgetResizable( .f. )
   ::qScroll:setMinimumWidth( 400 )
   ::qScroll:setBackgroundRole( QPalette_Dark )
   ::qSpliter:addWidget( ::qScroll )

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
   //
   qItem := QTreeWidgetItem():new()
   qItem:setText( 0, "Rect" )
   ::qTreeData:addTopLevelItem( qItem )
   qItem := QTreeWidgetItem():new()
   qItem:setText( 0, "Circle" )
   //
   ::qTreeData:addTopLevelItem( qItem )
   //
   ::qTreeData:setDragEnabled( .t. )

   ::qDesign := QFrame():new()
   ::qScroll:setWidget( ::qDesign )
   ::qDesign:setBackgroundRole( QPalette_Dark )
   ::qDesign:setGeometry( QRect():new( 0, 0, 600, 900 ) )

   ::qHRuler := QFrame():new( ::qDesign )
   ::qHRuler:setGeometry( QRect():new( 30, 0, ::qDesign:width(), 15 ) )
   ::qHRuler:setStyleSheet( "background-color: rgb(240,240,240);" )
   ::qVRuler := QFrame():new( ::qDesign )
   ::qVRuler:setStyleSheet( "background-color: rgb(240,240,240);" )
   ::qVRuler:setGeometry( QRect():new( 0, 30, 15, ::qDesign:height() ) )
#if 0
   ::qPort := QFrame():new( ::qDesign )
   ::qPort:setStyleSheet( "background-color: white;" )
   ::qPort:setGeometry( QRect():new( 30, 30, ::qDesign:width() - 45, ::qDesign:height() - 45 ) )
#else
   ::qView := QGraphicsView():new( ::qDesign )
   ::qView:setGeometry( QRect():new( 30, 30, ::qDesign:width() - 45, ::qDesign:height() - 45 ) )

   ::qScene := QGraphicsScene():new()
   ::qView:setScene( ::qScene )
   ::qScene:setSceneRect_1( 10, 10, 200, 400 )
#if 1
   ::qView:setAcceptDrops( .t. )
   ::qView:installEventFilter( ::pEvents )
   //::connect( ::qView, QEvent_DragEnter, {|p| ::execEvent( "viewWidget_dragEnterEvent", p ) } )
   ::connect( ::qView, QEvent_GraphicsSceneDragEnter, {|p| ::execEvent( "viewWidget_dragEnterEvent", p ) } )
   //::connect( ::qView, QEvent_Drop     , {|p| ::execEvent( "viewWidget_dropEvent"     , p ) } )
   ::connect( ::qView, QEvent_GraphicsSceneDrop     , {|p| ::execEvent( "viewWidget_dropEvent"     , p ) } )
#else
   ::qScene:installEventFilter( ::pEvents )
   ::connect( ::qScene, QEvent_GraphicsSceneDragEnter, {|p| ::execEvent( "viewWidget_dragEnterEvent", p ) } )
   ::connect( ::qScene, QEvent_Drop     , {|p| ::execEvent( "viewWidget_dropEvent"     , p ) } )
#endif
   aadd( ::aItems, IdeGraphicsItem():new( "Rect_1", "rect", ::qScene, { 10.0, 10.0, 200.0, 200.0 }, /*qPen*/, /*qBrush*/ ) )
#endif

   ::qWidget1:show()
   ::qWidget2:show()
   ::qWidget3:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:buildTabBar()

   ::qTabBar := QTabBar():new()
   //::qTabBar:setDocumentMode( .t. )
   ::qTabBar:setShape( QTabBar_TriangularNorth )

   ::qTabBar:addTab( "Code"    )
   ::qTabBar:addTab( "Dialogs" )
   ::qTabBar:addTab( "Page1"   )

   ::connect( ::qTabBar, "currentChanged(int)", {|p| ::execEvent( "tabBar_currentChanged", p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:buildStacks()

   ::qStack := QStackedWidget():new()

   ::qWidget1 := QWidget():new()
   ::qStack:addWidget( ::qWidget1 )

   ::qWidget2 := QWidget():new()
   ::qStack:addWidget( ::qWidget2 )

   ::qWidget3 := QWidget():new()
   ::qStack:addWidget( ::qWidget3 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:buildToolbar()

   ::qToolbar := IdeToolbar():new()
   ::qToolbar:orientation := Qt_Horizontal
   ::qToolbar:create( "ReportManager_Top_Toolbar" )

   ::qToolbar:addToolButton( "New"  , "New Report"  , hbide_image( "new"    ), {|| ::execEvent( "buttonNew_clicked"   ) } )
   ::qToolbar:addToolButton( "Open" , "Open Report" , hbide_image( "open3"  ), {|| ::execEvent( "buttonOpen_clicked"  ) } )
   ::qToolbar:addToolButton( "Save" , "Save Report" , hbide_image( "save3"  ), {|| ::execEvent( "buttonSave_clicked"  ) } )
   ::qToolbar:addToolButton( "Close", "Close Report", hbide_image( "close3" ), {|| ::execEvent( "buttonClose_clicked" ) } )
   ::qToolbar:addToolButton( "Print", "Print Report", hbide_image( "print"  ), {|| ::execEvent( "buttonPrint_clicked" ) } )
   ::qToolbar:addSeparator()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:buildToolbarAlign()

   ::qToolbarAlign := IdeToolbar():new()
   ::qToolbarAlign:orientation := Qt_Horizontal
   ::qToolbarAlign:create( "ReportManager_Top_Toolbar_Align" )

   ::qToolbarAlign:addToolButton( "FontG"  , "Font", hbide_image( "f-generic"       ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "FontB"  , "Text Bold"         , hbide_image( "f-bold-1"        ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "FontI"  , "Text Italic"       , hbide_image( "f-italic-1"      ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "FontU"  , "Text Underlined"   , hbide_image( "f-underline-1"   ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "FontS"  , "Text Strikethrough", hbide_image( "f-strike-1"      ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "JustL"  , "Align left"        , hbide_image( "f_align_left"    ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustC"  , "Align center"      , hbide_image( "f_align_center"  ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustR"  , "Align right"       , hbide_image( "f_align_right"   ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustJ"  , "Align justify"     , hbide_image( "f_align_justify" ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "JustT"  , "Align top"         , hbide_image( "f_align_top"     ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustM"  , "Align middle"      , hbide_image( "f_align_middle"  ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "JustB"  , "Align bottom"      , hbide_image( "f_align_bottom"  ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "BoxT"   , "Box-frame top"     , hbide_image( "f_box_top"       ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "BoxL"   , "Box-frame left"    , hbide_image( "f_box_left"      ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "BoxB"   , "Box-frame bottom"  , hbide_image( "f_box_bottom"    ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "BoxR"   , "Box-frame right"   , hbide_image( "f_box_right"     ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()
   ::qToolbarAlign:addToolButton( "BoxA"   , "Box-frame all"     , hbide_image( "f_box_all"       ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "BoxP"   , "No box-frame"      , hbide_image( "f_box_plain"     ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addToolButton( "BoxS"   , "Box shadowed"      , hbide_image( "f_box_shadow"    ), {|| ::execEvent( "button_clicked" ) } )
   ::qToolbarAlign:addSeparator()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:buildToolbarLeft()

   ::qToolbarL := IdeToolbar():new()
   ::qToolbarL:orientation := Qt_Vertical
   ::qToolbarL:create( "ReportManager_Left_Toolbar" )

   ::qToolbarL:addToolButton( "Image"   , "Image"   , hbide_image( "f-image"    ), {|| ::execEvent( "buttonNew_clicked"   ) } )
   ::qToolbarL:addToolButton( "Chart"   , "Chart"   , hbide_image( "f_chart"    ), {|| ::execEvent( "buttonNew_clicked"   ) } )
   ::qToolbarL:addToolButton( "Gradient", "Gradient", hbide_image( "f_gradient" ), {|| ::execEvent( "buttonNew_clicked"   ) } )
   ::qToolbarL:addToolButton( "Barcode" , "Barcode" , hbide_image( "f_barcode"  ), {|| ::execEvent( "buttonNew_clicked"   ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeReportsManager:buildStatusBar()
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
//
//                      Class IdeReportItem()
//
/*----------------------------------------------------------------------*/

CLASS IdeGraphicsItem INHERIT IdeObject

   DATA   oWidget

   DATA   cName                                   INIT ""
   DATA   cType
   DATA   qScene
   DATA   qPen
   DATA   qBrush
   DATA   aRect                                   //INIT { 20,20,60,20 }
   DATA   isMovable                               INIT .t.
   DATA   isSelectable                            INIT .f.
   DATA   isFocusable                             INIT .f.

   METHOD new( cName, cType, qScene, aRect, qPen, qBrush )
   METHOD execEvent( cEvent, p )

   ERROR  HANDLER onError( ... )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeGraphicsItem:new( cName, cType, qScene, aRect, qPen, qBrush )

   ::cName  := cName
   ::cType  := lower( cType )
   ::qScene := qScene

   SWITCH ::cType
   CASE "band"
      EXIT
   CASE "rect"
      DEFAULT aRect  TO { 20, 20, 100, 100 }
      DEFAULT qPen   TO QPen():new( "QColor", QColor():new( 0,0,255 ) )
      DEFAULT qBrush TO QBrush():new( Qt_yellow )

      ::qPen   := qPen
      ::qBrush := qBrush

      ::oWidget := QGraphicsItem():from( ::qScene:addRect_1( aRect[ 1 ], aRect[ 2 ], aRect[ 3 ], aRect[ 4 ], qPen, qBrush ) )

      ::oWidget:setFlag( QGraphicsItem_ItemIsMovable, .t. )
      ::oWidget:setAcceptDrops( .t. )
      ::oWidget:setAcceptHoverEvents( .t. )

      //::oWidget:installEventFilter( ::pEvents )
      //::connect( ::oWidget, QEvent_GraphicsSceneDragEnter, {|p| ::execEvent( "viewWidget_dragEnterEvent", p ) } )

      EXIT
   CASE "field"
      EXIT
   CASE "barcode"
      EXIT
   CASE "image"
      EXIT
   CASE "textbox"
      EXIT
   CASE "textline"
      EXIT
   CASE "gradient"
      EXIT
   CASE "chart"
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeGraphicsItem:execEvent( cEvent, p )
   HB_SYMBOL_UNUSED( p )
   SWITCH cEvent
   CASE "viewWidget_dragEnterEvent"
HB_TRACE( HB_TR_ALWAYS, "viewWidget_dragEnterEvent", "........." )
      EXIT
   ENDSWITCH
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeGraphicsItem:onError( ... )
   LOCAL cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF
   RETURN ::oWidget:&cMsg( ... )

/*----------------------------------------------------------------------*/

