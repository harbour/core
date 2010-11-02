/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QGraphicsScene( ... )
   RETURN HB_QGraphicsScene():new( ... )

FUNCTION QGraphicsSceneFromPointer( ... )
   RETURN HB_QGraphicsScene():fromPointer( ... )


CREATE CLASS QGraphicsScene INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QGraphicsScene

   METHOD  new( ... )

   METHOD  activeWindow                  // (  )                                               -> oQGraphicsWidget
   METHOD  addEllipse                    // ( oQRectF, oQPen, oQBrush )                        -> oQGraphicsEllipseItem
                                         // ( nX, nY, nW, nH, oQPen, oQBrush )                 -> oQGraphicsEllipseItem
   METHOD  addItem                       // ( oQGraphicsItem )                                 -> NIL
   METHOD  addLine                       // ( oQLineF, oQPen )                                 -> oQGraphicsLineItem
                                         // ( nX1, nY1, nX2, nY2, oQPen )                      -> oQGraphicsLineItem
   METHOD  addPath                       // ( oQPainterPath, oQPen, oQBrush )                  -> oQGraphicsPathItem
   METHOD  addPixmap                     // ( oQPixmap )                                       -> oQGraphicsPixmapItem
   METHOD  addPolygon                    // ( oQPolygonF, oQPen, oQBrush )                     -> oQGraphicsPolygonItem
   METHOD  addRect                       // ( oQRectF, oQPen, oQBrush )                        -> oQGraphicsRectItem
                                         // ( nX, nY, nW, nH, oQPen, oQBrush )                 -> oQGraphicsRectItem
   METHOD  addSimpleText                 // ( cText, oQFont )                                  -> oQGraphicsSimpleTextItem
   METHOD  addText                       // ( cText, oQFont )                                  -> oQGraphicsTextItem
   METHOD  addWidget                     // ( oQWidget, nWFlags )                              -> oQGraphicsProxyWidget
   METHOD  backgroundBrush               // (  )                                               -> oQBrush
   METHOD  bspTreeDepth                  // (  )                                               -> nInt
   METHOD  clearFocus                    // (  )                                               -> NIL
   METHOD  collidingItems                // ( oQGraphicsItem, nMode )                          -> oQList_QGraphicsItem
   METHOD  destroyItemGroup              // ( oQGraphicsItemGroup )                            -> NIL
   METHOD  focusItem                     // (  )                                               -> oQGraphicsItem
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  foregroundBrush               // (  )                                               -> oQBrush
   METHOD  hasFocus                      // (  )                                               -> lBool
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  invalidate                    // ( nX, nY, nW, nH, nLayers )                        -> NIL
   METHOD  isSortCacheEnabled            // (  )                                               -> lBool
   METHOD  itemAt                        // ( oQPointF )                                       -> oQGraphicsItem
                                         // ( nX, nY )                                         -> oQGraphicsItem
   METHOD  itemIndexMethod               // (  )                                               -> nItemIndexMethod
   METHOD  items                         // (  )                                               -> oQList_QGraphicsItem
                                         // ( oQPointF )                                       -> oQList_QGraphicsItem
                                         // ( nX, nY, nW, nH, nMode )                          -> oQList_QGraphicsItem
                                         // ( oQRectF, nMode )                                 -> oQList_QGraphicsItem
                                         // ( oQPolygonF, nMode )                              -> oQList_QGraphicsItem
                                         // ( oQPainterPath, nMode )                           -> oQList_QGraphicsItem
   METHOD  itemsBoundingRect             // (  )                                               -> oQRectF
   METHOD  mouseGrabberItem              // (  )                                               -> oQGraphicsItem
   METHOD  palette                       // (  )                                               -> oQPalette
   METHOD  removeItem                    // ( oQGraphicsItem )                                 -> NIL
   METHOD  render                        // ( oQPainter, oQRectF, oQRectF, nAspectRatioMode )  -> NIL
   METHOD  sceneRect                     // (  )                                               -> oQRectF
   METHOD  selectedItems                 // (  )                                               -> oQList_QGraphicsItem
   METHOD  selectionArea                 // (  )                                               -> oQPainterPath
   METHOD  setActiveWindow               // ( oQGraphicsWidget )                               -> NIL
   METHOD  setBackgroundBrush            // ( oQBrush )                                        -> NIL
   METHOD  setBspTreeDepth               // ( nDepth )                                         -> NIL
   METHOD  setFocus                      // ( nFocusReason )                                   -> NIL
   METHOD  setFocusItem                  // ( oQGraphicsItem, nFocusReason )                   -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setForegroundBrush            // ( oQBrush )                                        -> NIL
   METHOD  setItemIndexMethod            // ( nMethod )                                        -> NIL
   METHOD  setPalette                    // ( oQPalette )                                      -> NIL
   METHOD  setSceneRect                  // ( oQRectF )                                        -> NIL
                                         // ( nX, nY, nW, nH )                                 -> NIL
   METHOD  setSelectionArea              // ( oQPainterPath )                                  -> NIL
                                         // ( oQPainterPath, nMode )                           -> NIL
   METHOD  setSortCacheEnabled           // ( lEnabled )                                       -> NIL
   METHOD  setStickyFocus                // ( lEnabled )                                       -> NIL
   METHOD  setStyle                      // ( oQStyle )                                        -> NIL
   METHOD  stickyFocus                   // (  )                                               -> lBool
   METHOD  style                         // (  )                                               -> oQStyle
   METHOD  update                        // ( nX, nY, nW, nH )                                 -> NIL
   METHOD  views                         // (  )                                               -> oQList_QGraphicsView
   METHOD  width                         // (  )                                               -> nQreal
   METHOD  advance                       // (  )                                               -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  clearSelection                // (  )                                               -> NIL
                                         // ( oQRectF, nLayers )                               -> NIL
                                         // ( oQRectF )                                        -> NIL

   ENDCLASS


METHOD QGraphicsScene:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsScene( ... )
   RETURN Self


METHOD QGraphicsScene:activeWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsWidgetFromPointer( Qt_QGraphicsScene_activeWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addEllipse( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isObject( hb_pvalue( 6 ) )
         RETURN QGraphicsEllipseItemFromPointer( Qt_QGraphicsScene_addEllipse_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN QGraphicsEllipseItemFromPointer( Qt_QGraphicsScene_addEllipse_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QGraphicsEllipseItemFromPointer( Qt_QGraphicsScene_addEllipse_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QGraphicsEllipseItemFromPointer( Qt_QGraphicsScene_addEllipse( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QGraphicsEllipseItemFromPointer( Qt_QGraphicsScene_addEllipse( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsEllipseItemFromPointer( Qt_QGraphicsScene_addEllipse( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addLine( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN QGraphicsLineItemFromPointer( Qt_QGraphicsScene_addLine_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QGraphicsLineItemFromPointer( Qt_QGraphicsScene_addLine_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QGraphicsLineItemFromPointer( Qt_QGraphicsScene_addLine( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsLineItemFromPointer( Qt_QGraphicsScene_addLine( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addPath( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QGraphicsPathItemFromPointer( Qt_QGraphicsScene_addPath( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QGraphicsPathItemFromPointer( Qt_QGraphicsScene_addPath( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsPathItemFromPointer( Qt_QGraphicsScene_addPath( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsPixmapItemFromPointer( Qt_QGraphicsScene_addPixmap( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addPolygon( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QGraphicsPolygonItemFromPointer( Qt_QGraphicsScene_addPolygon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QGraphicsPolygonItemFromPointer( Qt_QGraphicsScene_addPolygon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsPolygonItemFromPointer( Qt_QGraphicsScene_addPolygon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addRect( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isObject( hb_pvalue( 6 ) )
         RETURN QGraphicsRectItemFromPointer( Qt_QGraphicsScene_addRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN QGraphicsRectItemFromPointer( Qt_QGraphicsScene_addRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QGraphicsRectItemFromPointer( Qt_QGraphicsScene_addRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QGraphicsRectItemFromPointer( Qt_QGraphicsScene_addRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QGraphicsRectItemFromPointer( Qt_QGraphicsScene_addRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsRectItemFromPointer( Qt_QGraphicsScene_addRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addSimpleText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QGraphicsSimpleTextItemFromPointer( Qt_QGraphicsScene_addSimpleText( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QGraphicsSimpleTextItemFromPointer( Qt_QGraphicsScene_addSimpleText( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QGraphicsTextItemFromPointer( Qt_QGraphicsScene_addText( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QGraphicsTextItemFromPointer( Qt_QGraphicsScene_addText( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:addWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QGraphicsProxyWidgetFromPointer( Qt_QGraphicsScene_addWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsProxyWidgetFromPointer( Qt_QGraphicsScene_addWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:backgroundBrush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QGraphicsScene_backgroundBrush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:bspTreeDepth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_bspTreeDepth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:clearFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_clearFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:collidingItems( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QGraphicsScene_collidingItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QGraphicsScene_collidingItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:destroyItemGroup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_destroyItemGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:focusItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsItemFromPointer( Qt_QGraphicsScene_focusItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QGraphicsScene_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:foregroundBrush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QGraphicsScene_foregroundBrush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:hasFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_hasFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:invalidate( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGraphicsScene_invalidate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsScene_invalidate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsScene_invalidate_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:isSortCacheEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_isSortCacheEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QGraphicsItemFromPointer( Qt_QGraphicsScene_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsItemFromPointer( Qt_QGraphicsScene_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:itemIndexMethod( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_itemIndexMethod( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:items( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QListFromPointer( Qt_QGraphicsScene_items_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QListFromPointer( Qt_QGraphicsScene_items_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGONF"
            RETURN QListFromPointer( Qt_QGraphicsScene_items_4( ::pPtr, ... ) )
         CASE "QRECTF"
            RETURN QListFromPointer( Qt_QGraphicsScene_items_3( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN QListFromPointer( Qt_QGraphicsScene_items_5( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN QListFromPointer( Qt_QGraphicsScene_items_3( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN QListFromPointer( Qt_QGraphicsScene_items_4( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN QListFromPointer( Qt_QGraphicsScene_items_1( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN QListFromPointer( Qt_QGraphicsScene_items_5( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QListFromPointer( Qt_QGraphicsScene_items( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:itemsBoundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsScene_itemsBoundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:mouseGrabberItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsItemFromPointer( Qt_QGraphicsScene_mouseGrabberItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:palette( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaletteFromPointer( Qt_QGraphicsScene_palette( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:removeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_removeItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:render( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsScene_render( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsScene_render( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsScene_render( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_render( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:sceneRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsScene_sceneRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:selectedItems( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QGraphicsScene_selectedItems( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:selectionArea( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPainterPathFromPointer( Qt_QGraphicsScene_selectionArea( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setActiveWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setActiveWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setBackgroundBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setBackgroundBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setBspTreeDepth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setBspTreeDepth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setFocus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setFocus( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsScene_setFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setFocusItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsScene_setFocusItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setFocusItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setForegroundBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setForegroundBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setItemIndexMethod( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setItemIndexMethod( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setPalette( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setPalette( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setSceneRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsScene_setSceneRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setSceneRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setSelectionArea( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsScene_setSelectionArea_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setSelectionArea( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setSortCacheEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setSortCacheEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setStickyFocus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setStickyFocus( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:stickyFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_stickyFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStyleFromPointer( Qt_QGraphicsScene_style( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:update( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsScene_update( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsScene_update_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QGraphicsScene_update_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:views( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QGraphicsScene_views( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:advance( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_advance( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsScene:clearSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsScene_clearSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

