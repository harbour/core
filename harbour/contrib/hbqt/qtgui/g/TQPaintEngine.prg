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


FUNCTION QPaintEngine( ... )
   RETURN HB_QPaintEngine():new( ... )

FUNCTION QPaintEngineFromPointer( ... )
   RETURN HB_QPaintEngine():fromPointer( ... )


CREATE CLASS QPaintEngine INHERIT HbQtObjectHandler FUNCTION HB_QPaintEngine

   METHOD  new( ... )

   METHOD  begin                         // ( oQPaintDevice )                                  -> lBool
   METHOD  drawEllipse                   // ( oQRectF )                                        -> NIL
                                         // ( oQRect )                                         -> NIL
   METHOD  drawImage                     // ( oQRectF, oQImage, oQRectF, nFlags )              -> NIL
   METHOD  drawLines                     // ( oQLineF, nLineCount )                            -> NIL
                                         // ( oQLine, nLineCount )                             -> NIL
   METHOD  drawPath                      // ( oQPainterPath )                                  -> NIL
   METHOD  drawPixmap                    // ( oQRectF, oQPixmap, oQRectF )                     -> NIL
   METHOD  drawPoints                    // ( oQPointF, nPointCount )                          -> NIL
                                         // ( oQPoint, nPointCount )                           -> NIL
   METHOD  drawPolygon                   // ( oQPointF, nPointCount, nMode )                   -> NIL
                                         // ( oQPoint, nPointCount, nMode )                    -> NIL
   METHOD  drawRects                     // ( oQRectF, nRectCount )                            -> NIL
                                         // ( oQRect, nRectCount )                             -> NIL
   METHOD  drawTextItem                  // ( oQPointF, oQTextItem )                           -> NIL
   METHOD  drawTiledPixmap               // ( oQRectF, oQPixmap, oQPointF )                    -> NIL
   METHOD  end                           // (  )                                               -> lBool
   METHOD  hasFeature                    // ( nFeature )                                       -> lBool
   METHOD  isActive                      // (  )                                               -> lBool
   METHOD  paintDevice                   // (  )                                               -> oQPaintDevice
   METHOD  painter                       // (  )                                               -> oQPainter
   METHOD  setActive                     // ( lState )                                         -> NIL
   METHOD  type                          // (  )                                               -> nType

   ENDCLASS


METHOD QPaintEngine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPaintEngine( ... )
   RETURN Self


METHOD QPaintEngine:begin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_begin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawEllipse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPaintEngine_drawEllipse( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPaintEngine_drawEllipse_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawImage( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPaintEngine_drawImage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPaintEngine_drawImage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawLines( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QLINEF"
            RETURN Qt_QPaintEngine_drawLines( ::pPtr, ... )
         CASE "QLINE"
            RETURN Qt_QPaintEngine_drawLines_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_drawPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPixmap( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPaintEngine_drawPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPoints( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPaintEngine_drawPoints( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPaintEngine_drawPoints_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawPolygon( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPaintEngine_drawPolygon( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QPaintEngine_drawPolygon_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawRects( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPaintEngine_drawRects( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QPaintEngine_drawRects_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawTextItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPaintEngine_drawTextItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:drawTiledPixmap( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPaintEngine_drawTiledPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:end( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintEngine_end( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:hasFeature( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_hasFeature( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:isActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintEngine_isActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:paintDevice( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintDeviceFromPointer( Qt_QPaintEngine_paintDevice( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:painter( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPainterFromPointer( Qt_QPaintEngine_painter( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:setActive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPaintEngine_setActive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintEngine:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintEngine_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

