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


FUNCTION QPainterPath( ... )
   RETURN HB_QPainterPath():new( ... )

FUNCTION QPainterPathFromPointer( ... )
   RETURN HB_QPainterPath():fromPointer( ... )


CREATE CLASS QPainterPath INHERIT HbQtObjectHandler FUNCTION HB_QPainterPath

   METHOD  new( ... )

   METHOD  addEllipse                    // ( oQRectF )                                        -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
                                         // ( oQPointF, nRx, nRy )                             -> NIL
   METHOD  addPath                       // ( oQPainterPath )                                  -> NIL
   METHOD  addPolygon                    // ( oQPolygonF )                                     -> NIL
   METHOD  addRect                       // ( oQRectF )                                        -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  addRegion                     // ( oQRegion )                                       -> NIL
   METHOD  addRoundedRect                // ( oQRectF, nXRadius, nYRadius, nMode )             -> NIL
                                         // ( nX, nY, nW, nH, nXRadius, nYRadius, nMode )      -> NIL
   METHOD  addText                       // ( oQPointF, oQFont, cText )                        -> NIL
                                         // ( nX, nY, oQFont, cText )                          -> NIL
   METHOD  angleAtPercent                // ( nT )                                             -> nQreal
   METHOD  arcMoveTo                     // ( oQRectF, nAngle )                                -> NIL
                                         // ( nX, nY, nWidth, nHeight, nAngle )                -> NIL
   METHOD  arcTo                         // ( oQRectF, nStartAngle, nSweepLength )             -> NIL
                                         // ( nX, nY, nWidth, nHeight, nStartAngle, nSweepLength ) -> NIL
   METHOD  boundingRect                  // (  )                                               -> oQRectF
   METHOD  closeSubpath                  // (  )                                               -> NIL
   METHOD  connectPath                   // ( oQPainterPath )                                  -> NIL
   METHOD  contains                      // ( oQPointF )                                       -> lBool
                                         // ( oQRectF )                                        -> lBool
                                         // ( oQPainterPath )                                  -> lBool
   METHOD  controlPointRect              // (  )                                               -> oQRectF
   METHOD  cubicTo                       // ( oQPointF, oQPointF, oQPointF )                   -> NIL
                                         // ( nC1X, nC1Y, nC2X, nC2Y, nEndPointX, nEndPointY ) -> NIL
   METHOD  currentPosition               // (  )                                               -> oQPointF
   METHOD  elementCount                  // (  )                                               -> nInt
   METHOD  fillRule                      // (  )                                               -> nQt_FillRule
   METHOD  intersected                   // ( oQPainterPath )                                  -> oQPainterPath
   METHOD  intersects                    // ( oQRectF )                                        -> lBool
                                         // ( oQPainterPath )                                  -> lBool
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  length                        // (  )                                               -> nQreal
   METHOD  lineTo                        // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  moveTo                        // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  percentAtLength               // ( nLen )                                           -> nQreal
   METHOD  pointAtPercent                // ( nT )                                             -> oQPointF
   METHOD  quadTo                        // ( oQPointF, oQPointF )                             -> NIL
                                         // ( nCx, nCy, nEndPointX, nEndPointY )               -> NIL
   METHOD  setElementPositionAt          // ( nIndex, nX, nY )                                 -> NIL
   METHOD  setFillRule                   // ( nFillRule )                                      -> NIL
   METHOD  simplified                    // (  )                                               -> oQPainterPath
   METHOD  slopeAtPercent                // ( nT )                                             -> nQreal
   METHOD  subtracted                    // ( oQPainterPath )                                  -> oQPainterPath
   METHOD  toFillPolygon                 // ( oQTransform )                                    -> oQPolygonF
                                         // ( oQMatrix )                                       -> oQPolygonF
   METHOD  toFillPolygons                // ( oQTransform )                                    -> oQList_QPolygonF>
                                         // ( oQMatrix )                                       -> oQList_QPolygonF>
   METHOD  toReversed                    // (  )                                               -> oQPainterPath
   METHOD  toSubpathPolygons             // ( oQTransform )                                    -> oQList_QPolygonF>
                                         // ( oQMatrix )                                       -> oQList_QPolygonF>
   METHOD  united                        // ( oQPainterPath )                                  -> oQPainterPath

   ENDCLASS


METHOD QPainterPath:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPainterPath( ... )
   RETURN Self


METHOD QPainterPath:addEllipse( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addEllipse_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_addEllipse_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_addEllipse( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:addPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_addPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:addPolygon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_addPolygon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:addRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_addRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:addRegion( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_addRegion( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:addRoundedRect( ... )
   SWITCH PCount()
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN Qt_QPainterPath_addRoundedRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainterPath_addRoundedRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:addText( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_addText_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_addText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:angleAtPercent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_angleAtPercent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:arcMoveTo( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QPainterPath_arcMoveTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_arcMoveTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:arcTo( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainterPath_arcTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_arcTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QPainterPath_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:closeSubpath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainterPath_closeSubpath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:connectPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_connectPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:contains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QPainterPath_contains( ::pPtr, ... )
         CASE "QRECTF"
            RETURN Qt_QPainterPath_contains_1( ::pPtr, ... )
         CASE "QPAINTERPATH"
            RETURN Qt_QPainterPath_contains_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:controlPointRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QPainterPath_controlPointRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:cubicTo( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QPainterPath_cubicTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_cubicTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:currentPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QPainterPath_currentPosition( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:elementCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainterPath_elementCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:fillRule( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainterPath_fillRule( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:intersected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPainterPathFromPointer( Qt_QPainterPath_intersected( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:intersects( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN Qt_QPainterPath_intersects( ::pPtr, ... )
         CASE "QPAINTERPATH"
            RETURN Qt_QPainterPath_intersects_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainterPath_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPainterPath_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:lineTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_lineTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_lineTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:moveTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_moveTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_moveTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:percentAtLength( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_percentAtLength( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:pointAtPercent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPointFFromPointer( Qt_QPainterPath_pointAtPercent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:quadTo( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPainterPath_quadTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPainterPath_quadTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:setElementPositionAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPainterPath_setElementPositionAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:setFillRule( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_setFillRule( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:simplified( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPainterPathFromPointer( Qt_QPainterPath_simplified( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:slopeAtPercent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPainterPath_slopeAtPercent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:subtracted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPainterPathFromPointer( Qt_QPainterPath_subtracted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:toFillPolygon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN QPolygonFFromPointer( Qt_QPainterPath_toFillPolygon( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN QPolygonFFromPointer( Qt_QPainterPath_toFillPolygon_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QPolygonFFromPointer( Qt_QPainterPath_toFillPolygon_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:toFillPolygons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN QListFromPointer( Qt_QPainterPath_toFillPolygons( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN QListFromPointer( Qt_QPainterPath_toFillPolygons_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QListFromPointer( Qt_QPainterPath_toFillPolygons_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:toReversed( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPainterPathFromPointer( Qt_QPainterPath_toReversed( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:toSubpathPolygons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN QListFromPointer( Qt_QPainterPath_toSubpathPolygons( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN QListFromPointer( Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QListFromPointer( Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPainterPath:united( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPainterPathFromPointer( Qt_QPainterPath_united( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

