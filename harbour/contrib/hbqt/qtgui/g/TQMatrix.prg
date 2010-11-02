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


FUNCTION QMatrix( ... )
   RETURN HB_QMatrix():new( ... )

FUNCTION QMatrixFromPointer( ... )
   RETURN HB_QMatrix():fromPointer( ... )


CREATE CLASS QMatrix INHERIT HbQtObjectHandler FUNCTION HB_QMatrix

   METHOD  new( ... )

   METHOD  m11                           // (  )                                               -> nQreal
   METHOD  m12                           // (  )                                               -> nQreal
   METHOD  m21                           // (  )                                               -> nQreal
   METHOD  m22                           // (  )                                               -> nQreal
   METHOD  det                           // (  )                                               -> nQreal
   METHOD  dx                            // (  )                                               -> nQreal
   METHOD  dy                            // (  )                                               -> nQreal
   METHOD  inverted                      // ( @lInvertible )                                   -> oQMatrix
   METHOD  isIdentity                    // (  )                                               -> lBool
   METHOD  isInvertible                  // (  )                                               -> lBool
   METHOD  map                           // ( nX, nY, @nTx, @nTy )                             -> NIL
                                         // ( nX, nY, @nTx, @nTy )                             -> NIL
                                         // ( oQPointF )                                       -> oQPointF
                                         // ( oQPoint )                                        -> oQPoint
                                         // ( oQLineF )                                        -> oQLineF
                                         // ( oQLine )                                         -> oQLine
                                         // ( oQPolygonF )                                     -> oQPolygonF
                                         // ( oQPolygon )                                      -> oQPolygon
                                         // ( oQRegion )                                       -> oQRegion
                                         // ( oQPainterPath )                                  -> oQPainterPath
   METHOD  mapRect                       // ( oQRectF )                                        -> oQRectF
                                         // ( oQRect )                                         -> oQRect
   METHOD  mapToPolygon                  // ( oQRect )                                         -> oQPolygon
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  rotate                        // ( nDegrees )                                       -> oQMatrix
   METHOD  scale                         // ( nSx, nSy )                                       -> oQMatrix
   METHOD  setMatrix                     // ( nM11, nM12, nM21, nM22, nDx, nDy )               -> NIL
   METHOD  shear                         // ( nSh, nSv )                                       -> oQMatrix
   METHOD  translate                     // ( nDx, nDy )                                       -> oQMatrix

   ENDCLASS


METHOD QMatrix:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMatrix( ... )
   RETURN Self


METHOD QMatrix:m11( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m11( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:m12( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m12( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:m21( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m21( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:m22( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m22( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:det( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_det( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:dx( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_dx( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:dy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_dy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:inverted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_inverted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QMatrixFromPointer( Qt_QMatrix_inverted( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:isIdentity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_isIdentity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:isInvertible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_isInvertible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:map( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QMatrix_map_1( ::pPtr, ... )
         // RETURN Qt_QMatrix_map( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGON"
            RETURN QPolygonFromPointer( Qt_QMatrix_map_7( ::pPtr, ... ) )
         CASE "QLINE"
            RETURN QLineFromPointer( Qt_QMatrix_map_5( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN QPointFromPointer( Qt_QMatrix_map_3( ::pPtr, ... ) )
         CASE "QREGION"
            RETURN QRegionFromPointer( Qt_QMatrix_map_8( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN QPointFFromPointer( Qt_QMatrix_map_2( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN QPolygonFFromPointer( Qt_QMatrix_map_6( ::pPtr, ... ) )
         CASE "QLINEF"
            RETURN QLineFFromPointer( Qt_QMatrix_map_4( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN QPainterPathFromPointer( Qt_QMatrix_map_9( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:mapRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN QRectFFromPointer( Qt_QMatrix_mapRect( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QRectFromPointer( Qt_QMatrix_mapRect_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:mapToPolygon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFromPointer( Qt_QMatrix_mapToPolygon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:rotate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_rotate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:scale( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_scale( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:setMatrix( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QMatrix_setMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:shear( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_shear( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_translate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

