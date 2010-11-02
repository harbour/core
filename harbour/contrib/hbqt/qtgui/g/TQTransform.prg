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


FUNCTION QTransform( ... )
   RETURN HB_QTransform():new( ... )

FUNCTION QTransformFromPointer( ... )
   RETURN HB_QTransform():fromPointer( ... )


CREATE CLASS QTransform INHERIT HbQtObjectHandler FUNCTION HB_QTransform

   METHOD  new( ... )

   METHOD  m11                           // (  )                                               -> nQreal
   METHOD  m12                           // (  )                                               -> nQreal
   METHOD  m13                           // (  )                                               -> nQreal
   METHOD  m21                           // (  )                                               -> nQreal
   METHOD  m22                           // (  )                                               -> nQreal
   METHOD  m23                           // (  )                                               -> nQreal
   METHOD  m31                           // (  )                                               -> nQreal
   METHOD  m32                           // (  )                                               -> nQreal
   METHOD  m33                           // (  )                                               -> nQreal
   METHOD  adjoint                       // (  )                                               -> oQTransform
   METHOD  det                           // (  )                                               -> nQreal
   METHOD  determinant                   // (  )                                               -> nQreal
   METHOD  dx                            // (  )                                               -> nQreal
   METHOD  dy                            // (  )                                               -> nQreal
   METHOD  inverted                      // ( @lInvertible )                                   -> oQTransform
   METHOD  isAffine                      // (  )                                               -> lBool
   METHOD  isIdentity                    // (  )                                               -> lBool
   METHOD  isInvertible                  // (  )                                               -> lBool
   METHOD  isRotating                    // (  )                                               -> lBool
   METHOD  isScaling                     // (  )                                               -> lBool
   METHOD  isTranslating                 // (  )                                               -> lBool
   METHOD  map                           // ( nX, nY, @nTx, @nTy )                             -> NIL
                                         // ( oQPointF )                                       -> oQPointF
                                         // ( oQPoint )                                        -> oQPoint
                                         // ( oQLine )                                         -> oQLine
                                         // ( oQLineF )                                        -> oQLineF
                                         // ( oQPolygonF )                                     -> oQPolygonF
                                         // ( oQPolygon )                                      -> oQPolygon
                                         // ( oQRegion )                                       -> oQRegion
                                         // ( oQPainterPath )                                  -> oQPainterPath
                                         // ( nX, nY, @nTx, @nTy )                             -> NIL
   METHOD  mapRect                       // ( oQRectF )                                        -> oQRectF
                                         // ( oQRect )                                         -> oQRect
   METHOD  mapToPolygon                  // ( oQRect )                                         -> oQPolygon
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  rotate                        // ( nAngle, nAxis )                                  -> oQTransform
   METHOD  rotateRadians                 // ( nAngle, nAxis )                                  -> oQTransform
   METHOD  scale                         // ( nSx, nSy )                                       -> oQTransform
   METHOD  setMatrix                     // ( nM11, nM12, nM13, nM21, nM22, nM23, nM31, nM32, nM33 ) -> NIL
   METHOD  shear                         // ( nSh, nSv )                                       -> oQTransform
   METHOD  toAffine                      // (  )                                               -> oQMatrix
   METHOD  translate                     // ( nDx, nDy )                                       -> oQTransform
   METHOD  transposed                    // (  )                                               -> oQTransform
   METHOD  type                          // (  )                                               -> nTransformationType
   METHOD  fromScale                     // ( nSx, nSy )                                       -> oQTransform
   METHOD  fromTranslate                 // ( nDx, nDy )                                       -> oQTransform
   METHOD  quadToQuad                    // ( oQPolygonF, oQPolygonF, oQTransform )            -> lBool
   METHOD  quadToSquare                  // ( oQPolygonF, oQTransform )                        -> lBool
   METHOD  squareToQuad                  // ( oQPolygonF, oQTransform )                        -> lBool

   ENDCLASS


METHOD QTransform:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTransform( ... )
   RETURN Self


METHOD QTransform:m11( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m11( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m12( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m12( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m13( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m13( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m21( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m21( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m22( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m22( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m23( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m23( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m31( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m31( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m32( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m32( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:m33( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_m33( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:adjoint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QTransform_adjoint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:det( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_det( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:determinant( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_determinant( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:dx( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_dx( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:dy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_dy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:inverted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN QTransformFromPointer( Qt_QTransform_inverted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QTransformFromPointer( Qt_QTransform_inverted( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:isAffine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_isAffine( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:isIdentity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_isIdentity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:isInvertible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_isInvertible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:isRotating( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_isRotating( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:isScaling( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_isScaling( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:isTranslating( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_isTranslating( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:map( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTransform_map( ::pPtr, ... )
         // RETURN Qt_QTransform_map_9( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN QPointFFromPointer( Qt_QTransform_map_1( ::pPtr, ... ) )
         CASE "QREGION"
            RETURN QRegionFromPointer( Qt_QTransform_map_7( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN QPointFromPointer( Qt_QTransform_map_2( ::pPtr, ... ) )
         CASE "QLINE"
            RETURN QLineFromPointer( Qt_QTransform_map_3( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN QPolygonFFromPointer( Qt_QTransform_map_5( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN QPainterPathFromPointer( Qt_QTransform_map_8( ::pPtr, ... ) )
         CASE "QPOLYGON"
            RETURN QPolygonFromPointer( Qt_QTransform_map_6( ::pPtr, ... ) )
         CASE "QLINEF"
            RETURN QLineFFromPointer( Qt_QTransform_map_4( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:mapRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN QRectFFromPointer( Qt_QTransform_mapRect( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QRectFromPointer( Qt_QTransform_mapRect_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:mapToPolygon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFromPointer( Qt_QTransform_mapToPolygon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:rotate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTransformFromPointer( Qt_QTransform_rotate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTransformFromPointer( Qt_QTransform_rotate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:rotateRadians( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTransformFromPointer( Qt_QTransform_rotateRadians( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTransformFromPointer( Qt_QTransform_rotateRadians( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:scale( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTransformFromPointer( Qt_QTransform_scale( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:setMatrix( ... )
   SWITCH PCount()
   CASE 9
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) ) .AND. hb_isNumeric( hb_pvalue( 9 ) )
         RETURN Qt_QTransform_setMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:shear( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTransformFromPointer( Qt_QTransform_shear( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:toAffine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMatrixFromPointer( Qt_QTransform_toAffine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTransformFromPointer( Qt_QTransform_translate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:transposed( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QTransform_transposed( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTransform_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:fromScale( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTransformFromPointer( Qt_QTransform_fromScale( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:fromTranslate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QTransformFromPointer( Qt_QTransform_fromTranslate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:quadToQuad( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTransform_quadToQuad( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:quadToSquare( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTransform_quadToSquare( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTransform:squareToQuad( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTransform_squareToQuad( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

