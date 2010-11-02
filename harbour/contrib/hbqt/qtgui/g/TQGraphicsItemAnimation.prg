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


FUNCTION QGraphicsItemAnimation( ... )
   RETURN HB_QGraphicsItemAnimation():new( ... )

FUNCTION QGraphicsItemAnimationFromPointer( ... )
   RETURN HB_QGraphicsItemAnimation():fromPointer( ... )


CREATE CLASS QGraphicsItemAnimation INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QGraphicsItemAnimation

   METHOD  new( ... )

   METHOD  clear                         // (  )                                               -> NIL
   METHOD  horizontalScaleAt             // ( nStep )                                          -> nQreal
   METHOD  horizontalShearAt             // ( nStep )                                          -> nQreal
   METHOD  item                          // (  )                                               -> oQGraphicsItem
   METHOD  matrixAt                      // ( nStep )                                          -> oQMatrix
   METHOD  posAt                         // ( nStep )                                          -> oQPointF
   METHOD  rotationAt                    // ( nStep )                                          -> nQreal
   METHOD  setItem                       // ( oQGraphicsItem )                                 -> NIL
   METHOD  setPosAt                      // ( nStep, oQPointF )                                -> NIL
   METHOD  setRotationAt                 // ( nStep, nAngle )                                  -> NIL
   METHOD  setScaleAt                    // ( nStep, nSx, nSy )                                -> NIL
   METHOD  setShearAt                    // ( nStep, nSh, nSv )                                -> NIL
   METHOD  setTimeLine                   // ( oQTimeLine )                                     -> NIL
   METHOD  setTranslationAt              // ( nStep, nDx, nDy )                                -> NIL
   METHOD  timeLine                      // (  )                                               -> oQTimeLine
   METHOD  verticalScaleAt               // ( nStep )                                          -> nQreal
   METHOD  verticalShearAt               // ( nStep )                                          -> nQreal
   METHOD  xTranslationAt                // ( nStep )                                          -> nQreal
   METHOD  yTranslationAt                // ( nStep )                                          -> nQreal
   METHOD  setStep                       // ( nStep )                                          -> NIL

   ENDCLASS


METHOD QGraphicsItemAnimation:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsItemAnimation( ... )
   RETURN Self


METHOD QGraphicsItemAnimation:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsItemAnimation_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:horizontalScaleAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_horizontalScaleAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:horizontalShearAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_horizontalShearAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:item( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsItemFromPointer( Qt_QGraphicsItemAnimation_item( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:matrixAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMatrixFromPointer( Qt_QGraphicsItemAnimation_matrixAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:posAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPointFFromPointer( Qt_QGraphicsItemAnimation_posAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:rotationAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_rotationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_setItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setPosAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsItemAnimation_setPosAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setRotationAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsItemAnimation_setRotationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setScaleAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsItemAnimation_setScaleAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setShearAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsItemAnimation_setShearAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setTimeLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_setTimeLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setTranslationAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsItemAnimation_setTranslationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:timeLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeLineFromPointer( Qt_QGraphicsItemAnimation_timeLine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:verticalScaleAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_verticalScaleAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:verticalShearAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_verticalShearAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:xTranslationAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_xTranslationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:yTranslationAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_yTranslationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemAnimation:setStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_setStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

