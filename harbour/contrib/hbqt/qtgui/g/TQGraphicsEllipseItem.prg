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


FUNCTION QGraphicsEllipseItem( ... )
   RETURN HB_QGraphicsEllipseItem():new( ... )

FUNCTION QGraphicsEllipseItemFromPointer( ... )
   RETURN HB_QGraphicsEllipseItem():fromPointer( ... )


CREATE CLASS QGraphicsEllipseItem INHERIT HbQtObjectHandler, HB_QAbstractGraphicsShapeItem FUNCTION HB_QGraphicsEllipseItem

   METHOD  new( ... )

   METHOD  rect                          // (  )                                               -> oQRectF
   METHOD  setRect                       // ( oQRectF )                                        -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  setSpanAngle                  // ( nAngle )                                         -> NIL
   METHOD  setStartAngle                 // ( nAngle )                                         -> NIL
   METHOD  spanAngle                     // (  )                                               -> nInt
   METHOD  startAngle                    // (  )                                               -> nInt

   ENDCLASS


METHOD QGraphicsEllipseItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsEllipseItem( ... )
   RETURN Self


METHOD QGraphicsEllipseItem:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsEllipseItem_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsEllipseItem:setRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsEllipseItem_setRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsEllipseItem_setRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsEllipseItem:setSpanAngle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsEllipseItem_setSpanAngle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsEllipseItem:setStartAngle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsEllipseItem_setStartAngle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsEllipseItem:spanAngle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsEllipseItem_spanAngle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsEllipseItem:startAngle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsEllipseItem_startAngle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

