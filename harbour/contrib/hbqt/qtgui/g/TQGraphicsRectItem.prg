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


FUNCTION QGraphicsRectItem( ... )
   RETURN HB_QGraphicsRectItem():new( ... )

FUNCTION QGraphicsRectItemFromPointer( ... )
   RETURN HB_QGraphicsRectItem():fromPointer( ... )


CREATE CLASS QGraphicsRectItem INHERIT HbQtObjectHandler, HB_QAbstractGraphicsShapeItem FUNCTION HB_QGraphicsRectItem

   METHOD  new( ... )

   METHOD  rect                          // (  )                                               -> oQRectF
   METHOD  setRect                       // ( oQRectF )                                        -> NIL
                                         // ( nX, nY, nWidth, nHeight )                        -> NIL

   ENDCLASS


METHOD QGraphicsRectItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsRectItem( ... )
   RETURN Self


METHOD QGraphicsRectItem:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsRectItem_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsRectItem:setRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsRectItem_setRect_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsRectItem_setRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

