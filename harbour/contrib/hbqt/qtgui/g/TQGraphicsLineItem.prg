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


FUNCTION QGraphicsLineItem( ... )
   RETURN HB_QGraphicsLineItem():new( ... )

FUNCTION QGraphicsLineItemFromPointer( ... )
   RETURN HB_QGraphicsLineItem():fromPointer( ... )


CREATE CLASS QGraphicsLineItem INHERIT HbQtObjectHandler, HB_QGraphicsItem FUNCTION HB_QGraphicsLineItem

   METHOD  new( ... )

   METHOD  line                          // (  )                                               -> oQLineF
   METHOD  pen                           // (  )                                               -> oQPen
   METHOD  setLine                       // ( oQLineF )                                        -> NIL
                                         // ( nX1, nY1, nX2, nY2 )                             -> NIL
   METHOD  setPen                        // ( oQPen )                                          -> NIL

   ENDCLASS


METHOD QGraphicsLineItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsLineItem( ... )
   RETURN Self


METHOD QGraphicsLineItem:line( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLineFFromPointer( Qt_QGraphicsLineItem_line( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLineItem:pen( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPenFromPointer( Qt_QGraphicsLineItem_pen( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLineItem:setLine( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsLineItem_setLine_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLineItem_setLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLineItem:setPen( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLineItem_setPen( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

