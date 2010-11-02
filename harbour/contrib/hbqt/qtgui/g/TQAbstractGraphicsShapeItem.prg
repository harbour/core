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


FUNCTION QAbstractGraphicsShapeItem( ... )
   RETURN HB_QAbstractGraphicsShapeItem():new( ... )

FUNCTION QAbstractGraphicsShapeItemFromPointer( ... )
   RETURN HB_QAbstractGraphicsShapeItem():fromPointer( ... )


CREATE CLASS QAbstractGraphicsShapeItem INHERIT HbQtObjectHandler, HB_QGraphicsItem FUNCTION HB_QAbstractGraphicsShapeItem

   METHOD  new( ... )

   METHOD  brush                         // (  )                                               -> oQBrush
   METHOD  pen                           // (  )                                               -> oQPen
   METHOD  setBrush                      // ( oQBrush )                                        -> NIL
   METHOD  setPen                        // ( oQPen )                                          -> NIL

   ENDCLASS


METHOD QAbstractGraphicsShapeItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractGraphicsShapeItem( ... )
   RETURN Self


METHOD QAbstractGraphicsShapeItem:brush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QAbstractGraphicsShapeItem_brush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractGraphicsShapeItem:pen( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPenFromPointer( Qt_QAbstractGraphicsShapeItem_pen( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractGraphicsShapeItem:setBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractGraphicsShapeItem_setBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractGraphicsShapeItem:setPen( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractGraphicsShapeItem_setPen( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

