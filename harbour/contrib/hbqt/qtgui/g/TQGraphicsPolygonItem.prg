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


FUNCTION QGraphicsPolygonItem( ... )
   RETURN HB_QGraphicsPolygonItem():new( ... )

FUNCTION QGraphicsPolygonItemFromPointer( ... )
   RETURN HB_QGraphicsPolygonItem():fromPointer( ... )


CREATE CLASS QGraphicsPolygonItem INHERIT HbQtObjectHandler, HB_QAbstractGraphicsShapeItem FUNCTION HB_QGraphicsPolygonItem

   METHOD  new( ... )

   METHOD  fillRule                      // (  )                                               -> nQt_FillRule
   METHOD  polygon                       // (  )                                               -> oQPolygonF
   METHOD  setFillRule                   // ( nRule )                                          -> NIL
   METHOD  setPolygon                    // ( oQPolygonF )                                     -> NIL

   ENDCLASS


METHOD QGraphicsPolygonItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsPolygonItem( ... )
   RETURN Self


METHOD QGraphicsPolygonItem:fillRule( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsPolygonItem_fillRule( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPolygonItem:polygon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPolygonFFromPointer( Qt_QGraphicsPolygonItem_polygon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPolygonItem:setFillRule( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsPolygonItem_setFillRule( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPolygonItem:setPolygon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsPolygonItem_setPolygon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

