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


FUNCTION QGraphicsPixmapItem( ... )
   RETURN HB_QGraphicsPixmapItem():new( ... )

FUNCTION QGraphicsPixmapItemFromPointer( ... )
   RETURN HB_QGraphicsPixmapItem():fromPointer( ... )


CREATE CLASS QGraphicsPixmapItem INHERIT HbQtObjectHandler, HB_QGraphicsItem FUNCTION HB_QGraphicsPixmapItem

   METHOD  new( ... )

   METHOD  offset                        // (  )                                               -> oQPointF
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  setOffset                     // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  setPixmap                     // ( oQPixmap )                                       -> NIL
   METHOD  setShapeMode                  // ( nMode )                                          -> NIL
   METHOD  setTransformationMode         // ( nMode )                                          -> NIL
   METHOD  shapeMode                     // (  )                                               -> nShapeMode
   METHOD  transformationMode            // (  )                                               -> nQt_TransformationMode

   ENDCLASS


METHOD QGraphicsPixmapItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsPixmapItem( ... )
   RETURN Self


METHOD QGraphicsPixmapItem:offset( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QGraphicsPixmapItem_offset( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPixmapItem:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QGraphicsPixmapItem_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPixmapItem:setOffset( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsPixmapItem_setOffset_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsPixmapItem_setOffset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPixmapItem:setPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsPixmapItem_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPixmapItem:setShapeMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsPixmapItem_setShapeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPixmapItem:setTransformationMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsPixmapItem_setTransformationMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPixmapItem:shapeMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsPixmapItem_shapeMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPixmapItem:transformationMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsPixmapItem_transformationMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

