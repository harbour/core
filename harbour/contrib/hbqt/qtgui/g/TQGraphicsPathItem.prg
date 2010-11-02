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


FUNCTION QGraphicsPathItem( ... )
   RETURN HB_QGraphicsPathItem():new( ... )

FUNCTION QGraphicsPathItemFromPointer( ... )
   RETURN HB_QGraphicsPathItem():fromPointer( ... )


CREATE CLASS QGraphicsPathItem INHERIT HbQtObjectHandler, HB_QAbstractGraphicsShapeItem FUNCTION HB_QGraphicsPathItem

   METHOD  new( ... )

   METHOD  path                          // (  )                                               -> oQPainterPath
   METHOD  setPath                       // ( oQPainterPath )                                  -> NIL

   ENDCLASS


METHOD QGraphicsPathItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsPathItem( ... )
   RETURN Self


METHOD QGraphicsPathItem:path( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPainterPathFromPointer( Qt_QGraphicsPathItem_path( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsPathItem:setPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsPathItem_setPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

