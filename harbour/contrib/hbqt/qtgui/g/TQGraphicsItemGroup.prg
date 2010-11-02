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


FUNCTION QGraphicsItemGroup( ... )
   RETURN HB_QGraphicsItemGroup():new( ... )

FUNCTION QGraphicsItemGroupFromPointer( ... )
   RETURN HB_QGraphicsItemGroup():fromPointer( ... )


CREATE CLASS QGraphicsItemGroup INHERIT HbQtObjectHandler, HB_QGraphicsItem FUNCTION HB_QGraphicsItemGroup

   METHOD  new( ... )

   METHOD  addToGroup                    // ( oQGraphicsItem )                                 -> NIL
   METHOD  removeFromGroup               // ( oQGraphicsItem )                                 -> NIL

   ENDCLASS


METHOD QGraphicsItemGroup:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsItemGroup( ... )
   RETURN Self


METHOD QGraphicsItemGroup:addToGroup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemGroup_addToGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsItemGroup:removeFromGroup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemGroup_removeFromGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

