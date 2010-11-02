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


FUNCTION QStyleOptionGraphicsItem( ... )
   RETURN HB_QStyleOptionGraphicsItem():new( ... )

FUNCTION QStyleOptionGraphicsItemFromPointer( ... )
   RETURN HB_QStyleOptionGraphicsItem():fromPointer( ... )


CREATE CLASS QStyleOptionGraphicsItem INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionGraphicsItem

   METHOD  new( ... )

   METHOD  exposedRect                   // (  )                                               -> oQRectF
   METHOD  levelOfDetail                 // (  )                                               -> nQreal
   METHOD  matrix                        // (  )                                               -> oQMatrix

   ENDCLASS


METHOD QStyleOptionGraphicsItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionGraphicsItem( ... )
   RETURN Self


METHOD QStyleOptionGraphicsItem:exposedRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QStyleOptionGraphicsItem_exposedRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionGraphicsItem:levelOfDetail( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionGraphicsItem_levelOfDetail( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionGraphicsItem:matrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMatrixFromPointer( Qt_QStyleOptionGraphicsItem_matrix( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

