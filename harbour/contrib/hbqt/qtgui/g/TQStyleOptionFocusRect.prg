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


FUNCTION QStyleOptionFocusRect( ... )
   RETURN HB_QStyleOptionFocusRect():new( ... )

FUNCTION QStyleOptionFocusRectFromPointer( ... )
   RETURN HB_QStyleOptionFocusRect():fromPointer( ... )


CREATE CLASS QStyleOptionFocusRect INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionFocusRect

   METHOD  new( ... )

   METHOD  backgroundColor               // (  )                                               -> oQColor

   ENDCLASS


METHOD QStyleOptionFocusRect:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionFocusRect( ... )
   RETURN Self


METHOD QStyleOptionFocusRect:backgroundColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QStyleOptionFocusRect_backgroundColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

