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


FUNCTION QStyleOptionSizeGrip( ... )
   RETURN HB_QStyleOptionSizeGrip():new( ... )

FUNCTION QStyleOptionSizeGripFromPointer( ... )
   RETURN HB_QStyleOptionSizeGrip():fromPointer( ... )


CREATE CLASS QStyleOptionSizeGrip INHERIT HbQtObjectHandler, HB_QStyleOptionComplex FUNCTION HB_QStyleOptionSizeGrip

   METHOD  new( ... )

   METHOD  corner                        // (  )                                               -> nQt_Corner

   ENDCLASS


METHOD QStyleOptionSizeGrip:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionSizeGrip( ... )
   RETURN Self


METHOD QStyleOptionSizeGrip:corner( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSizeGrip_corner( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

