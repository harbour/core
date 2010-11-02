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


FUNCTION QStyleOptionFrame( ... )
   RETURN HB_QStyleOptionFrame():new( ... )

FUNCTION QStyleOptionFrameFromPointer( ... )
   RETURN HB_QStyleOptionFrame():fromPointer( ... )


CREATE CLASS QStyleOptionFrame INHERIT HbQtObjectHandler, qStyleOption FUNCTION HB_QStyleOptionFrame

   METHOD  new( ... )

   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  midLineWidth                  // (  )                                               -> nInt

   ENDCLASS


METHOD QStyleOptionFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionFrame( ... )
   RETURN Self


METHOD QStyleOptionFrame:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionFrame_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionFrame:midLineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionFrame_midLineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

