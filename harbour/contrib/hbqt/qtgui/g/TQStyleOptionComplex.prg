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


FUNCTION QStyleOptionComplex( ... )
   RETURN HB_QStyleOptionComplex():new( ... )

FUNCTION QStyleOptionComplexFromPointer( ... )
   RETURN HB_QStyleOptionComplex():fromPointer( ... )


CREATE CLASS QStyleOptionComplex INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionComplex

   METHOD  new( ... )

   METHOD  activeSubControls             // (  )                                               -> nQStyle_SubControls
   METHOD  subControls                   // (  )                                               -> nQStyle_SubControls

   ENDCLASS


METHOD QStyleOptionComplex:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionComplex( ... )
   RETURN Self


METHOD QStyleOptionComplex:activeSubControls( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionComplex_activeSubControls( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionComplex:subControls( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionComplex_subControls( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

