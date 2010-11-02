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


FUNCTION QStyleOptionSpinBox( ... )
   RETURN HB_QStyleOptionSpinBox():new( ... )

FUNCTION QStyleOptionSpinBoxFromPointer( ... )
   RETURN HB_QStyleOptionSpinBox():fromPointer( ... )


CREATE CLASS QStyleOptionSpinBox INHERIT HbQtObjectHandler, HB_QStyleOptionComplex FUNCTION HB_QStyleOptionSpinBox

   METHOD  new( ... )

   METHOD  buttonSymbols                 // (  )                                               -> nQAbstractSpinBox_ButtonSymbols
   METHOD  frame                         // (  )                                               -> lBool
   METHOD  stepEnabled                   // (  )                                               -> nQAbstractSpinBox_StepEnabled

   ENDCLASS


METHOD QStyleOptionSpinBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionSpinBox( ... )
   RETURN Self


METHOD QStyleOptionSpinBox:buttonSymbols( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSpinBox_buttonSymbols( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSpinBox:frame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSpinBox_frame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSpinBox:stepEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSpinBox_stepEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

