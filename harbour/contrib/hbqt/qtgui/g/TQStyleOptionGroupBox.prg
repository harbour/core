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


FUNCTION QStyleOptionGroupBox( ... )
   RETURN HB_QStyleOptionGroupBox():new( ... )

FUNCTION QStyleOptionGroupBoxFromPointer( ... )
   RETURN HB_QStyleOptionGroupBox():fromPointer( ... )


CREATE CLASS QStyleOptionGroupBox INHERIT HbQtObjectHandler, HB_QStyleOptionComplex FUNCTION HB_QStyleOptionGroupBox

   METHOD  new( ... )

   METHOD  features                      // (  )                                               -> nQStyleOptionFrameV2_FrameFeatures
   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  midLineWidth                  // (  )                                               -> nInt
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textAlignment                 // (  )                                               -> nQt_Alignment
   METHOD  textColor                     // (  )                                               -> oQColor

   ENDCLASS


METHOD QStyleOptionGroupBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionGroupBox( ... )
   RETURN Self


METHOD QStyleOptionGroupBox:features( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionGroupBox_features( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionGroupBox:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionGroupBox_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionGroupBox:midLineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionGroupBox_midLineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionGroupBox:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionGroupBox_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionGroupBox:textAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionGroupBox_textAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionGroupBox:textColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QStyleOptionGroupBox_textColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

