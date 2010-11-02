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


FUNCTION QStyleOptionSlider( ... )
   RETURN HB_QStyleOptionSlider():new( ... )

FUNCTION QStyleOptionSliderFromPointer( ... )
   RETURN HB_QStyleOptionSlider():fromPointer( ... )


CREATE CLASS QStyleOptionSlider INHERIT HbQtObjectHandler, HB_QStyleOptionComplex FUNCTION HB_QStyleOptionSlider

   METHOD  new( ... )

   METHOD  dialWrapping                  // (  )                                               -> lBool
   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  notchTarget                   // (  )                                               -> nQreal
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  pageStep                      // (  )                                               -> nInt
   METHOD  singleStep                    // (  )                                               -> nInt
   METHOD  sliderPosition                // (  )                                               -> nInt
   METHOD  sliderValue                   // (  )                                               -> nInt
   METHOD  tickInterval                  // (  )                                               -> nInt
   METHOD  tickPosition                  // (  )                                               -> nQSlider_TickPosition
   METHOD  upsideDown                    // (  )                                               -> lBool

   ENDCLASS


METHOD QStyleOptionSlider:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionSlider( ... )
   RETURN Self


METHOD QStyleOptionSlider:dialWrapping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_dialWrapping( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:notchTarget( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_notchTarget( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:pageStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_pageStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:singleStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_singleStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:sliderPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_sliderPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:sliderValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_sliderValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:tickInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_tickInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:tickPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_tickPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionSlider:upsideDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionSlider_upsideDown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

