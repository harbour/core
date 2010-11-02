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


FUNCTION QSlider( ... )
   RETURN HB_QSlider():new( ... )

FUNCTION QSliderFromPointer( ... )
   RETURN HB_QSlider():fromPointer( ... )


CREATE CLASS QSlider INHERIT HbQtObjectHandler, HB_QAbstractSlider FUNCTION HB_QSlider

   METHOD  new( ... )

   METHOD  setTickInterval               // ( nTi )                                            -> NIL
   METHOD  setTickPosition               // ( nPosition )                                      -> NIL
   METHOD  tickInterval                  // (  )                                               -> nInt
   METHOD  tickPosition                  // (  )                                               -> nTickPosition

   ENDCLASS


METHOD QSlider:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSlider( ... )
   RETURN Self


METHOD QSlider:setTickInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSlider_setTickInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSlider:setTickPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSlider_setTickPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSlider:tickInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSlider_tickInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSlider:tickPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSlider_tickPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

