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


FUNCTION QDial( ... )
   RETURN HB_QDial():new( ... )

FUNCTION QDialFromPointer( ... )
   RETURN HB_QDial():fromPointer( ... )


CREATE CLASS QDial INHERIT HbQtObjectHandler, HB_QAbstractSlider FUNCTION HB_QDial

   METHOD  new( ... )

   METHOD  notchSize                     // (  )                                               -> nInt
   METHOD  notchTarget                   // (  )                                               -> nQreal
   METHOD  notchesVisible                // (  )                                               -> lBool
   METHOD  setNotchTarget                // ( nTarget )                                        -> NIL
   METHOD  wrapping                      // (  )                                               -> lBool
   METHOD  setNotchesVisible             // ( lVisible )                                       -> NIL
   METHOD  setWrapping                   // ( lOn )                                            -> NIL

   ENDCLASS


METHOD QDial:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDial( ... )
   RETURN Self


METHOD QDial:notchSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDial_notchSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDial:notchTarget( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDial_notchTarget( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDial:notchesVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDial_notchesVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDial:setNotchTarget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDial_setNotchTarget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDial:wrapping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDial_wrapping( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDial:setNotchesVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDial_setNotchesVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDial:setWrapping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDial_setWrapping( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

