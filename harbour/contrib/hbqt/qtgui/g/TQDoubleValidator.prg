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


FUNCTION QDoubleValidator( ... )
   RETURN HB_QDoubleValidator():new( ... )

FUNCTION QDoubleValidatorFromPointer( ... )
   RETURN HB_QDoubleValidator():fromPointer( ... )


CREATE CLASS QDoubleValidator INHERIT HbQtObjectHandler, HB_QValidator FUNCTION HB_QDoubleValidator

   METHOD  new( ... )

   METHOD  bottom                        // (  )                                               -> nDouble
   METHOD  decimals                      // (  )                                               -> nInt
   METHOD  notation                      // (  )                                               -> nNotation
   METHOD  setBottom                     // ( nDouble )                                        -> NIL
   METHOD  setDecimals                   // ( nInt )                                           -> NIL
   METHOD  setNotation                   // ( nNotation )                                      -> NIL
   METHOD  setRange                      // ( nMinimum, nMaximum, nDecimals )                  -> NIL
   METHOD  setTop                        // ( nDouble )                                        -> NIL
   METHOD  top                           // (  )                                               -> nDouble

   ENDCLASS


METHOD QDoubleValidator:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDoubleValidator( ... )
   RETURN Self


METHOD QDoubleValidator:bottom( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleValidator_bottom( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:decimals( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleValidator_decimals( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:notation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleValidator_notation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:setBottom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleValidator_setBottom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:setDecimals( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleValidator_setDecimals( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:setNotation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleValidator_setNotation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:setRange( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QDoubleValidator_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDoubleValidator_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:setTop( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDoubleValidator_setTop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDoubleValidator:top( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDoubleValidator_top( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

