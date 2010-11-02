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


FUNCTION QGradient( ... )
   RETURN HB_QGradient():new( ... )

FUNCTION QGradientFromPointer( ... )
   RETURN HB_QGradient():fromPointer( ... )


CREATE CLASS QGradient INHERIT HbQtObjectHandler FUNCTION HB_QGradient

   METHOD  new( ... )

   METHOD  coordinateMode                // (  )                                               -> nCoordinateMode
   METHOD  setColorAt                    // ( nPosition, oQColor )                             -> NIL
   METHOD  setCoordinateMode             // ( nMode )                                          -> NIL
   METHOD  setSpread                     // ( nMethod )                                        -> NIL
   METHOD  spread                        // (  )                                               -> nSpread
   METHOD  type                          // (  )                                               -> nType

   ENDCLASS


METHOD QGradient:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGradient( ... )
   RETURN Self


METHOD QGradient:coordinateMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGradient_coordinateMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGradient:setColorAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGradient_setColorAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGradient:setCoordinateMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGradient_setCoordinateMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGradient:setSpread( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGradient_setSpread( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGradient:spread( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGradient_spread( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGradient:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGradient_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

