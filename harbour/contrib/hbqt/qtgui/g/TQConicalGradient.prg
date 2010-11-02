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


FUNCTION QConicalGradient( ... )
   RETURN HB_QConicalGradient():new( ... )

FUNCTION QConicalGradientFromPointer( ... )
   RETURN HB_QConicalGradient():fromPointer( ... )


CREATE CLASS QConicalGradient INHERIT HbQtObjectHandler, HB_QGradient FUNCTION HB_QConicalGradient

   METHOD  new( ... )

   METHOD  angle                         // (  )                                               -> nQreal
   METHOD  center                        // (  )                                               -> oQPointF
   METHOD  setAngle                      // ( nAngle )                                         -> NIL
   METHOD  setCenter                     // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL

   ENDCLASS


METHOD QConicalGradient:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QConicalGradient( ... )
   RETURN Self


METHOD QConicalGradient:angle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QConicalGradient_angle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QConicalGradient:center( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QConicalGradient_center( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QConicalGradient:setAngle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QConicalGradient_setAngle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QConicalGradient:setCenter( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QConicalGradient_setCenter_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QConicalGradient_setCenter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

