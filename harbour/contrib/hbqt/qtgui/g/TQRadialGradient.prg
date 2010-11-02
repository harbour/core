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


FUNCTION QRadialGradient( ... )
   RETURN HB_QRadialGradient():new( ... )

FUNCTION QRadialGradientFromPointer( ... )
   RETURN HB_QRadialGradient():fromPointer( ... )


CREATE CLASS QRadialGradient INHERIT HbQtObjectHandler, HB_QGradient FUNCTION HB_QRadialGradient

   METHOD  new( ... )

   METHOD  center                        // (  )                                               -> oQPointF
   METHOD  focalPoint                    // (  )                                               -> oQPointF
   METHOD  radius                        // (  )                                               -> nQreal
   METHOD  setCenter                     // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  setFocalPoint                 // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  setRadius                     // ( nRadius )                                        -> NIL

   ENDCLASS


METHOD QRadialGradient:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRadialGradient( ... )
   RETURN Self


METHOD QRadialGradient:center( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QRadialGradient_center( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRadialGradient:focalPoint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QRadialGradient_focalPoint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRadialGradient:radius( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRadialGradient_radius( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRadialGradient:setCenter( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRadialGradient_setCenter_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRadialGradient_setCenter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRadialGradient:setFocalPoint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRadialGradient_setFocalPoint_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRadialGradient_setFocalPoint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRadialGradient:setRadius( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRadialGradient_setRadius( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

