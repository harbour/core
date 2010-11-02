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


FUNCTION QLinearGradient( ... )
   RETURN HB_QLinearGradient():new( ... )

FUNCTION QLinearGradientFromPointer( ... )
   RETURN HB_QLinearGradient():fromPointer( ... )


CREATE CLASS QLinearGradient INHERIT HbQtObjectHandler, HB_QGradient FUNCTION HB_QLinearGradient

   METHOD  new( ... )

   METHOD  finalStop                     // (  )                                               -> oQPointF
   METHOD  setFinalStop                  // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  setStart                      // ( oQPointF )                                       -> NIL
                                         // ( nX, nY )                                         -> NIL
   METHOD  start                         // (  )                                               -> oQPointF

   ENDCLASS


METHOD QLinearGradient:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLinearGradient( ... )
   RETURN Self


METHOD QLinearGradient:finalStop( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QLinearGradient_finalStop( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLinearGradient:setFinalStop( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLinearGradient_setFinalStop_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLinearGradient_setFinalStop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLinearGradient:setStart( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLinearGradient_setStart_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLinearGradient_setStart( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLinearGradient:start( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QLinearGradient_start( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

