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


REQUEST __HBQTCORE


FUNCTION QPointF( ... )
   RETURN HB_QPointF():new( ... )

FUNCTION QPointFFromPointer( ... )
   RETURN HB_QPointF():fromPointer( ... )


CREATE CLASS QPointF INHERIT HbQtObjectHandler FUNCTION HB_QPointF

   METHOD  new( ... )

   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  rx                            // (  )                                               -> nQreal
   METHOD  ry                            // (  )                                               -> nQreal
   METHOD  setX                          // ( nX )                                             -> NIL
   METHOD  setY                          // ( nY )                                             -> NIL
   METHOD  toPoint                       // (  )                                               -> oQPoint
   METHOD  x                             // (  )                                               -> nQreal
   METHOD  y                             // (  )                                               -> nQreal

   ENDCLASS


METHOD QPointF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPointF( ... )
   RETURN Self


METHOD QPointF:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPointF_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPointF:rx( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPointF_rx( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPointF:ry( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPointF_ry( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPointF:setX( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPointF_setX( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPointF:setY( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPointF_setY( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPointF:toPoint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QPointF_toPoint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPointF:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPointF_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPointF:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPointF_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

