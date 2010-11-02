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


FUNCTION QPoint( ... )
   RETURN HB_QPoint():new( ... )

FUNCTION QPointFromPointer( ... )
   RETURN HB_QPoint():fromPointer( ... )


CREATE CLASS QPoint INHERIT HbQtObjectHandler FUNCTION HB_QPoint

   METHOD  new( ... )

   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  manhattanLength               // (  )                                               -> nInt
   METHOD  rx                            // (  )                                               -> nInt
   METHOD  ry                            // (  )                                               -> nInt
   METHOD  setX                          // ( nX )                                             -> NIL
   METHOD  setY                          // ( nY )                                             -> NIL
   METHOD  x                             // (  )                                               -> nInt
   METHOD  y                             // (  )                                               -> nInt

   ENDCLASS


METHOD QPoint:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPoint( ... )
   RETURN Self


METHOD QPoint:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPoint_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPoint:manhattanLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPoint_manhattanLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPoint:rx( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPoint_rx( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPoint:ry( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPoint_ry( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPoint:setX( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPoint_setX( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPoint:setY( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPoint_setY( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPoint:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPoint_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPoint:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPoint_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

