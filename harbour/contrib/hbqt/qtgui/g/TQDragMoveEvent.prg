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


FUNCTION QDragMoveEvent( ... )
   RETURN HB_QDragMoveEvent():new( ... )

FUNCTION QDragMoveEventFromPointer( ... )
   RETURN HB_QDragMoveEvent():fromPointer( ... )


CREATE CLASS QDragMoveEvent INHERIT HbQtObjectHandler, HB_QDropEvent FUNCTION HB_QDragMoveEvent

   METHOD  new( ... )

   METHOD  accept                        // ( oQRect )                                         -> NIL
                                         // (  )                                               -> NIL
   METHOD  answerRect                    // (  )                                               -> oQRect
   METHOD  ignore                        // ( oQRect )                                         -> NIL
                                         // (  )                                               -> NIL

   ENDCLASS


METHOD QDragMoveEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDragMoveEvent( ... )
   RETURN Self


METHOD QDragMoveEvent:accept( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDragMoveEvent_accept( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDragMoveEvent_accept_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDragMoveEvent:answerRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QDragMoveEvent_answerRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDragMoveEvent:ignore( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDragMoveEvent_ignore( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDragMoveEvent_ignore_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

