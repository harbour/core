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


FUNCTION QGraphicsLayout( ... )
   RETURN HB_QGraphicsLayout():new( ... )

FUNCTION QGraphicsLayoutFromPointer( ... )
   RETURN HB_QGraphicsLayout():fromPointer( ... )


CREATE CLASS QGraphicsLayout INHERIT HbQtObjectHandler, HB_QGraphicsLayoutItem FUNCTION HB_QGraphicsLayout

   METHOD  new( ... )

   METHOD  activate                      // (  )                                               -> NIL
   METHOD  count                         // (  )                                               -> nInt
   METHOD  invalidate                    // (  )                                               -> NIL
   METHOD  isActivated                   // (  )                                               -> lBool
   METHOD  itemAt                        // ( nI )                                             -> oQGraphicsLayoutItem
   METHOD  removeAt                      // ( nIndex )                                         -> NIL
   METHOD  setContentsMargins            // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  widgetEvent                   // ( oQEvent )                                        -> NIL

   ENDCLASS


METHOD QGraphicsLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsLayout( ... )
   RETURN Self


METHOD QGraphicsLayout:activate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayout_activate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayout:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayout_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayout:invalidate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayout_invalidate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayout:isActivated( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsLayout_isActivated( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayout:itemAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QGraphicsLayoutItemFromPointer( Qt_QGraphicsLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayout:removeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayout_removeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayout:setContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsLayout_setContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsLayout:widgetEvent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsLayout_widgetEvent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

