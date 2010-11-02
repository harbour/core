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


FUNCTION QGraphicsProxyWidget( ... )
   RETURN HB_QGraphicsProxyWidget():new( ... )

FUNCTION QGraphicsProxyWidgetFromPointer( ... )
   RETURN HB_QGraphicsProxyWidget():fromPointer( ... )


CREATE CLASS QGraphicsProxyWidget INHERIT HbQtObjectHandler, HB_QGraphicsWidget FUNCTION HB_QGraphicsProxyWidget

   METHOD  new( ... )

   METHOD  createProxyForChildWidget     // ( oQWidget )                                       -> oQGraphicsProxyWidget
   METHOD  setWidget                     // ( oQWidget )                                       -> NIL
   METHOD  subWidgetRect                 // ( oQWidget )                                       -> oQRectF
   METHOD  widget                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QGraphicsProxyWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsProxyWidget( ... )
   RETURN Self


METHOD QGraphicsProxyWidget:createProxyForChildWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QGraphicsProxyWidgetFromPointer( Qt_QGraphicsProxyWidget_createProxyForChildWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsProxyWidget:setWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsProxyWidget_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsProxyWidget:subWidgetRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QGraphicsProxyWidget_subWidgetRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsProxyWidget:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QGraphicsProxyWidget_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

