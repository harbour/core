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


FUNCTION QStyleOption( ... )
   RETURN HB_QStyleOption():new( ... )

FUNCTION QStyleOptionFromPointer( ... )
   RETURN HB_QStyleOption():fromPointer( ... )


CREATE CLASS QStyleOption INHERIT HbQtObjectHandler FUNCTION HB_QStyleOption

   METHOD  new( ... )

   METHOD  initFrom                      // ( oQWidget )                                       -> NIL
   METHOD  direction                     // (  )                                               -> nQt_LayoutDirection
   METHOD  fontMetrics                   // (  )                                               -> oQFontMetrics
   METHOD  palette                       // (  )                                               -> oQPalette
   METHOD  rect                          // (  )                                               -> oQRect
   METHOD  state                         // (  )                                               -> nQStyle_State
   METHOD  type                          // (  )                                               -> nInt
   METHOD  version                       // (  )                                               -> nInt

   ENDCLASS


METHOD QStyleOption:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOption( ... )
   RETURN Self


METHOD QStyleOption:initFrom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStyleOption_initFrom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOption:direction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOption_direction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOption:fontMetrics( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontMetricsFromPointer( Qt_QStyleOption_fontMetrics( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOption:palette( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaletteFromPointer( Qt_QStyleOption_palette( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOption:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QStyleOption_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOption:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOption_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOption:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOption_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOption:version( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOption_version( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

