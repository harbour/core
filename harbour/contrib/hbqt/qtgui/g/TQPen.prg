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


FUNCTION QPen( ... )
   RETURN HB_QPen():new( ... )

FUNCTION QPenFromPointer( ... )
   RETURN HB_QPen():fromPointer( ... )


CREATE CLASS QPen INHERIT HbQtObjectHandler FUNCTION HB_QPen

   METHOD  new( ... )

   METHOD  brush                         // (  )                                               -> oQBrush
   METHOD  capStyle                      // (  )                                               -> nQt_PenCapStyle
   METHOD  color                         // (  )                                               -> oQColor
   METHOD  dashOffset                    // (  )                                               -> nQreal
   METHOD  isCosmetic                    // (  )                                               -> lBool
   METHOD  isSolid                       // (  )                                               -> lBool
   METHOD  joinStyle                     // (  )                                               -> nQt_PenJoinStyle
   METHOD  miterLimit                    // (  )                                               -> nQreal
   METHOD  setBrush                      // ( oQBrush )                                        -> NIL
   METHOD  setCapStyle                   // ( nStyle )                                         -> NIL
   METHOD  setColor                      // ( oQColor )                                        -> NIL
   METHOD  setCosmetic                   // ( lCosmetic )                                      -> NIL
   METHOD  setDashOffset                 // ( nOffset )                                        -> NIL
   METHOD  setJoinStyle                  // ( nStyle )                                         -> NIL
   METHOD  setMiterLimit                 // ( nLimit )                                         -> NIL
   METHOD  setStyle                      // ( nStyle )                                         -> NIL
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  setWidthF                     // ( nWidth )                                         -> NIL
   METHOD  style                         // (  )                                               -> nQt_PenStyle
   METHOD  width                         // (  )                                               -> nInt
   METHOD  widthF                        // (  )                                               -> nQreal

   ENDCLASS


METHOD QPen:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPen( ... )
   RETURN Self


METHOD QPen:brush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPen_brush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:capStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_capStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:color( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QPen_color( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:dashOffset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_dashOffset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:isCosmetic( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_isCosmetic( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:isSolid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_isSolid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:joinStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_joinStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:miterLimit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_miterLimit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setCapStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setCapStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setCosmetic( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setCosmetic( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setDashOffset( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setDashOffset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setJoinStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setJoinStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setMiterLimit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setMiterLimit( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:setWidthF( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPen_setWidthF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_style( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPen:widthF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPen_widthF( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

