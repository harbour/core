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


FUNCTION QStylePainter( ... )
   RETURN HB_QStylePainter():new( ... )

FUNCTION QStylePainterFromPointer( ... )
   RETURN HB_QStylePainter():fromPointer( ... )


CREATE CLASS QStylePainter INHERIT HbQtObjectHandler, HB_QPainter FUNCTION HB_QStylePainter

   METHOD  new( ... )

   METHOD  begin                         // ( oQWidget )                                       -> lBool
                                         // ( oQPaintDevice, oQWidget )                        -> lBool
   METHOD  drawComplexControl            // ( nCc, oQStyleOptionComplex )                      -> NIL
   METHOD  drawControl                   // ( nCe, oQStyleOption )                             -> NIL
   METHOD  drawItemPixmap                // ( oQRect, nFlags, oQPixmap )                       -> NIL
   METHOD  drawItemText                  // ( oQRect, nFlags, oQPalette, lEnabled, cText, nTextRole ) -> NIL
   METHOD  drawPrimitive                 // ( nPe, oQStyleOption )                             -> NIL
   METHOD  style                         // (  )                                               -> oQStyle

   ENDCLASS


METHOD QStylePainter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStylePainter( ... )
   RETURN Self


METHOD QStylePainter:begin( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStylePainter_begin_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStylePainter_begin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStylePainter:drawComplexControl( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStylePainter_drawComplexControl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStylePainter:drawControl( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStylePainter_drawControl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStylePainter:drawItemPixmap( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStylePainter_drawItemPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStylePainter:drawItemText( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isLogical( hb_pvalue( 4 ) ) .AND. hb_isChar( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QStylePainter_drawItemText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isLogical( hb_pvalue( 4 ) ) .AND. hb_isChar( hb_pvalue( 5 ) )
         RETURN Qt_QStylePainter_drawItemText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStylePainter:drawPrimitive( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStylePainter_drawPrimitive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStylePainter:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStyleFromPointer( Qt_QStylePainter_style( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

