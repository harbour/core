/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION HBQGraphicsItem( ... )
   RETURN HB_HBQGraphicsItem():new( ... )

FUNCTION HBQGraphicsItemFrom( ... )
   RETURN HB_HBQGraphicsItem():from( ... )

FUNCTION HBQGraphicsItemFromPointer( ... )
   RETURN HB_HBQGraphicsItem():fromPointer( ... )


CREATE CLASS HBQGraphicsItem INHERIT HbQtObjectHandler, HB_QGraphicsItem FUNCTION HB_HBQGraphicsItem

   METHOD  new( ... )

   METHOD  hbSetBlock                    // ( xBlock )                                         -> NIL
   METHOD  boundingRect                  // (  )                                               -> oQRectF
   METHOD  paint                         // ( oQPainter, oQStyleOptionGraphicsItem, oQWidget ) -> NIL
   METHOD  determineResizeMode           // ( oQPointF )                                       -> nInt
   METHOD  adjustRect                    // ( oQRectF )                                        -> oQRectF
   METHOD  prepare                       // ( oQPainter )                                      -> NIL
   METHOD  pen                           // (  )                                               -> oQPen
   METHOD  setPen                        // ( oQPen )                                          -> NIL
   METHOD  brush                         // (  )                                               -> oQBrush
   METHOD  setBrush                      // ( oQBrush )                                        -> NIL
   METHOD  backgroundBrush               // (  )                                               -> oQBrush
   METHOD  setBackgroundBrush            // ( oQBrush )                                        -> NIL
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  lineStyle                     // (  )                                               -> nInt
   METHOD  setLineStyle                  // ( nLineStyle )                                     -> NIL
   METHOD  startAngle                    // (  )                                               -> nInt
   METHOD  setStartAngle                 // ( nStartAngle )                                    -> NIL
   METHOD  spanAngle                     // (  )                                               -> nInt
   METHOD  setSpanAngle                  // ( nSpanAngle )                                     -> NIL
   METHOD  width                         // (  )                                               -> nQreal
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  setHeight                     // ( nHeight )                                        -> NIL
   METHOD  opacity                       // (  )                                               -> nInt
   METHOD  setOpacity                    // ( nOpacity )                                       -> NIL
   METHOD  geometry                      // (  )                                               -> oQRectF
   METHOD  setGeometry                   // ( oQRectF )                                        -> NIL
   METHOD  objectType                    // (  )                                               -> cQString
   METHOD  setObjectType                 // ( cType )                                          -> NIL
   METHOD  objectName                    // (  )                                               -> cQString
   METHOD  setObjectName                 // ( cName )                                          -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  setText                       // ( cType )                                          -> NIL
   METHOD  paintType                     // (  )                                               -> nInt
   METHOD  setPaintType                  // ( nPaintType )                                     -> NIL
   METHOD  frameType                     // (  )                                               -> nInt
   METHOD  setFrameType                  // ( nFrameType )                                     -> NIL
   METHOD  drawTextType                  // (  )                                               -> nInt
   METHOD  setDrawTextType               // ( nDrawTextType )                                  -> NIL
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  setPixmap                     // ( oQPixmap )                                       -> NIL
   METHOD  textColor                     // (  )                                               -> oQColor
   METHOD  setTextColor                  // ( oQColor )                                        -> NIL
   METHOD  borderWidth                   // (  )                                               -> nInt
   METHOD  setBorderWidth                // ( nBWidth )                                        -> NIL
   METHOD  borderColor                   // (  )                                               -> oQColor
   METHOD  setBorderColor                // ( oQColor )                                        -> NIL
   METHOD  sizePolicy                    // (  )                                               -> nInt
   METHOD  setSizePolicy                 // ( nSizePolicy )                                    -> NIL
   METHOD  textFlags                     // (  )                                               -> nInt
   METHOD  setTextFlags                  // ( nTextFlags )                                     -> NIL
   METHOD  resizeFlags                   // (  )                                               -> nInt
   METHOD  setResizeFlags                // ( nResizeFlags )                                   -> NIL
   METHOD  resizeHandle                  // (  )                                               -> nInt
   METHOD  setResizeHandle               // ( nResizeHandle )                                  -> NIL
   METHOD  barsIdentation                // (  )                                               -> nInt
   METHOD  setBarsIdentation             // ( nBarsIdentation )                                -> NIL
   METHOD  drawBorder                    // (  )                                               -> lBool
   METHOD  setDrawBorder                 // ( lDrawBorder )                                    -> NIL
   METHOD  showGrid                      // (  )                                               -> lBool
   METHOD  setShowGrid                   // ( lShowGrid )                                      -> NIL
   METHOD  showLabels                    // (  )                                               -> lBool
   METHOD  setShowLabels                 // ( lShowLabels )                                    -> NIL
   METHOD  toColorFactor                 // (  )                                               -> nQreal
   METHOD  setToColorFactor              // ( nToColorFactor )                                 -> NIL
   METHOD  setBarValues                  // ( oQStringList )                                   -> NIL
   METHOD  setLegendColorRectWidth       // ( nLegendColorRectWidth )                          -> NIL

   ENDCLASS


METHOD HBQGraphicsItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQGraphicsItem( ... )
   RETURN Self


METHOD HBQGraphicsItem:hbSetBlock( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE (  hb_pvalue( 1 ) != NIL )
         RETURN Qt_HBQGraphicsItem_hbSetBlock( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_HBQGraphicsItem_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:paint( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_HBQGraphicsItem_paint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_HBQGraphicsItem_paint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:determineResizeMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_determineResizeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:adjustRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_HBQGraphicsItem_adjustRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:prepare( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_prepare( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:pen( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPenFromPointer( Qt_HBQGraphicsItem_pen( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setPen( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setPen( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:brush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_HBQGraphicsItem_brush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:backgroundBrush( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_HBQGraphicsItem_backgroundBrush( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setBackgroundBrush( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setBackgroundBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_HBQGraphicsItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:lineStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_lineStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setLineStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setLineStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:startAngle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_startAngle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setStartAngle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setStartAngle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:spanAngle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_spanAngle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setSpanAngle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setSpanAngle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:opacity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_opacity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setOpacity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setOpacity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:geometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_HBQGraphicsItem_geometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:objectType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_objectType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setObjectType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setObjectType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:objectName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_objectName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setObjectName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setObjectName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:paintType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_paintType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setPaintType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setPaintType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:frameType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_frameType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setFrameType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setFrameType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:drawTextType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_drawTextType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setDrawTextType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setDrawTextType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_HBQGraphicsItem_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:textColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_HBQGraphicsItem_textColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setTextColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setTextColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:borderWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_borderWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setBorderWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setBorderWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:borderColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_HBQGraphicsItem_borderColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setBorderColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setBorderColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:sizePolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_sizePolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setSizePolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setSizePolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:textFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_textFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setTextFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setTextFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:resizeFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_resizeFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setResizeFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setResizeFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:resizeHandle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_resizeHandle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setResizeHandle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setResizeHandle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:barsIdentation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_barsIdentation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setBarsIdentation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setBarsIdentation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:drawBorder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_drawBorder( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setDrawBorder( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setDrawBorder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:showGrid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_showGrid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setShowGrid( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setShowGrid( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:showLabels( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_showLabels( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setShowLabels( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setShowLabels( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:toColorFactor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsItem_toColorFactor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setToColorFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setToColorFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setBarValues( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setBarValues( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsItem:setLegendColorRectWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsItem_setLegendColorRectWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

