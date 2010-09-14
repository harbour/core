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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION HBQGraphicsItem( ... )
   RETURN HB_HBQGraphicsItem():new( ... )


CREATE CLASS HBQGraphicsItem INHERIT HbQtObjectHandler, HB_QGraphicsItem FUNCTION HB_HBQGraphicsItem

   METHOD  new( ... )

   METHOD  hbSetBlock( xBlock )
   METHOD  boundingRect()
   METHOD  paint( pPainter, pOption, pWidget )
   METHOD  determineResizeMode( pPos )
   METHOD  adjustRect( pRect )
   METHOD  prepare( pPainter )
   METHOD  pen()
   METHOD  setPen( pPen )
   METHOD  brush()
   METHOD  setBrush( pBrush )
   METHOD  backgroundBrush()
   METHOD  setBackgroundBrush( pBrush )
   METHOD  font()
   METHOD  setFont( pFont )
   METHOD  lineStyle()
   METHOD  setLineStyle( nLineStyle )
   METHOD  startAngle()
   METHOD  setStartAngle( nStartAngle )
   METHOD  spanAngle()
   METHOD  setSpanAngle( nSpanAngle )
   METHOD  width()
   METHOD  setWidth( nWidth )
   METHOD  height()
   METHOD  setHeight( nHeight )
   METHOD  opacity()
   METHOD  setOpacity( nOpacity )
   METHOD  geometry()
   METHOD  setGeometry( pRect )
   METHOD  objectType()
   METHOD  setObjectType( cType )
   METHOD  objectName()
   METHOD  setObjectName( cName )
   METHOD  text()
   METHOD  setText( cType )
   METHOD  paintType()
   METHOD  setPaintType( nPaintType )
   METHOD  frameType()
   METHOD  setFrameType( nFrameType )
   METHOD  drawTextType()
   METHOD  setDrawTextType( nDrawTextType )
   METHOD  pixmap()
   METHOD  setPixmap( pPixmap )
   METHOD  textColor()
   METHOD  setTextColor( pColor )
   METHOD  borderWidth()
   METHOD  setBorderWidth( nBWidth )
   METHOD  borderColor()
   METHOD  setBorderColor( pColor )
   METHOD  sizePolicy()
   METHOD  setSizePolicy( nSizePolicy )
   METHOD  textFlags()
   METHOD  setTextFlags( nTextFlags )
   METHOD  resizeFlags()
   METHOD  setResizeFlags( nResizeFlags )
   METHOD  resizeHandle()
   METHOD  setResizeHandle( nResizeHandle )
   METHOD  barsIdentation()
   METHOD  setBarsIdentation( nBarsIdentation )
   METHOD  drawBorder()
   METHOD  setDrawBorder( lDrawBorder )
   METHOD  showGrid()
   METHOD  setShowGrid( lShowGrid )
   METHOD  showLabels()
   METHOD  setShowLabels( lShowLabels )
   METHOD  toColorFactor()
   METHOD  setToColorFactor( nToColorFactor )
   METHOD  setBarValues( pList )
   METHOD  setLegendColorRectWidth( nLegendColorRectWidth )

   ENDCLASS


METHOD HBQGraphicsItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQGraphicsItem( ... )
   RETURN Self


METHOD HBQGraphicsItem:hbSetBlock( xBlock )
   RETURN Qt_HBQGraphicsItem_hbSetBlock( ::pPtr, xBlock )


METHOD HBQGraphicsItem:boundingRect()
   RETURN Qt_HBQGraphicsItem_boundingRect( ::pPtr )


METHOD HBQGraphicsItem:paint( pPainter, pOption, pWidget )
   RETURN Qt_HBQGraphicsItem_paint( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pOption ), hbqt_ptr( pWidget ) )


METHOD HBQGraphicsItem:determineResizeMode( pPos )
   RETURN Qt_HBQGraphicsItem_determineResizeMode( ::pPtr, hbqt_ptr( pPos ) )


METHOD HBQGraphicsItem:adjustRect( pRect )
   RETURN Qt_HBQGraphicsItem_adjustRect( ::pPtr, hbqt_ptr( pRect ) )


METHOD HBQGraphicsItem:prepare( pPainter )
   RETURN Qt_HBQGraphicsItem_prepare( ::pPtr, hbqt_ptr( pPainter ) )


METHOD HBQGraphicsItem:pen()
   RETURN Qt_HBQGraphicsItem_pen( ::pPtr )


METHOD HBQGraphicsItem:setPen( pPen )
   RETURN Qt_HBQGraphicsItem_setPen( ::pPtr, hbqt_ptr( pPen ) )


METHOD HBQGraphicsItem:brush()
   RETURN Qt_HBQGraphicsItem_brush( ::pPtr )


METHOD HBQGraphicsItem:setBrush( pBrush )
   RETURN Qt_HBQGraphicsItem_setBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD HBQGraphicsItem:backgroundBrush()
   RETURN Qt_HBQGraphicsItem_backgroundBrush( ::pPtr )


METHOD HBQGraphicsItem:setBackgroundBrush( pBrush )
   RETURN Qt_HBQGraphicsItem_setBackgroundBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD HBQGraphicsItem:font()
   RETURN Qt_HBQGraphicsItem_font( ::pPtr )


METHOD HBQGraphicsItem:setFont( pFont )
   RETURN Qt_HBQGraphicsItem_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD HBQGraphicsItem:lineStyle()
   RETURN Qt_HBQGraphicsItem_lineStyle( ::pPtr )


METHOD HBQGraphicsItem:setLineStyle( nLineStyle )
   RETURN Qt_HBQGraphicsItem_setLineStyle( ::pPtr, nLineStyle )


METHOD HBQGraphicsItem:startAngle()
   RETURN Qt_HBQGraphicsItem_startAngle( ::pPtr )


METHOD HBQGraphicsItem:setStartAngle( nStartAngle )
   RETURN Qt_HBQGraphicsItem_setStartAngle( ::pPtr, nStartAngle )


METHOD HBQGraphicsItem:spanAngle()
   RETURN Qt_HBQGraphicsItem_spanAngle( ::pPtr )


METHOD HBQGraphicsItem:setSpanAngle( nSpanAngle )
   RETURN Qt_HBQGraphicsItem_setSpanAngle( ::pPtr, nSpanAngle )


METHOD HBQGraphicsItem:width()
   RETURN Qt_HBQGraphicsItem_width( ::pPtr )


METHOD HBQGraphicsItem:setWidth( nWidth )
   RETURN Qt_HBQGraphicsItem_setWidth( ::pPtr, nWidth )


METHOD HBQGraphicsItem:height()
   RETURN Qt_HBQGraphicsItem_height( ::pPtr )


METHOD HBQGraphicsItem:setHeight( nHeight )
   RETURN Qt_HBQGraphicsItem_setHeight( ::pPtr, nHeight )


METHOD HBQGraphicsItem:opacity()
   RETURN Qt_HBQGraphicsItem_opacity( ::pPtr )


METHOD HBQGraphicsItem:setOpacity( nOpacity )
   RETURN Qt_HBQGraphicsItem_setOpacity( ::pPtr, nOpacity )


METHOD HBQGraphicsItem:geometry()
   RETURN Qt_HBQGraphicsItem_geometry( ::pPtr )


METHOD HBQGraphicsItem:setGeometry( pRect )
   RETURN Qt_HBQGraphicsItem_setGeometry( ::pPtr, hbqt_ptr( pRect ) )


METHOD HBQGraphicsItem:objectType()
   RETURN Qt_HBQGraphicsItem_objectType( ::pPtr )


METHOD HBQGraphicsItem:setObjectType( cType )
   RETURN Qt_HBQGraphicsItem_setObjectType( ::pPtr, cType )


METHOD HBQGraphicsItem:objectName()
   RETURN Qt_HBQGraphicsItem_objectName( ::pPtr )


METHOD HBQGraphicsItem:setObjectName( cName )
   RETURN Qt_HBQGraphicsItem_setObjectName( ::pPtr, cName )


METHOD HBQGraphicsItem:text()
   RETURN Qt_HBQGraphicsItem_text( ::pPtr )


METHOD HBQGraphicsItem:setText( cType )
   RETURN Qt_HBQGraphicsItem_setText( ::pPtr, cType )


METHOD HBQGraphicsItem:paintType()
   RETURN Qt_HBQGraphicsItem_paintType( ::pPtr )


METHOD HBQGraphicsItem:setPaintType( nPaintType )
   RETURN Qt_HBQGraphicsItem_setPaintType( ::pPtr, nPaintType )


METHOD HBQGraphicsItem:frameType()
   RETURN Qt_HBQGraphicsItem_frameType( ::pPtr )


METHOD HBQGraphicsItem:setFrameType( nFrameType )
   RETURN Qt_HBQGraphicsItem_setFrameType( ::pPtr, nFrameType )


METHOD HBQGraphicsItem:drawTextType()
   RETURN Qt_HBQGraphicsItem_drawTextType( ::pPtr )


METHOD HBQGraphicsItem:setDrawTextType( nDrawTextType )
   RETURN Qt_HBQGraphicsItem_setDrawTextType( ::pPtr, nDrawTextType )


METHOD HBQGraphicsItem:pixmap()
   RETURN Qt_HBQGraphicsItem_pixmap( ::pPtr )


METHOD HBQGraphicsItem:setPixmap( pPixmap )
   RETURN Qt_HBQGraphicsItem_setPixmap( ::pPtr, hbqt_ptr( pPixmap ) )


METHOD HBQGraphicsItem:textColor()
   RETURN Qt_HBQGraphicsItem_textColor( ::pPtr )


METHOD HBQGraphicsItem:setTextColor( pColor )
   RETURN Qt_HBQGraphicsItem_setTextColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD HBQGraphicsItem:borderWidth()
   RETURN Qt_HBQGraphicsItem_borderWidth( ::pPtr )


METHOD HBQGraphicsItem:setBorderWidth( nBWidth )
   RETURN Qt_HBQGraphicsItem_setBorderWidth( ::pPtr, nBWidth )


METHOD HBQGraphicsItem:borderColor()
   RETURN Qt_HBQGraphicsItem_borderColor( ::pPtr )


METHOD HBQGraphicsItem:setBorderColor( pColor )
   RETURN Qt_HBQGraphicsItem_setBorderColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD HBQGraphicsItem:sizePolicy()
   RETURN Qt_HBQGraphicsItem_sizePolicy( ::pPtr )


METHOD HBQGraphicsItem:setSizePolicy( nSizePolicy )
   RETURN Qt_HBQGraphicsItem_setSizePolicy( ::pPtr, nSizePolicy )


METHOD HBQGraphicsItem:textFlags()
   RETURN Qt_HBQGraphicsItem_textFlags( ::pPtr )


METHOD HBQGraphicsItem:setTextFlags( nTextFlags )
   RETURN Qt_HBQGraphicsItem_setTextFlags( ::pPtr, nTextFlags )


METHOD HBQGraphicsItem:resizeFlags()
   RETURN Qt_HBQGraphicsItem_resizeFlags( ::pPtr )


METHOD HBQGraphicsItem:setResizeFlags( nResizeFlags )
   RETURN Qt_HBQGraphicsItem_setResizeFlags( ::pPtr, nResizeFlags )


METHOD HBQGraphicsItem:resizeHandle()
   RETURN Qt_HBQGraphicsItem_resizeHandle( ::pPtr )


METHOD HBQGraphicsItem:setResizeHandle( nResizeHandle )
   RETURN Qt_HBQGraphicsItem_setResizeHandle( ::pPtr, nResizeHandle )


METHOD HBQGraphicsItem:barsIdentation()
   RETURN Qt_HBQGraphicsItem_barsIdentation( ::pPtr )


METHOD HBQGraphicsItem:setBarsIdentation( nBarsIdentation )
   RETURN Qt_HBQGraphicsItem_setBarsIdentation( ::pPtr, nBarsIdentation )


METHOD HBQGraphicsItem:drawBorder()
   RETURN Qt_HBQGraphicsItem_drawBorder( ::pPtr )


METHOD HBQGraphicsItem:setDrawBorder( lDrawBorder )
   RETURN Qt_HBQGraphicsItem_setDrawBorder( ::pPtr, lDrawBorder )


METHOD HBQGraphicsItem:showGrid()
   RETURN Qt_HBQGraphicsItem_showGrid( ::pPtr )


METHOD HBQGraphicsItem:setShowGrid( lShowGrid )
   RETURN Qt_HBQGraphicsItem_setShowGrid( ::pPtr, lShowGrid )


METHOD HBQGraphicsItem:showLabels()
   RETURN Qt_HBQGraphicsItem_showLabels( ::pPtr )


METHOD HBQGraphicsItem:setShowLabels( lShowLabels )
   RETURN Qt_HBQGraphicsItem_setShowLabels( ::pPtr, lShowLabels )


METHOD HBQGraphicsItem:toColorFactor()
   RETURN Qt_HBQGraphicsItem_toColorFactor( ::pPtr )


METHOD HBQGraphicsItem:setToColorFactor( nToColorFactor )
   RETURN Qt_HBQGraphicsItem_setToColorFactor( ::pPtr, nToColorFactor )


METHOD HBQGraphicsItem:setBarValues( pList )
   RETURN Qt_HBQGraphicsItem_setBarValues( ::pPtr, hbqt_ptr( pList ) )


METHOD HBQGraphicsItem:setLegendColorRectWidth( nLegendColorRectWidth )
   RETURN Qt_HBQGraphicsItem_setLegendColorRectWidth( ::pPtr, nLegendColorRectWidth )

