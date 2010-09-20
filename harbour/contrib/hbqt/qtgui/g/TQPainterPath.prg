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


FUNCTION QPainterPath( ... )
   RETURN HB_QPainterPath():new( ... )


CREATE CLASS QPainterPath INHERIT HbQtObjectHandler FUNCTION HB_QPainterPath

   METHOD  new( ... )

   METHOD  addEllipse( ... )
   METHOD  addPath( pPath )
   METHOD  addPolygon( pPolygon )
   METHOD  addRect( ... )
   METHOD  addRegion( pRegion )
   METHOD  addRoundedRect( ... )
   METHOD  addText( ... )
   METHOD  angleAtPercent( nT )
   METHOD  arcMoveTo( ... )
   METHOD  arcTo( ... )
   METHOD  boundingRect()
   METHOD  closeSubpath()
   METHOD  connectPath( pPath )
   METHOD  contains( ... )
   METHOD  controlPointRect()
   METHOD  cubicTo( ... )
   METHOD  currentPosition()
   METHOD  elementCount()
   METHOD  fillRule()
   METHOD  intersected( pP )
   METHOD  intersects( ... )
   METHOD  isEmpty()
   METHOD  length()
   METHOD  lineTo( ... )
   METHOD  moveTo( ... )
   METHOD  percentAtLength( nLen )
   METHOD  pointAtPercent( nT )
   METHOD  quadTo( ... )
   METHOD  setElementPositionAt( nIndex, nX, nY )
   METHOD  setFillRule( nFillRule )
   METHOD  simplified()
   METHOD  slopeAtPercent( nT )
   METHOD  subtracted( pP )
   METHOD  toFillPolygon( pMatrix )
   METHOD  toFillPolygon_1( pMatrix )
   METHOD  toFillPolygons( pMatrix )
   METHOD  toFillPolygons_1( pMatrix )
   METHOD  toReversed()
   METHOD  toSubpathPolygons( pMatrix )
   METHOD  toSubpathPolygons_1( pMatrix )
   METHOD  united( pP )

   ENDCLASS


METHOD QPainterPath:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPainterPath( ... )
   RETURN Self


METHOD QPainterPath:addEllipse( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_addEllipse( ::pPtr, ... )


METHOD QPainterPath:addPath( pPath )
   RETURN Qt_QPainterPath_addPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:addPolygon( pPolygon )
   RETURN Qt_QPainterPath_addPolygon( ::pPtr, hbqt_ptr( pPolygon ) )


METHOD QPainterPath:addRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_addRect( ::pPtr, ... )


METHOD QPainterPath:addRegion( pRegion )
   RETURN Qt_QPainterPath_addRegion( ::pPtr, hbqt_ptr( pRegion ) )


METHOD QPainterPath:addRoundedRect( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_addRoundedRect( ::pPtr, ... )


METHOD QPainterPath:addText( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_addText( ::pPtr, ... )


METHOD QPainterPath:angleAtPercent( nT )
   RETURN Qt_QPainterPath_angleAtPercent( ::pPtr, nT )


METHOD QPainterPath:arcMoveTo( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_arcMoveTo( ::pPtr, ... )


METHOD QPainterPath:arcTo( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_arcTo( ::pPtr, ... )


METHOD QPainterPath:boundingRect()
   RETURN Qt_QPainterPath_boundingRect( ::pPtr )


METHOD QPainterPath:closeSubpath()
   RETURN Qt_QPainterPath_closeSubpath( ::pPtr )


METHOD QPainterPath:connectPath( pPath )
   RETURN Qt_QPainterPath_connectPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPainterPath:contains( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_contains( ::pPtr, ... )


METHOD QPainterPath:controlPointRect()
   RETURN Qt_QPainterPath_controlPointRect( ::pPtr )


METHOD QPainterPath:cubicTo( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_cubicTo( ::pPtr, ... )


METHOD QPainterPath:currentPosition()
   RETURN Qt_QPainterPath_currentPosition( ::pPtr )


METHOD QPainterPath:elementCount()
   RETURN Qt_QPainterPath_elementCount( ::pPtr )


METHOD QPainterPath:fillRule()
   RETURN Qt_QPainterPath_fillRule( ::pPtr )


METHOD QPainterPath:intersected( pP )
   RETURN Qt_QPainterPath_intersected( ::pPtr, hbqt_ptr( pP ) )


METHOD QPainterPath:intersects( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_intersects( ::pPtr, ... )


METHOD QPainterPath:isEmpty()
   RETURN Qt_QPainterPath_isEmpty( ::pPtr )


METHOD QPainterPath:length()
   RETURN Qt_QPainterPath_length( ::pPtr )


METHOD QPainterPath:lineTo( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_lineTo( ::pPtr, ... )


METHOD QPainterPath:moveTo( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_moveTo( ::pPtr, ... )


METHOD QPainterPath:percentAtLength( nLen )
   RETURN Qt_QPainterPath_percentAtLength( ::pPtr, nLen )


METHOD QPainterPath:pointAtPercent( nT )
   RETURN Qt_QPainterPath_pointAtPercent( ::pPtr, nT )


METHOD QPainterPath:quadTo( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QPainterPath_quadTo( ::pPtr, ... )


METHOD QPainterPath:setElementPositionAt( nIndex, nX, nY )
   RETURN Qt_QPainterPath_setElementPositionAt( ::pPtr, nIndex, nX, nY )


METHOD QPainterPath:setFillRule( nFillRule )
   RETURN Qt_QPainterPath_setFillRule( ::pPtr, nFillRule )


METHOD QPainterPath:simplified()
   RETURN Qt_QPainterPath_simplified( ::pPtr )


METHOD QPainterPath:slopeAtPercent( nT )
   RETURN Qt_QPainterPath_slopeAtPercent( ::pPtr, nT )


METHOD QPainterPath:subtracted( pP )
   RETURN Qt_QPainterPath_subtracted( ::pPtr, hbqt_ptr( pP ) )


METHOD QPainterPath:toFillPolygon( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygon( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toFillPolygon_1( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygon_1( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toFillPolygons( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygons( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toFillPolygons_1( pMatrix )
   RETURN Qt_QPainterPath_toFillPolygons_1( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toReversed()
   RETURN Qt_QPainterPath_toReversed( ::pPtr )


METHOD QPainterPath:toSubpathPolygons( pMatrix )
   RETURN Qt_QPainterPath_toSubpathPolygons( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:toSubpathPolygons_1( pMatrix )
   RETURN Qt_QPainterPath_toSubpathPolygons_1( ::pPtr, hbqt_ptr( pMatrix ) )


METHOD QPainterPath:united( pP )
   RETURN Qt_QPainterPath_united( ::pPtr, hbqt_ptr( pP ) )

