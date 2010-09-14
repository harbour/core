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


FUNCTION QPaintEngine( ... )
   RETURN HB_QPaintEngine():new( ... )


CREATE CLASS QPaintEngine INHERIT HbQtObjectHandler FUNCTION HB_QPaintEngine

   METHOD  new( ... )

   METHOD  begin( pPdev )
   METHOD  drawEllipse( pRect )
   METHOD  drawEllipse_1( pRect )
   METHOD  drawImage( pRectangle, pImage, pSr, nFlags )
   METHOD  drawLines( pLines, nLineCount )
   METHOD  drawLines_1( pLines, nLineCount )
   METHOD  drawPath( pPath )
   METHOD  drawPixmap( pR, pPm, pSr )
   METHOD  drawPoints( pPoints, nPointCount )
   METHOD  drawPoints_1( pPoints, nPointCount )
   METHOD  drawPolygon( pPoints, nPointCount, nMode )
   METHOD  drawPolygon_1( pPoints, nPointCount, nMode )
   METHOD  drawRects( pRects, nRectCount )
   METHOD  drawRects_1( pRects, nRectCount )
   METHOD  drawTextItem( pP, pTextItem )
   METHOD  drawTiledPixmap( pRect, pPixmap, pP )
   METHOD  end()
   METHOD  hasFeature( nFeature )
   METHOD  isActive()
   METHOD  paintDevice()
   METHOD  painter()
   METHOD  setActive( lState )
   METHOD  type()

   ENDCLASS


METHOD QPaintEngine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPaintEngine( ... )
   RETURN Self


METHOD QPaintEngine:begin( pPdev )
   RETURN Qt_QPaintEngine_begin( ::pPtr, hbqt_ptr( pPdev ) )


METHOD QPaintEngine:drawEllipse( pRect )
   RETURN Qt_QPaintEngine_drawEllipse( ::pPtr, hbqt_ptr( pRect ) )


METHOD QPaintEngine:drawEllipse_1( pRect )
   RETURN Qt_QPaintEngine_drawEllipse_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QPaintEngine:drawImage( pRectangle, pImage, pSr, nFlags )
   RETURN Qt_QPaintEngine_drawImage( ::pPtr, hbqt_ptr( pRectangle ), hbqt_ptr( pImage ), hbqt_ptr( pSr ), nFlags )


METHOD QPaintEngine:drawLines( pLines, nLineCount )
   RETURN Qt_QPaintEngine_drawLines( ::pPtr, hbqt_ptr( pLines ), nLineCount )


METHOD QPaintEngine:drawLines_1( pLines, nLineCount )
   RETURN Qt_QPaintEngine_drawLines_1( ::pPtr, hbqt_ptr( pLines ), nLineCount )


METHOD QPaintEngine:drawPath( pPath )
   RETURN Qt_QPaintEngine_drawPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QPaintEngine:drawPixmap( pR, pPm, pSr )
   RETURN Qt_QPaintEngine_drawPixmap( ::pPtr, hbqt_ptr( pR ), hbqt_ptr( pPm ), hbqt_ptr( pSr ) )


METHOD QPaintEngine:drawPoints( pPoints, nPointCount )
   RETURN Qt_QPaintEngine_drawPoints( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPaintEngine:drawPoints_1( pPoints, nPointCount )
   RETURN Qt_QPaintEngine_drawPoints_1( ::pPtr, hbqt_ptr( pPoints ), nPointCount )


METHOD QPaintEngine:drawPolygon( pPoints, nPointCount, nMode )
   RETURN Qt_QPaintEngine_drawPolygon( ::pPtr, hbqt_ptr( pPoints ), nPointCount, nMode )


METHOD QPaintEngine:drawPolygon_1( pPoints, nPointCount, nMode )
   RETURN Qt_QPaintEngine_drawPolygon_1( ::pPtr, hbqt_ptr( pPoints ), nPointCount, nMode )


METHOD QPaintEngine:drawRects( pRects, nRectCount )
   RETURN Qt_QPaintEngine_drawRects( ::pPtr, hbqt_ptr( pRects ), nRectCount )


METHOD QPaintEngine:drawRects_1( pRects, nRectCount )
   RETURN Qt_QPaintEngine_drawRects_1( ::pPtr, hbqt_ptr( pRects ), nRectCount )


METHOD QPaintEngine:drawTextItem( pP, pTextItem )
   RETURN Qt_QPaintEngine_drawTextItem( ::pPtr, hbqt_ptr( pP ), hbqt_ptr( pTextItem ) )


METHOD QPaintEngine:drawTiledPixmap( pRect, pPixmap, pP )
   RETURN Qt_QPaintEngine_drawTiledPixmap( ::pPtr, hbqt_ptr( pRect ), hbqt_ptr( pPixmap ), hbqt_ptr( pP ) )


METHOD QPaintEngine:end()
   RETURN Qt_QPaintEngine_end( ::pPtr )


METHOD QPaintEngine:hasFeature( nFeature )
   RETURN Qt_QPaintEngine_hasFeature( ::pPtr, nFeature )


METHOD QPaintEngine:isActive()
   RETURN Qt_QPaintEngine_isActive( ::pPtr )


METHOD QPaintEngine:paintDevice()
   RETURN Qt_QPaintEngine_paintDevice( ::pPtr )


METHOD QPaintEngine:painter()
   RETURN Qt_QPaintEngine_painter( ::pPtr )


METHOD QPaintEngine:setActive( lState )
   RETURN Qt_QPaintEngine_setActive( ::pPtr, lState )


METHOD QPaintEngine:type()
   RETURN Qt_QPaintEngine_type( ::pPtr )

