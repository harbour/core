/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QStyle INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QStyle_destroy( ::pPtr )

   METHOD  combinedLayoutSpacing( nControls1, nControls2, nOrientation, pOption, pWidget )  INLINE  Qt_QStyle_combinedLayoutSpacing( ::pPtr, nControls1, nControls2, nOrientation, pOption, pWidget )
   METHOD  drawComplexControl( nControl, pOption, pPainter, pWidget )  INLINE  Qt_QStyle_drawComplexControl( ::pPtr, nControl, pOption, pPainter, pWidget )
   METHOD  drawControl( nElement, pOption, pPainter, pWidget )  INLINE  Qt_QStyle_drawControl( ::pPtr, nElement, pOption, pPainter, pWidget )
   METHOD  drawItemPixmap( pPainter, pRectangle, nAlignment, pPixmap )  INLINE  Qt_QStyle_drawItemPixmap( ::pPtr, pPainter, pRectangle, nAlignment, pPixmap )
   METHOD  drawItemText( pPainter, pRectangle, nAlignment, pPalette, lEnabled, cText, nTextRole )  INLINE  Qt_QStyle_drawItemText( ::pPtr, pPainter, pRectangle, nAlignment, pPalette, lEnabled, cText, nTextRole )
   METHOD  drawPrimitive( nElement, pOption, pPainter, pWidget )  INLINE  Qt_QStyle_drawPrimitive( ::pPtr, nElement, pOption, pPainter, pWidget )
   METHOD  generatedIconPixmap( nIconMode, pPixmap, pOption )  INLINE  Qt_QStyle_generatedIconPixmap( ::pPtr, nIconMode, pPixmap, pOption )
   METHOD  hitTestComplexControl( nControl, pOption, pPosition, pWidget )  INLINE  Qt_QStyle_hitTestComplexControl( ::pPtr, nControl, pOption, pPosition, pWidget )
   METHOD  itemPixmapRect( pRectangle, nAlignment, pPixmap )  INLINE  Qt_QStyle_itemPixmapRect( ::pPtr, pRectangle, nAlignment, pPixmap )
   METHOD  itemTextRect( pMetrics, pRectangle, nAlignment, lEnabled, cText )  INLINE  Qt_QStyle_itemTextRect( ::pPtr, pMetrics, pRectangle, nAlignment, lEnabled, cText )
   METHOD  layoutSpacing( nControl1, nControl2, nOrientation, pOption, pWidget )  INLINE  Qt_QStyle_layoutSpacing( ::pPtr, nControl1, nControl2, nOrientation, pOption, pWidget )
   METHOD  pixelMetric( nMetric, pOption, pWidget )  INLINE  Qt_QStyle_pixelMetric( ::pPtr, nMetric, pOption, pWidget )
   METHOD  polish( pWidget )                   INLINE  Qt_QStyle_polish( ::pPtr, pWidget )
   METHOD  polish_1( pApplication )            INLINE  Qt_QStyle_polish_1( ::pPtr, pApplication )
   METHOD  polish_2( pPalette )                INLINE  Qt_QStyle_polish_2( ::pPtr, pPalette )
   METHOD  sizeFromContents( nType, pOption, pContentsSize, pWidget )  INLINE  Qt_QStyle_sizeFromContents( ::pPtr, nType, pOption, pContentsSize, pWidget )
   METHOD  standardIcon( nStandardIcon, pOption, pWidget )  INLINE  Qt_QStyle_standardIcon( ::pPtr, nStandardIcon, pOption, pWidget )
   METHOD  standardPalette()                   INLINE  Qt_QStyle_standardPalette( ::pPtr )
   METHOD  styleHint( nHint, pOption, pWidget, pReturnData )  INLINE  Qt_QStyle_styleHint( ::pPtr, nHint, pOption, pWidget, pReturnData )
   METHOD  subControlRect( nControl, pOption, nSubControl, pWidget )  INLINE  Qt_QStyle_subControlRect( ::pPtr, nControl, pOption, nSubControl, pWidget )
   METHOD  subElementRect( nElement, pOption, pWidget )  INLINE  Qt_QStyle_subElementRect( ::pPtr, nElement, pOption, pWidget )
   METHOD  unpolish( pWidget )                 INLINE  Qt_QStyle_unpolish( ::pPtr, pWidget )
   METHOD  unpolish_1( pApplication )          INLINE  Qt_QStyle_unpolish_1( ::pPtr, pApplication )
   METHOD  alignedRect( nDirection, nAlignment, pSize, pRectangle )  INLINE  Qt_QStyle_alignedRect( ::pPtr, nDirection, nAlignment, pSize, pRectangle )
   METHOD  sliderPositionFromValue( nMin, nMax, nLogicalValue, nSpan, lUpsideDown )  INLINE  Qt_QStyle_sliderPositionFromValue( ::pPtr, nMin, nMax, nLogicalValue, nSpan, lUpsideDown )
   METHOD  sliderValueFromPosition( nMin, nMax, nPosition, nSpan, lUpsideDown )  INLINE  Qt_QStyle_sliderValueFromPosition( ::pPtr, nMin, nMax, nPosition, nSpan, lUpsideDown )
   METHOD  visualAlignment( nDirection, nAlignment )  INLINE  Qt_QStyle_visualAlignment( ::pPtr, nDirection, nAlignment )
   METHOD  visualPos( nDirection, pBoundingRectangle, pLogicalPosition )  INLINE  Qt_QStyle_visualPos( ::pPtr, nDirection, pBoundingRectangle, pLogicalPosition )
   METHOD  visualRect( nDirection, pBoundingRectangle, pLogicalRectangle )  INLINE  Qt_QStyle_visualRect( ::pPtr, nDirection, pBoundingRectangle, pLogicalRectangle )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QStyle

   ::pParent := pParent

   ::pPtr := Qt_QStyle( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QStyle

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
