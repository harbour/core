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


FUNCTION QStyle( ... )
   RETURN HB_QStyle():new( ... )


CREATE CLASS QStyle INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QStyle

   METHOD  new( ... )

   METHOD  combinedLayoutSpacing( nControls1, nControls2, nOrientation, pOption, pWidget )
   METHOD  drawComplexControl( nControl, pOption, pPainter, pWidget )
   METHOD  drawControl( nElement, pOption, pPainter, pWidget )
   METHOD  drawItemPixmap( pPainter, pRectangle, nAlignment, pPixmap )
   METHOD  drawItemText( pPainter, pRectangle, nAlignment, pPalette, lEnabled, cText, nTextRole )
   METHOD  drawPrimitive( nElement, pOption, pPainter, pWidget )
   METHOD  generatedIconPixmap( nIconMode, pPixmap, pOption )
   METHOD  hitTestComplexControl( nControl, pOption, pPosition, pWidget )
   METHOD  itemPixmapRect( pRectangle, nAlignment, pPixmap )
   METHOD  itemTextRect( pMetrics, pRectangle, nAlignment, lEnabled, cText )
   METHOD  layoutSpacing( nControl1, nControl2, nOrientation, pOption, pWidget )
   METHOD  pixelMetric( nMetric, pOption, pWidget )
   METHOD  polish( ... )
   METHOD  sizeFromContents( nType, pOption, pContentsSize, pWidget )
   METHOD  standardIcon( nStandardIcon, pOption, pWidget )
   METHOD  standardPalette()
   METHOD  styleHint( nHint, pOption, pWidget, pReturnData )
   METHOD  subControlRect( nControl, pOption, nSubControl, pWidget )
   METHOD  subElementRect( nElement, pOption, pWidget )
   METHOD  unpolish( ... )
   METHOD  alignedRect( nDirection, nAlignment, pSize, pRectangle )
   METHOD  sliderPositionFromValue( nMin, nMax, nLogicalValue, nSpan, lUpsideDown )
   METHOD  sliderValueFromPosition( nMin, nMax, nPosition, nSpan, lUpsideDown )
   METHOD  visualAlignment( nDirection, nAlignment )
   METHOD  visualPos( nDirection, pBoundingRectangle, pLogicalPosition )
   METHOD  visualRect( nDirection, pBoundingRectangle, pLogicalRectangle )

   ENDCLASS


METHOD QStyle:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyle( ... )
   RETURN Self


METHOD QStyle:combinedLayoutSpacing( nControls1, nControls2, nOrientation, pOption, pWidget )
   RETURN Qt_QStyle_combinedLayoutSpacing( ::pPtr, nControls1, nControls2, nOrientation, hbqt_ptr( pOption ), hbqt_ptr( pWidget ) )


METHOD QStyle:drawComplexControl( nControl, pOption, pPainter, pWidget )
   RETURN Qt_QStyle_drawComplexControl( ::pPtr, nControl, hbqt_ptr( pOption ), hbqt_ptr( pPainter ), hbqt_ptr( pWidget ) )


METHOD QStyle:drawControl( nElement, pOption, pPainter, pWidget )
   RETURN Qt_QStyle_drawControl( ::pPtr, nElement, hbqt_ptr( pOption ), hbqt_ptr( pPainter ), hbqt_ptr( pWidget ) )


METHOD QStyle:drawItemPixmap( pPainter, pRectangle, nAlignment, pPixmap )
   RETURN Qt_QStyle_drawItemPixmap( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pRectangle ), nAlignment, hbqt_ptr( pPixmap ) )


METHOD QStyle:drawItemText( pPainter, pRectangle, nAlignment, pPalette, lEnabled, cText, nTextRole )
   RETURN Qt_QStyle_drawItemText( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pRectangle ), nAlignment, hbqt_ptr( pPalette ), lEnabled, cText, nTextRole )


METHOD QStyle:drawPrimitive( nElement, pOption, pPainter, pWidget )
   RETURN Qt_QStyle_drawPrimitive( ::pPtr, nElement, hbqt_ptr( pOption ), hbqt_ptr( pPainter ), hbqt_ptr( pWidget ) )


METHOD QStyle:generatedIconPixmap( nIconMode, pPixmap, pOption )
   RETURN HB_QPixmap():from( Qt_QStyle_generatedIconPixmap( ::pPtr, nIconMode, hbqt_ptr( pPixmap ), hbqt_ptr( pOption ) ) )


METHOD QStyle:hitTestComplexControl( nControl, pOption, pPosition, pWidget )
   RETURN Qt_QStyle_hitTestComplexControl( ::pPtr, nControl, hbqt_ptr( pOption ), hbqt_ptr( pPosition ), hbqt_ptr( pWidget ) )


METHOD QStyle:itemPixmapRect( pRectangle, nAlignment, pPixmap )
   RETURN HB_QRect():from( Qt_QStyle_itemPixmapRect( ::pPtr, hbqt_ptr( pRectangle ), nAlignment, hbqt_ptr( pPixmap ) ) )


METHOD QStyle:itemTextRect( pMetrics, pRectangle, nAlignment, lEnabled, cText )
   RETURN HB_QRect():from( Qt_QStyle_itemTextRect( ::pPtr, hbqt_ptr( pMetrics ), hbqt_ptr( pRectangle ), nAlignment, lEnabled, cText ) )


METHOD QStyle:layoutSpacing( nControl1, nControl2, nOrientation, pOption, pWidget )
   RETURN Qt_QStyle_layoutSpacing( ::pPtr, nControl1, nControl2, nOrientation, hbqt_ptr( pOption ), hbqt_ptr( pWidget ) )


METHOD QStyle:pixelMetric( nMetric, pOption, pWidget )
   RETURN Qt_QStyle_pixelMetric( ::pPtr, nMetric, hbqt_ptr( pOption ), hbqt_ptr( pWidget ) )


METHOD QStyle:polish( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN Qt_QStyle_polish( ::pPtr, ... )
         CASE "QAPPLICATION"
            RETURN Qt_QStyle_polish_1( ::pPtr, ... )
         CASE "QPALETTE"
            RETURN Qt_QStyle_polish_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QStyle:sizeFromContents( nType, pOption, pContentsSize, pWidget )
   RETURN HB_QSize():from( Qt_QStyle_sizeFromContents( ::pPtr, nType, hbqt_ptr( pOption ), hbqt_ptr( pContentsSize ), hbqt_ptr( pWidget ) ) )


METHOD QStyle:standardIcon( nStandardIcon, pOption, pWidget )
   RETURN HB_QIcon():from( Qt_QStyle_standardIcon( ::pPtr, nStandardIcon, hbqt_ptr( pOption ), hbqt_ptr( pWidget ) ) )


METHOD QStyle:standardPalette()
   RETURN HB_QPalette():from( Qt_QStyle_standardPalette( ::pPtr ) )


METHOD QStyle:styleHint( nHint, pOption, pWidget, pReturnData )
   RETURN Qt_QStyle_styleHint( ::pPtr, nHint, hbqt_ptr( pOption ), hbqt_ptr( pWidget ), hbqt_ptr( pReturnData ) )


METHOD QStyle:subControlRect( nControl, pOption, nSubControl, pWidget )
   RETURN HB_QRect():from( Qt_QStyle_subControlRect( ::pPtr, nControl, hbqt_ptr( pOption ), nSubControl, hbqt_ptr( pWidget ) ) )


METHOD QStyle:subElementRect( nElement, pOption, pWidget )
   RETURN HB_QRect():from( Qt_QStyle_subElementRect( ::pPtr, nElement, hbqt_ptr( pOption ), hbqt_ptr( pWidget ) ) )


METHOD QStyle:unpolish( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN Qt_QStyle_unpolish( ::pPtr, ... )
         CASE "QAPPLICATION"
            RETURN Qt_QStyle_unpolish_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QStyle:alignedRect( nDirection, nAlignment, pSize, pRectangle )
   RETURN HB_QRect():from( Qt_QStyle_alignedRect( ::pPtr, nDirection, nAlignment, hbqt_ptr( pSize ), hbqt_ptr( pRectangle ) ) )


METHOD QStyle:sliderPositionFromValue( nMin, nMax, nLogicalValue, nSpan, lUpsideDown )
   RETURN Qt_QStyle_sliderPositionFromValue( ::pPtr, nMin, nMax, nLogicalValue, nSpan, lUpsideDown )


METHOD QStyle:sliderValueFromPosition( nMin, nMax, nPosition, nSpan, lUpsideDown )
   RETURN Qt_QStyle_sliderValueFromPosition( ::pPtr, nMin, nMax, nPosition, nSpan, lUpsideDown )


METHOD QStyle:visualAlignment( nDirection, nAlignment )
   RETURN Qt_QStyle_visualAlignment( ::pPtr, nDirection, nAlignment )


METHOD QStyle:visualPos( nDirection, pBoundingRectangle, pLogicalPosition )
   RETURN HB_QPoint():from( Qt_QStyle_visualPos( ::pPtr, nDirection, hbqt_ptr( pBoundingRectangle ), hbqt_ptr( pLogicalPosition ) ) )


METHOD QStyle:visualRect( nDirection, pBoundingRectangle, pLogicalRectangle )
   RETURN HB_QRect():from( Qt_QStyle_visualRect( ::pPtr, nDirection, hbqt_ptr( pBoundingRectangle ), hbqt_ptr( pLogicalRectangle ) ) )

