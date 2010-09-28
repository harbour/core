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


FUNCTION QFontMetricsF( ... )
   RETURN HB_QFontMetricsF():new( ... )


CREATE CLASS QFontMetricsF INHERIT HbQtObjectHandler FUNCTION HB_QFontMetricsF

   METHOD  new( ... )

   METHOD  ascent()
   METHOD  averageCharWidth()
   METHOD  boundingRect( ... )
   METHOD  descent()
   METHOD  elidedText( cText, nMode, nWidth, nFlags )
   METHOD  height()
   METHOD  inFont( pCh )
   METHOD  leading()
   METHOD  leftBearing( pCh )
   METHOD  lineSpacing()
   METHOD  lineWidth()
   METHOD  maxWidth()
   METHOD  minLeftBearing()
   METHOD  minRightBearing()
   METHOD  overlinePos()
   METHOD  rightBearing( pCh )
   METHOD  size( nFlags, cText, nTabStops, nTabArray )
   METHOD  strikeOutPos()
   METHOD  tightBoundingRect( cText )
   METHOD  underlinePos()
   METHOD  width( ... )
   METHOD  xHeight()

   ENDCLASS


METHOD QFontMetricsF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontMetricsF( ... )
   RETURN Self


METHOD QFontMetricsF:ascent()
   RETURN Qt_QFontMetricsF_ascent( ::pPtr )


METHOD QFontMetricsF:averageCharWidth()
   RETURN Qt_QFontMetricsF_averageCharWidth( ::pPtr )


METHOD QFontMetricsF:boundingRect( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QRectF():from( Qt_QFontMetricsF_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN HB_QRectF():from( Qt_QFontMetricsF_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QFontMetricsF_boundingRect( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QFontMetricsF_boundingRect_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFontMetricsF:descent()
   RETURN Qt_QFontMetricsF_descent( ::pPtr )


METHOD QFontMetricsF:elidedText( cText, nMode, nWidth, nFlags )
   RETURN Qt_QFontMetricsF_elidedText( ::pPtr, cText, nMode, nWidth, nFlags )


METHOD QFontMetricsF:height()
   RETURN Qt_QFontMetricsF_height( ::pPtr )


METHOD QFontMetricsF:inFont( pCh )
   RETURN Qt_QFontMetricsF_inFont( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetricsF:leading()
   RETURN Qt_QFontMetricsF_leading( ::pPtr )


METHOD QFontMetricsF:leftBearing( pCh )
   RETURN Qt_QFontMetricsF_leftBearing( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetricsF:lineSpacing()
   RETURN Qt_QFontMetricsF_lineSpacing( ::pPtr )


METHOD QFontMetricsF:lineWidth()
   RETURN Qt_QFontMetricsF_lineWidth( ::pPtr )


METHOD QFontMetricsF:maxWidth()
   RETURN Qt_QFontMetricsF_maxWidth( ::pPtr )


METHOD QFontMetricsF:minLeftBearing()
   RETURN Qt_QFontMetricsF_minLeftBearing( ::pPtr )


METHOD QFontMetricsF:minRightBearing()
   RETURN Qt_QFontMetricsF_minRightBearing( ::pPtr )


METHOD QFontMetricsF:overlinePos()
   RETURN Qt_QFontMetricsF_overlinePos( ::pPtr )


METHOD QFontMetricsF:rightBearing( pCh )
   RETURN Qt_QFontMetricsF_rightBearing( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetricsF:size( nFlags, cText, nTabStops, nTabArray )
   RETURN HB_QSizeF():from( Qt_QFontMetricsF_size( ::pPtr, nFlags, cText, nTabStops, nTabArray ) )


METHOD QFontMetricsF:strikeOutPos()
   RETURN Qt_QFontMetricsF_strikeOutPos( ::pPtr )


METHOD QFontMetricsF:tightBoundingRect( cText )
   RETURN HB_QRectF():from( Qt_QFontMetricsF_tightBoundingRect( ::pPtr, cText ) )


METHOD QFontMetricsF:underlinePos()
   RETURN Qt_QFontMetricsF_underlinePos( ::pPtr )


METHOD QFontMetricsF:width( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetricsF_width( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetricsF_width_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFontMetricsF:xHeight()
   RETURN Qt_QFontMetricsF_xHeight( ::pPtr )

