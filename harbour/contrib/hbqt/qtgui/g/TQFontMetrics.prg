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


FUNCTION QFontMetrics( ... )
   RETURN HB_QFontMetrics():new( ... )


CREATE CLASS QFontMetrics INHERIT HbQtObjectHandler FUNCTION HB_QFontMetrics

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


METHOD QFontMetrics:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontMetrics( ... )
   RETURN Self


METHOD QFontMetrics:ascent()
   RETURN Qt_QFontMetrics_ascent( ::pPtr )


METHOD QFontMetrics:averageCharWidth()
   RETURN Qt_QFontMetrics_averageCharWidth( ::pPtr )


METHOD QFontMetrics:boundingRect( ... )
   SWITCH PCount()
   CASE 8
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) )
         RETURN HB_QRect():from( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) )
         RETURN HB_QRect():from( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN HB_QRect():from( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN HB_QRect():from( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QRect():from( Qt_QFontMetrics_boundingRect_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QRect():from( Qt_QFontMetrics_boundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFontMetrics:descent()
   RETURN Qt_QFontMetrics_descent( ::pPtr )


METHOD QFontMetrics:elidedText( cText, nMode, nWidth, nFlags )
   RETURN Qt_QFontMetrics_elidedText( ::pPtr, cText, nMode, nWidth, nFlags )


METHOD QFontMetrics:height()
   RETURN Qt_QFontMetrics_height( ::pPtr )


METHOD QFontMetrics:inFont( pCh )
   RETURN Qt_QFontMetrics_inFont( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetrics:leading()
   RETURN Qt_QFontMetrics_leading( ::pPtr )


METHOD QFontMetrics:leftBearing( pCh )
   RETURN Qt_QFontMetrics_leftBearing( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetrics:lineSpacing()
   RETURN Qt_QFontMetrics_lineSpacing( ::pPtr )


METHOD QFontMetrics:lineWidth()
   RETURN Qt_QFontMetrics_lineWidth( ::pPtr )


METHOD QFontMetrics:maxWidth()
   RETURN Qt_QFontMetrics_maxWidth( ::pPtr )


METHOD QFontMetrics:minLeftBearing()
   RETURN Qt_QFontMetrics_minLeftBearing( ::pPtr )


METHOD QFontMetrics:minRightBearing()
   RETURN Qt_QFontMetrics_minRightBearing( ::pPtr )


METHOD QFontMetrics:overlinePos()
   RETURN Qt_QFontMetrics_overlinePos( ::pPtr )


METHOD QFontMetrics:rightBearing( pCh )
   RETURN Qt_QFontMetrics_rightBearing( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetrics:size( nFlags, cText, nTabStops, nTabArray )
   RETURN HB_QSize():from( Qt_QFontMetrics_size( ::pPtr, nFlags, cText, nTabStops, nTabArray ) )


METHOD QFontMetrics:strikeOutPos()
   RETURN Qt_QFontMetrics_strikeOutPos( ::pPtr )


METHOD QFontMetrics:tightBoundingRect( cText )
   RETURN HB_QRect():from( Qt_QFontMetrics_tightBoundingRect( ::pPtr, cText ) )


METHOD QFontMetrics:underlinePos()
   RETURN Qt_QFontMetrics_underlinePos( ::pPtr )


METHOD QFontMetrics:width( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFontMetrics_width( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_width( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_width_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFontMetrics:xHeight()
   RETURN Qt_QFontMetrics_xHeight( ::pPtr )

