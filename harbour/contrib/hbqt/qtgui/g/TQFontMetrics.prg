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

FUNCTION QFontMetricsFromPointer( ... )
   RETURN HB_QFontMetrics():fromPointer( ... )


CREATE CLASS QFontMetrics INHERIT HbQtObjectHandler FUNCTION HB_QFontMetrics

   METHOD  new( ... )

   METHOD  ascent                        // (  )                                               -> nInt
   METHOD  averageCharWidth              // (  )                                               -> nInt
   METHOD  boundingRect                  // ( oQChar )                                         -> oQRect
                                         // ( cText )                                          -> oQRect
                                         // ( nX, nY, nWidth, nHeight, nFlags, cText, nTabStops, @nTabArray ) -> oQRect
                                         // ( oQRect, nFlags, cText, nTabStops, @nTabArray )   -> oQRect
   METHOD  descent                       // (  )                                               -> nInt
   METHOD  elidedText                    // ( cText, nMode, nWidth, nFlags )                   -> cQString
   METHOD  height                        // (  )                                               -> nInt
   METHOD  inFont                        // ( oQChar )                                         -> lBool
   METHOD  leading                       // (  )                                               -> nInt
   METHOD  leftBearing                   // ( oQChar )                                         -> nInt
   METHOD  lineSpacing                   // (  )                                               -> nInt
   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  maxWidth                      // (  )                                               -> nInt
   METHOD  minLeftBearing                // (  )                                               -> nInt
   METHOD  minRightBearing               // (  )                                               -> nInt
   METHOD  overlinePos                   // (  )                                               -> nInt
   METHOD  rightBearing                  // ( oQChar )                                         -> nInt
   METHOD  size                          // ( nFlags, cText, nTabStops, @nTabArray )           -> oQSize
   METHOD  strikeOutPos                  // (  )                                               -> nInt
   METHOD  tightBoundingRect             // ( cText )                                          -> oQRect
   METHOD  underlinePos                  // (  )                                               -> nInt
   METHOD  width                         // ( cText, nLen )                                    -> nInt
                                         // ( oQChar )                                         -> nInt
   METHOD  xHeight                       // (  )                                               -> nInt

   ENDCLASS


METHOD QFontMetrics:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontMetrics( ... )
   RETURN Self


METHOD QFontMetrics:ascent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_ascent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:averageCharWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_averageCharWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:boundingRect( ... )
   SWITCH PCount()
   CASE 8
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) ) .AND. hb_isNumeric( hb_pvalue( 8 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 7
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) ) .AND. hb_isNumeric( hb_pvalue( 7 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isChar( hb_pvalue( 6 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_boundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:descent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_descent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:elidedText( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QFontMetrics_elidedText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFontMetrics_elidedText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:inFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_inFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:leading( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_leading( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:leftBearing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_leftBearing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:lineSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_lineSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:maxWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_maxWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:minLeftBearing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_minLeftBearing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:minRightBearing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_minRightBearing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:overlinePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_overlinePos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:rightBearing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontMetrics_rightBearing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:size( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QSizeFromPointer( Qt_QFontMetrics_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QSizeFromPointer( Qt_QFontMetrics_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QSizeFromPointer( Qt_QFontMetrics_size( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:strikeOutPos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_strikeOutPos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:tightBoundingRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QFontMetrics_tightBoundingRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontMetrics:underlinePos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_underlinePos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QFontMetrics:xHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontMetrics_xHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

