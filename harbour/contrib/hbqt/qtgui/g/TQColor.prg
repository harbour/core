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


FUNCTION QColor( ... )
   RETURN HB_QColor():new( ... )

FUNCTION QColorFrom( ... )
   RETURN HB_QColor():from( ... )

FUNCTION QColorFromPointer( ... )
   RETURN HB_QColor():fromPointer( ... )


CREATE CLASS QColor INHERIT HbQtObjectHandler FUNCTION HB_QColor

   METHOD  new( ... )

   METHOD  QColor                        // (  )                                               -> oQColor
                                         // ( nR, nG, nB, nA )                                 -> oQColor
                                         // ( nColor )                                         -> oQColor
                                         // ( nColor )                                         -> oQColor
                                         // ( cName )                                          -> oQColor
                                         // ( oQColor )                                        -> oQColor
   METHOD  alpha                         // (  )                                               -> nInt
   METHOD  alphaF                        // (  )                                               -> nQreal
   METHOD  black                         // (  )                                               -> nInt
   METHOD  blackF                        // (  )                                               -> nQreal
   METHOD  blue                          // (  )                                               -> nInt
   METHOD  blueF                         // (  )                                               -> nQreal
   METHOD  convertTo                     // ( nColorSpec )                                     -> oQColor
   METHOD  cyan                          // (  )                                               -> nInt
   METHOD  cyanF                         // (  )                                               -> nQreal
   METHOD  darker                        // ( nFactor )                                        -> oQColor
   METHOD  getCmyk                       // ( @nC, @nM, @nY, @nK, @nA )                        -> NIL
   METHOD  getCmykF                      // ( @nC, @nM, @nY, @nK, @nA )                        -> NIL
   METHOD  getHsv                        // ( @nH, @nS, @nV, @nA )                             -> NIL
   METHOD  getHsvF                       // ( @nH, @nS, @nV, @nA )                             -> NIL
   METHOD  getRgb                        // ( @nR, @nG, @nB, @nA )                             -> NIL
   METHOD  getRgbF                       // ( @nR, @nG, @nB, @nA )                             -> NIL
   METHOD  green                         // (  )                                               -> nInt
   METHOD  greenF                        // (  )                                               -> nQreal
   METHOD  hue                           // (  )                                               -> nInt
   METHOD  hueF                          // (  )                                               -> nQreal
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  lighter                       // ( nFactor )                                        -> oQColor
   METHOD  magenta                       // (  )                                               -> nInt
   METHOD  magentaF                      // (  )                                               -> nQreal
   METHOD  name                          // (  )                                               -> cQString
   METHOD  red                           // (  )                                               -> nInt
   METHOD  redF                          // (  )                                               -> nQreal
   METHOD  rgb                           // (  )                                               -> nQRgb
   METHOD  rgba                          // (  )                                               -> nQRgb
   METHOD  saturation                    // (  )                                               -> nInt
   METHOD  saturationF                   // (  )                                               -> nQreal
   METHOD  setAlpha                      // ( nAlpha )                                         -> NIL
   METHOD  setAlphaF                     // ( nAlpha )                                         -> NIL
   METHOD  setBlue                       // ( nBlue )                                          -> NIL
   METHOD  setBlueF                      // ( nBlue )                                          -> NIL
   METHOD  setCmyk                       // ( nC, nM, nY, nK, nA )                             -> NIL
   METHOD  setCmykF                      // ( nC, nM, nY, nK, nA )                             -> NIL
   METHOD  setGreen                      // ( nGreen )                                         -> NIL
   METHOD  setGreenF                     // ( nGreen )                                         -> NIL
   METHOD  setHsv                        // ( nH, nS, nV, nA )                                 -> NIL
   METHOD  setHsvF                       // ( nH, nS, nV, nA )                                 -> NIL
   METHOD  setNamedColor                 // ( cName )                                          -> NIL
   METHOD  setRed                        // ( nRed )                                           -> NIL
   METHOD  setRedF                       // ( nRed )                                           -> NIL
   METHOD  setRgb                        // ( nRgb )                                           -> NIL
                                         // ( nR, nG, nB, nA )                                 -> NIL
   METHOD  setRgba                       // ( nRgba )                                          -> NIL
   METHOD  setRgbF                       // ( nR, nG, nB, nA )                                 -> NIL
   METHOD  spec                          // (  )                                               -> nSpec
   METHOD  toCmyk                        // (  )                                               -> oQColor
   METHOD  toHsv                         // (  )                                               -> oQColor
   METHOD  toRgb                         // (  )                                               -> oQColor
   METHOD  value                         // (  )                                               -> nInt
   METHOD  valueF                        // (  )                                               -> nQreal
   METHOD  yellow                        // (  )                                               -> nInt
   METHOD  yellowF                       // (  )                                               -> nQreal
   METHOD  colorNames                    // (  )                                               -> oQStringList
   METHOD  fromCmyk                      // ( nC, nM, nY, nK, nA )                             -> oQColor
   METHOD  fromCmykF                     // ( nC, nM, nY, nK, nA )                             -> oQColor
   METHOD  fromHsv                       // ( nH, nS, nV, nA )                                 -> oQColor
   METHOD  fromHsvF                      // ( nH, nS, nV, nA )                                 -> oQColor
   METHOD  fromRgb                       // ( nRgb )                                           -> oQColor
                                         // ( nR, nG, nB, nA )                                 -> oQColor
   METHOD  fromRgbF                      // ( nR, nG, nB, nA )                                 -> oQColor
   METHOD  fromRgba                      // ( nRgba )                                          -> oQColor

   ENDCLASS


METHOD QColor:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QColor( ... )
   RETURN Self


METHOD QColor:QColor( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColor_QColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QColorFromPointer( Qt_QColor_QColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_QColor_4( ::pPtr, ... ) )
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_QColor_2( ::pPtr, ... ) )
         // RETURN QColorFromPointer( Qt_QColor_QColor_3( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_QColor_5( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QColorFromPointer( Qt_QColor_QColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:alpha( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_alpha( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:alphaF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_alphaF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:black( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_black( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:blackF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_blackF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:blue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_blue( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:blueF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_blueF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:convertTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_convertTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:cyan( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_cyan( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:cyanF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_cyanF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:darker( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_darker( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QColorFromPointer( Qt_QColor_darker( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:getCmyk( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QColor_getCmyk( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_getCmyk( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:getCmykF( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QColor_getCmykF( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_getCmykF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:getHsv( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_getHsv( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_getHsv( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:getHsvF( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_getHsvF( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_getHsvF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:getRgb( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_getRgb( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_getRgb( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:getRgbF( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_getRgbF( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_getRgbF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:green( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_green( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:greenF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_greenF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:hue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_hue( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:hueF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_hueF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:lighter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_lighter( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QColorFromPointer( Qt_QColor_lighter( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:magenta( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_magenta( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:magentaF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_magentaF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_name( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:red( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_red( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:redF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_redF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:rgb( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_rgb( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:rgba( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_rgba( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:saturation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_saturation( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:saturationF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_saturationF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setAlpha( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setAlpha( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setAlphaF( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setAlphaF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setBlue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setBlue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setBlueF( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setBlueF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setCmyk( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QColor_setCmyk( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_setCmyk( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setCmykF( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QColor_setCmykF( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_setCmykF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setGreen( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setGreen( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setGreenF( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setGreenF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setHsv( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_setHsv( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_setHsv( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setHsvF( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_setHsvF( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_setHsvF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setNamedColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setNamedColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setRed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setRed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setRedF( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setRedF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setRgb( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_setRgb_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_setRgb_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setRgb( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setRgba( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColor_setRgba( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:setRgbF( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QColor_setRgbF( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QColor_setRgbF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:spec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_spec( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:toCmyk( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QColor_toCmyk( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:toHsv( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QColor_toHsv( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:toRgb( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QColor_toRgb( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_value( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:valueF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_valueF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:yellow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_yellow( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:yellowF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColor_yellowF( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:colorNames( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QColor_colorNames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:fromCmyk( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QColorFromPointer( Qt_QColor_fromCmyk( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColor_fromCmyk( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:fromCmykF( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QColorFromPointer( Qt_QColor_fromCmykF( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColor_fromCmykF( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:fromHsv( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColor_fromHsv( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QColorFromPointer( Qt_QColor_fromHsv( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:fromHsvF( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColor_fromHsvF( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QColorFromPointer( Qt_QColor_fromHsvF( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:fromRgb( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColor_fromRgb_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QColorFromPointer( Qt_QColor_fromRgb_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_fromRgb( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:fromRgbF( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColor_fromRgbF( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QColorFromPointer( Qt_QColor_fromRgbF( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColor:fromRgba( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColor_fromRgba( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

