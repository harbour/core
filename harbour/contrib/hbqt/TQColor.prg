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


CREATE CLASS QColor

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QColor_destroy( ::pPtr )

   METHOD  alpha()                             INLINE  Qt_QColor_alpha( ::pPtr )
   METHOD  alphaF()                            INLINE  Qt_QColor_alphaF( ::pPtr )
   METHOD  black()                             INLINE  Qt_QColor_black( ::pPtr )
   METHOD  blackF()                            INLINE  Qt_QColor_blackF( ::pPtr )
   METHOD  blue()                              INLINE  Qt_QColor_blue( ::pPtr )
   METHOD  blueF()                             INLINE  Qt_QColor_blueF( ::pPtr )
   METHOD  convertTo( nColorSpec )             INLINE  Qt_QColor_convertTo( ::pPtr, nColorSpec )
   METHOD  cyan()                              INLINE  Qt_QColor_cyan( ::pPtr )
   METHOD  cyanF()                             INLINE  Qt_QColor_cyanF( ::pPtr )
   METHOD  darker( nFactor )                   INLINE  Qt_QColor_darker( ::pPtr, nFactor )
   METHOD  getCmyk( nC, nM, nY, nK, nA )       INLINE  Qt_QColor_getCmyk( ::pPtr, nC, nM, nY, nK, nA )
   METHOD  getCmykF( nC, nM, nY, nK, nA )      INLINE  Qt_QColor_getCmykF( ::pPtr, nC, nM, nY, nK, nA )
   METHOD  getHsv( nH, nS, nV, nA )            INLINE  Qt_QColor_getHsv( ::pPtr, nH, nS, nV, nA )
   METHOD  getHsvF( nH, nS, nV, nA )           INLINE  Qt_QColor_getHsvF( ::pPtr, nH, nS, nV, nA )
   METHOD  getRgb( nR, nG, nB, nA )            INLINE  Qt_QColor_getRgb( ::pPtr, nR, nG, nB, nA )
   METHOD  getRgbF( nR, nG, nB, nA )           INLINE  Qt_QColor_getRgbF( ::pPtr, nR, nG, nB, nA )
   METHOD  green()                             INLINE  Qt_QColor_green( ::pPtr )
   METHOD  greenF()                            INLINE  Qt_QColor_greenF( ::pPtr )
   METHOD  hue()                               INLINE  Qt_QColor_hue( ::pPtr )
   METHOD  hueF()                              INLINE  Qt_QColor_hueF( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QColor_isValid( ::pPtr )
   METHOD  lighter( nFactor )                  INLINE  Qt_QColor_lighter( ::pPtr, nFactor )
   METHOD  magenta()                           INLINE  Qt_QColor_magenta( ::pPtr )
   METHOD  magentaF()                          INLINE  Qt_QColor_magentaF( ::pPtr )
   METHOD  name()                              INLINE  Qt_QColor_name( ::pPtr )
   METHOD  red()                               INLINE  Qt_QColor_red( ::pPtr )
   METHOD  redF()                              INLINE  Qt_QColor_redF( ::pPtr )
   METHOD  rgb()                               INLINE  Qt_QColor_rgb( ::pPtr )
   METHOD  rgba()                              INLINE  Qt_QColor_rgba( ::pPtr )
   METHOD  saturation()                        INLINE  Qt_QColor_saturation( ::pPtr )
   METHOD  saturationF()                       INLINE  Qt_QColor_saturationF( ::pPtr )
   METHOD  setAlpha( nAlpha )                  INLINE  Qt_QColor_setAlpha( ::pPtr, nAlpha )
   METHOD  setAlphaF( nAlpha )                 INLINE  Qt_QColor_setAlphaF( ::pPtr, nAlpha )
   METHOD  setBlue( nBlue )                    INLINE  Qt_QColor_setBlue( ::pPtr, nBlue )
   METHOD  setBlueF( nBlue )                   INLINE  Qt_QColor_setBlueF( ::pPtr, nBlue )
   METHOD  setCmyk( nC, nM, nY, nK, nA )       INLINE  Qt_QColor_setCmyk( ::pPtr, nC, nM, nY, nK, nA )
   METHOD  setCmykF( nC, nM, nY, nK, nA )      INLINE  Qt_QColor_setCmykF( ::pPtr, nC, nM, nY, nK, nA )
   METHOD  setGreen( nGreen )                  INLINE  Qt_QColor_setGreen( ::pPtr, nGreen )
   METHOD  setGreenF( nGreen )                 INLINE  Qt_QColor_setGreenF( ::pPtr, nGreen )
   METHOD  setHsv( nH, nS, nV, nA )            INLINE  Qt_QColor_setHsv( ::pPtr, nH, nS, nV, nA )
   METHOD  setHsvF( nH, nS, nV, nA )           INLINE  Qt_QColor_setHsvF( ::pPtr, nH, nS, nV, nA )
   METHOD  setNamedColor( cName )              INLINE  Qt_QColor_setNamedColor( ::pPtr, cName )
   METHOD  setRed( nRed )                      INLINE  Qt_QColor_setRed( ::pPtr, nRed )
   METHOD  setRedF( nRed )                     INLINE  Qt_QColor_setRedF( ::pPtr, nRed )
   METHOD  setRgb( nRgb )                      INLINE  Qt_QColor_setRgb( ::pPtr, nRgb )
   METHOD  setRgb_1( nR, nG, nB, nA )          INLINE  Qt_QColor_setRgb_1( ::pPtr, nR, nG, nB, nA )
   METHOD  setRgba( nRgba )                    INLINE  Qt_QColor_setRgba( ::pPtr, nRgba )
   METHOD  setRgbF( nR, nG, nB, nA )           INLINE  Qt_QColor_setRgbF( ::pPtr, nR, nG, nB, nA )
   METHOD  spec()                              INLINE  Qt_QColor_spec( ::pPtr )
   METHOD  toCmyk()                            INLINE  Qt_QColor_toCmyk( ::pPtr )
   METHOD  toHsv()                             INLINE  Qt_QColor_toHsv( ::pPtr )
   METHOD  toRgb()                             INLINE  Qt_QColor_toRgb( ::pPtr )
   METHOD  value()                             INLINE  Qt_QColor_value( ::pPtr )
   METHOD  valueF()                            INLINE  Qt_QColor_valueF( ::pPtr )
   METHOD  yellow()                            INLINE  Qt_QColor_yellow( ::pPtr )
   METHOD  yellowF()                           INLINE  Qt_QColor_yellowF( ::pPtr )
   METHOD  colorNames()                        INLINE  Qt_QColor_colorNames( ::pPtr )
   METHOD  fromCmyk( nC, nM, nY, nK, nA )      INLINE  Qt_QColor_fromCmyk( ::pPtr, nC, nM, nY, nK, nA )
   METHOD  fromCmykF( nC, nM, nY, nK, nA )     INLINE  Qt_QColor_fromCmykF( ::pPtr, nC, nM, nY, nK, nA )
   METHOD  fromHsv( nH, nS, nV, nA )           INLINE  Qt_QColor_fromHsv( ::pPtr, nH, nS, nV, nA )
   METHOD  fromHsvF( nH, nS, nV, nA )          INLINE  Qt_QColor_fromHsvF( ::pPtr, nH, nS, nV, nA )
   METHOD  fromRgb( nRgb )                     INLINE  Qt_QColor_fromRgb( ::pPtr, nRgb )
   METHOD  fromRgb_1( nR, nG, nB, nA )         INLINE  Qt_QColor_fromRgb_1( ::pPtr, nR, nG, nB, nA )
   METHOD  fromRgbF( nR, nG, nB, nA )          INLINE  Qt_QColor_fromRgbF( ::pPtr, nR, nG, nB, nA )
   METHOD  fromRgba( nRgba )                   INLINE  Qt_QColor_fromRgba( ::pPtr, nRgba )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( ... ) CLASS QColor

   ::pPtr := Qt_QColor( ... )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QColor

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
