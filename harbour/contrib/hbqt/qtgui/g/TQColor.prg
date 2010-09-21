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


FUNCTION QColor( ... )
   RETURN HB_QColor():new( ... )


CREATE CLASS QColor INHERIT HbQtObjectHandler FUNCTION HB_QColor

   METHOD  new( ... )

   METHOD  alpha()
   METHOD  alphaF()
   METHOD  black()
   METHOD  blackF()
   METHOD  blue()
   METHOD  blueF()
   METHOD  convertTo( nColorSpec )
   METHOD  cyan()
   METHOD  cyanF()
   METHOD  darker( nFactor )
   METHOD  getCmyk( nC, nM, nY, nK, nA )
   METHOD  getCmykF( nC, nM, nY, nK, nA )
   METHOD  getHsv( nH, nS, nV, nA )
   METHOD  getHsvF( nH, nS, nV, nA )
   METHOD  getRgb( nR, nG, nB, nA )
   METHOD  getRgbF( nR, nG, nB, nA )
   METHOD  green()
   METHOD  greenF()
   METHOD  hue()
   METHOD  hueF()
   METHOD  isValid()
   METHOD  lighter( nFactor )
   METHOD  magenta()
   METHOD  magentaF()
   METHOD  name()
   METHOD  red()
   METHOD  redF()
   METHOD  rgb()
   METHOD  rgba()
   METHOD  saturation()
   METHOD  saturationF()
   METHOD  setAlpha( nAlpha )
   METHOD  setAlphaF( nAlpha )
   METHOD  setBlue( nBlue )
   METHOD  setBlueF( nBlue )
   METHOD  setCmyk( nC, nM, nY, nK, nA )
   METHOD  setCmykF( nC, nM, nY, nK, nA )
   METHOD  setGreen( nGreen )
   METHOD  setGreenF( nGreen )
   METHOD  setHsv( nH, nS, nV, nA )
   METHOD  setHsvF( nH, nS, nV, nA )
   METHOD  setNamedColor( cName )
   METHOD  setRed( nRed )
   METHOD  setRedF( nRed )
   METHOD  setRgb( ... )
   METHOD  setRgba( nRgba )
   METHOD  setRgbF( nR, nG, nB, nA )
   METHOD  spec()
   METHOD  toCmyk()
   METHOD  toHsv()
   METHOD  toRgb()
   METHOD  value()
   METHOD  valueF()
   METHOD  yellow()
   METHOD  yellowF()
   METHOD  colorNames()
   METHOD  fromCmyk( nC, nM, nY, nK, nA )
   METHOD  fromCmykF( nC, nM, nY, nK, nA )
   METHOD  fromHsv( nH, nS, nV, nA )
   METHOD  fromHsvF( nH, nS, nV, nA )
   METHOD  fromRgb( ... )
   METHOD  fromRgbF( nR, nG, nB, nA )
   METHOD  fromRgba( nRgba )

   ENDCLASS


METHOD QColor:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QColor( ... )
   RETURN Self


METHOD QColor:alpha()
   RETURN Qt_QColor_alpha( ::pPtr )


METHOD QColor:alphaF()
   RETURN Qt_QColor_alphaF( ::pPtr )


METHOD QColor:black()
   RETURN Qt_QColor_black( ::pPtr )


METHOD QColor:blackF()
   RETURN Qt_QColor_blackF( ::pPtr )


METHOD QColor:blue()
   RETURN Qt_QColor_blue( ::pPtr )


METHOD QColor:blueF()
   RETURN Qt_QColor_blueF( ::pPtr )


METHOD QColor:convertTo( nColorSpec )
   RETURN Qt_QColor_convertTo( ::pPtr, nColorSpec )


METHOD QColor:cyan()
   RETURN Qt_QColor_cyan( ::pPtr )


METHOD QColor:cyanF()
   RETURN Qt_QColor_cyanF( ::pPtr )


METHOD QColor:darker( nFactor )
   RETURN Qt_QColor_darker( ::pPtr, nFactor )


METHOD QColor:getCmyk( nC, nM, nY, nK, nA )
   RETURN Qt_QColor_getCmyk( ::pPtr, nC, nM, nY, nK, nA )


METHOD QColor:getCmykF( nC, nM, nY, nK, nA )
   RETURN Qt_QColor_getCmykF( ::pPtr, nC, nM, nY, nK, nA )


METHOD QColor:getHsv( nH, nS, nV, nA )
   RETURN Qt_QColor_getHsv( ::pPtr, nH, nS, nV, nA )


METHOD QColor:getHsvF( nH, nS, nV, nA )
   RETURN Qt_QColor_getHsvF( ::pPtr, nH, nS, nV, nA )


METHOD QColor:getRgb( nR, nG, nB, nA )
   RETURN Qt_QColor_getRgb( ::pPtr, nR, nG, nB, nA )


METHOD QColor:getRgbF( nR, nG, nB, nA )
   RETURN Qt_QColor_getRgbF( ::pPtr, nR, nG, nB, nA )


METHOD QColor:green()
   RETURN Qt_QColor_green( ::pPtr )


METHOD QColor:greenF()
   RETURN Qt_QColor_greenF( ::pPtr )


METHOD QColor:hue()
   RETURN Qt_QColor_hue( ::pPtr )


METHOD QColor:hueF()
   RETURN Qt_QColor_hueF( ::pPtr )


METHOD QColor:isValid()
   RETURN Qt_QColor_isValid( ::pPtr )


METHOD QColor:lighter( nFactor )
   RETURN Qt_QColor_lighter( ::pPtr, nFactor )


METHOD QColor:magenta()
   RETURN Qt_QColor_magenta( ::pPtr )


METHOD QColor:magentaF()
   RETURN Qt_QColor_magentaF( ::pPtr )


METHOD QColor:name()
   RETURN Qt_QColor_name( ::pPtr )


METHOD QColor:red()
   RETURN Qt_QColor_red( ::pPtr )


METHOD QColor:redF()
   RETURN Qt_QColor_redF( ::pPtr )


METHOD QColor:rgb()
   RETURN Qt_QColor_rgb( ::pPtr )


METHOD QColor:rgba()
   RETURN Qt_QColor_rgba( ::pPtr )


METHOD QColor:saturation()
   RETURN Qt_QColor_saturation( ::pPtr )


METHOD QColor:saturationF()
   RETURN Qt_QColor_saturationF( ::pPtr )


METHOD QColor:setAlpha( nAlpha )
   RETURN Qt_QColor_setAlpha( ::pPtr, nAlpha )


METHOD QColor:setAlphaF( nAlpha )
   RETURN Qt_QColor_setAlphaF( ::pPtr, nAlpha )


METHOD QColor:setBlue( nBlue )
   RETURN Qt_QColor_setBlue( ::pPtr, nBlue )


METHOD QColor:setBlueF( nBlue )
   RETURN Qt_QColor_setBlueF( ::pPtr, nBlue )


METHOD QColor:setCmyk( nC, nM, nY, nK, nA )
   RETURN Qt_QColor_setCmyk( ::pPtr, nC, nM, nY, nK, nA )


METHOD QColor:setCmykF( nC, nM, nY, nK, nA )
   RETURN Qt_QColor_setCmykF( ::pPtr, nC, nM, nY, nK, nA )


METHOD QColor:setGreen( nGreen )
   RETURN Qt_QColor_setGreen( ::pPtr, nGreen )


METHOD QColor:setGreenF( nGreen )
   RETURN Qt_QColor_setGreenF( ::pPtr, nGreen )


METHOD QColor:setHsv( nH, nS, nV, nA )
   RETURN Qt_QColor_setHsv( ::pPtr, nH, nS, nV, nA )


METHOD QColor:setHsvF( nH, nS, nV, nA )
   RETURN Qt_QColor_setHsvF( ::pPtr, nH, nS, nV, nA )


METHOD QColor:setNamedColor( cName )
   RETURN Qt_QColor_setNamedColor( ::pPtr, cName )


METHOD QColor:setRed( nRed )
   RETURN Qt_QColor_setRed( ::pPtr, nRed )


METHOD QColor:setRedF( nRed )
   RETURN Qt_QColor_setRedF( ::pPtr, nRed )


METHOD QColor:setRgb( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QColor_setRgb( ::pPtr, ... )


METHOD QColor:setRgba( nRgba )
   RETURN Qt_QColor_setRgba( ::pPtr, nRgba )


METHOD QColor:setRgbF( nR, nG, nB, nA )
   RETURN Qt_QColor_setRgbF( ::pPtr, nR, nG, nB, nA )


METHOD QColor:spec()
   RETURN Qt_QColor_spec( ::pPtr )


METHOD QColor:toCmyk()
   RETURN Qt_QColor_toCmyk( ::pPtr )


METHOD QColor:toHsv()
   RETURN Qt_QColor_toHsv( ::pPtr )


METHOD QColor:toRgb()
   RETURN Qt_QColor_toRgb( ::pPtr )


METHOD QColor:value()
   RETURN Qt_QColor_value( ::pPtr )


METHOD QColor:valueF()
   RETURN Qt_QColor_valueF( ::pPtr )


METHOD QColor:yellow()
   RETURN Qt_QColor_yellow( ::pPtr )


METHOD QColor:yellowF()
   RETURN Qt_QColor_yellowF( ::pPtr )


METHOD QColor:colorNames()
   RETURN Qt_QColor_colorNames( ::pPtr )


METHOD QColor:fromCmyk( nC, nM, nY, nK, nA )
   RETURN Qt_QColor_fromCmyk( ::pPtr, nC, nM, nY, nK, nA )


METHOD QColor:fromCmykF( nC, nM, nY, nK, nA )
   RETURN Qt_QColor_fromCmykF( ::pPtr, nC, nM, nY, nK, nA )


METHOD QColor:fromHsv( nH, nS, nV, nA )
   RETURN Qt_QColor_fromHsv( ::pPtr, nH, nS, nV, nA )


METHOD QColor:fromHsvF( nH, nS, nV, nA )
   RETURN Qt_QColor_fromHsvF( ::pPtr, nH, nS, nV, nA )


METHOD QColor:fromRgb( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QColor_fromRgb( ::pPtr, ... )


METHOD QColor:fromRgbF( nR, nG, nB, nA )
   RETURN Qt_QColor_fromRgbF( ::pPtr, nR, nG, nB, nA )


METHOD QColor:fromRgba( nRgba )
   RETURN Qt_QColor_fromRgba( ::pPtr, nRgba )

