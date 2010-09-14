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


FUNCTION QFont( ... )
   RETURN HB_QFont():new( ... )


CREATE CLASS QFont INHERIT HbQtObjectHandler FUNCTION HB_QFont

   METHOD  new( ... )

   METHOD  bold()
   METHOD  capitalization()
   METHOD  defaultFamily()
   METHOD  exactMatch()
   METHOD  family()
   METHOD  fixedPitch()
   METHOD  fromString( cDescrip )
   METHOD  italic()
   METHOD  kerning()
   METHOD  key()
   METHOD  lastResortFamily()
   METHOD  lastResortFont()
   METHOD  letterSpacing()
   METHOD  letterSpacingType()
   METHOD  overline()
   METHOD  pixelSize()
   METHOD  pointSize()
   METHOD  pointSizeF()
   METHOD  rawMode()
   METHOD  rawName()
   METHOD  setBold( lEnable )
   METHOD  setCapitalization( nCaps )
   METHOD  setFamily( cFamily )
   METHOD  setFixedPitch( lEnable )
   METHOD  setItalic( lEnable )
   METHOD  setKerning( lEnable )
   METHOD  setLetterSpacing( nType, nSpacing )
   METHOD  setOverline( lEnable )
   METHOD  setPixelSize( nPixelSize )
   METHOD  setPointSize( nPointSize )
   METHOD  setPointSizeF( nPointSize )
   METHOD  setRawMode( lEnable )
   METHOD  setRawName( cName )
   METHOD  setStretch( nFactor )
   METHOD  setStrikeOut( lEnable )
   METHOD  setStyle( nStyle )
   METHOD  setStyleHint( nHint, nStrategy )
   METHOD  setStyleStrategy( nS )
   METHOD  setUnderline( lEnable )
   METHOD  setWeight( nWeight )
   METHOD  setWordSpacing( nSpacing )
   METHOD  stretch()
   METHOD  strikeOut()
   METHOD  style()
   METHOD  styleHint()
   METHOD  styleStrategy()
   METHOD  toString()
   METHOD  underline()
   METHOD  weight()
   METHOD  wordSpacing()
   METHOD  cleanup()
   METHOD  initialize()
   METHOD  insertSubstitution( cFamilyName, cSubstituteName )
   METHOD  insertSubstitutions( cFamilyName, pSubstituteNames )
   METHOD  removeSubstitution( cFamilyName )
   METHOD  substitute( cFamilyName )
   METHOD  substitutes( cFamilyName )
   METHOD  substitutions()

   ENDCLASS


METHOD QFont:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFont( ... )
   RETURN Self


METHOD QFont:bold()
   RETURN Qt_QFont_bold( ::pPtr )


METHOD QFont:capitalization()
   RETURN Qt_QFont_capitalization( ::pPtr )


METHOD QFont:defaultFamily()
   RETURN Qt_QFont_defaultFamily( ::pPtr )


METHOD QFont:exactMatch()
   RETURN Qt_QFont_exactMatch( ::pPtr )


METHOD QFont:family()
   RETURN Qt_QFont_family( ::pPtr )


METHOD QFont:fixedPitch()
   RETURN Qt_QFont_fixedPitch( ::pPtr )


METHOD QFont:fromString( cDescrip )
   RETURN Qt_QFont_fromString( ::pPtr, cDescrip )


METHOD QFont:italic()
   RETURN Qt_QFont_italic( ::pPtr )


METHOD QFont:kerning()
   RETURN Qt_QFont_kerning( ::pPtr )


METHOD QFont:key()
   RETURN Qt_QFont_key( ::pPtr )


METHOD QFont:lastResortFamily()
   RETURN Qt_QFont_lastResortFamily( ::pPtr )


METHOD QFont:lastResortFont()
   RETURN Qt_QFont_lastResortFont( ::pPtr )


METHOD QFont:letterSpacing()
   RETURN Qt_QFont_letterSpacing( ::pPtr )


METHOD QFont:letterSpacingType()
   RETURN Qt_QFont_letterSpacingType( ::pPtr )


METHOD QFont:overline()
   RETURN Qt_QFont_overline( ::pPtr )


METHOD QFont:pixelSize()
   RETURN Qt_QFont_pixelSize( ::pPtr )


METHOD QFont:pointSize()
   RETURN Qt_QFont_pointSize( ::pPtr )


METHOD QFont:pointSizeF()
   RETURN Qt_QFont_pointSizeF( ::pPtr )


METHOD QFont:rawMode()
   RETURN Qt_QFont_rawMode( ::pPtr )


METHOD QFont:rawName()
   RETURN Qt_QFont_rawName( ::pPtr )


METHOD QFont:setBold( lEnable )
   RETURN Qt_QFont_setBold( ::pPtr, lEnable )


METHOD QFont:setCapitalization( nCaps )
   RETURN Qt_QFont_setCapitalization( ::pPtr, nCaps )


METHOD QFont:setFamily( cFamily )
   RETURN Qt_QFont_setFamily( ::pPtr, cFamily )


METHOD QFont:setFixedPitch( lEnable )
   RETURN Qt_QFont_setFixedPitch( ::pPtr, lEnable )


METHOD QFont:setItalic( lEnable )
   RETURN Qt_QFont_setItalic( ::pPtr, lEnable )


METHOD QFont:setKerning( lEnable )
   RETURN Qt_QFont_setKerning( ::pPtr, lEnable )


METHOD QFont:setLetterSpacing( nType, nSpacing )
   RETURN Qt_QFont_setLetterSpacing( ::pPtr, nType, nSpacing )


METHOD QFont:setOverline( lEnable )
   RETURN Qt_QFont_setOverline( ::pPtr, lEnable )


METHOD QFont:setPixelSize( nPixelSize )
   RETURN Qt_QFont_setPixelSize( ::pPtr, nPixelSize )


METHOD QFont:setPointSize( nPointSize )
   RETURN Qt_QFont_setPointSize( ::pPtr, nPointSize )


METHOD QFont:setPointSizeF( nPointSize )
   RETURN Qt_QFont_setPointSizeF( ::pPtr, nPointSize )


METHOD QFont:setRawMode( lEnable )
   RETURN Qt_QFont_setRawMode( ::pPtr, lEnable )


METHOD QFont:setRawName( cName )
   RETURN Qt_QFont_setRawName( ::pPtr, cName )


METHOD QFont:setStretch( nFactor )
   RETURN Qt_QFont_setStretch( ::pPtr, nFactor )


METHOD QFont:setStrikeOut( lEnable )
   RETURN Qt_QFont_setStrikeOut( ::pPtr, lEnable )


METHOD QFont:setStyle( nStyle )
   RETURN Qt_QFont_setStyle( ::pPtr, nStyle )


METHOD QFont:setStyleHint( nHint, nStrategy )
   RETURN Qt_QFont_setStyleHint( ::pPtr, nHint, nStrategy )


METHOD QFont:setStyleStrategy( nS )
   RETURN Qt_QFont_setStyleStrategy( ::pPtr, nS )


METHOD QFont:setUnderline( lEnable )
   RETURN Qt_QFont_setUnderline( ::pPtr, lEnable )


METHOD QFont:setWeight( nWeight )
   RETURN Qt_QFont_setWeight( ::pPtr, nWeight )


METHOD QFont:setWordSpacing( nSpacing )
   RETURN Qt_QFont_setWordSpacing( ::pPtr, nSpacing )


METHOD QFont:stretch()
   RETURN Qt_QFont_stretch( ::pPtr )


METHOD QFont:strikeOut()
   RETURN Qt_QFont_strikeOut( ::pPtr )


METHOD QFont:style()
   RETURN Qt_QFont_style( ::pPtr )


METHOD QFont:styleHint()
   RETURN Qt_QFont_styleHint( ::pPtr )


METHOD QFont:styleStrategy()
   RETURN Qt_QFont_styleStrategy( ::pPtr )


METHOD QFont:toString()
   RETURN Qt_QFont_toString( ::pPtr )


METHOD QFont:underline()
   RETURN Qt_QFont_underline( ::pPtr )


METHOD QFont:weight()
   RETURN Qt_QFont_weight( ::pPtr )


METHOD QFont:wordSpacing()
   RETURN Qt_QFont_wordSpacing( ::pPtr )


METHOD QFont:cleanup()
   RETURN Qt_QFont_cleanup( ::pPtr )


METHOD QFont:initialize()
   RETURN Qt_QFont_initialize( ::pPtr )


METHOD QFont:insertSubstitution( cFamilyName, cSubstituteName )
   RETURN Qt_QFont_insertSubstitution( ::pPtr, cFamilyName, cSubstituteName )


METHOD QFont:insertSubstitutions( cFamilyName, pSubstituteNames )
   RETURN Qt_QFont_insertSubstitutions( ::pPtr, cFamilyName, hbqt_ptr( pSubstituteNames ) )


METHOD QFont:removeSubstitution( cFamilyName )
   RETURN Qt_QFont_removeSubstitution( ::pPtr, cFamilyName )


METHOD QFont:substitute( cFamilyName )
   RETURN Qt_QFont_substitute( ::pPtr, cFamilyName )


METHOD QFont:substitutes( cFamilyName )
   RETURN Qt_QFont_substitutes( ::pPtr, cFamilyName )


METHOD QFont:substitutions()
   RETURN Qt_QFont_substitutions( ::pPtr )

