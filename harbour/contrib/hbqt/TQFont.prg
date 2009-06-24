/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


CREATE CLASS QFont

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  bold()                              INLINE  Qt_QFont_bold( ::pPtr )
   METHOD  capitalization()                    INLINE  Qt_QFont_capitalization( ::pPtr )
   METHOD  defaultFamily()                     INLINE  Qt_QFont_defaultFamily( ::pPtr )
   METHOD  exactMatch()                        INLINE  Qt_QFont_exactMatch( ::pPtr )
   METHOD  family()                            INLINE  Qt_QFont_family( ::pPtr )
   METHOD  fixedPitch()                        INLINE  Qt_QFont_fixedPitch( ::pPtr )
   METHOD  fromString( cDescrip )              INLINE  Qt_QFont_fromString( ::pPtr, cDescrip )
   METHOD  italic()                            INLINE  Qt_QFont_italic( ::pPtr )
   METHOD  kerning()                           INLINE  Qt_QFont_kerning( ::pPtr )
   METHOD  key()                               INLINE  Qt_QFont_key( ::pPtr )
   METHOD  lastResortFamily()                  INLINE  Qt_QFont_lastResortFamily( ::pPtr )
   METHOD  lastResortFont()                    INLINE  Qt_QFont_lastResortFont( ::pPtr )
   METHOD  letterSpacing()                     INLINE  Qt_QFont_letterSpacing( ::pPtr )
   METHOD  letterSpacingType()                 INLINE  Qt_QFont_letterSpacingType( ::pPtr )
   METHOD  overline()                          INLINE  Qt_QFont_overline( ::pPtr )
   METHOD  pixelSize()                         INLINE  Qt_QFont_pixelSize( ::pPtr )
   METHOD  pointSize()                         INLINE  Qt_QFont_pointSize( ::pPtr )
   METHOD  pointSizeF()                        INLINE  Qt_QFont_pointSizeF( ::pPtr )
   METHOD  rawMode()                           INLINE  Qt_QFont_rawMode( ::pPtr )
   METHOD  rawName()                           INLINE  Qt_QFont_rawName( ::pPtr )
   METHOD  setBold( lEnable )                  INLINE  Qt_QFont_setBold( ::pPtr, lEnable )
   METHOD  setCapitalization( nCaps )          INLINE  Qt_QFont_setCapitalization( ::pPtr, nCaps )
   METHOD  setFamily( cFamily )                INLINE  Qt_QFont_setFamily( ::pPtr, cFamily )
   METHOD  setFixedPitch( lEnable )            INLINE  Qt_QFont_setFixedPitch( ::pPtr, lEnable )
   METHOD  setItalic( lEnable )                INLINE  Qt_QFont_setItalic( ::pPtr, lEnable )
   METHOD  setKerning( lEnable )               INLINE  Qt_QFont_setKerning( ::pPtr, lEnable )
   METHOD  setLetterSpacing( nType, nSpacing )  INLINE  Qt_QFont_setLetterSpacing( ::pPtr, nType, nSpacing )
   METHOD  setOverline( lEnable )              INLINE  Qt_QFont_setOverline( ::pPtr, lEnable )
   METHOD  setPixelSize( nPixelSize )          INLINE  Qt_QFont_setPixelSize( ::pPtr, nPixelSize )
   METHOD  setPointSize( nPointSize )          INLINE  Qt_QFont_setPointSize( ::pPtr, nPointSize )
   METHOD  setPointSizeF( nPointSize )         INLINE  Qt_QFont_setPointSizeF( ::pPtr, nPointSize )
   METHOD  setRawMode( lEnable )               INLINE  Qt_QFont_setRawMode( ::pPtr, lEnable )
   METHOD  setRawName( cName )                 INLINE  Qt_QFont_setRawName( ::pPtr, cName )
   METHOD  setStretch( nFactor )               INLINE  Qt_QFont_setStretch( ::pPtr, nFactor )
   METHOD  setStrikeOut( lEnable )             INLINE  Qt_QFont_setStrikeOut( ::pPtr, lEnable )
   METHOD  setStyle( nStyle )                  INLINE  Qt_QFont_setStyle( ::pPtr, nStyle )
   METHOD  setStyleHint( nHint, nStrategy )    INLINE  Qt_QFont_setStyleHint( ::pPtr, nHint, nStrategy )
   METHOD  setStyleStrategy( nS )              INLINE  Qt_QFont_setStyleStrategy( ::pPtr, nS )
   METHOD  setUnderline( lEnable )             INLINE  Qt_QFont_setUnderline( ::pPtr, lEnable )
   METHOD  setWeight( nWeight )                INLINE  Qt_QFont_setWeight( ::pPtr, nWeight )
   METHOD  setWordSpacing( nSpacing )          INLINE  Qt_QFont_setWordSpacing( ::pPtr, nSpacing )
   METHOD  stretch()                           INLINE  Qt_QFont_stretch( ::pPtr )
   METHOD  strikeOut()                         INLINE  Qt_QFont_strikeOut( ::pPtr )
   METHOD  style()                             INLINE  Qt_QFont_style( ::pPtr )
   METHOD  styleHint()                         INLINE  Qt_QFont_styleHint( ::pPtr )
   METHOD  styleStrategy()                     INLINE  Qt_QFont_styleStrategy( ::pPtr )
   METHOD  toString()                          INLINE  Qt_QFont_toString( ::pPtr )
   METHOD  underline()                         INLINE  Qt_QFont_underline( ::pPtr )
   METHOD  weight()                            INLINE  Qt_QFont_weight( ::pPtr )
   METHOD  wordSpacing()                       INLINE  Qt_QFont_wordSpacing( ::pPtr )
   METHOD  cleanup()                           INLINE  Qt_QFont_cleanup( ::pPtr )
   METHOD  initialize()                        INLINE  Qt_QFont_initialize( ::pPtr )
   METHOD  insertSubstitution( cFamilyName, cSubstituteName )  INLINE  Qt_QFont_insertSubstitution( ::pPtr, cFamilyName, cSubstituteName )
   METHOD  insertSubstitutions( cFamilyName, pSubstituteNames )  INLINE  Qt_QFont_insertSubstitutions( ::pPtr, cFamilyName, pSubstituteNames )
   METHOD  removeSubstitution( cFamilyName )   INLINE  Qt_QFont_removeSubstitution( ::pPtr, cFamilyName )
   METHOD  substitute( cFamilyName )           INLINE  Qt_QFont_substitute( ::pPtr, cFamilyName )
   METHOD  substitutes( cFamilyName )          INLINE  Qt_QFont_substitutes( ::pPtr, cFamilyName )
   METHOD  substitutions()                     INLINE  Qt_QFont_substitutions( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFont

   ::pParent := pParent

   ::pPtr := Qt_QFont( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

