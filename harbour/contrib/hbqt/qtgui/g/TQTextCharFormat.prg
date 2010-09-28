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


FUNCTION QTextCharFormat( ... )
   RETURN HB_QTextCharFormat():new( ... )


CREATE CLASS QTextCharFormat INHERIT HbQtObjectHandler, HB_QTextFormat FUNCTION HB_QTextCharFormat

   METHOD  new( ... )

   METHOD  anchorHref()
   METHOD  anchorNames()
   METHOD  font()
   METHOD  fontCapitalization()
   METHOD  fontFamily()
   METHOD  fontFixedPitch()
   METHOD  fontItalic()
   METHOD  fontKerning()
   METHOD  fontLetterSpacing()
   METHOD  fontOverline()
   METHOD  fontPointSize()
   METHOD  fontStrikeOut()
   METHOD  fontStyleHint()
   METHOD  fontStyleStrategy()
   METHOD  fontUnderline()
   METHOD  fontWeight()
   METHOD  fontWordSpacing()
   METHOD  isAnchor()
   METHOD  isValid()
   METHOD  setAnchor( lAnchor )
   METHOD  setAnchorHref( cValue )
   METHOD  setAnchorNames( pNames )
   METHOD  setFont( pFont )
   METHOD  setFontCapitalization( nCapitalization )
   METHOD  setFontFamily( cFamily )
   METHOD  setFontFixedPitch( lFixedPitch )
   METHOD  setFontItalic( lItalic )
   METHOD  setFontKerning( lEnable )
   METHOD  setFontLetterSpacing( nSpacing )
   METHOD  setFontOverline( lOverline )
   METHOD  setFontPointSize( nSize )
   METHOD  setFontStrikeOut( lStrikeOut )
   METHOD  setFontStyleHint( nHint, nStrategy )
   METHOD  setFontStyleStrategy( nStrategy )
   METHOD  setFontUnderline( lUnderline )
   METHOD  setFontWeight( nWeight )
   METHOD  setFontWordSpacing( nSpacing )
   METHOD  setTextOutline( pPen )
   METHOD  setToolTip( cText )
   METHOD  setUnderlineColor( pColor )
   METHOD  setUnderlineStyle( nStyle )
   METHOD  setVerticalAlignment( nAlignment )
   METHOD  textOutline()
   METHOD  toolTip()
   METHOD  underlineColor()
   METHOD  underlineStyle()
   METHOD  verticalAlignment()

   ENDCLASS


METHOD QTextCharFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCharFormat( ... )
   RETURN Self


METHOD QTextCharFormat:anchorHref()
   RETURN Qt_QTextCharFormat_anchorHref( ::pPtr )


METHOD QTextCharFormat:anchorNames()
   RETURN HB_QStringList():from( Qt_QTextCharFormat_anchorNames( ::pPtr ) )


METHOD QTextCharFormat:font()
   RETURN HB_QFont():from( Qt_QTextCharFormat_font( ::pPtr ) )


METHOD QTextCharFormat:fontCapitalization()
   RETURN Qt_QTextCharFormat_fontCapitalization( ::pPtr )


METHOD QTextCharFormat:fontFamily()
   RETURN Qt_QTextCharFormat_fontFamily( ::pPtr )


METHOD QTextCharFormat:fontFixedPitch()
   RETURN Qt_QTextCharFormat_fontFixedPitch( ::pPtr )


METHOD QTextCharFormat:fontItalic()
   RETURN Qt_QTextCharFormat_fontItalic( ::pPtr )


METHOD QTextCharFormat:fontKerning()
   RETURN Qt_QTextCharFormat_fontKerning( ::pPtr )


METHOD QTextCharFormat:fontLetterSpacing()
   RETURN Qt_QTextCharFormat_fontLetterSpacing( ::pPtr )


METHOD QTextCharFormat:fontOverline()
   RETURN Qt_QTextCharFormat_fontOverline( ::pPtr )


METHOD QTextCharFormat:fontPointSize()
   RETURN Qt_QTextCharFormat_fontPointSize( ::pPtr )


METHOD QTextCharFormat:fontStrikeOut()
   RETURN Qt_QTextCharFormat_fontStrikeOut( ::pPtr )


METHOD QTextCharFormat:fontStyleHint()
   RETURN Qt_QTextCharFormat_fontStyleHint( ::pPtr )


METHOD QTextCharFormat:fontStyleStrategy()
   RETURN Qt_QTextCharFormat_fontStyleStrategy( ::pPtr )


METHOD QTextCharFormat:fontUnderline()
   RETURN Qt_QTextCharFormat_fontUnderline( ::pPtr )


METHOD QTextCharFormat:fontWeight()
   RETURN Qt_QTextCharFormat_fontWeight( ::pPtr )


METHOD QTextCharFormat:fontWordSpacing()
   RETURN Qt_QTextCharFormat_fontWordSpacing( ::pPtr )


METHOD QTextCharFormat:isAnchor()
   RETURN Qt_QTextCharFormat_isAnchor( ::pPtr )


METHOD QTextCharFormat:isValid()
   RETURN Qt_QTextCharFormat_isValid( ::pPtr )


METHOD QTextCharFormat:setAnchor( lAnchor )
   RETURN Qt_QTextCharFormat_setAnchor( ::pPtr, lAnchor )


METHOD QTextCharFormat:setAnchorHref( cValue )
   RETURN Qt_QTextCharFormat_setAnchorHref( ::pPtr, cValue )


METHOD QTextCharFormat:setAnchorNames( pNames )
   RETURN Qt_QTextCharFormat_setAnchorNames( ::pPtr, hbqt_ptr( pNames ) )


METHOD QTextCharFormat:setFont( pFont )
   RETURN Qt_QTextCharFormat_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QTextCharFormat:setFontCapitalization( nCapitalization )
   RETURN Qt_QTextCharFormat_setFontCapitalization( ::pPtr, nCapitalization )


METHOD QTextCharFormat:setFontFamily( cFamily )
   RETURN Qt_QTextCharFormat_setFontFamily( ::pPtr, cFamily )


METHOD QTextCharFormat:setFontFixedPitch( lFixedPitch )
   RETURN Qt_QTextCharFormat_setFontFixedPitch( ::pPtr, lFixedPitch )


METHOD QTextCharFormat:setFontItalic( lItalic )
   RETURN Qt_QTextCharFormat_setFontItalic( ::pPtr, lItalic )


METHOD QTextCharFormat:setFontKerning( lEnable )
   RETURN Qt_QTextCharFormat_setFontKerning( ::pPtr, lEnable )


METHOD QTextCharFormat:setFontLetterSpacing( nSpacing )
   RETURN Qt_QTextCharFormat_setFontLetterSpacing( ::pPtr, nSpacing )


METHOD QTextCharFormat:setFontOverline( lOverline )
   RETURN Qt_QTextCharFormat_setFontOverline( ::pPtr, lOverline )


METHOD QTextCharFormat:setFontPointSize( nSize )
   RETURN Qt_QTextCharFormat_setFontPointSize( ::pPtr, nSize )


METHOD QTextCharFormat:setFontStrikeOut( lStrikeOut )
   RETURN Qt_QTextCharFormat_setFontStrikeOut( ::pPtr, lStrikeOut )


METHOD QTextCharFormat:setFontStyleHint( nHint, nStrategy )
   RETURN Qt_QTextCharFormat_setFontStyleHint( ::pPtr, nHint, nStrategy )


METHOD QTextCharFormat:setFontStyleStrategy( nStrategy )
   RETURN Qt_QTextCharFormat_setFontStyleStrategy( ::pPtr, nStrategy )


METHOD QTextCharFormat:setFontUnderline( lUnderline )
   RETURN Qt_QTextCharFormat_setFontUnderline( ::pPtr, lUnderline )


METHOD QTextCharFormat:setFontWeight( nWeight )
   RETURN Qt_QTextCharFormat_setFontWeight( ::pPtr, nWeight )


METHOD QTextCharFormat:setFontWordSpacing( nSpacing )
   RETURN Qt_QTextCharFormat_setFontWordSpacing( ::pPtr, nSpacing )


METHOD QTextCharFormat:setTextOutline( pPen )
   RETURN Qt_QTextCharFormat_setTextOutline( ::pPtr, hbqt_ptr( pPen ) )


METHOD QTextCharFormat:setToolTip( cText )
   RETURN Qt_QTextCharFormat_setToolTip( ::pPtr, cText )


METHOD QTextCharFormat:setUnderlineColor( pColor )
   RETURN Qt_QTextCharFormat_setUnderlineColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD QTextCharFormat:setUnderlineStyle( nStyle )
   RETURN Qt_QTextCharFormat_setUnderlineStyle( ::pPtr, nStyle )


METHOD QTextCharFormat:setVerticalAlignment( nAlignment )
   RETURN Qt_QTextCharFormat_setVerticalAlignment( ::pPtr, nAlignment )


METHOD QTextCharFormat:textOutline()
   RETURN HB_QPen():from( Qt_QTextCharFormat_textOutline( ::pPtr ) )


METHOD QTextCharFormat:toolTip()
   RETURN Qt_QTextCharFormat_toolTip( ::pPtr )


METHOD QTextCharFormat:underlineColor()
   RETURN HB_QColor():from( Qt_QTextCharFormat_underlineColor( ::pPtr ) )


METHOD QTextCharFormat:underlineStyle()
   RETURN Qt_QTextCharFormat_underlineStyle( ::pPtr )


METHOD QTextCharFormat:verticalAlignment()
   RETURN Qt_QTextCharFormat_verticalAlignment( ::pPtr )

