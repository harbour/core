/*
 * $Id$
 */

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


CREATE CLASS QTextCharFormat INHERIT QTextFormat

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTextCharFormat_destroy( ::pPtr )

   METHOD  anchorHref()                        INLINE  Qt_QTextCharFormat_anchorHref( ::pPtr )
   METHOD  anchorNames()                       INLINE  Qt_QTextCharFormat_anchorNames( ::pPtr )
   METHOD  font()                              INLINE  Qt_QTextCharFormat_font( ::pPtr )
   METHOD  fontCapitalization()                INLINE  Qt_QTextCharFormat_fontCapitalization( ::pPtr )
   METHOD  fontFamily()                        INLINE  Qt_QTextCharFormat_fontFamily( ::pPtr )
   METHOD  fontFixedPitch()                    INLINE  Qt_QTextCharFormat_fontFixedPitch( ::pPtr )
   METHOD  fontItalic()                        INLINE  Qt_QTextCharFormat_fontItalic( ::pPtr )
   METHOD  fontKerning()                       INLINE  Qt_QTextCharFormat_fontKerning( ::pPtr )
   METHOD  fontLetterSpacing()                 INLINE  Qt_QTextCharFormat_fontLetterSpacing( ::pPtr )
   METHOD  fontOverline()                      INLINE  Qt_QTextCharFormat_fontOverline( ::pPtr )
   METHOD  fontPointSize()                     INLINE  Qt_QTextCharFormat_fontPointSize( ::pPtr )
   METHOD  fontStrikeOut()                     INLINE  Qt_QTextCharFormat_fontStrikeOut( ::pPtr )
   METHOD  fontStyleHint()                     INLINE  Qt_QTextCharFormat_fontStyleHint( ::pPtr )
   METHOD  fontStyleStrategy()                 INLINE  Qt_QTextCharFormat_fontStyleStrategy( ::pPtr )
   METHOD  fontUnderline()                     INLINE  Qt_QTextCharFormat_fontUnderline( ::pPtr )
   METHOD  fontWeight()                        INLINE  Qt_QTextCharFormat_fontWeight( ::pPtr )
   METHOD  fontWordSpacing()                   INLINE  Qt_QTextCharFormat_fontWordSpacing( ::pPtr )
   METHOD  isAnchor()                          INLINE  Qt_QTextCharFormat_isAnchor( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QTextCharFormat_isValid( ::pPtr )
   METHOD  setAnchor( lAnchor )                INLINE  Qt_QTextCharFormat_setAnchor( ::pPtr, lAnchor )
   METHOD  setAnchorHref( cValue )             INLINE  Qt_QTextCharFormat_setAnchorHref( ::pPtr, cValue )
   METHOD  setAnchorNames( pNames )            INLINE  Qt_QTextCharFormat_setAnchorNames( ::pPtr, pNames )
   METHOD  setFont( pFont )                    INLINE  Qt_QTextCharFormat_setFont( ::pPtr, pFont )
   METHOD  setFontCapitalization( nCapitalization )  INLINE  Qt_QTextCharFormat_setFontCapitalization( ::pPtr, nCapitalization )
   METHOD  setFontFamily( cFamily )            INLINE  Qt_QTextCharFormat_setFontFamily( ::pPtr, cFamily )
   METHOD  setFontFixedPitch( lFixedPitch )    INLINE  Qt_QTextCharFormat_setFontFixedPitch( ::pPtr, lFixedPitch )
   METHOD  setFontItalic( lItalic )            INLINE  Qt_QTextCharFormat_setFontItalic( ::pPtr, lItalic )
   METHOD  setFontKerning( lEnable )           INLINE  Qt_QTextCharFormat_setFontKerning( ::pPtr, lEnable )
   METHOD  setFontLetterSpacing( nSpacing )    INLINE  Qt_QTextCharFormat_setFontLetterSpacing( ::pPtr, nSpacing )
   METHOD  setFontOverline( lOverline )        INLINE  Qt_QTextCharFormat_setFontOverline( ::pPtr, lOverline )
   METHOD  setFontPointSize( nSize )           INLINE  Qt_QTextCharFormat_setFontPointSize( ::pPtr, nSize )
   METHOD  setFontStrikeOut( lStrikeOut )      INLINE  Qt_QTextCharFormat_setFontStrikeOut( ::pPtr, lStrikeOut )
   METHOD  setFontStyleHint( nHint, nStrategy )  INLINE  Qt_QTextCharFormat_setFontStyleHint( ::pPtr, nHint, nStrategy )
   METHOD  setFontStyleStrategy( nStrategy )   INLINE  Qt_QTextCharFormat_setFontStyleStrategy( ::pPtr, nStrategy )
   METHOD  setFontUnderline( lUnderline )      INLINE  Qt_QTextCharFormat_setFontUnderline( ::pPtr, lUnderline )
   METHOD  setFontWeight( nWeight )            INLINE  Qt_QTextCharFormat_setFontWeight( ::pPtr, nWeight )
   METHOD  setFontWordSpacing( nSpacing )      INLINE  Qt_QTextCharFormat_setFontWordSpacing( ::pPtr, nSpacing )
   METHOD  setTextOutline( pPen )              INLINE  Qt_QTextCharFormat_setTextOutline( ::pPtr, pPen )
   METHOD  setToolTip( cText )                 INLINE  Qt_QTextCharFormat_setToolTip( ::pPtr, cText )
   METHOD  setUnderlineColor( pColor )         INLINE  Qt_QTextCharFormat_setUnderlineColor( ::pPtr, pColor )
   METHOD  setUnderlineStyle( nStyle )         INLINE  Qt_QTextCharFormat_setUnderlineStyle( ::pPtr, nStyle )
   METHOD  setVerticalAlignment( nAlignment )  INLINE  Qt_QTextCharFormat_setVerticalAlignment( ::pPtr, nAlignment )
   METHOD  textOutline()                       INLINE  Qt_QTextCharFormat_textOutline( ::pPtr )
   METHOD  toolTip()                           INLINE  Qt_QTextCharFormat_toolTip( ::pPtr )
   METHOD  underlineColor()                    INLINE  Qt_QTextCharFormat_underlineColor( ::pPtr )
   METHOD  underlineStyle()                    INLINE  Qt_QTextCharFormat_underlineStyle( ::pPtr )
   METHOD  verticalAlignment()                 INLINE  Qt_QTextCharFormat_verticalAlignment( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextCharFormat

   ::pParent := pParent

   ::pPtr := Qt_QTextCharFormat( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTextCharFormat

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
