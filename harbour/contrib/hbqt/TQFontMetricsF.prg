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


CREATE CLASS QFontMetricsF

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  ascent()                            INLINE  Qt_QFontMetricsF_ascent( ::pPtr )
   METHOD  averageCharWidth()                  INLINE  Qt_QFontMetricsF_averageCharWidth( ::pPtr )
   METHOD  boundingRect( cText )               INLINE  Qt_QFontMetricsF_boundingRect( ::pPtr, cText )
   METHOD  boundingRect_1( nCh )               INLINE  Qt_QFontMetricsF_boundingRect_1( ::pPtr, nCh )
   METHOD  boundingRect_2( pRect, nFlags, cText, nTabStops, nTabArray )  INLINE  Qt_QFontMetricsF_boundingRect_2( ::pPtr, pRect, nFlags, cText, nTabStops, nTabArray )
   METHOD  descent()                           INLINE  Qt_QFontMetricsF_descent( ::pPtr )
   METHOD  elidedText( cText, nMode, nWidth, nFlags )  INLINE  Qt_QFontMetricsF_elidedText( ::pPtr, cText, nMode, nWidth, nFlags )
   METHOD  height()                            INLINE  Qt_QFontMetricsF_height( ::pPtr )
   METHOD  inFont( nCh )                       INLINE  Qt_QFontMetricsF_inFont( ::pPtr, nCh )
   METHOD  leading()                           INLINE  Qt_QFontMetricsF_leading( ::pPtr )
   METHOD  leftBearing( nCh )                  INLINE  Qt_QFontMetricsF_leftBearing( ::pPtr, nCh )
   METHOD  lineSpacing()                       INLINE  Qt_QFontMetricsF_lineSpacing( ::pPtr )
   METHOD  lineWidth()                         INLINE  Qt_QFontMetricsF_lineWidth( ::pPtr )
   METHOD  maxWidth()                          INLINE  Qt_QFontMetricsF_maxWidth( ::pPtr )
   METHOD  minLeftBearing()                    INLINE  Qt_QFontMetricsF_minLeftBearing( ::pPtr )
   METHOD  minRightBearing()                   INLINE  Qt_QFontMetricsF_minRightBearing( ::pPtr )
   METHOD  overlinePos()                       INLINE  Qt_QFontMetricsF_overlinePos( ::pPtr )
   METHOD  rightBearing( nCh )                 INLINE  Qt_QFontMetricsF_rightBearing( ::pPtr, nCh )
   METHOD  size( nFlags, cText, nTabStops, nTabArray )  INLINE  Qt_QFontMetricsF_size( ::pPtr, nFlags, cText, nTabStops, nTabArray )
   METHOD  strikeOutPos()                      INLINE  Qt_QFontMetricsF_strikeOutPos( ::pPtr )
   METHOD  tightBoundingRect( cText )          INLINE  Qt_QFontMetricsF_tightBoundingRect( ::pPtr, cText )
   METHOD  underlinePos()                      INLINE  Qt_QFontMetricsF_underlinePos( ::pPtr )
   METHOD  width( cText )                      INLINE  Qt_QFontMetricsF_width( ::pPtr, cText )
   METHOD  width_1( nCh )                      INLINE  Qt_QFontMetricsF_width_1( ::pPtr, nCh )
   METHOD  xHeight()                           INLINE  Qt_QFontMetricsF_xHeight( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( ... ) CLASS QFontMetricsF

   ::pPtr := Qt_QFontMetricsF( ... )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QFontMetricsF

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
