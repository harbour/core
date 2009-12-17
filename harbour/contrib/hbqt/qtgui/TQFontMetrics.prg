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


CREATE CLASS QFontMetrics

   VAR     pPtr

   METHOD  new()
   METHOD  configure( xObject )

   METHOD  ascent()
   METHOD  averageCharWidth()
   METHOD  boundingRect( nCh )
   METHOD  boundingRect_1( cText )
   METHOD  boundingRect_2( nX, nY, nWidth, nHeight, nFlags, cText, nTabStops, nTabArray )
   METHOD  boundingRect_3( pRect, nFlags, cText, nTabStops, nTabArray )
   METHOD  descent()
   METHOD  elidedText( cText, nMode, nWidth, nFlags )
   METHOD  height()
   METHOD  inFont( nCh )
   METHOD  leading()
   METHOD  leftBearing( nCh )
   METHOD  lineSpacing()
   METHOD  lineWidth()
   METHOD  maxWidth()
   METHOD  minLeftBearing()
   METHOD  minRightBearing()
   METHOD  overlinePos()
   METHOD  rightBearing( nCh )
   METHOD  size( nFlags, cText, nTabStops, nTabArray )
   METHOD  strikeOutPos()
   METHOD  tightBoundingRect( cText )
   METHOD  underlinePos()
   METHOD  width( cText, nLen )
   METHOD  width_1( nCh )
   METHOD  xHeight()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD QFontMetrics:new( ... )

   ::pPtr := Qt_QFontMetrics( ... )

   RETURN Self


METHOD QFontMetrics:configure( xObject )
   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF
   RETURN Self


METHOD QFontMetrics:ascent()
   RETURN Qt_QFontMetrics_ascent( ::pPtr )


METHOD QFontMetrics:averageCharWidth()
   RETURN Qt_QFontMetrics_averageCharWidth( ::pPtr )


METHOD QFontMetrics:boundingRect( nCh )
   RETURN Qt_QFontMetrics_boundingRect( ::pPtr, nCh )


METHOD QFontMetrics:boundingRect_1( cText )
   RETURN Qt_QFontMetrics_boundingRect_1( ::pPtr, cText )


METHOD QFontMetrics:boundingRect_2( nX, nY, nWidth, nHeight, nFlags, cText, nTabStops, nTabArray )
   RETURN Qt_QFontMetrics_boundingRect_2( ::pPtr, nX, nY, nWidth, nHeight, nFlags, cText, nTabStops, nTabArray )


METHOD QFontMetrics:boundingRect_3( pRect, nFlags, cText, nTabStops, nTabArray )
   RETURN Qt_QFontMetrics_boundingRect_3( ::pPtr, hbqt_ptr( pRect ), nFlags, cText, nTabStops, nTabArray )


METHOD QFontMetrics:descent()
   RETURN Qt_QFontMetrics_descent( ::pPtr )


METHOD QFontMetrics:elidedText( cText, nMode, nWidth, nFlags )
   RETURN Qt_QFontMetrics_elidedText( ::pPtr, cText, nMode, nWidth, nFlags )


METHOD QFontMetrics:height()
   RETURN Qt_QFontMetrics_height( ::pPtr )


METHOD QFontMetrics:inFont( nCh )
   RETURN Qt_QFontMetrics_inFont( ::pPtr, nCh )


METHOD QFontMetrics:leading()
   RETURN Qt_QFontMetrics_leading( ::pPtr )


METHOD QFontMetrics:leftBearing( nCh )
   RETURN Qt_QFontMetrics_leftBearing( ::pPtr, nCh )


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


METHOD QFontMetrics:rightBearing( nCh )
   RETURN Qt_QFontMetrics_rightBearing( ::pPtr, nCh )


METHOD QFontMetrics:size( nFlags, cText, nTabStops, nTabArray )
   RETURN Qt_QFontMetrics_size( ::pPtr, nFlags, cText, nTabStops, nTabArray )


METHOD QFontMetrics:strikeOutPos()
   RETURN Qt_QFontMetrics_strikeOutPos( ::pPtr )


METHOD QFontMetrics:tightBoundingRect( cText )
   RETURN Qt_QFontMetrics_tightBoundingRect( ::pPtr, cText )


METHOD QFontMetrics:underlinePos()
   RETURN Qt_QFontMetrics_underlinePos( ::pPtr )


METHOD QFontMetrics:width( cText, nLen )
   RETURN Qt_QFontMetrics_width( ::pPtr, cText, nLen )


METHOD QFontMetrics:width_1( nCh )
   RETURN Qt_QFontMetrics_width_1( ::pPtr, nCh )


METHOD QFontMetrics:xHeight()
   RETURN Qt_QFontMetrics_xHeight( ::pPtr )

