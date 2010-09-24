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


FUNCTION QFontMetricsF( ... )
   RETURN HB_QFontMetricsF():new( ... )


CREATE CLASS QFontMetricsF INHERIT HbQtObjectHandler FUNCTION HB_QFontMetricsF

   METHOD  new( ... )

   METHOD  ascent()
   METHOD  averageCharWidth()
   METHOD  boundingRect( ... )
   METHOD  descent()
   METHOD  elidedText( cText, nMode, nWidth, nFlags )
   METHOD  height()
   METHOD  inFont( pCh )
   METHOD  leading()
   METHOD  leftBearing( pCh )
   METHOD  lineSpacing()
   METHOD  lineWidth()
   METHOD  maxWidth()
   METHOD  minLeftBearing()
   METHOD  minRightBearing()
   METHOD  overlinePos()
   METHOD  rightBearing( pCh )
   METHOD  size( nFlags, cText, nTabStops, nTabArray )
   METHOD  strikeOutPos()
   METHOD  tightBoundingRect( cText )
   METHOD  underlinePos()
   METHOD  width( ... )
   METHOD  xHeight()

   ENDCLASS


METHOD QFontMetricsF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontMetricsF( ... )
   RETURN Self


METHOD QFontMetricsF:ascent()
   RETURN Qt_QFontMetricsF_ascent( ::pPtr )


METHOD QFontMetricsF:averageCharWidth()
   RETURN Qt_QFontMetricsF_averageCharWidth( ::pPtr )


METHOD QFontMetricsF:boundingRect( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 5
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "C" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // QRectF boundingRect ( const QRectF & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
                // PO p QRectF, N n int, C c QString, N n int, N @ int
         RETURN QRectF():from( Qt_QFontMetricsF_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "C"
                // QRectF boundingRect ( const QRectF & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
                // PO p QRectF, N n int, C c QString, N n int, N @ int
         RETURN QRectF():from( Qt_QFontMetricsF_boundingRect_2( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QRectF boundingRect ( const QString & text ) const
                // C c QString
         RETURN QRectF():from( Qt_QFontMetricsF_boundingRect( ::pPtr, ... ) )
      CASE aV[ 1 ] $ "PO"
                // QRectF boundingRect ( QChar ch ) const
                // PO p QChar
         RETURN QRectF():from( Qt_QFontMetricsF_boundingRect_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFontMetricsF:descent()
   RETURN Qt_QFontMetricsF_descent( ::pPtr )


METHOD QFontMetricsF:elidedText( cText, nMode, nWidth, nFlags )
   RETURN Qt_QFontMetricsF_elidedText( ::pPtr, cText, nMode, nWidth, nFlags )


METHOD QFontMetricsF:height()
   RETURN Qt_QFontMetricsF_height( ::pPtr )


METHOD QFontMetricsF:inFont( pCh )
   RETURN Qt_QFontMetricsF_inFont( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetricsF:leading()
   RETURN Qt_QFontMetricsF_leading( ::pPtr )


METHOD QFontMetricsF:leftBearing( pCh )
   RETURN Qt_QFontMetricsF_leftBearing( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetricsF:lineSpacing()
   RETURN Qt_QFontMetricsF_lineSpacing( ::pPtr )


METHOD QFontMetricsF:lineWidth()
   RETURN Qt_QFontMetricsF_lineWidth( ::pPtr )


METHOD QFontMetricsF:maxWidth()
   RETURN Qt_QFontMetricsF_maxWidth( ::pPtr )


METHOD QFontMetricsF:minLeftBearing()
   RETURN Qt_QFontMetricsF_minLeftBearing( ::pPtr )


METHOD QFontMetricsF:minRightBearing()
   RETURN Qt_QFontMetricsF_minRightBearing( ::pPtr )


METHOD QFontMetricsF:overlinePos()
   RETURN Qt_QFontMetricsF_overlinePos( ::pPtr )


METHOD QFontMetricsF:rightBearing( pCh )
   RETURN Qt_QFontMetricsF_rightBearing( ::pPtr, hbqt_ptr( pCh ) )


METHOD QFontMetricsF:size( nFlags, cText, nTabStops, nTabArray )
   RETURN Qt_QFontMetricsF_size( ::pPtr, nFlags, cText, nTabStops, nTabArray )


METHOD QFontMetricsF:strikeOutPos()
   RETURN Qt_QFontMetricsF_strikeOutPos( ::pPtr )


METHOD QFontMetricsF:tightBoundingRect( cText )
   RETURN Qt_QFontMetricsF_tightBoundingRect( ::pPtr, cText )


METHOD QFontMetricsF:underlinePos()
   RETURN Qt_QFontMetricsF_underlinePos( ::pPtr )


METHOD QFontMetricsF:width( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // qreal width ( const QString & text ) const
                // C c QString
         RETURN Qt_QFontMetricsF_width( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO"
                // qreal width ( QChar ch ) const
                // PO p QChar
         RETURN Qt_QFontMetricsF_width_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFontMetricsF:xHeight()
   RETURN Qt_QFontMetricsF_xHeight( ::pPtr )

