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


FUNCTION QStylePainter( ... )
   RETURN HB_QStylePainter():new( ... )


CREATE CLASS QStylePainter INHERIT HbQtObjectHandler, HB_QPainter FUNCTION HB_QStylePainter

   METHOD  new( ... )

   METHOD  begin( ... )
   METHOD  drawComplexControl( nCc, pOption )
   METHOD  drawControl( nCe, pOption )
   METHOD  drawItemPixmap( pRect, nFlags, pPixmap )
   METHOD  drawItemText( pRect, nFlags, pPal, lEnabled, cText, nTextRole )
   METHOD  drawPrimitive( nPe, pOption )
   METHOD  style()

   ENDCLASS


METHOD QStylePainter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStylePainter( ... )
   RETURN Self


METHOD QStylePainter:begin( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // bool begin ( QPaintDevice * pd, QWidget * widget )
                // PO p QPaintDevice, PO p QWidget
         RETURN Qt_QStylePainter_begin_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // bool begin ( QWidget * widget )
                // PO p QWidget
         RETURN Qt_QStylePainter_begin( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QStylePainter:drawComplexControl( nCc, pOption )
   RETURN Qt_QStylePainter_drawComplexControl( ::pPtr, nCc, hbqt_ptr( pOption ) )


METHOD QStylePainter:drawControl( nCe, pOption )
   RETURN Qt_QStylePainter_drawControl( ::pPtr, nCe, hbqt_ptr( pOption ) )


METHOD QStylePainter:drawItemPixmap( pRect, nFlags, pPixmap )
   RETURN Qt_QStylePainter_drawItemPixmap( ::pPtr, hbqt_ptr( pRect ), nFlags, hbqt_ptr( pPixmap ) )


METHOD QStylePainter:drawItemText( pRect, nFlags, pPal, lEnabled, cText, nTextRole )
   RETURN Qt_QStylePainter_drawItemText( ::pPtr, hbqt_ptr( pRect ), nFlags, hbqt_ptr( pPal ), lEnabled, cText, nTextRole )


METHOD QStylePainter:drawPrimitive( nPe, pOption )
   RETURN Qt_QStylePainter_drawPrimitive( ::pPtr, nPe, hbqt_ptr( pOption ) )


METHOD QStylePainter:style()
   RETURN Qt_QStylePainter_style( ::pPtr )

