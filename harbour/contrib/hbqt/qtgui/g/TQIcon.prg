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


FUNCTION QIcon( ... )
   RETURN HB_QIcon():new( ... )


CREATE CLASS QIcon INHERIT HbQtObjectHandler FUNCTION HB_QIcon

   METHOD  new( ... )

   METHOD  actualSize( pSize, nMode, nState )
   METHOD  addFile( cFileName, pSize, nMode, nState )
   METHOD  addPixmap( pPixmap, nMode, nState )
   METHOD  availableSizes( nMode, nState )
   METHOD  cacheKey()
   METHOD  isNull()
   METHOD  paint( ... )
   METHOD  pixmap( ... )

   ENDCLASS


METHOD QIcon:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QIcon( ... )
   RETURN Self


METHOD QIcon:actualSize( pSize, nMode, nState )
   RETURN Qt_QIcon_actualSize( ::pPtr, hbqt_ptr( pSize ), nMode, nState )


METHOD QIcon:addFile( cFileName, pSize, nMode, nState )
   RETURN Qt_QIcon_addFile( ::pPtr, cFileName, hbqt_ptr( pSize ), nMode, nState )


METHOD QIcon:addPixmap( pPixmap, nMode, nState )
   RETURN Qt_QIcon_addPixmap( ::pPtr, hbqt_ptr( pPixmap ), nMode, nState )


METHOD QIcon:availableSizes( nMode, nState )
   RETURN Qt_QIcon_availableSizes( ::pPtr, nMode, nState )


METHOD QIcon:cacheKey()
   RETURN Qt_QIcon_cacheKey( ::pPtr )


METHOD QIcon:isNull()
   RETURN Qt_QIcon_isNull( ::pPtr )


METHOD QIcon:paint( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 8
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N" .AND. aV[ 6 ] $ "N" .AND. aV[ 7 ] $ "N" .AND. aV[ 8 ] $ "N"
                // void paint ( QPainter * painter, int x, int y, int w, int h, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
                // PO p QPainter, N n int, N n int, N n int, N n int, N n Qt::Alignment, N n QIcon::Mode, N n QIcon::State
         RETURN Qt_QIcon_paint_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 5
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // void paint ( QPainter * painter, int x, int y, int w, int h, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
                // PO p QPainter, N n int, N n int, N n int, N n int, N n Qt::Alignment, N n QIcon::Mode, N n QIcon::State
         RETURN Qt_QIcon_paint_1( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N" .AND. aV[ 5 ] $ "N"
                // void paint ( QPainter * painter, const QRect & rect, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
                // PO p QPainter, PO p QRect, N n Qt::Alignment, N n QIcon::Mode, N n QIcon::State
         RETURN Qt_QIcon_paint( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // void paint ( QPainter * painter, const QRect & rect, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
                // PO p QPainter, PO p QRect, N n Qt::Alignment, N n QIcon::Mode, N n QIcon::State
         RETURN Qt_QIcon_paint( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QIcon:pixmap( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // QPixmap pixmap ( int w, int h, Mode mode = Normal, State state = Off ) const
                // N n int, N n int, N n QIcon::Mode, N n QIcon::State
         RETURN QPixmap():from( Qt_QIcon_pixmap_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // QPixmap pixmap ( const QSize & size, Mode mode = Normal, State state = Off ) const
                // PO p QSize, N n QIcon::Mode, N n QIcon::State
         RETURN QPixmap():from( Qt_QIcon_pixmap( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QPixmap pixmap ( int w, int h, Mode mode = Normal, State state = Off ) const
                // N n int, N n int, N n QIcon::Mode, N n QIcon::State
         RETURN QPixmap():from( Qt_QIcon_pixmap_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QPixmap pixmap ( const QSize & size, Mode mode = Normal, State state = Off ) const
                // PO p QSize, N n QIcon::Mode, N n QIcon::State
         RETURN QPixmap():from( Qt_QIcon_pixmap( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL

