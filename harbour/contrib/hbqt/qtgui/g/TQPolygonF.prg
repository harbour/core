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


FUNCTION QPolygonF( ... )
   RETURN HB_QPolygonF():new( ... )


CREATE CLASS QPolygonF INHERIT HbQtObjectHandler FUNCTION HB_QPolygonF

   METHOD  new( ... )

   METHOD  boundingRect()
   METHOD  containsPoint( pPoint, nFillRule )
   METHOD  intersected( pR )
   METHOD  isClosed()
   METHOD  subtracted( pR )
   METHOD  toPolygon()
   METHOD  translate( ... )
   METHOD  united( pR )

   ENDCLASS


METHOD QPolygonF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPolygonF( ... )
   RETURN Self


METHOD QPolygonF:boundingRect()
   RETURN Qt_QPolygonF_boundingRect( ::pPtr )


METHOD QPolygonF:containsPoint( pPoint, nFillRule )
   RETURN Qt_QPolygonF_containsPoint( ::pPtr, hbqt_ptr( pPoint ), nFillRule )


METHOD QPolygonF:intersected( pR )
   RETURN Qt_QPolygonF_intersected( ::pPtr, hbqt_ptr( pR ) )


METHOD QPolygonF:isClosed()
   RETURN Qt_QPolygonF_isClosed( ::pPtr )


METHOD QPolygonF:subtracted( pR )
   RETURN Qt_QPolygonF_subtracted( ::pPtr, hbqt_ptr( pR ) )


METHOD QPolygonF:toPolygon()
   RETURN Qt_QPolygonF_toPolygon( ::pPtr )


METHOD QPolygonF:translate( ... )
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
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // void translate ( qreal dx, qreal dy )
                // N n qreal, N n qreal
         RETURN Qt_QPolygonF_translate_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void translate ( const QPointF & offset )
                // PO p QPointF
         RETURN Qt_QPolygonF_translate( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPolygonF:united( pR )
   RETURN Qt_QPolygonF_united( ::pPtr, hbqt_ptr( pR ) )

