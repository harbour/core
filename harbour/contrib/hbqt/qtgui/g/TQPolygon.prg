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


FUNCTION QPolygon( ... )
   RETURN HB_QPolygon():new( ... )


CREATE CLASS QPolygon INHERIT HbQtObjectHandler FUNCTION HB_QPolygon

   METHOD  new( ... )

   METHOD  boundingRect()
   METHOD  containsPoint( pPoint, nFillRule )
   METHOD  intersected( pR )
   METHOD  point( ... )
   METHOD  putPoints( nIndex, nNPoints, pFromPolygon, nFromIndex )
   METHOD  setPoint( ... )
   METHOD  setPoints( nNPoints, nPoints )
   METHOD  subtracted( pR )
   METHOD  translate( ... )
   METHOD  united( pR )

   ENDCLASS


METHOD QPolygon:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPolygon( ... )
   RETURN Self


METHOD QPolygon:boundingRect()
   RETURN Qt_QPolygon_boundingRect( ::pPtr )


METHOD QPolygon:containsPoint( pPoint, nFillRule )
   RETURN Qt_QPolygon_containsPoint( ::pPtr, hbqt_ptr( pPoint ), nFillRule )


METHOD QPolygon:intersected( pR )
   RETURN Qt_QPolygon_intersected( ::pPtr, hbqt_ptr( pR ) )


METHOD QPolygon:point( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void point ( int index, int * x, int * y ) const
                // N n int, N @ int, N @ int
         RETURN Qt_QPolygon_point( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // QPoint point ( int index ) const
                // N n int
         RETURN QPoint():from( Qt_QPolygon_point_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPolygon:putPoints( nIndex, nNPoints, pFromPolygon, nFromIndex )
   RETURN Qt_QPolygon_putPoints( ::pPtr, nIndex, nNPoints, hbqt_ptr( pFromPolygon ), nFromIndex )


METHOD QPolygon:setPoint( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void setPoint ( int index, int x, int y )
                // N n int, N n int, N n int
         RETURN Qt_QPolygon_setPoint( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO"
                // void setPoint ( int index, const QPoint & point )
                // N n int, PO p QPoint
         RETURN Qt_QPolygon_setPoint_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPolygon:setPoints( nNPoints, nPoints )
   RETURN Qt_QPolygon_setPoints( ::pPtr, nNPoints, nPoints )


METHOD QPolygon:subtracted( pR )
   RETURN Qt_QPolygon_subtracted( ::pPtr, hbqt_ptr( pR ) )


METHOD QPolygon:translate( ... )
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
                // void translate ( int dx, int dy )
                // N n int, N n int
         RETURN Qt_QPolygon_translate( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void translate ( const QPoint & offset )
                // PO p QPoint
         RETURN Qt_QPolygon_translate_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QPolygon:united( pR )
   RETURN Qt_QPolygon_united( ::pPtr, hbqt_ptr( pR ) )

