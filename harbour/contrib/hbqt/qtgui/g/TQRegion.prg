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


FUNCTION QRegion( ... )
   RETURN HB_QRegion():new( ... )


CREATE CLASS QRegion INHERIT HbQtObjectHandler FUNCTION HB_QRegion

   METHOD  new( ... )

   METHOD  boundingRect()
   METHOD  contains( ... )
   METHOD  intersected( ... )
   METHOD  intersects( ... )
   METHOD  isEmpty()
   METHOD  numRects()
   METHOD  setRects( pRects, nNumber )
   METHOD  subtracted( pR )
   METHOD  translate( ... )
   METHOD  translated( ... )
   METHOD  united( ... )
   METHOD  xored( pR )

   ENDCLASS


METHOD QRegion:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRegion( ... )
   RETURN Self


METHOD QRegion:boundingRect()
   RETURN Qt_QRegion_boundingRect( ::pPtr )


METHOD QRegion:contains( ... )
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
      CASE aV[ 1 ] $ "PO"
                // bool contains ( const QPoint & p ) const
                // PO p QPoint
         RETURN Qt_QRegion_contains( ::pPtr, ... )
                // bool contains ( const QRect & r ) const
                // PO p QRect
         // RETURN Qt_QRegion_contains_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QRegion:intersected( ... )
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
      CASE aV[ 1 ] $ "PO"
                // QRegion intersected ( const QRegion & r ) const
                // PO p QRegion
         RETURN QRegion():from( Qt_QRegion_intersected( ::pPtr, ... ) )
                // QRegion intersected ( const QRect & rect ) const
                // PO p QRect
         // RETURN QRegion():from( Qt_QRegion_intersected_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QRegion:intersects( ... )
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
      CASE aV[ 1 ] $ "PO"
                // bool intersects ( const QRegion & region ) const
                // PO p QRegion
         RETURN Qt_QRegion_intersects( ::pPtr, ... )
                // bool intersects ( const QRect & rect ) const
                // PO p QRect
         // RETURN Qt_QRegion_intersects_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QRegion:isEmpty()
   RETURN Qt_QRegion_isEmpty( ::pPtr )


METHOD QRegion:numRects()
   RETURN Qt_QRegion_numRects( ::pPtr )


METHOD QRegion:setRects( pRects, nNumber )
   RETURN Qt_QRegion_setRects( ::pPtr, hbqt_ptr( pRects ), nNumber )


METHOD QRegion:subtracted( pR )
   RETURN Qt_QRegion_subtracted( ::pPtr, hbqt_ptr( pR ) )


METHOD QRegion:translate( ... )
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
         RETURN Qt_QRegion_translate( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void translate ( const QPoint & point )
                // PO p QPoint
         RETURN Qt_QRegion_translate_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QRegion:translated( ... )
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
                // QRegion translated ( int dx, int dy ) const
                // N n int, N n int
         RETURN QRegion():from( Qt_QRegion_translated( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QRegion translated ( const QPoint & p ) const
                // PO p QPoint
         RETURN QRegion():from( Qt_QRegion_translated_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QRegion:united( ... )
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
      CASE aV[ 1 ] $ "PO"
                // QRegion united ( const QRegion & r ) const
                // PO p QRegion
         RETURN QRegion():from( Qt_QRegion_united( ::pPtr, ... ) )
                // QRegion united ( const QRect & rect ) const
                // PO p QRect
         // RETURN QRegion():from( Qt_QRegion_united_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QRegion:xored( pR )
   RETURN Qt_QRegion_xored( ::pPtr, hbqt_ptr( pR ) )

