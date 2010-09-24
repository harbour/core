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


FUNCTION QBitArray( ... )
   RETURN HB_QBitArray():new( ... )


CREATE CLASS QBitArray INHERIT HbQtObjectHandler FUNCTION HB_QBitArray

   METHOD  new( ... )

   METHOD  at( nI )
   METHOD  clear()
   METHOD  clearBit( nI )
   METHOD  count( ... )
   METHOD  fill( ... )
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  resize( nSize )
   METHOD  setBit( ... )
   METHOD  size()
   METHOD  testBit( nI )
   METHOD  toggleBit( nI )
   METHOD  truncate( nPos )

   ENDCLASS


METHOD QBitArray:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QBitArray( ... )
   RETURN Self


METHOD QBitArray:at( nI )
   RETURN Qt_QBitArray_at( ::pPtr, nI )


METHOD QBitArray:clear()
   RETURN Qt_QBitArray_clear( ::pPtr )


METHOD QBitArray:clearBit( nI )
   RETURN Qt_QBitArray_clearBit( ::pPtr, nI )


METHOD QBitArray:count( ... )
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
      CASE aV[ 1 ] $ "L"
                // int count ( bool on ) const
                // L l bool
         RETURN Qt_QBitArray_count_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // int count () const
      RETURN Qt_QBitArray_count( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QBitArray:fill( ... )
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
      CASE aV[ 1 ] $ "L" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void fill ( bool value, int begin, int end )
                // L l bool, N n int, N n int
         RETURN Qt_QBitArray_fill_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "L" .AND. aV[ 2 ] $ "N"
                // bool fill ( bool value, int size = -1 )
                // L l bool, N n int
         RETURN Qt_QBitArray_fill( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "L"
                // bool fill ( bool value, int size = -1 )
                // L l bool, N n int
         RETURN Qt_QBitArray_fill( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QBitArray:isEmpty()
   RETURN Qt_QBitArray_isEmpty( ::pPtr )


METHOD QBitArray:isNull()
   RETURN Qt_QBitArray_isNull( ::pPtr )


METHOD QBitArray:resize( nSize )
   RETURN Qt_QBitArray_resize( ::pPtr, nSize )


METHOD QBitArray:setBit( ... )
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
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "L"
                // void setBit ( int i, bool value )
                // N n int, L l bool
         RETURN Qt_QBitArray_setBit_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // void setBit ( int i )
                // N n int
         RETURN Qt_QBitArray_setBit( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QBitArray:size()
   RETURN Qt_QBitArray_size( ::pPtr )


METHOD QBitArray:testBit( nI )
   RETURN Qt_QBitArray_testBit( ::pPtr, nI )


METHOD QBitArray:toggleBit( nI )
   RETURN Qt_QBitArray_toggleBit( ::pPtr, nI )


METHOD QBitArray:truncate( nPos )
   RETURN Qt_QBitArray_truncate( ::pPtr, nPos )

