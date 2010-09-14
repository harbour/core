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
   METHOD  count()
   METHOD  count_1( lOn )
   METHOD  fill( lValue, nSize )
   METHOD  fill_1( lValue, nBegin, nEnd )
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  resize( nSize )
   METHOD  setBit( nI )
   METHOD  setBit_1( nI, lValue )
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


METHOD QBitArray:count()
   RETURN Qt_QBitArray_count( ::pPtr )


METHOD QBitArray:count_1( lOn )
   RETURN Qt_QBitArray_count_1( ::pPtr, lOn )


METHOD QBitArray:fill( lValue, nSize )
   RETURN Qt_QBitArray_fill( ::pPtr, lValue, nSize )


METHOD QBitArray:fill_1( lValue, nBegin, nEnd )
   RETURN Qt_QBitArray_fill_1( ::pPtr, lValue, nBegin, nEnd )


METHOD QBitArray:isEmpty()
   RETURN Qt_QBitArray_isEmpty( ::pPtr )


METHOD QBitArray:isNull()
   RETURN Qt_QBitArray_isNull( ::pPtr )


METHOD QBitArray:resize( nSize )
   RETURN Qt_QBitArray_resize( ::pPtr, nSize )


METHOD QBitArray:setBit( nI )
   RETURN Qt_QBitArray_setBit( ::pPtr, nI )


METHOD QBitArray:setBit_1( nI, lValue )
   RETURN Qt_QBitArray_setBit_1( ::pPtr, nI, lValue )


METHOD QBitArray:size()
   RETURN Qt_QBitArray_size( ::pPtr )


METHOD QBitArray:testBit( nI )
   RETURN Qt_QBitArray_testBit( ::pPtr, nI )


METHOD QBitArray:toggleBit( nI )
   RETURN Qt_QBitArray_toggleBit( ::pPtr, nI )


METHOD QBitArray:truncate( nPos )
   RETURN Qt_QBitArray_truncate( ::pPtr, nPos )

