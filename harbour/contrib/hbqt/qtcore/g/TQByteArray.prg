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


FUNCTION QByteArray( ... )
   RETURN HB_QByteArray():new( ... )


CREATE CLASS QByteArray INHERIT HbQtObjectHandler FUNCTION HB_QByteArray

   METHOD  new( ... )

   METHOD  append( pBa )
   METHOD  append_1( cStr )
   METHOD  append_2( pStr )
   METHOD  append_3( pStr, nLen )
   METHOD  append_4( cCh )
   METHOD  at( nI )
   METHOD  capacity()
   METHOD  chop( nN )
   METHOD  clear()
   METHOD  constData()
   METHOD  count( pBa )
   METHOD  count_1( pStr )
   METHOD  count_2( cCh )
   METHOD  count_3()
   METHOD  data()
   METHOD  data_1()
   METHOD  endsWith( pBa )
   METHOD  endsWith_1( pStr )
   METHOD  endsWith_2( cCh )
   METHOD  fill( cCh, nSize )
   METHOD  indexOf( pBa, nFrom )
   METHOD  indexOf_1( cStr, nFrom )
   METHOD  indexOf_2( pStr, nFrom )
   METHOD  indexOf_3( cCh, nFrom )
   METHOD  insert( nI, pBa )
   METHOD  insert_1( nI, cStr )
   METHOD  insert_2( nI, pStr )
   METHOD  insert_3( nI, cCh )
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  lastIndexOf( pBa, nFrom )
   METHOD  lastIndexOf_1( cStr, nFrom )
   METHOD  lastIndexOf_2( pStr, nFrom )
   METHOD  lastIndexOf_3( cCh, nFrom )
   METHOD  left( nLen )
   METHOD  leftJustified( nWidth, cFill, lTruncate )
   METHOD  length()
   METHOD  mid( nPos, nLen )
   METHOD  prepend( pBa )
   METHOD  prepend_1( pStr )
   METHOD  prepend_2( cCh )
   METHOD  push_back( pOther )
   METHOD  push_back_1( pStr )
   METHOD  push_back_2( cCh )
   METHOD  push_front( pOther )
   METHOD  push_front_1( pStr )
   METHOD  push_front_2( cCh )
   METHOD  remove( nPos, nLen )
   METHOD  repeated( nTimes )
   METHOD  replace( nPos, nLen, pAfter )
   METHOD  replace_1( nPos, nLen, pAfter )
   METHOD  replace_2( pBefore, pAfter )
   METHOD  replace_3( pBefore, pAfter )
   METHOD  replace_4( pBefore, nBsize, pAfter, nAsize )
   METHOD  replace_5( pBefore, pAfter )
   METHOD  replace_6( cBefore, pAfter )
   METHOD  replace_7( cBefore, pAfter )
   METHOD  replace_8( pBefore, pAfter )
   METHOD  replace_9( cBefore, pAfter )
   METHOD  replace_10( cBefore, cAfter )
   METHOD  replace_11( cBefore, pAfter )
   METHOD  replace_12( cBefore, cAfter )
   METHOD  reserve( nSize )
   METHOD  resize( nSize )
   METHOD  right( nLen )
   METHOD  rightJustified( nWidth, cFill, lTruncate )
   METHOD  setNum( nN, nBase )
   METHOD  setNum_1( nN, nBase )
   METHOD  setNum_2( nN, nBase )
   METHOD  setNum_3( nN, nBase )
   METHOD  setNum_4( nN, nBase )
   METHOD  setNum_5( nN, nBase )
   METHOD  setNum_6( nN, cF, nPrec )
   METHOD  setNum_7( nN, cF, nPrec )
   METHOD  simplified()
   METHOD  size()
   METHOD  split( cSep )
   METHOD  squeeze()
   METHOD  startsWith( pBa )
   METHOD  startsWith_1( pStr )
   METHOD  startsWith_2( cCh )
   METHOD  toBase64()
   METHOD  toDouble( lOk )
   METHOD  toFloat( lOk )
   METHOD  toHex()
   METHOD  toInt( lOk, nBase )
   METHOD  toLong( lOk, nBase )
   METHOD  toLongLong( lOk, nBase )
   METHOD  toLower()
   METHOD  toPercentEncoding( pExclude, pInclude, cPercent )
   METHOD  toShort( lOk, nBase )
   METHOD  toUInt( lOk, nBase )
   METHOD  toULong( lOk, nBase )
   METHOD  toULongLong( lOk, nBase )
   METHOD  toUShort( lOk, nBase )
   METHOD  toUpper()
   METHOD  trimmed()
   METHOD  truncate( nPos )

   ENDCLASS


METHOD QByteArray:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QByteArray( ... )
   RETURN Self


METHOD QByteArray:append( pBa )
   RETURN Qt_QByteArray_append( ::pPtr, hbqt_ptr( pBa ) )


METHOD QByteArray:append_1( cStr )
   RETURN Qt_QByteArray_append_1( ::pPtr, cStr )


METHOD QByteArray:append_2( pStr )
   RETURN Qt_QByteArray_append_2( ::pPtr, hbqt_ptr( pStr ) )


METHOD QByteArray:append_3( pStr, nLen )
   RETURN Qt_QByteArray_append_3( ::pPtr, hbqt_ptr( pStr ), nLen )


METHOD QByteArray:append_4( cCh )
   RETURN Qt_QByteArray_append_4( ::pPtr, cCh )


METHOD QByteArray:at( nI )
   RETURN Qt_QByteArray_at( ::pPtr, nI )


METHOD QByteArray:capacity()
   RETURN Qt_QByteArray_capacity( ::pPtr )


METHOD QByteArray:chop( nN )
   RETURN Qt_QByteArray_chop( ::pPtr, nN )


METHOD QByteArray:clear()
   RETURN Qt_QByteArray_clear( ::pPtr )


METHOD QByteArray:constData()
   RETURN Qt_QByteArray_constData( ::pPtr )


METHOD QByteArray:count( pBa )
   RETURN Qt_QByteArray_count( ::pPtr, hbqt_ptr( pBa ) )


METHOD QByteArray:count_1( pStr )
   RETURN Qt_QByteArray_count_1( ::pPtr, hbqt_ptr( pStr ) )


METHOD QByteArray:count_2( cCh )
   RETURN Qt_QByteArray_count_2( ::pPtr, cCh )


METHOD QByteArray:count_3()
   RETURN Qt_QByteArray_count_3( ::pPtr )


METHOD QByteArray:data()
   RETURN Qt_QByteArray_data( ::pPtr )


METHOD QByteArray:data_1()
   RETURN Qt_QByteArray_data_1( ::pPtr )


METHOD QByteArray:endsWith( pBa )
   RETURN Qt_QByteArray_endsWith( ::pPtr, hbqt_ptr( pBa ) )


METHOD QByteArray:endsWith_1( pStr )
   RETURN Qt_QByteArray_endsWith_1( ::pPtr, hbqt_ptr( pStr ) )


METHOD QByteArray:endsWith_2( cCh )
   RETURN Qt_QByteArray_endsWith_2( ::pPtr, cCh )


METHOD QByteArray:fill( cCh, nSize )
   RETURN Qt_QByteArray_fill( ::pPtr, cCh, nSize )


METHOD QByteArray:indexOf( pBa, nFrom )
   RETURN Qt_QByteArray_indexOf( ::pPtr, hbqt_ptr( pBa ), nFrom )


METHOD QByteArray:indexOf_1( cStr, nFrom )
   RETURN Qt_QByteArray_indexOf_1( ::pPtr, cStr, nFrom )


METHOD QByteArray:indexOf_2( pStr, nFrom )
   RETURN Qt_QByteArray_indexOf_2( ::pPtr, hbqt_ptr( pStr ), nFrom )


METHOD QByteArray:indexOf_3( cCh, nFrom )
   RETURN Qt_QByteArray_indexOf_3( ::pPtr, cCh, nFrom )


METHOD QByteArray:insert( nI, pBa )
   RETURN Qt_QByteArray_insert( ::pPtr, nI, hbqt_ptr( pBa ) )


METHOD QByteArray:insert_1( nI, cStr )
   RETURN Qt_QByteArray_insert_1( ::pPtr, nI, cStr )


METHOD QByteArray:insert_2( nI, pStr )
   RETURN Qt_QByteArray_insert_2( ::pPtr, nI, hbqt_ptr( pStr ) )


METHOD QByteArray:insert_3( nI, cCh )
   RETURN Qt_QByteArray_insert_3( ::pPtr, nI, cCh )


METHOD QByteArray:isEmpty()
   RETURN Qt_QByteArray_isEmpty( ::pPtr )


METHOD QByteArray:isNull()
   RETURN Qt_QByteArray_isNull( ::pPtr )


METHOD QByteArray:lastIndexOf( pBa, nFrom )
   RETURN Qt_QByteArray_lastIndexOf( ::pPtr, hbqt_ptr( pBa ), nFrom )


METHOD QByteArray:lastIndexOf_1( cStr, nFrom )
   RETURN Qt_QByteArray_lastIndexOf_1( ::pPtr, cStr, nFrom )


METHOD QByteArray:lastIndexOf_2( pStr, nFrom )
   RETURN Qt_QByteArray_lastIndexOf_2( ::pPtr, hbqt_ptr( pStr ), nFrom )


METHOD QByteArray:lastIndexOf_3( cCh, nFrom )
   RETURN Qt_QByteArray_lastIndexOf_3( ::pPtr, cCh, nFrom )


METHOD QByteArray:left( nLen )
   RETURN Qt_QByteArray_left( ::pPtr, nLen )


METHOD QByteArray:leftJustified( nWidth, cFill, lTruncate )
   RETURN Qt_QByteArray_leftJustified( ::pPtr, nWidth, cFill, lTruncate )


METHOD QByteArray:length()
   RETURN Qt_QByteArray_length( ::pPtr )


METHOD QByteArray:mid( nPos, nLen )
   RETURN Qt_QByteArray_mid( ::pPtr, nPos, nLen )


METHOD QByteArray:prepend( pBa )
   RETURN Qt_QByteArray_prepend( ::pPtr, hbqt_ptr( pBa ) )


METHOD QByteArray:prepend_1( pStr )
   RETURN Qt_QByteArray_prepend_1( ::pPtr, hbqt_ptr( pStr ) )


METHOD QByteArray:prepend_2( cCh )
   RETURN Qt_QByteArray_prepend_2( ::pPtr, cCh )


METHOD QByteArray:push_back( pOther )
   RETURN Qt_QByteArray_push_back( ::pPtr, hbqt_ptr( pOther ) )


METHOD QByteArray:push_back_1( pStr )
   RETURN Qt_QByteArray_push_back_1( ::pPtr, hbqt_ptr( pStr ) )


METHOD QByteArray:push_back_2( cCh )
   RETURN Qt_QByteArray_push_back_2( ::pPtr, cCh )


METHOD QByteArray:push_front( pOther )
   RETURN Qt_QByteArray_push_front( ::pPtr, hbqt_ptr( pOther ) )


METHOD QByteArray:push_front_1( pStr )
   RETURN Qt_QByteArray_push_front_1( ::pPtr, hbqt_ptr( pStr ) )


METHOD QByteArray:push_front_2( cCh )
   RETURN Qt_QByteArray_push_front_2( ::pPtr, cCh )


METHOD QByteArray:remove( nPos, nLen )
   RETURN Qt_QByteArray_remove( ::pPtr, nPos, nLen )


METHOD QByteArray:repeated( nTimes )
   RETURN Qt_QByteArray_repeated( ::pPtr, nTimes )


METHOD QByteArray:replace( nPos, nLen, pAfter )
   RETURN Qt_QByteArray_replace( ::pPtr, nPos, nLen, hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_1( nPos, nLen, pAfter )
   RETURN Qt_QByteArray_replace_1( ::pPtr, nPos, nLen, hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_2( pBefore, pAfter )
   RETURN Qt_QByteArray_replace_2( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_3( pBefore, pAfter )
   RETURN Qt_QByteArray_replace_3( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_4( pBefore, nBsize, pAfter, nAsize )
   RETURN Qt_QByteArray_replace_4( ::pPtr, hbqt_ptr( pBefore ), nBsize, hbqt_ptr( pAfter ), nAsize )


METHOD QByteArray:replace_5( pBefore, pAfter )
   RETURN Qt_QByteArray_replace_5( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_6( cBefore, pAfter )
   RETURN Qt_QByteArray_replace_6( ::pPtr, cBefore, hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_7( cBefore, pAfter )
   RETURN Qt_QByteArray_replace_7( ::pPtr, cBefore, hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_8( pBefore, pAfter )
   RETURN Qt_QByteArray_replace_8( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_9( cBefore, pAfter )
   RETURN Qt_QByteArray_replace_9( ::pPtr, cBefore, hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_10( cBefore, cAfter )
   RETURN Qt_QByteArray_replace_10( ::pPtr, cBefore, cAfter )


METHOD QByteArray:replace_11( cBefore, pAfter )
   RETURN Qt_QByteArray_replace_11( ::pPtr, cBefore, hbqt_ptr( pAfter ) )


METHOD QByteArray:replace_12( cBefore, cAfter )
   RETURN Qt_QByteArray_replace_12( ::pPtr, cBefore, cAfter )


METHOD QByteArray:reserve( nSize )
   RETURN Qt_QByteArray_reserve( ::pPtr, nSize )


METHOD QByteArray:resize( nSize )
   RETURN Qt_QByteArray_resize( ::pPtr, nSize )


METHOD QByteArray:right( nLen )
   RETURN Qt_QByteArray_right( ::pPtr, nLen )


METHOD QByteArray:rightJustified( nWidth, cFill, lTruncate )
   RETURN Qt_QByteArray_rightJustified( ::pPtr, nWidth, cFill, lTruncate )


METHOD QByteArray:setNum( nN, nBase )
   RETURN Qt_QByteArray_setNum( ::pPtr, nN, nBase )


METHOD QByteArray:setNum_1( nN, nBase )
   RETURN Qt_QByteArray_setNum_1( ::pPtr, nN, nBase )


METHOD QByteArray:setNum_2( nN, nBase )
   RETURN Qt_QByteArray_setNum_2( ::pPtr, nN, nBase )


METHOD QByteArray:setNum_3( nN, nBase )
   RETURN Qt_QByteArray_setNum_3( ::pPtr, nN, nBase )


METHOD QByteArray:setNum_4( nN, nBase )
   RETURN Qt_QByteArray_setNum_4( ::pPtr, nN, nBase )


METHOD QByteArray:setNum_5( nN, nBase )
   RETURN Qt_QByteArray_setNum_5( ::pPtr, nN, nBase )


METHOD QByteArray:setNum_6( nN, cF, nPrec )
   RETURN Qt_QByteArray_setNum_6( ::pPtr, nN, cF, nPrec )


METHOD QByteArray:setNum_7( nN, cF, nPrec )
   RETURN Qt_QByteArray_setNum_7( ::pPtr, nN, cF, nPrec )


METHOD QByteArray:simplified()
   RETURN Qt_QByteArray_simplified( ::pPtr )


METHOD QByteArray:size()
   RETURN Qt_QByteArray_size( ::pPtr )


METHOD QByteArray:split( cSep )
   RETURN Qt_QByteArray_split( ::pPtr, cSep )


METHOD QByteArray:squeeze()
   RETURN Qt_QByteArray_squeeze( ::pPtr )


METHOD QByteArray:startsWith( pBa )
   RETURN Qt_QByteArray_startsWith( ::pPtr, hbqt_ptr( pBa ) )


METHOD QByteArray:startsWith_1( pStr )
   RETURN Qt_QByteArray_startsWith_1( ::pPtr, hbqt_ptr( pStr ) )


METHOD QByteArray:startsWith_2( cCh )
   RETURN Qt_QByteArray_startsWith_2( ::pPtr, cCh )


METHOD QByteArray:toBase64()
   RETURN Qt_QByteArray_toBase64( ::pPtr )


METHOD QByteArray:toDouble( lOk )
   RETURN Qt_QByteArray_toDouble( ::pPtr, lOk )


METHOD QByteArray:toFloat( lOk )
   RETURN Qt_QByteArray_toFloat( ::pPtr, lOk )


METHOD QByteArray:toHex()
   RETURN Qt_QByteArray_toHex( ::pPtr )


METHOD QByteArray:toInt( lOk, nBase )
   RETURN Qt_QByteArray_toInt( ::pPtr, lOk, nBase )


METHOD QByteArray:toLong( lOk, nBase )
   RETURN Qt_QByteArray_toLong( ::pPtr, lOk, nBase )


METHOD QByteArray:toLongLong( lOk, nBase )
   RETURN Qt_QByteArray_toLongLong( ::pPtr, lOk, nBase )


METHOD QByteArray:toLower()
   RETURN Qt_QByteArray_toLower( ::pPtr )


METHOD QByteArray:toPercentEncoding( pExclude, pInclude, cPercent )
   RETURN Qt_QByteArray_toPercentEncoding( ::pPtr, hbqt_ptr( pExclude ), hbqt_ptr( pInclude ), cPercent )


METHOD QByteArray:toShort( lOk, nBase )
   RETURN Qt_QByteArray_toShort( ::pPtr, lOk, nBase )


METHOD QByteArray:toUInt( lOk, nBase )
   RETURN Qt_QByteArray_toUInt( ::pPtr, lOk, nBase )


METHOD QByteArray:toULong( lOk, nBase )
   RETURN Qt_QByteArray_toULong( ::pPtr, lOk, nBase )


METHOD QByteArray:toULongLong( lOk, nBase )
   RETURN Qt_QByteArray_toULongLong( ::pPtr, lOk, nBase )


METHOD QByteArray:toUShort( lOk, nBase )
   RETURN Qt_QByteArray_toUShort( ::pPtr, lOk, nBase )


METHOD QByteArray:toUpper()
   RETURN Qt_QByteArray_toUpper( ::pPtr )


METHOD QByteArray:trimmed()
   RETURN Qt_QByteArray_trimmed( ::pPtr )


METHOD QByteArray:truncate( nPos )
   RETURN Qt_QByteArray_truncate( ::pPtr, nPos )

