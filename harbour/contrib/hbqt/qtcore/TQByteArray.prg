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


CREATE CLASS QByteArray

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  append( pBa )                       INLINE  Qt_QByteArray_append( ::pPtr, pBa )
   METHOD  append_1( cStr )                    INLINE  Qt_QByteArray_append_1( ::pPtr, cStr )
   METHOD  append_2( pStr )                    INLINE  Qt_QByteArray_append_2( ::pPtr, pStr )
   METHOD  append_3( pStr, nLen )              INLINE  Qt_QByteArray_append_3( ::pPtr, pStr, nLen )
   METHOD  append_4( cCh )                     INLINE  Qt_QByteArray_append_4( ::pPtr, cCh )
   METHOD  at( nI )                            INLINE  Qt_QByteArray_at( ::pPtr, nI )
   METHOD  capacity()                          INLINE  Qt_QByteArray_capacity( ::pPtr )
   METHOD  chop( nN )                          INLINE  Qt_QByteArray_chop( ::pPtr, nN )
   METHOD  clear()                             INLINE  Qt_QByteArray_clear( ::pPtr )
   METHOD  constData()                         INLINE  Qt_QByteArray_constData( ::pPtr )
   METHOD  count( pBa )                        INLINE  Qt_QByteArray_count( ::pPtr, pBa )
   METHOD  count_1( pStr )                     INLINE  Qt_QByteArray_count_1( ::pPtr, pStr )
   METHOD  count_2( cCh )                      INLINE  Qt_QByteArray_count_2( ::pPtr, cCh )
   METHOD  count_3()                           INLINE  Qt_QByteArray_count_3( ::pPtr )
   METHOD  data()                              INLINE  Qt_QByteArray_data( ::pPtr )
   METHOD  data_1()                            INLINE  Qt_QByteArray_data_1( ::pPtr )
   METHOD  endsWith( pBa )                     INLINE  Qt_QByteArray_endsWith( ::pPtr, pBa )
   METHOD  endsWith_1( pStr )                  INLINE  Qt_QByteArray_endsWith_1( ::pPtr, pStr )
   METHOD  endsWith_2( cCh )                   INLINE  Qt_QByteArray_endsWith_2( ::pPtr, cCh )
   METHOD  fill( cCh, nSize )                  INLINE  Qt_QByteArray_fill( ::pPtr, cCh, nSize )
   METHOD  indexOf( pBa, nFrom )               INLINE  Qt_QByteArray_indexOf( ::pPtr, pBa, nFrom )
   METHOD  indexOf_1( cStr, nFrom )            INLINE  Qt_QByteArray_indexOf_1( ::pPtr, cStr, nFrom )
   METHOD  indexOf_2( pStr, nFrom )            INLINE  Qt_QByteArray_indexOf_2( ::pPtr, pStr, nFrom )
   METHOD  indexOf_3( cCh, nFrom )             INLINE  Qt_QByteArray_indexOf_3( ::pPtr, cCh, nFrom )
   METHOD  insert( nI, pBa )                   INLINE  Qt_QByteArray_insert( ::pPtr, nI, pBa )
   METHOD  insert_1( nI, cStr )                INLINE  Qt_QByteArray_insert_1( ::pPtr, nI, cStr )
   METHOD  insert_2( nI, pStr )                INLINE  Qt_QByteArray_insert_2( ::pPtr, nI, pStr )
   METHOD  insert_3( nI, cCh )                 INLINE  Qt_QByteArray_insert_3( ::pPtr, nI, cCh )
   METHOD  isEmpty()                           INLINE  Qt_QByteArray_isEmpty( ::pPtr )
   METHOD  isNull()                            INLINE  Qt_QByteArray_isNull( ::pPtr )
   METHOD  lastIndexOf( pBa, nFrom )           INLINE  Qt_QByteArray_lastIndexOf( ::pPtr, pBa, nFrom )
   METHOD  lastIndexOf_1( cStr, nFrom )        INLINE  Qt_QByteArray_lastIndexOf_1( ::pPtr, cStr, nFrom )
   METHOD  lastIndexOf_2( pStr, nFrom )        INLINE  Qt_QByteArray_lastIndexOf_2( ::pPtr, pStr, nFrom )
   METHOD  lastIndexOf_3( cCh, nFrom )         INLINE  Qt_QByteArray_lastIndexOf_3( ::pPtr, cCh, nFrom )
   METHOD  left( nLen )                        INLINE  Qt_QByteArray_left( ::pPtr, nLen )
   METHOD  leftJustified( nWidth, cFill, lTruncate )  INLINE  Qt_QByteArray_leftJustified( ::pPtr, nWidth, cFill, lTruncate )
   METHOD  length()                            INLINE  Qt_QByteArray_length( ::pPtr )
   METHOD  mid( nPos, nLen )                   INLINE  Qt_QByteArray_mid( ::pPtr, nPos, nLen )
   METHOD  prepend( pBa )                      INLINE  Qt_QByteArray_prepend( ::pPtr, pBa )
   METHOD  prepend_1( pStr )                   INLINE  Qt_QByteArray_prepend_1( ::pPtr, pStr )
   METHOD  prepend_2( cCh )                    INLINE  Qt_QByteArray_prepend_2( ::pPtr, cCh )
   METHOD  push_back( pOther )                 INLINE  Qt_QByteArray_push_back( ::pPtr, pOther )
   METHOD  push_back_1( pStr )                 INLINE  Qt_QByteArray_push_back_1( ::pPtr, pStr )
   METHOD  push_back_2( cCh )                  INLINE  Qt_QByteArray_push_back_2( ::pPtr, cCh )
   METHOD  push_front( pOther )                INLINE  Qt_QByteArray_push_front( ::pPtr, pOther )
   METHOD  push_front_1( pStr )                INLINE  Qt_QByteArray_push_front_1( ::pPtr, pStr )
   METHOD  push_front_2( cCh )                 INLINE  Qt_QByteArray_push_front_2( ::pPtr, cCh )
   METHOD  remove( nPos, nLen )                INLINE  Qt_QByteArray_remove( ::pPtr, nPos, nLen )
   METHOD  repeated( nTimes )                  INLINE  Qt_QByteArray_repeated( ::pPtr, nTimes )
   METHOD  replace( nPos, nLen, pAfter )       INLINE  Qt_QByteArray_replace( ::pPtr, nPos, nLen, pAfter )
   METHOD  replace_1( nPos, nLen, pAfter )     INLINE  Qt_QByteArray_replace_1( ::pPtr, nPos, nLen, pAfter )
   METHOD  replace_2( pBefore, pAfter )        INLINE  Qt_QByteArray_replace_2( ::pPtr, pBefore, pAfter )
   METHOD  replace_3( pBefore, pAfter )        INLINE  Qt_QByteArray_replace_3( ::pPtr, pBefore, pAfter )
   METHOD  replace_4( pBefore, nBsize, pAfter, nAsize )  INLINE  Qt_QByteArray_replace_4( ::pPtr, pBefore, nBsize, pAfter, nAsize )
   METHOD  replace_5( pBefore, pAfter )        INLINE  Qt_QByteArray_replace_5( ::pPtr, pBefore, pAfter )
   METHOD  replace_6( cBefore, pAfter )        INLINE  Qt_QByteArray_replace_6( ::pPtr, cBefore, pAfter )
   METHOD  replace_7( cBefore, pAfter )        INLINE  Qt_QByteArray_replace_7( ::pPtr, cBefore, pAfter )
   METHOD  replace_8( pBefore, pAfter )        INLINE  Qt_QByteArray_replace_8( ::pPtr, pBefore, pAfter )
   METHOD  replace_9( cBefore, pAfter )        INLINE  Qt_QByteArray_replace_9( ::pPtr, cBefore, pAfter )
   METHOD  replace_10( cBefore, cAfter )       INLINE  Qt_QByteArray_replace_10( ::pPtr, cBefore, cAfter )
   METHOD  replace_11( cBefore, pAfter )       INLINE  Qt_QByteArray_replace_11( ::pPtr, cBefore, pAfter )
   METHOD  replace_12( cBefore, cAfter )       INLINE  Qt_QByteArray_replace_12( ::pPtr, cBefore, cAfter )
   METHOD  reserve( nSize )                    INLINE  Qt_QByteArray_reserve( ::pPtr, nSize )
   METHOD  resize( nSize )                     INLINE  Qt_QByteArray_resize( ::pPtr, nSize )
   METHOD  right( nLen )                       INLINE  Qt_QByteArray_right( ::pPtr, nLen )
   METHOD  rightJustified( nWidth, cFill, lTruncate )  INLINE  Qt_QByteArray_rightJustified( ::pPtr, nWidth, cFill, lTruncate )
   METHOD  setNum( nN, nBase )                 INLINE  Qt_QByteArray_setNum( ::pPtr, nN, nBase )
   METHOD  setNum_1( nN, nBase )               INLINE  Qt_QByteArray_setNum_1( ::pPtr, nN, nBase )
   METHOD  setNum_2( nN, nBase )               INLINE  Qt_QByteArray_setNum_2( ::pPtr, nN, nBase )
   METHOD  setNum_3( nN, nBase )               INLINE  Qt_QByteArray_setNum_3( ::pPtr, nN, nBase )
   METHOD  setNum_4( nN, nBase )               INLINE  Qt_QByteArray_setNum_4( ::pPtr, nN, nBase )
   METHOD  setNum_5( nN, nBase )               INLINE  Qt_QByteArray_setNum_5( ::pPtr, nN, nBase )
   METHOD  setNum_6( nN, cF, nPrec )           INLINE  Qt_QByteArray_setNum_6( ::pPtr, nN, cF, nPrec )
   METHOD  setNum_7( nN, cF, nPrec )           INLINE  Qt_QByteArray_setNum_7( ::pPtr, nN, cF, nPrec )
   METHOD  simplified()                        INLINE  Qt_QByteArray_simplified( ::pPtr )
   METHOD  size()                              INLINE  Qt_QByteArray_size( ::pPtr )
   METHOD  squeeze()                           INLINE  Qt_QByteArray_squeeze( ::pPtr )
   METHOD  startsWith( pBa )                   INLINE  Qt_QByteArray_startsWith( ::pPtr, pBa )
   METHOD  startsWith_1( pStr )                INLINE  Qt_QByteArray_startsWith_1( ::pPtr, pStr )
   METHOD  startsWith_2( cCh )                 INLINE  Qt_QByteArray_startsWith_2( ::pPtr, cCh )
   METHOD  toBase64()                          INLINE  Qt_QByteArray_toBase64( ::pPtr )
   METHOD  toDouble( lOk )                     INLINE  Qt_QByteArray_toDouble( ::pPtr, lOk )
   METHOD  toFloat( lOk )                      INLINE  Qt_QByteArray_toFloat( ::pPtr, lOk )
   METHOD  toHex()                             INLINE  Qt_QByteArray_toHex( ::pPtr )
   METHOD  toInt( lOk, nBase )                 INLINE  Qt_QByteArray_toInt( ::pPtr, lOk, nBase )
   METHOD  toLong( lOk, nBase )                INLINE  Qt_QByteArray_toLong( ::pPtr, lOk, nBase )
   METHOD  toLongLong( lOk, nBase )            INLINE  Qt_QByteArray_toLongLong( ::pPtr, lOk, nBase )
   METHOD  toLower()                           INLINE  Qt_QByteArray_toLower( ::pPtr )
   METHOD  toPercentEncoding( pExclude, pInclude, cPercent )  INLINE  Qt_QByteArray_toPercentEncoding( ::pPtr, pExclude, pInclude, cPercent )
   METHOD  toShort( lOk, nBase )               INLINE  Qt_QByteArray_toShort( ::pPtr, lOk, nBase )
   METHOD  toUInt( lOk, nBase )                INLINE  Qt_QByteArray_toUInt( ::pPtr, lOk, nBase )
   METHOD  toULong( lOk, nBase )               INLINE  Qt_QByteArray_toULong( ::pPtr, lOk, nBase )
   METHOD  toULongLong( lOk, nBase )           INLINE  Qt_QByteArray_toULongLong( ::pPtr, lOk, nBase )
   METHOD  toUShort( lOk, nBase )              INLINE  Qt_QByteArray_toUShort( ::pPtr, lOk, nBase )
   METHOD  toUpper()                           INLINE  Qt_QByteArray_toUpper( ::pPtr )
   METHOD  trimmed()                           INLINE  Qt_QByteArray_trimmed( ::pPtr )
   METHOD  truncate( nPos )                    INLINE  Qt_QByteArray_truncate( ::pPtr, nPos )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QByteArray

   ::pParent := pParent

   ::pPtr := Qt_QByteArray( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QByteArray

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
