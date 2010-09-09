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


CREATE CLASS HBQString INHERIT HbQtObjectHandler

   METHOD  new( ... )

   METHOD  append( cStr )
   METHOD  append_1( pReference )
   METHOD  append_2( pStr )
   METHOD  append_3( pBa )
   METHOD  append_4( pStr )
   METHOD  append_5( pCh )
   METHOD  arg( cA, nFieldWidth, pFillChar )
   METHOD  arg_1( cA1, cA2 )
   METHOD  arg_2( cA1, cA2, cA3 )
   METHOD  arg_3( cA1, cA2, cA3, cA4 )
   METHOD  arg_4( cA1, cA2, cA3, cA4, cA5 )
   METHOD  arg_5( cA1, cA2, cA3, cA4, cA5, cA6 )
   METHOD  arg_6( cA1, cA2, cA3, cA4, cA5, cA6, cA7 )
   METHOD  arg_7( cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8 )
   METHOD  arg_8( cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8, cA9 )
   METHOD  arg_9( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_10( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_11( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_12( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_13( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_14( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_15( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_16( nA, nFieldWidth, nBase, pFillChar )
   METHOD  arg_17( pA, nFieldWidth, pFillChar )
   METHOD  arg_18( cA, nFieldWidth, pFillChar )
   METHOD  arg_19( nA, nFieldWidth, cFormat, nPrecision, pFillChar )
   METHOD  at( nPosition )
   METHOD  capacity()
   METHOD  chop( nN )
   METHOD  clear()
   METHOD  compare( cOther )
   METHOD  compare_1( cOther, nCs )
   METHOD  compare_2( pOther, nCs )
   METHOD  compare_3( pRef, nCs )
   METHOD  constData()
   METHOD  count( cStr, nCs )
   METHOD  count_1( pCh, nCs )
   METHOD  count_2( pRx )
   METHOD  count_3()
   METHOD  data()
   METHOD  data_1()
   METHOD  endsWith( cS, nCs )
   METHOD  endsWith_1( pS, nCs )
   METHOD  endsWith_2( pC, nCs )
   METHOD  fill( pCh, nSize )
   METHOD  indexOf( cStr, nFrom, nCs )
   METHOD  indexOf_1( pStr, nFrom, nCs )
   METHOD  indexOf_2( pCh, nFrom, nCs )
   METHOD  indexOf_3( pRx, nFrom )
   METHOD  indexOf_4( pRx, nFrom )
   METHOD  insert( nPosition, cStr )
   METHOD  insert_1( nPosition, pStr )
   METHOD  insert_2( nPosition, pUnicode, nSize )
   METHOD  insert_3( nPosition, pCh )
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  lastIndexOf( cStr, nFrom, nCs )
   METHOD  lastIndexOf_1( pStr, nFrom, nCs )
   METHOD  lastIndexOf_2( pCh, nFrom, nCs )
   METHOD  lastIndexOf_3( pRx, nFrom )
   METHOD  lastIndexOf_4( pRx, nFrom )
   METHOD  left( nN )
   METHOD  leftJustified( nWidth, pFill, lTruncate )
   METHOD  leftRef( nN )
   METHOD  length()
   METHOD  localeAwareCompare( pOther )
   METHOD  localeAwareCompare_1( cOther )
   METHOD  mid( nPosition, nN )
   METHOD  midRef( nPosition, nN )
   METHOD  normalized( nMode )
   METHOD  normalized_1( nMode, nVersion )
   METHOD  prepend( cStr )
   METHOD  prepend_1( pStr )
   METHOD  prepend_2( pBa )
   METHOD  prepend_3( pStr )
   METHOD  prepend_4( pCh )
   METHOD  push_back( cOther )
   METHOD  push_back_1( pCh )
   METHOD  push_front( cOther )
   METHOD  push_front_1( pCh )
   METHOD  remove( nPosition, nN )
   METHOD  remove_1( pCh, nCs )
   METHOD  remove_2( cStr, nCs )
   METHOD  remove_3( pRx )
   METHOD  repeated( nTimes )
   METHOD  replace( nPosition, nN, cAfter )
   METHOD  replace_1( nPosition, nN, pUnicode, nSize )
   METHOD  replace_2( nPosition, nN, pAfter )
   METHOD  replace_3( cBefore, cAfter, nCs )
   METHOD  replace_4( pBefore, nBlen, pAfter, nAlen, nCs )
   METHOD  replace_5( pCh, cAfter, nCs )
   METHOD  replace_6( pBefore, pAfter, nCs )
   METHOD  replace_7( pBefore, pAfter, nCs )
   METHOD  replace_8( pBefore, cAfter, nCs )
   METHOD  replace_9( cBefore, pAfter, nCs )
   METHOD  replace_10( pC, pAfter, nCs )
   METHOD  replace_11( pRx, cAfter )
   METHOD  reserve( nSize )
   METHOD  resize( nSize )
   METHOD  right( nN )
   METHOD  rightJustified( nWidth, pFill, lTruncate )
   METHOD  rightRef( nN )
   METHOD  section( pSep, nStart, nEnd, nFlags )
   METHOD  section_1( cSep, nStart, nEnd, nFlags )
   METHOD  section_2( pReg, nStart, nEnd, nFlags )
   METHOD  setNum( nN, nBase )
   METHOD  setNum_1( nN, nBase )
   METHOD  setNum_2( nN, nBase )
   METHOD  setNum_3( nN, nBase )
   METHOD  setNum_4( nN, nBase )
   METHOD  setNum_5( nN, nBase )
   METHOD  setNum_6( nN, nBase )
   METHOD  setNum_7( nN, nBase )
   METHOD  setNum_8( nN, cFormat, nPrecision )
   METHOD  setNum_9( nN, cFormat, nPrecision )
   METHOD  setUnicode( pUnicode, nSize )
   METHOD  setUtf16( nUnicode, nSize )
   METHOD  simplified()
   METHOD  size()
   METHOD  split( cSep, nBehavior, nCs )
   METHOD  split_1( pSep, nBehavior, nCs )
   METHOD  split_2( pRx, nBehavior )
   METHOD  squeeze()
   METHOD  startsWith( cS, nCs )
   METHOD  startsWith_1( pS, nCs )
   METHOD  startsWith_2( pC, nCs )
   METHOD  toAscii()
   METHOD  toCaseFolded()
   METHOD  toDouble( lOk )
   METHOD  toFloat( lOk )
   METHOD  toInt( lOk, nBase )
   METHOD  toLatin1()
   METHOD  toLocal8Bit()
   METHOD  toLong( lOk, nBase )
   METHOD  toLongLong( lOk, nBase )
   METHOD  toLower()
   METHOD  toShort( lOk, nBase )
   METHOD  toUInt( lOk, nBase )
   METHOD  toULong( lOk, nBase )
   METHOD  toULongLong( lOk, nBase )
   METHOD  toUShort( lOk, nBase )
   METHOD  toUpper()
   METHOD  toUtf8()
   METHOD  trimmed()
   METHOD  truncate( nPosition )
   METHOD  unicode()
   METHOD  compare_4( cS1, cS2, nCs )
   METHOD  compare_5( cS1, cS2 )
   METHOD  compare_6( cS1, pS2, nCs )
   METHOD  compare_7( pS1, cS2, nCs )
   METHOD  compare_8( cS1, pS2, nCs )
   METHOD  fromAscii( pStr, nSize )
   METHOD  fromLatin1( pStr, nSize )
   METHOD  fromLocal8Bit( pStr, nSize )
   METHOD  fromRawData( pUnicode, nSize )
   METHOD  fromUcs4( nUnicode, nSize )
   METHOD  fromUtf8( pStr, nSize )
   METHOD  fromUtf16( nUnicode, nSize )
   METHOD  localeAwareCompare_2( cS1, cS2 )
   METHOD  localeAwareCompare_3( cS1, pS2 )
   METHOD  number( nN, nBase )
   METHOD  number_1( nN, cFormat, nPrecision )
   METHOD  number_2( nN, nBase )
   METHOD  number_3( nN, nBase )
   METHOD  number_4( nN, nBase )
   METHOD  number_5( nN, nBase )
   METHOD  number_6( nN, nBase )

   ENDCLASS


METHOD HBQString:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQString( ... )
   RETURN Self


METHOD HBQString:append( cStr )
   RETURN Qt_HBQString_append( ::pPtr, cStr )


METHOD HBQString:append_1( pReference )
   RETURN Qt_HBQString_append_1( ::pPtr, hbqt_ptr( pReference ) )


METHOD HBQString:append_2( pStr )
   RETURN Qt_HBQString_append_2( ::pPtr, hbqt_ptr( pStr ) )


METHOD HBQString:append_3( pBa )
   RETURN Qt_HBQString_append_3( ::pPtr, hbqt_ptr( pBa ) )


METHOD HBQString:append_4( pStr )
   RETURN Qt_HBQString_append_4( ::pPtr, hbqt_ptr( pStr ) )


METHOD HBQString:append_5( pCh )
   RETURN Qt_HBQString_append_5( ::pPtr, hbqt_ptr( pCh ) )


METHOD HBQString:arg( cA, nFieldWidth, pFillChar )
   RETURN Qt_HBQString_arg( ::pPtr, cA, nFieldWidth, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_1( cA1, cA2 )
   RETURN Qt_HBQString_arg_1( ::pPtr, cA1, cA2 )


METHOD HBQString:arg_2( cA1, cA2, cA3 )
   RETURN Qt_HBQString_arg_2( ::pPtr, cA1, cA2, cA3 )


METHOD HBQString:arg_3( cA1, cA2, cA3, cA4 )
   RETURN Qt_HBQString_arg_3( ::pPtr, cA1, cA2, cA3, cA4 )


METHOD HBQString:arg_4( cA1, cA2, cA3, cA4, cA5 )
   RETURN Qt_HBQString_arg_4( ::pPtr, cA1, cA2, cA3, cA4, cA5 )


METHOD HBQString:arg_5( cA1, cA2, cA3, cA4, cA5, cA6 )
   RETURN Qt_HBQString_arg_5( ::pPtr, cA1, cA2, cA3, cA4, cA5, cA6 )


METHOD HBQString:arg_6( cA1, cA2, cA3, cA4, cA5, cA6, cA7 )
   RETURN Qt_HBQString_arg_6( ::pPtr, cA1, cA2, cA3, cA4, cA5, cA6, cA7 )


METHOD HBQString:arg_7( cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8 )
   RETURN Qt_HBQString_arg_7( ::pPtr, cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8 )


METHOD HBQString:arg_8( cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8, cA9 )
   RETURN Qt_HBQString_arg_8( ::pPtr, cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8, cA9 )


METHOD HBQString:arg_9( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_9( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_10( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_10( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_11( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_11( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_12( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_12( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_13( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_13( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_14( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_14( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_15( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_15( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_16( nA, nFieldWidth, nBase, pFillChar )
   RETURN Qt_HBQString_arg_16( ::pPtr, nA, nFieldWidth, nBase, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_17( pA, nFieldWidth, pFillChar )
   RETURN Qt_HBQString_arg_17( ::pPtr, hbqt_ptr( pA ), nFieldWidth, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_18( cA, nFieldWidth, pFillChar )
   RETURN Qt_HBQString_arg_18( ::pPtr, cA, nFieldWidth, hbqt_ptr( pFillChar ) )


METHOD HBQString:arg_19( nA, nFieldWidth, cFormat, nPrecision, pFillChar )
   RETURN Qt_HBQString_arg_19( ::pPtr, nA, nFieldWidth, cFormat, nPrecision, hbqt_ptr( pFillChar ) )


METHOD HBQString:at( nPosition )
   RETURN Qt_HBQString_at( ::pPtr, nPosition )


METHOD HBQString:capacity()
   RETURN Qt_HBQString_capacity( ::pPtr )


METHOD HBQString:chop( nN )
   RETURN Qt_HBQString_chop( ::pPtr, nN )


METHOD HBQString:clear()
   RETURN Qt_HBQString_clear( ::pPtr )


METHOD HBQString:compare( cOther )
   RETURN Qt_HBQString_compare( ::pPtr, cOther )


METHOD HBQString:compare_1( cOther, nCs )
   RETURN Qt_HBQString_compare_1( ::pPtr, cOther, nCs )


METHOD HBQString:compare_2( pOther, nCs )
   RETURN Qt_HBQString_compare_2( ::pPtr, hbqt_ptr( pOther ), nCs )


METHOD HBQString:compare_3( pRef, nCs )
   RETURN Qt_HBQString_compare_3( ::pPtr, hbqt_ptr( pRef ), nCs )


METHOD HBQString:constData()
   RETURN Qt_HBQString_constData( ::pPtr )


METHOD HBQString:count( cStr, nCs )
   RETURN Qt_HBQString_count( ::pPtr, cStr, nCs )


METHOD HBQString:count_1( pCh, nCs )
   RETURN Qt_HBQString_count_1( ::pPtr, hbqt_ptr( pCh ), nCs )


METHOD HBQString:count_2( pRx )
   RETURN Qt_HBQString_count_2( ::pPtr, hbqt_ptr( pRx ) )


METHOD HBQString:count_3()
   RETURN Qt_HBQString_count_3( ::pPtr )


METHOD HBQString:data()
   RETURN Qt_HBQString_data( ::pPtr )


METHOD HBQString:data_1()
   RETURN Qt_HBQString_data_1( ::pPtr )


METHOD HBQString:endsWith( cS, nCs )
   RETURN Qt_HBQString_endsWith( ::pPtr, cS, nCs )


METHOD HBQString:endsWith_1( pS, nCs )
   RETURN Qt_HBQString_endsWith_1( ::pPtr, hbqt_ptr( pS ), nCs )


METHOD HBQString:endsWith_2( pC, nCs )
   RETURN Qt_HBQString_endsWith_2( ::pPtr, hbqt_ptr( pC ), nCs )


METHOD HBQString:fill( pCh, nSize )
   RETURN Qt_HBQString_fill( ::pPtr, hbqt_ptr( pCh ), nSize )


METHOD HBQString:indexOf( cStr, nFrom, nCs )
   RETURN Qt_HBQString_indexOf( ::pPtr, cStr, nFrom, nCs )


METHOD HBQString:indexOf_1( pStr, nFrom, nCs )
   RETURN Qt_HBQString_indexOf_1( ::pPtr, hbqt_ptr( pStr ), nFrom, nCs )


METHOD HBQString:indexOf_2( pCh, nFrom, nCs )
   RETURN Qt_HBQString_indexOf_2( ::pPtr, hbqt_ptr( pCh ), nFrom, nCs )


METHOD HBQString:indexOf_3( pRx, nFrom )
   RETURN Qt_HBQString_indexOf_3( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD HBQString:indexOf_4( pRx, nFrom )
   RETURN Qt_HBQString_indexOf_4( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD HBQString:insert( nPosition, cStr )
   RETURN Qt_HBQString_insert( ::pPtr, nPosition, cStr )


METHOD HBQString:insert_1( nPosition, pStr )
   RETURN Qt_HBQString_insert_1( ::pPtr, nPosition, hbqt_ptr( pStr ) )


METHOD HBQString:insert_2( nPosition, pUnicode, nSize )
   RETURN Qt_HBQString_insert_2( ::pPtr, nPosition, hbqt_ptr( pUnicode ), nSize )


METHOD HBQString:insert_3( nPosition, pCh )
   RETURN Qt_HBQString_insert_3( ::pPtr, nPosition, hbqt_ptr( pCh ) )


METHOD HBQString:isEmpty()
   RETURN Qt_HBQString_isEmpty( ::pPtr )


METHOD HBQString:isNull()
   RETURN Qt_HBQString_isNull( ::pPtr )


METHOD HBQString:lastIndexOf( cStr, nFrom, nCs )
   RETURN Qt_HBQString_lastIndexOf( ::pPtr, cStr, nFrom, nCs )


METHOD HBQString:lastIndexOf_1( pStr, nFrom, nCs )
   RETURN Qt_HBQString_lastIndexOf_1( ::pPtr, hbqt_ptr( pStr ), nFrom, nCs )


METHOD HBQString:lastIndexOf_2( pCh, nFrom, nCs )
   RETURN Qt_HBQString_lastIndexOf_2( ::pPtr, hbqt_ptr( pCh ), nFrom, nCs )


METHOD HBQString:lastIndexOf_3( pRx, nFrom )
   RETURN Qt_HBQString_lastIndexOf_3( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD HBQString:lastIndexOf_4( pRx, nFrom )
   RETURN Qt_HBQString_lastIndexOf_4( ::pPtr, hbqt_ptr( pRx ), nFrom )


METHOD HBQString:left( nN )
   RETURN Qt_HBQString_left( ::pPtr, nN )


METHOD HBQString:leftJustified( nWidth, pFill, lTruncate )
   RETURN Qt_HBQString_leftJustified( ::pPtr, nWidth, hbqt_ptr( pFill ), lTruncate )


METHOD HBQString:leftRef( nN )
   RETURN Qt_HBQString_leftRef( ::pPtr, nN )


METHOD HBQString:length()
   RETURN Qt_HBQString_length( ::pPtr )


METHOD HBQString:localeAwareCompare( pOther )
   RETURN Qt_HBQString_localeAwareCompare( ::pPtr, hbqt_ptr( pOther ) )


METHOD HBQString:localeAwareCompare_1( cOther )
   RETURN Qt_HBQString_localeAwareCompare_1( ::pPtr, cOther )


METHOD HBQString:mid( nPosition, nN )
   RETURN Qt_HBQString_mid( ::pPtr, nPosition, nN )


METHOD HBQString:midRef( nPosition, nN )
   RETURN Qt_HBQString_midRef( ::pPtr, nPosition, nN )


METHOD HBQString:normalized( nMode )
   RETURN Qt_HBQString_normalized( ::pPtr, nMode )


METHOD HBQString:normalized_1( nMode, nVersion )
   RETURN Qt_HBQString_normalized_1( ::pPtr, nMode, nVersion )


METHOD HBQString:prepend( cStr )
   RETURN Qt_HBQString_prepend( ::pPtr, cStr )


METHOD HBQString:prepend_1( pStr )
   RETURN Qt_HBQString_prepend_1( ::pPtr, hbqt_ptr( pStr ) )


METHOD HBQString:prepend_2( pBa )
   RETURN Qt_HBQString_prepend_2( ::pPtr, hbqt_ptr( pBa ) )


METHOD HBQString:prepend_3( pStr )
   RETURN Qt_HBQString_prepend_3( ::pPtr, hbqt_ptr( pStr ) )


METHOD HBQString:prepend_4( pCh )
   RETURN Qt_HBQString_prepend_4( ::pPtr, hbqt_ptr( pCh ) )


METHOD HBQString:push_back( cOther )
   RETURN Qt_HBQString_push_back( ::pPtr, cOther )


METHOD HBQString:push_back_1( pCh )
   RETURN Qt_HBQString_push_back_1( ::pPtr, hbqt_ptr( pCh ) )


METHOD HBQString:push_front( cOther )
   RETURN Qt_HBQString_push_front( ::pPtr, cOther )


METHOD HBQString:push_front_1( pCh )
   RETURN Qt_HBQString_push_front_1( ::pPtr, hbqt_ptr( pCh ) )


METHOD HBQString:remove( nPosition, nN )
   RETURN Qt_HBQString_remove( ::pPtr, nPosition, nN )


METHOD HBQString:remove_1( pCh, nCs )
   RETURN Qt_HBQString_remove_1( ::pPtr, hbqt_ptr( pCh ), nCs )


METHOD HBQString:remove_2( cStr, nCs )
   RETURN Qt_HBQString_remove_2( ::pPtr, cStr, nCs )


METHOD HBQString:remove_3( pRx )
   RETURN Qt_HBQString_remove_3( ::pPtr, hbqt_ptr( pRx ) )


METHOD HBQString:repeated( nTimes )
   RETURN Qt_HBQString_repeated( ::pPtr, nTimes )


METHOD HBQString:replace( nPosition, nN, cAfter )
   RETURN Qt_HBQString_replace( ::pPtr, nPosition, nN, cAfter )


METHOD HBQString:replace_1( nPosition, nN, pUnicode, nSize )
   RETURN Qt_HBQString_replace_1( ::pPtr, nPosition, nN, hbqt_ptr( pUnicode ), nSize )


METHOD HBQString:replace_2( nPosition, nN, pAfter )
   RETURN Qt_HBQString_replace_2( ::pPtr, nPosition, nN, hbqt_ptr( pAfter ) )


METHOD HBQString:replace_3( cBefore, cAfter, nCs )
   RETURN Qt_HBQString_replace_3( ::pPtr, cBefore, cAfter, nCs )


METHOD HBQString:replace_4( pBefore, nBlen, pAfter, nAlen, nCs )
   RETURN Qt_HBQString_replace_4( ::pPtr, hbqt_ptr( pBefore ), nBlen, hbqt_ptr( pAfter ), nAlen, nCs )


METHOD HBQString:replace_5( pCh, cAfter, nCs )
   RETURN Qt_HBQString_replace_5( ::pPtr, hbqt_ptr( pCh ), cAfter, nCs )


METHOD HBQString:replace_6( pBefore, pAfter, nCs )
   RETURN Qt_HBQString_replace_6( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAfter ), nCs )


METHOD HBQString:replace_7( pBefore, pAfter, nCs )
   RETURN Qt_HBQString_replace_7( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAfter ), nCs )


METHOD HBQString:replace_8( pBefore, cAfter, nCs )
   RETURN Qt_HBQString_replace_8( ::pPtr, hbqt_ptr( pBefore ), cAfter, nCs )


METHOD HBQString:replace_9( cBefore, pAfter, nCs )
   RETURN Qt_HBQString_replace_9( ::pPtr, cBefore, hbqt_ptr( pAfter ), nCs )


METHOD HBQString:replace_10( pC, pAfter, nCs )
   RETURN Qt_HBQString_replace_10( ::pPtr, hbqt_ptr( pC ), hbqt_ptr( pAfter ), nCs )


METHOD HBQString:replace_11( pRx, cAfter )
   RETURN Qt_HBQString_replace_11( ::pPtr, hbqt_ptr( pRx ), cAfter )


METHOD HBQString:reserve( nSize )
   RETURN Qt_HBQString_reserve( ::pPtr, nSize )


METHOD HBQString:resize( nSize )
   RETURN Qt_HBQString_resize( ::pPtr, nSize )


METHOD HBQString:right( nN )
   RETURN Qt_HBQString_right( ::pPtr, nN )


METHOD HBQString:rightJustified( nWidth, pFill, lTruncate )
   RETURN Qt_HBQString_rightJustified( ::pPtr, nWidth, hbqt_ptr( pFill ), lTruncate )


METHOD HBQString:rightRef( nN )
   RETURN Qt_HBQString_rightRef( ::pPtr, nN )


METHOD HBQString:section( pSep, nStart, nEnd, nFlags )
   RETURN Qt_HBQString_section( ::pPtr, hbqt_ptr( pSep ), nStart, nEnd, nFlags )


METHOD HBQString:section_1( cSep, nStart, nEnd, nFlags )
   RETURN Qt_HBQString_section_1( ::pPtr, cSep, nStart, nEnd, nFlags )


METHOD HBQString:section_2( pReg, nStart, nEnd, nFlags )
   RETURN Qt_HBQString_section_2( ::pPtr, hbqt_ptr( pReg ), nStart, nEnd, nFlags )


METHOD HBQString:setNum( nN, nBase )
   RETURN Qt_HBQString_setNum( ::pPtr, nN, nBase )


METHOD HBQString:setNum_1( nN, nBase )
   RETURN Qt_HBQString_setNum_1( ::pPtr, nN, nBase )


METHOD HBQString:setNum_2( nN, nBase )
   RETURN Qt_HBQString_setNum_2( ::pPtr, nN, nBase )


METHOD HBQString:setNum_3( nN, nBase )
   RETURN Qt_HBQString_setNum_3( ::pPtr, nN, nBase )


METHOD HBQString:setNum_4( nN, nBase )
   RETURN Qt_HBQString_setNum_4( ::pPtr, nN, nBase )


METHOD HBQString:setNum_5( nN, nBase )
   RETURN Qt_HBQString_setNum_5( ::pPtr, nN, nBase )


METHOD HBQString:setNum_6( nN, nBase )
   RETURN Qt_HBQString_setNum_6( ::pPtr, nN, nBase )


METHOD HBQString:setNum_7( nN, nBase )
   RETURN Qt_HBQString_setNum_7( ::pPtr, nN, nBase )


METHOD HBQString:setNum_8( nN, cFormat, nPrecision )
   RETURN Qt_HBQString_setNum_8( ::pPtr, nN, cFormat, nPrecision )


METHOD HBQString:setNum_9( nN, cFormat, nPrecision )
   RETURN Qt_HBQString_setNum_9( ::pPtr, nN, cFormat, nPrecision )


METHOD HBQString:setUnicode( pUnicode, nSize )
   RETURN Qt_HBQString_setUnicode( ::pPtr, hbqt_ptr( pUnicode ), nSize )


METHOD HBQString:setUtf16( nUnicode, nSize )
   RETURN Qt_HBQString_setUtf16( ::pPtr, nUnicode, nSize )


METHOD HBQString:simplified()
   RETURN Qt_HBQString_simplified( ::pPtr )


METHOD HBQString:size()
   RETURN Qt_HBQString_size( ::pPtr )


METHOD HBQString:split( cSep, nBehavior, nCs )
   RETURN Qt_HBQString_split( ::pPtr, cSep, nBehavior, nCs )


METHOD HBQString:split_1( pSep, nBehavior, nCs )
   RETURN Qt_HBQString_split_1( ::pPtr, hbqt_ptr( pSep ), nBehavior, nCs )


METHOD HBQString:split_2( pRx, nBehavior )
   RETURN Qt_HBQString_split_2( ::pPtr, hbqt_ptr( pRx ), nBehavior )


METHOD HBQString:squeeze()
   RETURN Qt_HBQString_squeeze( ::pPtr )


METHOD HBQString:startsWith( cS, nCs )
   RETURN Qt_HBQString_startsWith( ::pPtr, cS, nCs )


METHOD HBQString:startsWith_1( pS, nCs )
   RETURN Qt_HBQString_startsWith_1( ::pPtr, hbqt_ptr( pS ), nCs )


METHOD HBQString:startsWith_2( pC, nCs )
   RETURN Qt_HBQString_startsWith_2( ::pPtr, hbqt_ptr( pC ), nCs )


METHOD HBQString:toAscii()
   RETURN Qt_HBQString_toAscii( ::pPtr )


METHOD HBQString:toCaseFolded()
   RETURN Qt_HBQString_toCaseFolded( ::pPtr )


METHOD HBQString:toDouble( lOk )
   RETURN Qt_HBQString_toDouble( ::pPtr, lOk )


METHOD HBQString:toFloat( lOk )
   RETURN Qt_HBQString_toFloat( ::pPtr, lOk )


METHOD HBQString:toInt( lOk, nBase )
   RETURN Qt_HBQString_toInt( ::pPtr, lOk, nBase )


METHOD HBQString:toLatin1()
   RETURN Qt_HBQString_toLatin1( ::pPtr )


METHOD HBQString:toLocal8Bit()
   RETURN Qt_HBQString_toLocal8Bit( ::pPtr )


METHOD HBQString:toLong( lOk, nBase )
   RETURN Qt_HBQString_toLong( ::pPtr, lOk, nBase )


METHOD HBQString:toLongLong( lOk, nBase )
   RETURN Qt_HBQString_toLongLong( ::pPtr, lOk, nBase )


METHOD HBQString:toLower()
   RETURN Qt_HBQString_toLower( ::pPtr )


METHOD HBQString:toShort( lOk, nBase )
   RETURN Qt_HBQString_toShort( ::pPtr, lOk, nBase )


METHOD HBQString:toUInt( lOk, nBase )
   RETURN Qt_HBQString_toUInt( ::pPtr, lOk, nBase )


METHOD HBQString:toULong( lOk, nBase )
   RETURN Qt_HBQString_toULong( ::pPtr, lOk, nBase )


METHOD HBQString:toULongLong( lOk, nBase )
   RETURN Qt_HBQString_toULongLong( ::pPtr, lOk, nBase )


METHOD HBQString:toUShort( lOk, nBase )
   RETURN Qt_HBQString_toUShort( ::pPtr, lOk, nBase )


METHOD HBQString:toUpper()
   RETURN Qt_HBQString_toUpper( ::pPtr )


METHOD HBQString:toUtf8()
   RETURN Qt_HBQString_toUtf8( ::pPtr )


METHOD HBQString:trimmed()
   RETURN Qt_HBQString_trimmed( ::pPtr )


METHOD HBQString:truncate( nPosition )
   RETURN Qt_HBQString_truncate( ::pPtr, nPosition )


METHOD HBQString:unicode()
   RETURN Qt_HBQString_unicode( ::pPtr )


METHOD HBQString:compare_4( cS1, cS2, nCs )
   RETURN Qt_HBQString_compare_4( ::pPtr, cS1, cS2, nCs )


METHOD HBQString:compare_5( cS1, cS2 )
   RETURN Qt_HBQString_compare_5( ::pPtr, cS1, cS2 )


METHOD HBQString:compare_6( cS1, pS2, nCs )
   RETURN Qt_HBQString_compare_6( ::pPtr, cS1, hbqt_ptr( pS2 ), nCs )


METHOD HBQString:compare_7( pS1, cS2, nCs )
   RETURN Qt_HBQString_compare_7( ::pPtr, hbqt_ptr( pS1 ), cS2, nCs )


METHOD HBQString:compare_8( cS1, pS2, nCs )
   RETURN Qt_HBQString_compare_8( ::pPtr, cS1, hbqt_ptr( pS2 ), nCs )


METHOD HBQString:fromAscii( pStr, nSize )
   RETURN Qt_HBQString_fromAscii( ::pPtr, hbqt_ptr( pStr ), nSize )


METHOD HBQString:fromLatin1( pStr, nSize )
   RETURN Qt_HBQString_fromLatin1( ::pPtr, hbqt_ptr( pStr ), nSize )


METHOD HBQString:fromLocal8Bit( pStr, nSize )
   RETURN Qt_HBQString_fromLocal8Bit( ::pPtr, hbqt_ptr( pStr ), nSize )


METHOD HBQString:fromRawData( pUnicode, nSize )
   RETURN Qt_HBQString_fromRawData( ::pPtr, hbqt_ptr( pUnicode ), nSize )


METHOD HBQString:fromUcs4( nUnicode, nSize )
   RETURN Qt_HBQString_fromUcs4( ::pPtr, nUnicode, nSize )


METHOD HBQString:fromUtf8( pStr, nSize )
   RETURN Qt_HBQString_fromUtf8( ::pPtr, hbqt_ptr( pStr ), nSize )


METHOD HBQString:fromUtf16( nUnicode, nSize )
   RETURN Qt_HBQString_fromUtf16( ::pPtr, nUnicode, nSize )


METHOD HBQString:localeAwareCompare_2( cS1, cS2 )
   RETURN Qt_HBQString_localeAwareCompare_2( ::pPtr, cS1, cS2 )


METHOD HBQString:localeAwareCompare_3( cS1, pS2 )
   RETURN Qt_HBQString_localeAwareCompare_3( ::pPtr, cS1, hbqt_ptr( pS2 ) )


METHOD HBQString:number( nN, nBase )
   RETURN Qt_HBQString_number( ::pPtr, nN, nBase )


METHOD HBQString:number_1( nN, cFormat, nPrecision )
   RETURN Qt_HBQString_number_1( ::pPtr, nN, cFormat, nPrecision )


METHOD HBQString:number_2( nN, nBase )
   RETURN Qt_HBQString_number_2( ::pPtr, nN, nBase )


METHOD HBQString:number_3( nN, nBase )
   RETURN Qt_HBQString_number_3( ::pPtr, nN, nBase )


METHOD HBQString:number_4( nN, nBase )
   RETURN Qt_HBQString_number_4( ::pPtr, nN, nBase )


METHOD HBQString:number_5( nN, nBase )
   RETURN Qt_HBQString_number_5( ::pPtr, nN, nBase )


METHOD HBQString:number_6( nN, nBase )
   RETURN Qt_HBQString_number_6( ::pPtr, nN, nBase )

