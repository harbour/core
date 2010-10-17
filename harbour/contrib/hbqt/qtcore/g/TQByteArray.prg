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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QByteArray( ... )
   RETURN HB_QByteArray():new( ... )

FUNCTION QByteArrayFrom( ... )
   RETURN HB_QByteArray():from( ... )

FUNCTION QByteArrayFromPointer( ... )
   RETURN HB_QByteArray():fromPointer( ... )


CREATE CLASS QByteArray INHERIT HbQtObjectHandler FUNCTION HB_QByteArray

   METHOD  new( ... )

   METHOD  append                        // ( oQByteArray )                                    -> oQByteArray
   METHOD  append_1                      // ( cStr )                                           -> oQByteArray
   METHOD  append_2                      // ( cStr )                                           -> oQByteArray
   METHOD  append_3                      // ( cStr, nLen )                                     -> oQByteArray
   METHOD  append_4                      // ( nCh )                                            -> oQByteArray
   METHOD  at                            // ( nI )                                             -> cChar
   METHOD  capacity                      // (  )                                               -> nInt
   METHOD  chop                          // ( nN )                                             -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  constData                     // (  )                                               -> cChar
   METHOD  count                         // ( oQByteArray )                                    -> nInt
   METHOD  count_1                       // ( cStr )                                           -> nInt
   METHOD  count_2                       // ( nCh )                                            -> nInt
   METHOD  count_3                       // (  )                                               -> nInt
   METHOD  data                          // (  )                                               -> cChar
   METHOD  data_1                        // (  )                                               -> cChar
   METHOD  endsWith                      // ( oQByteArray )                                    -> lBool
   METHOD  endsWith_1                    // ( cStr )                                           -> lBool
   METHOD  endsWith_2                    // ( nCh )                                            -> lBool
   METHOD  fill                          // ( nCh, nSize )                                     -> oQByteArray
   METHOD  indexOf                       // ( oQByteArray, nFrom )                             -> nInt
   METHOD  indexOf_1                     // ( cStr, nFrom )                                    -> nInt
   METHOD  indexOf_2                     // ( cStr, nFrom )                                    -> nInt
   METHOD  indexOf_3                     // ( nCh, nFrom )                                     -> nInt
   METHOD  insert                        // ( nI, oQByteArray )                                -> oQByteArray
   METHOD  insert_1                      // ( nI, cStr )                                       -> oQByteArray
   METHOD  insert_2                      // ( nI, cStr )                                       -> oQByteArray
   METHOD  insert_3                      // ( nI, nCh )                                        -> oQByteArray
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  lastIndexOf                   // ( oQByteArray, nFrom )                             -> nInt
   METHOD  lastIndexOf_1                 // ( cStr, nFrom )                                    -> nInt
   METHOD  lastIndexOf_2                 // ( cStr, nFrom )                                    -> nInt
   METHOD  lastIndexOf_3                 // ( nCh, nFrom )                                     -> nInt
   METHOD  left                          // ( nLen )                                           -> oQByteArray
   METHOD  leftJustified                 // ( nWidth, nFill, lTruncate )                       -> oQByteArray
   METHOD  length                        // (  )                                               -> nInt
   METHOD  mid                           // ( nPos, nLen )                                     -> oQByteArray
   METHOD  prepend                       // ( oQByteArray )                                    -> oQByteArray
   METHOD  prepend_1                     // ( cStr )                                           -> oQByteArray
   METHOD  prepend_2                     // ( nCh )                                            -> oQByteArray
   METHOD  push_back                     // ( oQByteArray )                                    -> NIL
   METHOD  push_back_1                   // ( cStr )                                           -> NIL
   METHOD  push_back_2                   // ( nCh )                                            -> NIL
   METHOD  push_front                    // ( oQByteArray )                                    -> NIL
   METHOD  push_front_1                  // ( cStr )                                           -> NIL
   METHOD  push_front_2                  // ( nCh )                                            -> NIL
   METHOD  remove                        // ( nPos, nLen )                                     -> oQByteArray
   METHOD  repeated                      // ( nTimes )                                         -> oQByteArray
   METHOD  replace                       // ( nPos, nLen, oQByteArray )                        -> oQByteArray
   METHOD  replace_1                     // ( nPos, nLen, cAfter )                             -> oQByteArray
   METHOD  replace_2                     // ( oQByteArray, oQByteArray )                       -> oQByteArray
   METHOD  replace_3                     // ( cBefore, oQByteArray )                           -> oQByteArray
   METHOD  replace_4                     // ( cBefore, nBsize, cAfter, nAsize )                -> oQByteArray
   METHOD  replace_5                     // ( oQByteArray, cAfter )                            -> oQByteArray
   METHOD  replace_6                     // ( cBefore, oQByteArray )                           -> oQByteArray
   METHOD  replace_7                     // ( cBefore, cAfter )                                -> oQByteArray
   METHOD  replace_8                     // ( cBefore, cAfter )                                -> oQByteArray
   METHOD  replace_9                     // ( nBefore, oQByteArray )                           -> oQByteArray
   METHOD  replace_10                    // ( nBefore, cAfter )                                -> oQByteArray
   METHOD  replace_11                    // ( nBefore, cAfter )                                -> oQByteArray
   METHOD  replace_12                    // ( nBefore, nAfter )                                -> oQByteArray
   METHOD  reserve                       // ( nSize )                                          -> NIL
   METHOD  resize                        // ( nSize )                                          -> NIL
   METHOD  right                         // ( nLen )                                           -> oQByteArray
   METHOD  rightJustified                // ( nWidth, nFill, lTruncate )                       -> oQByteArray
   METHOD  setNum                        // ( nN, nBase )                                      -> oQByteArray
   METHOD  setNum_1                      // ( nN, nBase )                                      -> oQByteArray
   METHOD  setNum_2                      // ( nN, nBase )                                      -> oQByteArray
   METHOD  setNum_3                      // ( nN, nBase )                                      -> oQByteArray
   METHOD  setNum_4                      // ( nN, nBase )                                      -> oQByteArray
   METHOD  setNum_5                      // ( nN, nBase )                                      -> oQByteArray
   METHOD  setNum_6                      // ( nN, nF, nPrec )                                  -> oQByteArray
   METHOD  setNum_7                      // ( nN, nF, nPrec )                                  -> oQByteArray
   METHOD  simplified                    // (  )                                               -> oQByteArray
   METHOD  size                          // (  )                                               -> nInt
   METHOD  split                         // ( nSep )                                           -> oQList_QByteArray>
   METHOD  squeeze                       // (  )                                               -> NIL
   METHOD  startsWith                    // ( oQByteArray )                                    -> lBool
   METHOD  startsWith_1                  // ( cStr )                                           -> lBool
   METHOD  startsWith_2                  // ( nCh )                                            -> lBool
   METHOD  toBase64                      // (  )                                               -> oQByteArray
   METHOD  toDouble                      // ( @lOk )                                           -> nDouble
   METHOD  toFloat                       // ( @lOk )                                           -> nFloat
   METHOD  toHex                         // (  )                                               -> oQByteArray
   METHOD  toInt                         // ( @lOk, nBase )                                    -> nInt
   METHOD  toLong                        // ( @lOk, nBase )                                    -> nLong
   METHOD  toLongLong                    // ( @lOk, nBase )                                    -> nQlonglong
   METHOD  toLower                       // (  )                                               -> oQByteArray
   METHOD  toPercentEncoding             // ( oQByteArray, oQByteArray, nPercent )             -> oQByteArray
   METHOD  toShort                       // ( @lOk, nBase )                                    -> nShort
   METHOD  toUInt                        // ( @lOk, nBase )                                    -> nUint
   METHOD  toULong                       // ( @lOk, nBase )                                    -> nUlong
   METHOD  toULongLong                   // ( @lOk, nBase )                                    -> nQulonglong
   METHOD  toUShort                      // ( @lOk, nBase )                                    -> nUshort
   METHOD  toUpper                       // (  )                                               -> oQByteArray
   METHOD  trimmed                       // (  )                                               -> oQByteArray
   METHOD  truncate                      // ( nPos )                                           -> NIL

   ENDCLASS


METHOD QByteArray:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QByteArray( ... )
   RETURN Self


METHOD QByteArray:append( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_append( ::pPtr, ... ) )


METHOD QByteArray:append_1( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_append_1( ::pPtr, ... ) )


METHOD QByteArray:append_2( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_append_2( ::pPtr, ... ) )


METHOD QByteArray:append_3( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_append_3( ::pPtr, ... ) )


METHOD QByteArray:append_4( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_append_4( ::pPtr, ... ) )


METHOD QByteArray:at( ... )
   RETURN Qt_QByteArray_at( ::pPtr, ... )


METHOD QByteArray:capacity( ... )
   RETURN Qt_QByteArray_capacity( ::pPtr, ... )


METHOD QByteArray:chop( ... )
   RETURN Qt_QByteArray_chop( ::pPtr, ... )


METHOD QByteArray:clear( ... )
   RETURN Qt_QByteArray_clear( ::pPtr, ... )


METHOD QByteArray:constData( ... )
   RETURN Qt_QByteArray_constData( ::pPtr, ... )


METHOD QByteArray:count( ... )
   RETURN Qt_QByteArray_count( ::pPtr, ... )


METHOD QByteArray:count_1( ... )
   RETURN Qt_QByteArray_count_1( ::pPtr, ... )


METHOD QByteArray:count_2( ... )
   RETURN Qt_QByteArray_count_2( ::pPtr, ... )


METHOD QByteArray:count_3( ... )
   RETURN Qt_QByteArray_count_3( ::pPtr, ... )


METHOD QByteArray:data( ... )
   RETURN Qt_QByteArray_data( ::pPtr, ... )


METHOD QByteArray:data_1( ... )
   RETURN Qt_QByteArray_data_1( ::pPtr, ... )


METHOD QByteArray:endsWith( ... )
   RETURN Qt_QByteArray_endsWith( ::pPtr, ... )


METHOD QByteArray:endsWith_1( ... )
   RETURN Qt_QByteArray_endsWith_1( ::pPtr, ... )


METHOD QByteArray:endsWith_2( ... )
   RETURN Qt_QByteArray_endsWith_2( ::pPtr, ... )


METHOD QByteArray:fill( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_fill( ::pPtr, ... ) )


METHOD QByteArray:indexOf( ... )
   RETURN Qt_QByteArray_indexOf( ::pPtr, ... )


METHOD QByteArray:indexOf_1( ... )
   RETURN Qt_QByteArray_indexOf_1( ::pPtr, ... )


METHOD QByteArray:indexOf_2( ... )
   RETURN Qt_QByteArray_indexOf_2( ::pPtr, ... )


METHOD QByteArray:indexOf_3( ... )
   RETURN Qt_QByteArray_indexOf_3( ::pPtr, ... )


METHOD QByteArray:insert( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_insert( ::pPtr, ... ) )


METHOD QByteArray:insert_1( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_insert_1( ::pPtr, ... ) )


METHOD QByteArray:insert_2( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_insert_2( ::pPtr, ... ) )


METHOD QByteArray:insert_3( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_insert_3( ::pPtr, ... ) )


METHOD QByteArray:isEmpty( ... )
   RETURN Qt_QByteArray_isEmpty( ::pPtr, ... )


METHOD QByteArray:isNull( ... )
   RETURN Qt_QByteArray_isNull( ::pPtr, ... )


METHOD QByteArray:lastIndexOf( ... )
   RETURN Qt_QByteArray_lastIndexOf( ::pPtr, ... )


METHOD QByteArray:lastIndexOf_1( ... )
   RETURN Qt_QByteArray_lastIndexOf_1( ::pPtr, ... )


METHOD QByteArray:lastIndexOf_2( ... )
   RETURN Qt_QByteArray_lastIndexOf_2( ::pPtr, ... )


METHOD QByteArray:lastIndexOf_3( ... )
   RETURN Qt_QByteArray_lastIndexOf_3( ::pPtr, ... )


METHOD QByteArray:left( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_left( ::pPtr, ... ) )


METHOD QByteArray:leftJustified( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_leftJustified( ::pPtr, ... ) )


METHOD QByteArray:length( ... )
   RETURN Qt_QByteArray_length( ::pPtr, ... )


METHOD QByteArray:mid( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_mid( ::pPtr, ... ) )


METHOD QByteArray:prepend( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_prepend( ::pPtr, ... ) )


METHOD QByteArray:prepend_1( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_prepend_1( ::pPtr, ... ) )


METHOD QByteArray:prepend_2( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_prepend_2( ::pPtr, ... ) )


METHOD QByteArray:push_back( ... )
   RETURN Qt_QByteArray_push_back( ::pPtr, ... )


METHOD QByteArray:push_back_1( ... )
   RETURN Qt_QByteArray_push_back_1( ::pPtr, ... )


METHOD QByteArray:push_back_2( ... )
   RETURN Qt_QByteArray_push_back_2( ::pPtr, ... )


METHOD QByteArray:push_front( ... )
   RETURN Qt_QByteArray_push_front( ::pPtr, ... )


METHOD QByteArray:push_front_1( ... )
   RETURN Qt_QByteArray_push_front_1( ::pPtr, ... )


METHOD QByteArray:push_front_2( ... )
   RETURN Qt_QByteArray_push_front_2( ::pPtr, ... )


METHOD QByteArray:remove( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_remove( ::pPtr, ... ) )


METHOD QByteArray:repeated( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_repeated( ::pPtr, ... ) )


METHOD QByteArray:replace( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace( ::pPtr, ... ) )


METHOD QByteArray:replace_1( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_1( ::pPtr, ... ) )


METHOD QByteArray:replace_2( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_2( ::pPtr, ... ) )


METHOD QByteArray:replace_3( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_3( ::pPtr, ... ) )


METHOD QByteArray:replace_4( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_4( ::pPtr, ... ) )


METHOD QByteArray:replace_5( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_5( ::pPtr, ... ) )


METHOD QByteArray:replace_6( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_6( ::pPtr, ... ) )


METHOD QByteArray:replace_7( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_7( ::pPtr, ... ) )


METHOD QByteArray:replace_8( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_8( ::pPtr, ... ) )


METHOD QByteArray:replace_9( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_9( ::pPtr, ... ) )


METHOD QByteArray:replace_10( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_10( ::pPtr, ... ) )


METHOD QByteArray:replace_11( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_11( ::pPtr, ... ) )


METHOD QByteArray:replace_12( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_replace_12( ::pPtr, ... ) )


METHOD QByteArray:reserve( ... )
   RETURN Qt_QByteArray_reserve( ::pPtr, ... )


METHOD QByteArray:resize( ... )
   RETURN Qt_QByteArray_resize( ::pPtr, ... )


METHOD QByteArray:right( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_right( ::pPtr, ... ) )


METHOD QByteArray:rightJustified( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_rightJustified( ::pPtr, ... ) )


METHOD QByteArray:setNum( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum( ::pPtr, ... ) )


METHOD QByteArray:setNum_1( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum_1( ::pPtr, ... ) )


METHOD QByteArray:setNum_2( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum_2( ::pPtr, ... ) )


METHOD QByteArray:setNum_3( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum_3( ::pPtr, ... ) )


METHOD QByteArray:setNum_4( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum_4( ::pPtr, ... ) )


METHOD QByteArray:setNum_5( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum_5( ::pPtr, ... ) )


METHOD QByteArray:setNum_6( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum_6( ::pPtr, ... ) )


METHOD QByteArray:setNum_7( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_setNum_7( ::pPtr, ... ) )


METHOD QByteArray:simplified( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_simplified( ::pPtr, ... ) )


METHOD QByteArray:size( ... )
   RETURN Qt_QByteArray_size( ::pPtr, ... )


METHOD QByteArray:split( ... )
   RETURN QListFromPointer( Qt_QByteArray_split( ::pPtr, ... ) )


METHOD QByteArray:squeeze( ... )
   RETURN Qt_QByteArray_squeeze( ::pPtr, ... )


METHOD QByteArray:startsWith( ... )
   RETURN Qt_QByteArray_startsWith( ::pPtr, ... )


METHOD QByteArray:startsWith_1( ... )
   RETURN Qt_QByteArray_startsWith_1( ::pPtr, ... )


METHOD QByteArray:startsWith_2( ... )
   RETURN Qt_QByteArray_startsWith_2( ::pPtr, ... )


METHOD QByteArray:toBase64( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_toBase64( ::pPtr, ... ) )


METHOD QByteArray:toDouble( ... )
   RETURN Qt_QByteArray_toDouble( ::pPtr, ... )


METHOD QByteArray:toFloat( ... )
   RETURN Qt_QByteArray_toFloat( ::pPtr, ... )


METHOD QByteArray:toHex( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_toHex( ::pPtr, ... ) )


METHOD QByteArray:toInt( ... )
   RETURN Qt_QByteArray_toInt( ::pPtr, ... )


METHOD QByteArray:toLong( ... )
   RETURN Qt_QByteArray_toLong( ::pPtr, ... )


METHOD QByteArray:toLongLong( ... )
   RETURN Qt_QByteArray_toLongLong( ::pPtr, ... )


METHOD QByteArray:toLower( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_toLower( ::pPtr, ... ) )


METHOD QByteArray:toPercentEncoding( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_toPercentEncoding( ::pPtr, ... ) )


METHOD QByteArray:toShort( ... )
   RETURN Qt_QByteArray_toShort( ::pPtr, ... )


METHOD QByteArray:toUInt( ... )
   RETURN Qt_QByteArray_toUInt( ::pPtr, ... )


METHOD QByteArray:toULong( ... )
   RETURN Qt_QByteArray_toULong( ::pPtr, ... )


METHOD QByteArray:toULongLong( ... )
   RETURN Qt_QByteArray_toULongLong( ::pPtr, ... )


METHOD QByteArray:toUShort( ... )
   RETURN Qt_QByteArray_toUShort( ::pPtr, ... )


METHOD QByteArray:toUpper( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_toUpper( ::pPtr, ... ) )


METHOD QByteArray:trimmed( ... )
   RETURN QByteArrayFromPointer( Qt_QByteArray_trimmed( ::pPtr, ... ) )


METHOD QByteArray:truncate( ... )
   RETURN Qt_QByteArray_truncate( ::pPtr, ... )

