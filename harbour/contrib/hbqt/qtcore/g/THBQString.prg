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


FUNCTION HBQString( ... )
   RETURN HB_HBQString():new( ... )


CREATE CLASS HBQString INHERIT HbQtObjectHandler FUNCTION HB_HBQString

   METHOD  new( ... )

   METHOD  append                        // ( cStr )                                           -> cQString
   METHOD  append_1                      // ( oQStringRef )                                    -> cQString
   METHOD  append_2                      // ( oQLatin1String )                                 -> cQString
   METHOD  append_3                      // ( oQByteArray )                                    -> cQString
   METHOD  append_4                      // ( cStr )                                           -> cQString
   METHOD  append_5                      // ( oQChar )                                         -> cQString
   METHOD  arg                           // ( cA, nFieldWidth, oQChar )                        -> cQString
   METHOD  arg_1                         // ( cA1, cA2 )                                       -> cQString
   METHOD  arg_2                         // ( cA1, cA2, cA3 )                                  -> cQString
   METHOD  arg_3                         // ( cA1, cA2, cA3, cA4 )                             -> cQString
   METHOD  arg_4                         // ( cA1, cA2, cA3, cA4, cA5 )                        -> cQString
   METHOD  arg_5                         // ( cA1, cA2, cA3, cA4, cA5, cA6 )                   -> cQString
   METHOD  arg_6                         // ( cA1, cA2, cA3, cA4, cA5, cA6, cA7 )              -> cQString
   METHOD  arg_7                         // ( cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8 )         -> cQString
   METHOD  arg_8                         // ( cA1, cA2, cA3, cA4, cA5, cA6, cA7, cA8, cA9 )    -> cQString
   METHOD  arg_9                         // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_10                        // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_11                        // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_12                        // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_13                        // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_14                        // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_15                        // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_16                        // ( nA, nFieldWidth, nBase, oQChar )                 -> cQString
   METHOD  arg_17                        // ( oQChar, nFieldWidth, oQChar )                    -> cQString
   METHOD  arg_18                        // ( nA, nFieldWidth, oQChar )                        -> cQString
   METHOD  arg_19                        // ( nA, nFieldWidth, nFormat, nPrecision, oQChar )   -> cQString
   METHOD  at                            // ( nPosition )                                      -> oQChar
   METHOD  capacity                      // (  )                                               -> nInt
   METHOD  chop                          // ( nN )                                             -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  compare                       // ( cOther )                                         -> nInt
   METHOD  compare_1                     // ( cOther, nCs )                                    -> nInt
   METHOD  compare_2                     // ( oQLatin1String, nCs )                            -> nInt
   METHOD  compare_3                     // ( oQStringRef, nCs )                               -> nInt
   METHOD  constData                     // (  )                                               -> oQChar
   METHOD  count                         // ( cStr, nCs )                                      -> nInt
   METHOD  count_1                       // ( oQChar, nCs )                                    -> nInt
   METHOD  count_2                       // ( oQRegExp )                                       -> nInt
   METHOD  count_3                       // (  )                                               -> nInt
   METHOD  data                          // (  )                                               -> oQChar
   METHOD  data_1                        // (  )                                               -> oQChar
   METHOD  endsWith                      // ( cS, nCs )                                        -> lBool
   METHOD  endsWith_1                    // ( oQLatin1String, nCs )                            -> lBool
   METHOD  endsWith_2                    // ( oQChar, nCs )                                    -> lBool
   METHOD  fill                          // ( oQChar, nSize )                                  -> cQString
   METHOD  indexOf                       // ( cStr, nFrom, nCs )                               -> nInt
   METHOD  indexOf_1                     // ( oQLatin1String, nFrom, nCs )                     -> nInt
   METHOD  indexOf_2                     // ( oQChar, nFrom, nCs )                             -> nInt
   METHOD  indexOf_3                     // ( oQRegExp, nFrom )                                -> nInt
   METHOD  indexOf_4                     // ( oQRegExp, nFrom )                                -> nInt
   METHOD  insert                        // ( nPosition, cStr )                                -> cQString
   METHOD  insert_1                      // ( nPosition, oQLatin1String )                      -> cQString
   METHOD  insert_2                      // ( nPosition, oQChar, nSize )                       -> cQString
   METHOD  insert_3                      // ( nPosition, oQChar )                              -> cQString
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  lastIndexOf                   // ( cStr, nFrom, nCs )                               -> nInt
   METHOD  lastIndexOf_1                 // ( oQLatin1String, nFrom, nCs )                     -> nInt
   METHOD  lastIndexOf_2                 // ( oQChar, nFrom, nCs )                             -> nInt
   METHOD  lastIndexOf_3                 // ( oQRegExp, nFrom )                                -> nInt
   METHOD  lastIndexOf_4                 // ( oQRegExp, nFrom )                                -> nInt
   METHOD  left                          // ( nN )                                             -> cQString
   METHOD  leftJustified                 // ( nWidth, oQChar, lTruncate )                      -> cQString
   METHOD  leftRef                       // ( nN )                                             -> oQStringRef
   METHOD  length                        // (  )                                               -> nInt
   METHOD  localeAwareCompare            // ( oQStringRef )                                    -> nInt
   METHOD  localeAwareCompare_1          // ( cOther )                                         -> nInt
   METHOD  mid                           // ( nPosition, nN )                                  -> cQString
   METHOD  midRef                        // ( nPosition, nN )                                  -> oQStringRef
   METHOD  normalized                    // ( nMode )                                          -> cQString
   METHOD  normalized_1                  // ( nMode, nVersion )                                -> cQString
   METHOD  prepend                       // ( cStr )                                           -> cQString
   METHOD  prepend_1                     // ( oQLatin1String )                                 -> cQString
   METHOD  prepend_2                     // ( oQByteArray )                                    -> cQString
   METHOD  prepend_3                     // ( cStr )                                           -> cQString
   METHOD  prepend_4                     // ( oQChar )                                         -> cQString
   METHOD  push_back                     // ( cOther )                                         -> NIL
   METHOD  push_back_1                   // ( oQChar )                                         -> NIL
   METHOD  push_front                    // ( cOther )                                         -> NIL
   METHOD  push_front_1                  // ( oQChar )                                         -> NIL
   METHOD  remove                        // ( nPosition, nN )                                  -> cQString
   METHOD  remove_1                      // ( oQChar, nCs )                                    -> cQString
   METHOD  remove_2                      // ( cStr, nCs )                                      -> cQString
   METHOD  remove_3                      // ( oQRegExp )                                       -> cQString
   METHOD  repeated                      // ( nTimes )                                         -> cQString
   METHOD  replace                       // ( nPosition, nN, cAfter )                          -> cQString
   METHOD  replace_1                     // ( nPosition, nN, oQChar, nSize )                   -> cQString
   METHOD  replace_2                     // ( nPosition, nN, oQChar )                          -> cQString
   METHOD  replace_3                     // ( cBefore, cAfter, nCs )                           -> cQString
   METHOD  replace_4                     // ( oQChar, nBlen, oQChar, nAlen, nCs )              -> cQString
   METHOD  replace_5                     // ( oQChar, cAfter, nCs )                            -> cQString
   METHOD  replace_6                     // ( oQChar, oQChar, nCs )                            -> cQString
   METHOD  replace_7                     // ( oQLatin1String, oQLatin1String, nCs )            -> cQString
   METHOD  replace_8                     // ( oQLatin1String, cAfter, nCs )                    -> cQString
   METHOD  replace_9                     // ( cBefore, oQLatin1String, nCs )                   -> cQString
   METHOD  replace_10                    // ( oQChar, oQLatin1String, nCs )                    -> cQString
   METHOD  replace_11                    // ( oQRegExp, cAfter )                               -> cQString
   METHOD  reserve                       // ( nSize )                                          -> NIL
   METHOD  resize                        // ( nSize )                                          -> NIL
   METHOD  right                         // ( nN )                                             -> cQString
   METHOD  rightJustified                // ( nWidth, oQChar, lTruncate )                      -> cQString
   METHOD  rightRef                      // ( nN )                                             -> oQStringRef
   METHOD  section                       // ( oQChar, nStart, nEnd, nFlags )                   -> cQString
   METHOD  section_1                     // ( cSep, nStart, nEnd, nFlags )                     -> cQString
   METHOD  section_2                     // ( oQRegExp, nStart, nEnd, nFlags )                 -> cQString
   METHOD  setNum                        // ( nN, nBase )                                      -> cQString
   METHOD  setNum_1                      // ( nN, nBase )                                      -> cQString
   METHOD  setNum_2                      // ( nN, nBase )                                      -> cQString
   METHOD  setNum_3                      // ( nN, nBase )                                      -> cQString
   METHOD  setNum_4                      // ( nN, nBase )                                      -> cQString
   METHOD  setNum_5                      // ( nN, nBase )                                      -> cQString
   METHOD  setNum_6                      // ( nN, nBase )                                      -> cQString
   METHOD  setNum_7                      // ( nN, nBase )                                      -> cQString
   METHOD  setNum_8                      // ( nN, nFormat, nPrecision )                        -> cQString
   METHOD  setNum_9                      // ( nN, nFormat, nPrecision )                        -> cQString
   METHOD  setUnicode                    // ( oQChar, nSize )                                  -> cQString
   METHOD  setUtf16                      // ( @nUnicode, nSize )                               -> cQString
   METHOD  simplified                    // (  )                                               -> cQString
   METHOD  size                          // (  )                                               -> nInt
   METHOD  split                         // ( cSep, nBehavior, nCs )                           -> oQStringList
   METHOD  split_1                       // ( oQChar, nBehavior, nCs )                         -> oQStringList
   METHOD  split_2                       // ( oQRegExp, nBehavior )                            -> oQStringList
   METHOD  squeeze                       // (  )                                               -> NIL
   METHOD  startsWith                    // ( cS, nCs )                                        -> lBool
   METHOD  startsWith_1                  // ( oQLatin1String, nCs )                            -> lBool
   METHOD  startsWith_2                  // ( oQChar, nCs )                                    -> lBool
   METHOD  toAscii                       // (  )                                               -> oQByteArray
   METHOD  toCaseFolded                  // (  )                                               -> cQString
   METHOD  toDouble                      // ( @lOk )                                           -> nDouble
   METHOD  toFloat                       // ( @lOk )                                           -> nFloat
   METHOD  toInt                         // ( @lOk, nBase )                                    -> nInt
   METHOD  toLatin1                      // (  )                                               -> oQByteArray
   METHOD  toLocal8Bit                   // (  )                                               -> oQByteArray
   METHOD  toLong                        // ( @lOk, nBase )                                    -> nLong
   METHOD  toLongLong                    // ( @lOk, nBase )                                    -> nQlonglong
   METHOD  toLower                       // (  )                                               -> cQString
   METHOD  toShort                       // ( @lOk, nBase )                                    -> nShort
   METHOD  toUInt                        // ( @lOk, nBase )                                    -> nUint
   METHOD  toULong                       // ( @lOk, nBase )                                    -> nUlong
   METHOD  toULongLong                   // ( @lOk, nBase )                                    -> nQulonglong
   METHOD  toUShort                      // ( @lOk, nBase )                                    -> nUshort
   METHOD  toUpper                       // (  )                                               -> cQString
   METHOD  toUtf8                        // (  )                                               -> oQByteArray
   METHOD  trimmed                       // (  )                                               -> cQString
   METHOD  truncate                      // ( nPosition )                                      -> NIL
   METHOD  unicode                       // (  )                                               -> oQChar
   METHOD  compare_4                     // ( cS1, cS2, nCs )                                  -> nInt
   METHOD  compare_5                     // ( cS1, cS2 )                                       -> nInt
   METHOD  compare_6                     // ( cS1, oQLatin1String, nCs )                       -> nInt
   METHOD  compare_7                     // ( oQLatin1String, cS2, nCs )                       -> nInt
   METHOD  compare_8                     // ( cS1, oQStringRef, nCs )                          -> nInt
   METHOD  fromAscii                     // ( cStr, nSize )                                    -> cQString
   METHOD  fromLatin1                    // ( cStr, nSize )                                    -> cQString
   METHOD  fromLocal8Bit                 // ( cStr, nSize )                                    -> cQString
   METHOD  fromRawData                   // ( oQChar, nSize )                                  -> cQString
   METHOD  fromUcs4                      // ( @nUnicode, nSize )                               -> cQString
   METHOD  fromUtf8                      // ( cStr, nSize )                                    -> cQString
   METHOD  fromUtf16                     // ( @nUnicode, nSize )                               -> cQString
   METHOD  localeAwareCompare_2          // ( cS1, cS2 )                                       -> nInt
   METHOD  localeAwareCompare_3          // ( cS1, oQStringRef )                               -> nInt
   METHOD  number                        // ( nN, nBase )                                      -> cQString
   METHOD  number_1                      // ( nN, nFormat, nPrecision )                        -> cQString
   METHOD  number_2                      // ( nN, nBase )                                      -> cQString
   METHOD  number_3                      // ( nN, nBase )                                      -> cQString
   METHOD  number_4                      // ( nN, nBase )                                      -> cQString
   METHOD  number_5                      // ( nN, nBase )                                      -> cQString
   METHOD  number_6                      // ( nN, nBase )                                      -> cQString

   ENDCLASS


METHOD HBQString:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQString( ... )
   RETURN Self


METHOD HBQString:append( ... )
   RETURN Qt_HBQString_append( ::pPtr, ... )


METHOD HBQString:append_1( ... )
   RETURN Qt_HBQString_append_1( ::pPtr, ... )


METHOD HBQString:append_2( ... )
   RETURN Qt_HBQString_append_2( ::pPtr, ... )


METHOD HBQString:append_3( ... )
   RETURN Qt_HBQString_append_3( ::pPtr, ... )


METHOD HBQString:append_4( ... )
   RETURN Qt_HBQString_append_4( ::pPtr, ... )


METHOD HBQString:append_5( ... )
   RETURN Qt_HBQString_append_5( ::pPtr, ... )


METHOD HBQString:arg( ... )
   RETURN Qt_HBQString_arg( ::pPtr, ... )


METHOD HBQString:arg_1( ... )
   RETURN Qt_HBQString_arg_1( ::pPtr, ... )


METHOD HBQString:arg_2( ... )
   RETURN Qt_HBQString_arg_2( ::pPtr, ... )


METHOD HBQString:arg_3( ... )
   RETURN Qt_HBQString_arg_3( ::pPtr, ... )


METHOD HBQString:arg_4( ... )
   RETURN Qt_HBQString_arg_4( ::pPtr, ... )


METHOD HBQString:arg_5( ... )
   RETURN Qt_HBQString_arg_5( ::pPtr, ... )


METHOD HBQString:arg_6( ... )
   RETURN Qt_HBQString_arg_6( ::pPtr, ... )


METHOD HBQString:arg_7( ... )
   RETURN Qt_HBQString_arg_7( ::pPtr, ... )


METHOD HBQString:arg_8( ... )
   RETURN Qt_HBQString_arg_8( ::pPtr, ... )


METHOD HBQString:arg_9( ... )
   RETURN Qt_HBQString_arg_9( ::pPtr, ... )


METHOD HBQString:arg_10( ... )
   RETURN Qt_HBQString_arg_10( ::pPtr, ... )


METHOD HBQString:arg_11( ... )
   RETURN Qt_HBQString_arg_11( ::pPtr, ... )


METHOD HBQString:arg_12( ... )
   RETURN Qt_HBQString_arg_12( ::pPtr, ... )


METHOD HBQString:arg_13( ... )
   RETURN Qt_HBQString_arg_13( ::pPtr, ... )


METHOD HBQString:arg_14( ... )
   RETURN Qt_HBQString_arg_14( ::pPtr, ... )


METHOD HBQString:arg_15( ... )
   RETURN Qt_HBQString_arg_15( ::pPtr, ... )


METHOD HBQString:arg_16( ... )
   RETURN Qt_HBQString_arg_16( ::pPtr, ... )


METHOD HBQString:arg_17( ... )
   RETURN Qt_HBQString_arg_17( ::pPtr, ... )


METHOD HBQString:arg_18( ... )
   RETURN Qt_HBQString_arg_18( ::pPtr, ... )


METHOD HBQString:arg_19( ... )
   RETURN Qt_HBQString_arg_19( ::pPtr, ... )


METHOD HBQString:at( ... )
   RETURN HB_QChar():from( Qt_HBQString_at( ::pPtr, ... ) )


METHOD HBQString:capacity( ... )
   RETURN Qt_HBQString_capacity( ::pPtr, ... )


METHOD HBQString:chop( ... )
   RETURN Qt_HBQString_chop( ::pPtr, ... )


METHOD HBQString:clear( ... )
   RETURN Qt_HBQString_clear( ::pPtr, ... )


METHOD HBQString:compare( ... )
   RETURN Qt_HBQString_compare( ::pPtr, ... )


METHOD HBQString:compare_1( ... )
   RETURN Qt_HBQString_compare_1( ::pPtr, ... )


METHOD HBQString:compare_2( ... )
   RETURN Qt_HBQString_compare_2( ::pPtr, ... )


METHOD HBQString:compare_3( ... )
   RETURN Qt_HBQString_compare_3( ::pPtr, ... )


METHOD HBQString:constData( ... )
   RETURN HB_QChar():from( Qt_HBQString_constData( ::pPtr, ... ) )


METHOD HBQString:count( ... )
   RETURN Qt_HBQString_count( ::pPtr, ... )


METHOD HBQString:count_1( ... )
   RETURN Qt_HBQString_count_1( ::pPtr, ... )


METHOD HBQString:count_2( ... )
   RETURN Qt_HBQString_count_2( ::pPtr, ... )


METHOD HBQString:count_3( ... )
   RETURN Qt_HBQString_count_3( ::pPtr, ... )


METHOD HBQString:data( ... )
   RETURN HB_QChar():from( Qt_HBQString_data( ::pPtr, ... ) )


METHOD HBQString:data_1( ... )
   RETURN HB_QChar():from( Qt_HBQString_data_1( ::pPtr, ... ) )


METHOD HBQString:endsWith( ... )
   RETURN Qt_HBQString_endsWith( ::pPtr, ... )


METHOD HBQString:endsWith_1( ... )
   RETURN Qt_HBQString_endsWith_1( ::pPtr, ... )


METHOD HBQString:endsWith_2( ... )
   RETURN Qt_HBQString_endsWith_2( ::pPtr, ... )


METHOD HBQString:fill( ... )
   RETURN Qt_HBQString_fill( ::pPtr, ... )


METHOD HBQString:indexOf( ... )
   RETURN Qt_HBQString_indexOf( ::pPtr, ... )


METHOD HBQString:indexOf_1( ... )
   RETURN Qt_HBQString_indexOf_1( ::pPtr, ... )


METHOD HBQString:indexOf_2( ... )
   RETURN Qt_HBQString_indexOf_2( ::pPtr, ... )


METHOD HBQString:indexOf_3( ... )
   RETURN Qt_HBQString_indexOf_3( ::pPtr, ... )


METHOD HBQString:indexOf_4( ... )
   RETURN Qt_HBQString_indexOf_4( ::pPtr, ... )


METHOD HBQString:insert( ... )
   RETURN Qt_HBQString_insert( ::pPtr, ... )


METHOD HBQString:insert_1( ... )
   RETURN Qt_HBQString_insert_1( ::pPtr, ... )


METHOD HBQString:insert_2( ... )
   RETURN Qt_HBQString_insert_2( ::pPtr, ... )


METHOD HBQString:insert_3( ... )
   RETURN Qt_HBQString_insert_3( ::pPtr, ... )


METHOD HBQString:isEmpty( ... )
   RETURN Qt_HBQString_isEmpty( ::pPtr, ... )


METHOD HBQString:isNull( ... )
   RETURN Qt_HBQString_isNull( ::pPtr, ... )


METHOD HBQString:lastIndexOf( ... )
   RETURN Qt_HBQString_lastIndexOf( ::pPtr, ... )


METHOD HBQString:lastIndexOf_1( ... )
   RETURN Qt_HBQString_lastIndexOf_1( ::pPtr, ... )


METHOD HBQString:lastIndexOf_2( ... )
   RETURN Qt_HBQString_lastIndexOf_2( ::pPtr, ... )


METHOD HBQString:lastIndexOf_3( ... )
   RETURN Qt_HBQString_lastIndexOf_3( ::pPtr, ... )


METHOD HBQString:lastIndexOf_4( ... )
   RETURN Qt_HBQString_lastIndexOf_4( ::pPtr, ... )


METHOD HBQString:left( ... )
   RETURN Qt_HBQString_left( ::pPtr, ... )


METHOD HBQString:leftJustified( ... )
   RETURN Qt_HBQString_leftJustified( ::pPtr, ... )


METHOD HBQString:leftRef( ... )
   RETURN HB_QStringRef():from( Qt_HBQString_leftRef( ::pPtr, ... ) )


METHOD HBQString:length( ... )
   RETURN Qt_HBQString_length( ::pPtr, ... )


METHOD HBQString:localeAwareCompare( ... )
   RETURN Qt_HBQString_localeAwareCompare( ::pPtr, ... )


METHOD HBQString:localeAwareCompare_1( ... )
   RETURN Qt_HBQString_localeAwareCompare_1( ::pPtr, ... )


METHOD HBQString:mid( ... )
   RETURN Qt_HBQString_mid( ::pPtr, ... )


METHOD HBQString:midRef( ... )
   RETURN HB_QStringRef():from( Qt_HBQString_midRef( ::pPtr, ... ) )


METHOD HBQString:normalized( ... )
   RETURN Qt_HBQString_normalized( ::pPtr, ... )


METHOD HBQString:normalized_1( ... )
   RETURN Qt_HBQString_normalized_1( ::pPtr, ... )


METHOD HBQString:prepend( ... )
   RETURN Qt_HBQString_prepend( ::pPtr, ... )


METHOD HBQString:prepend_1( ... )
   RETURN Qt_HBQString_prepend_1( ::pPtr, ... )


METHOD HBQString:prepend_2( ... )
   RETURN Qt_HBQString_prepend_2( ::pPtr, ... )


METHOD HBQString:prepend_3( ... )
   RETURN Qt_HBQString_prepend_3( ::pPtr, ... )


METHOD HBQString:prepend_4( ... )
   RETURN Qt_HBQString_prepend_4( ::pPtr, ... )


METHOD HBQString:push_back( ... )
   RETURN Qt_HBQString_push_back( ::pPtr, ... )


METHOD HBQString:push_back_1( ... )
   RETURN Qt_HBQString_push_back_1( ::pPtr, ... )


METHOD HBQString:push_front( ... )
   RETURN Qt_HBQString_push_front( ::pPtr, ... )


METHOD HBQString:push_front_1( ... )
   RETURN Qt_HBQString_push_front_1( ::pPtr, ... )


METHOD HBQString:remove( ... )
   RETURN Qt_HBQString_remove( ::pPtr, ... )


METHOD HBQString:remove_1( ... )
   RETURN Qt_HBQString_remove_1( ::pPtr, ... )


METHOD HBQString:remove_2( ... )
   RETURN Qt_HBQString_remove_2( ::pPtr, ... )


METHOD HBQString:remove_3( ... )
   RETURN Qt_HBQString_remove_3( ::pPtr, ... )


METHOD HBQString:repeated( ... )
   RETURN Qt_HBQString_repeated( ::pPtr, ... )


METHOD HBQString:replace( ... )
   RETURN Qt_HBQString_replace( ::pPtr, ... )


METHOD HBQString:replace_1( ... )
   RETURN Qt_HBQString_replace_1( ::pPtr, ... )


METHOD HBQString:replace_2( ... )
   RETURN Qt_HBQString_replace_2( ::pPtr, ... )


METHOD HBQString:replace_3( ... )
   RETURN Qt_HBQString_replace_3( ::pPtr, ... )


METHOD HBQString:replace_4( ... )
   RETURN Qt_HBQString_replace_4( ::pPtr, ... )


METHOD HBQString:replace_5( ... )
   RETURN Qt_HBQString_replace_5( ::pPtr, ... )


METHOD HBQString:replace_6( ... )
   RETURN Qt_HBQString_replace_6( ::pPtr, ... )


METHOD HBQString:replace_7( ... )
   RETURN Qt_HBQString_replace_7( ::pPtr, ... )


METHOD HBQString:replace_8( ... )
   RETURN Qt_HBQString_replace_8( ::pPtr, ... )


METHOD HBQString:replace_9( ... )
   RETURN Qt_HBQString_replace_9( ::pPtr, ... )


METHOD HBQString:replace_10( ... )
   RETURN Qt_HBQString_replace_10( ::pPtr, ... )


METHOD HBQString:replace_11( ... )
   RETURN Qt_HBQString_replace_11( ::pPtr, ... )


METHOD HBQString:reserve( ... )
   RETURN Qt_HBQString_reserve( ::pPtr, ... )


METHOD HBQString:resize( ... )
   RETURN Qt_HBQString_resize( ::pPtr, ... )


METHOD HBQString:right( ... )
   RETURN Qt_HBQString_right( ::pPtr, ... )


METHOD HBQString:rightJustified( ... )
   RETURN Qt_HBQString_rightJustified( ::pPtr, ... )


METHOD HBQString:rightRef( ... )
   RETURN HB_QStringRef():from( Qt_HBQString_rightRef( ::pPtr, ... ) )


METHOD HBQString:section( ... )
   RETURN Qt_HBQString_section( ::pPtr, ... )


METHOD HBQString:section_1( ... )
   RETURN Qt_HBQString_section_1( ::pPtr, ... )


METHOD HBQString:section_2( ... )
   RETURN Qt_HBQString_section_2( ::pPtr, ... )


METHOD HBQString:setNum( ... )
   RETURN Qt_HBQString_setNum( ::pPtr, ... )


METHOD HBQString:setNum_1( ... )
   RETURN Qt_HBQString_setNum_1( ::pPtr, ... )


METHOD HBQString:setNum_2( ... )
   RETURN Qt_HBQString_setNum_2( ::pPtr, ... )


METHOD HBQString:setNum_3( ... )
   RETURN Qt_HBQString_setNum_3( ::pPtr, ... )


METHOD HBQString:setNum_4( ... )
   RETURN Qt_HBQString_setNum_4( ::pPtr, ... )


METHOD HBQString:setNum_5( ... )
   RETURN Qt_HBQString_setNum_5( ::pPtr, ... )


METHOD HBQString:setNum_6( ... )
   RETURN Qt_HBQString_setNum_6( ::pPtr, ... )


METHOD HBQString:setNum_7( ... )
   RETURN Qt_HBQString_setNum_7( ::pPtr, ... )


METHOD HBQString:setNum_8( ... )
   RETURN Qt_HBQString_setNum_8( ::pPtr, ... )


METHOD HBQString:setNum_9( ... )
   RETURN Qt_HBQString_setNum_9( ::pPtr, ... )


METHOD HBQString:setUnicode( ... )
   RETURN Qt_HBQString_setUnicode( ::pPtr, ... )


METHOD HBQString:setUtf16( ... )
   RETURN Qt_HBQString_setUtf16( ::pPtr, ... )


METHOD HBQString:simplified( ... )
   RETURN Qt_HBQString_simplified( ::pPtr, ... )


METHOD HBQString:size( ... )
   RETURN Qt_HBQString_size( ::pPtr, ... )


METHOD HBQString:split( ... )
   RETURN HB_QStringList():from( Qt_HBQString_split( ::pPtr, ... ) )


METHOD HBQString:split_1( ... )
   RETURN HB_QStringList():from( Qt_HBQString_split_1( ::pPtr, ... ) )


METHOD HBQString:split_2( ... )
   RETURN HB_QStringList():from( Qt_HBQString_split_2( ::pPtr, ... ) )


METHOD HBQString:squeeze( ... )
   RETURN Qt_HBQString_squeeze( ::pPtr, ... )


METHOD HBQString:startsWith( ... )
   RETURN Qt_HBQString_startsWith( ::pPtr, ... )


METHOD HBQString:startsWith_1( ... )
   RETURN Qt_HBQString_startsWith_1( ::pPtr, ... )


METHOD HBQString:startsWith_2( ... )
   RETURN Qt_HBQString_startsWith_2( ::pPtr, ... )


METHOD HBQString:toAscii( ... )
   RETURN HB_QByteArray():from( Qt_HBQString_toAscii( ::pPtr, ... ) )


METHOD HBQString:toCaseFolded( ... )
   RETURN Qt_HBQString_toCaseFolded( ::pPtr, ... )


METHOD HBQString:toDouble( ... )
   RETURN Qt_HBQString_toDouble( ::pPtr, ... )


METHOD HBQString:toFloat( ... )
   RETURN Qt_HBQString_toFloat( ::pPtr, ... )


METHOD HBQString:toInt( ... )
   RETURN Qt_HBQString_toInt( ::pPtr, ... )


METHOD HBQString:toLatin1( ... )
   RETURN HB_QByteArray():from( Qt_HBQString_toLatin1( ::pPtr, ... ) )


METHOD HBQString:toLocal8Bit( ... )
   RETURN HB_QByteArray():from( Qt_HBQString_toLocal8Bit( ::pPtr, ... ) )


METHOD HBQString:toLong( ... )
   RETURN Qt_HBQString_toLong( ::pPtr, ... )


METHOD HBQString:toLongLong( ... )
   RETURN Qt_HBQString_toLongLong( ::pPtr, ... )


METHOD HBQString:toLower( ... )
   RETURN Qt_HBQString_toLower( ::pPtr, ... )


METHOD HBQString:toShort( ... )
   RETURN Qt_HBQString_toShort( ::pPtr, ... )


METHOD HBQString:toUInt( ... )
   RETURN Qt_HBQString_toUInt( ::pPtr, ... )


METHOD HBQString:toULong( ... )
   RETURN Qt_HBQString_toULong( ::pPtr, ... )


METHOD HBQString:toULongLong( ... )
   RETURN Qt_HBQString_toULongLong( ::pPtr, ... )


METHOD HBQString:toUShort( ... )
   RETURN Qt_HBQString_toUShort( ::pPtr, ... )


METHOD HBQString:toUpper( ... )
   RETURN Qt_HBQString_toUpper( ::pPtr, ... )


METHOD HBQString:toUtf8( ... )
   RETURN HB_QByteArray():from( Qt_HBQString_toUtf8( ::pPtr, ... ) )


METHOD HBQString:trimmed( ... )
   RETURN Qt_HBQString_trimmed( ::pPtr, ... )


METHOD HBQString:truncate( ... )
   RETURN Qt_HBQString_truncate( ::pPtr, ... )


METHOD HBQString:unicode( ... )
   RETURN HB_QChar():from( Qt_HBQString_unicode( ::pPtr, ... ) )


METHOD HBQString:compare_4( ... )
   RETURN Qt_HBQString_compare_4( ::pPtr, ... )


METHOD HBQString:compare_5( ... )
   RETURN Qt_HBQString_compare_5( ::pPtr, ... )


METHOD HBQString:compare_6( ... )
   RETURN Qt_HBQString_compare_6( ::pPtr, ... )


METHOD HBQString:compare_7( ... )
   RETURN Qt_HBQString_compare_7( ::pPtr, ... )


METHOD HBQString:compare_8( ... )
   RETURN Qt_HBQString_compare_8( ::pPtr, ... )


METHOD HBQString:fromAscii( ... )
   RETURN Qt_HBQString_fromAscii( ::pPtr, ... )


METHOD HBQString:fromLatin1( ... )
   RETURN Qt_HBQString_fromLatin1( ::pPtr, ... )


METHOD HBQString:fromLocal8Bit( ... )
   RETURN Qt_HBQString_fromLocal8Bit( ::pPtr, ... )


METHOD HBQString:fromRawData( ... )
   RETURN Qt_HBQString_fromRawData( ::pPtr, ... )


METHOD HBQString:fromUcs4( ... )
   RETURN Qt_HBQString_fromUcs4( ::pPtr, ... )


METHOD HBQString:fromUtf8( ... )
   RETURN Qt_HBQString_fromUtf8( ::pPtr, ... )


METHOD HBQString:fromUtf16( ... )
   RETURN Qt_HBQString_fromUtf16( ::pPtr, ... )


METHOD HBQString:localeAwareCompare_2( ... )
   RETURN Qt_HBQString_localeAwareCompare_2( ::pPtr, ... )


METHOD HBQString:localeAwareCompare_3( ... )
   RETURN Qt_HBQString_localeAwareCompare_3( ::pPtr, ... )


METHOD HBQString:number( ... )
   RETURN Qt_HBQString_number( ::pPtr, ... )


METHOD HBQString:number_1( ... )
   RETURN Qt_HBQString_number_1( ::pPtr, ... )


METHOD HBQString:number_2( ... )
   RETURN Qt_HBQString_number_2( ::pPtr, ... )


METHOD HBQString:number_3( ... )
   RETURN Qt_HBQString_number_3( ::pPtr, ... )


METHOD HBQString:number_4( ... )
   RETURN Qt_HBQString_number_4( ::pPtr, ... )


METHOD HBQString:number_5( ... )
   RETURN Qt_HBQString_number_5( ::pPtr, ... )


METHOD HBQString:number_6( ... )
   RETURN Qt_HBQString_number_6( ::pPtr, ... )

