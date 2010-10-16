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


FUNCTION QTextStream( ... )
   RETURN HB_QTextStream():new( ... )

FUNCTION QTextStreamFrom( ... )
   RETURN HB_QTextStream():from( ... )

FUNCTION QTextStreamFromPointer( ... )
   RETURN HB_QTextStream():fromPointer( ... )


CREATE CLASS QTextStream INHERIT HbQtObjectHandler FUNCTION HB_QTextStream

   METHOD  new( ... )

   METHOD  padChar                       // (  )                                               -> oQChar
   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  autoDetectUnicode             // (  )                                               -> lBool
   METHOD  codec                         // (  )                                               -> oQTextCodec
   METHOD  device                        // (  )                                               -> oQIODevice
   METHOD  fieldAlignment                // (  )                                               -> nFieldAlignment
   METHOD  fieldWidth                    // (  )                                               -> nInt
   METHOD  flush                         // (  )                                               -> NIL
   METHOD  generateByteOrderMark         // (  )                                               -> lBool
   METHOD  integerBase                   // (  )                                               -> nInt
   METHOD  locale                        // (  )                                               -> oQLocale
   METHOD  numberFlags                   // (  )                                               -> nNumberFlags
   METHOD  pos                           // (  )                                               -> nQint64
   METHOD  read                          // ( nMaxlen )                                        -> cQString
   METHOD  readAll                       // (  )                                               -> cQString
   METHOD  readLine                      // ( nMaxlen )                                        -> cQString
   METHOD  realNumberNotation            // (  )                                               -> nRealNumberNotation
   METHOD  realNumberPrecision           // (  )                                               -> nInt
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  resetStatus                   // (  )                                               -> NIL
   METHOD  seek                          // ( nPos )                                           -> lBool
   METHOD  setAutoDetectUnicode          // ( lEnabled )                                       -> NIL
   METHOD  setCodec                      // ( oQTextCodec )                                    -> NIL
                                         // ( cCodecName )                                     -> NIL
   METHOD  setDevice                     // ( oQIODevice )                                     -> NIL
   METHOD  setFieldAlignment             // ( nMode )                                          -> NIL
   METHOD  setFieldWidth                 // ( nWidth )                                         -> NIL
   METHOD  setGenerateByteOrderMark      // ( lGenerate )                                      -> NIL
   METHOD  setIntegerBase                // ( nBase )                                          -> NIL
   METHOD  setLocale                     // ( oQLocale )                                       -> NIL
   METHOD  setNumberFlags                // ( nFlags )                                         -> NIL
   METHOD  setPadChar                    // ( oQChar )                                         -> NIL
   METHOD  setRealNumberNotation         // ( nNotation )                                      -> NIL
   METHOD  setRealNumberPrecision        // ( nPrecision )                                     -> NIL
   METHOD  setStatus                     // ( nStatus )                                        -> NIL
   METHOD  skipWhiteSpace                // (  )                                               -> NIL
   METHOD  status                        // (  )                                               -> nStatus

   ENDCLASS


METHOD QTextStream:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextStream( ... )
   RETURN Self


METHOD QTextStream:padChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QTextStream_padChar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:autoDetectUnicode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_autoDetectUnicode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:codec( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextStream_codec( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QTextStream_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:fieldAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_fieldAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:fieldWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_fieldWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:flush( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_flush( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:generateByteOrderMark( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_generateByteOrderMark( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:integerBase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_integerBase( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:locale( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLocaleFromPointer( Qt_QTextStream_locale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:numberFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_numberFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_pos( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:readAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_readAll( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:readLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_readLine( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextStream_readLine( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:realNumberNotation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_realNumberNotation( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:realNumberPrecision( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_realNumberPrecision( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:resetStatus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_resetStatus( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:seek( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_seek( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setAutoDetectUnicode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setAutoDetectUnicode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setCodec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setCodec_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setCodec( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setFieldAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setFieldAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setFieldWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setFieldWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setGenerateByteOrderMark( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setGenerateByteOrderMark( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setIntegerBase( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setIntegerBase( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setLocale( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setLocale( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setNumberFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setNumberFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setPadChar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setPadChar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setRealNumberNotation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setRealNumberNotation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setRealNumberPrecision( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setRealNumberPrecision( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setStatus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextStream_setStatus( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:skipWhiteSpace( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_skipWhiteSpace( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:status( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextStream_status( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

