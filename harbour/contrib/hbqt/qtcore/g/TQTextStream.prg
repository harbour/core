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


CREATE CLASS QTextStream INHERIT HbQtObjectHandler FUNCTION HB_QTextStream

   METHOD  new( ... )

   METHOD  padChar()
   METHOD  atEnd()
   METHOD  autoDetectUnicode()
   METHOD  codec()
   METHOD  device()
   METHOD  fieldAlignment()
   METHOD  fieldWidth()
   METHOD  flush()
   METHOD  generateByteOrderMark()
   METHOD  integerBase()
   METHOD  locale()
   METHOD  numberFlags()
   METHOD  pos()
   METHOD  read( nMaxlen )
   METHOD  readAll()
   METHOD  readLine( nMaxlen )
   METHOD  realNumberNotation()
   METHOD  realNumberPrecision()
   METHOD  reset()
   METHOD  resetStatus()
   METHOD  seek( nPos )
   METHOD  setAutoDetectUnicode( lEnabled )
   METHOD  setCodec( ... )
   METHOD  setDevice( pDevice )
   METHOD  setFieldAlignment( nMode )
   METHOD  setFieldWidth( nWidth )
   METHOD  setGenerateByteOrderMark( lGenerate )
   METHOD  setIntegerBase( nBase )
   METHOD  setLocale( pLocale )
   METHOD  setNumberFlags( nFlags )
   METHOD  setPadChar( pCh )
   METHOD  setRealNumberNotation( nNotation )
   METHOD  setRealNumberPrecision( nPrecision )
   METHOD  setStatus( nStatus )
   METHOD  skipWhiteSpace()
   METHOD  status()

   ENDCLASS


METHOD QTextStream:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextStream( ... )
   RETURN Self


METHOD QTextStream:padChar()
   RETURN HB_QChar():from( Qt_QTextStream_padChar( ::pPtr ) )


METHOD QTextStream:atEnd()
   RETURN Qt_QTextStream_atEnd( ::pPtr )


METHOD QTextStream:autoDetectUnicode()
   RETURN Qt_QTextStream_autoDetectUnicode( ::pPtr )


METHOD QTextStream:codec()
   RETURN HB_QTextCodec():from( Qt_QTextStream_codec( ::pPtr ) )


METHOD QTextStream:device()
   RETURN HB_QIODevice():from( Qt_QTextStream_device( ::pPtr ) )


METHOD QTextStream:fieldAlignment()
   RETURN Qt_QTextStream_fieldAlignment( ::pPtr )


METHOD QTextStream:fieldWidth()
   RETURN Qt_QTextStream_fieldWidth( ::pPtr )


METHOD QTextStream:flush()
   RETURN Qt_QTextStream_flush( ::pPtr )


METHOD QTextStream:generateByteOrderMark()
   RETURN Qt_QTextStream_generateByteOrderMark( ::pPtr )


METHOD QTextStream:integerBase()
   RETURN Qt_QTextStream_integerBase( ::pPtr )


METHOD QTextStream:locale()
   RETURN HB_QLocale():from( Qt_QTextStream_locale( ::pPtr ) )


METHOD QTextStream:numberFlags()
   RETURN Qt_QTextStream_numberFlags( ::pPtr )


METHOD QTextStream:pos()
   RETURN Qt_QTextStream_pos( ::pPtr )


METHOD QTextStream:read( nMaxlen )
   RETURN Qt_QTextStream_read( ::pPtr, nMaxlen )


METHOD QTextStream:readAll()
   RETURN Qt_QTextStream_readAll( ::pPtr )


METHOD QTextStream:readLine( nMaxlen )
   RETURN Qt_QTextStream_readLine( ::pPtr, nMaxlen )


METHOD QTextStream:realNumberNotation()
   RETURN Qt_QTextStream_realNumberNotation( ::pPtr )


METHOD QTextStream:realNumberPrecision()
   RETURN Qt_QTextStream_realNumberPrecision( ::pPtr )


METHOD QTextStream:reset()
   RETURN Qt_QTextStream_reset( ::pPtr )


METHOD QTextStream:resetStatus()
   RETURN Qt_QTextStream_resetStatus( ::pPtr )


METHOD QTextStream:seek( nPos )
   RETURN Qt_QTextStream_seek( ::pPtr, nPos )


METHOD QTextStream:setAutoDetectUnicode( lEnabled )
   RETURN Qt_QTextStream_setAutoDetectUnicode( ::pPtr, lEnabled )


METHOD QTextStream:setCodec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTEXTCODEC"
            RETURN Qt_QTextStream_setCodec( ::pPtr, ... )
         // RETURN Qt_QTextStream_setCodec_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextStream:setDevice( pDevice )
   RETURN Qt_QTextStream_setDevice( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QTextStream:setFieldAlignment( nMode )
   RETURN Qt_QTextStream_setFieldAlignment( ::pPtr, nMode )


METHOD QTextStream:setFieldWidth( nWidth )
   RETURN Qt_QTextStream_setFieldWidth( ::pPtr, nWidth )


METHOD QTextStream:setGenerateByteOrderMark( lGenerate )
   RETURN Qt_QTextStream_setGenerateByteOrderMark( ::pPtr, lGenerate )


METHOD QTextStream:setIntegerBase( nBase )
   RETURN Qt_QTextStream_setIntegerBase( ::pPtr, nBase )


METHOD QTextStream:setLocale( pLocale )
   RETURN Qt_QTextStream_setLocale( ::pPtr, hbqt_ptr( pLocale ) )


METHOD QTextStream:setNumberFlags( nFlags )
   RETURN Qt_QTextStream_setNumberFlags( ::pPtr, nFlags )


METHOD QTextStream:setPadChar( pCh )
   RETURN Qt_QTextStream_setPadChar( ::pPtr, hbqt_ptr( pCh ) )


METHOD QTextStream:setRealNumberNotation( nNotation )
   RETURN Qt_QTextStream_setRealNumberNotation( ::pPtr, nNotation )


METHOD QTextStream:setRealNumberPrecision( nPrecision )
   RETURN Qt_QTextStream_setRealNumberPrecision( ::pPtr, nPrecision )


METHOD QTextStream:setStatus( nStatus )
   RETURN Qt_QTextStream_setStatus( ::pPtr, nStatus )


METHOD QTextStream:skipWhiteSpace()
   RETURN Qt_QTextStream_skipWhiteSpace( ::pPtr )


METHOD QTextStream:status()
   RETURN Qt_QTextStream_status( ::pPtr )

