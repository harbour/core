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


FUNCTION QTextCodec( ... )
   RETURN HB_QTextCodec():new( ... )

FUNCTION QTextCodecFrom( ... )
   RETURN HB_QTextCodec():from( ... )

FUNCTION QTextCodecFromPointer( ... )
   RETURN HB_QTextCodec():fromPointer( ... )


CREATE CLASS QTextCodec INHERIT HbQtObjectHandler FUNCTION HB_QTextCodec

   METHOD  new( ... )

   METHOD  aliases                       // (  )                                               -> oQList_QByteArray>
   METHOD  canEncode                     // ( oQChar )                                         -> lBool
                                         // ( cS )                                             -> lBool
   METHOD  fromUnicode                   // ( cStr )                                           -> oQByteArray
   METHOD  makeDecoder                   // (  )                                               -> oQTextDecoder
   METHOD  makeEncoder                   // (  )                                               -> oQTextEncoder
   METHOD  mibEnum                       // (  )                                               -> nInt
   METHOD  name                          // (  )                                               -> oQByteArray
   METHOD  toUnicode                     // ( oQByteArray )                                    -> cQString
                                         // ( cChars )                                         -> cQString
   METHOD  codecForCStrings              // (  )                                               -> oQTextCodec
   METHOD  codecForHtml                  // ( oQByteArray, oQTextCodec )                       -> oQTextCodec
                                         // ( oQByteArray )                                    -> oQTextCodec
   METHOD  codecForLocale                // (  )                                               -> oQTextCodec
   METHOD  codecForMib                   // ( nMib )                                           -> oQTextCodec
   METHOD  codecForName                  // ( oQByteArray )                                    -> oQTextCodec
                                         // ( cName )                                          -> oQTextCodec
   METHOD  codecForTr                    // (  )                                               -> oQTextCodec
   METHOD  setCodecForCStrings           // ( oQTextCodec )                                    -> NIL
   METHOD  setCodecForLocale             // ( oQTextCodec )                                    -> NIL
   METHOD  setCodecForTr                 // ( oQTextCodec )                                    -> NIL

   ENDCLASS


METHOD QTextCodec:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCodec( ... )
   RETURN Self


METHOD QTextCodec:aliases( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTextCodec_aliases( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:canEncode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_canEncode_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_canEncode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:fromUnicode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QTextCodec_fromUnicode( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:makeDecoder( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDecoderFromPointer( Qt_QTextCodec_makeDecoder( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:makeEncoder( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextEncoderFromPointer( Qt_QTextCodec_makeEncoder( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:mibEnum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCodec_mibEnum( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QTextCodec_name( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:toUnicode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_toUnicode_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_toUnicode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:codecForCStrings( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForCStrings( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:codecForHtml( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForHtml( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForHtml_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:codecForLocale( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForLocale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:codecForMib( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForMib( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:codecForName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForName_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForName( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:codecForTr( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextCodec_codecForTr( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:setCodecForCStrings( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_setCodecForCStrings( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:setCodecForLocale( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_setCodecForLocale( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCodec:setCodecForTr( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCodec_setCodecForTr( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

