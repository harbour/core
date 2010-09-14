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


FUNCTION QTextCodec( ... )
   RETURN HB_QTextCodec():new( ... )


CREATE CLASS QTextCodec INHERIT HbQtObjectHandler FUNCTION HB_QTextCodec

   METHOD  new( ... )

   METHOD  aliases()
   METHOD  canEncode( pCh )
   METHOD  canEncode_1( cS )
   METHOD  fromUnicode( cStr )
   METHOD  makeDecoder()
   METHOD  makeEncoder()
   METHOD  mibEnum()
   METHOD  name()
   METHOD  toUnicode( pA )
   METHOD  toUnicode_1( pChars )
   METHOD  codecForCStrings()
   METHOD  codecForHtml( pBa, pDefaultCodec )
   METHOD  codecForHtml_1( pBa )
   METHOD  codecForLocale()
   METHOD  codecForMib( nMib )
   METHOD  codecForName( pName )
   METHOD  codecForName_1( pName )
   METHOD  codecForTr()
   METHOD  setCodecForCStrings( pCodec )
   METHOD  setCodecForLocale( pC )
   METHOD  setCodecForTr( pC )

   ENDCLASS


METHOD QTextCodec:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCodec( ... )
   RETURN Self


METHOD QTextCodec:aliases()
   RETURN Qt_QTextCodec_aliases( ::pPtr )


METHOD QTextCodec:canEncode( pCh )
   RETURN Qt_QTextCodec_canEncode( ::pPtr, hbqt_ptr( pCh ) )


METHOD QTextCodec:canEncode_1( cS )
   RETURN Qt_QTextCodec_canEncode_1( ::pPtr, cS )


METHOD QTextCodec:fromUnicode( cStr )
   RETURN Qt_QTextCodec_fromUnicode( ::pPtr, cStr )


METHOD QTextCodec:makeDecoder()
   RETURN Qt_QTextCodec_makeDecoder( ::pPtr )


METHOD QTextCodec:makeEncoder()
   RETURN Qt_QTextCodec_makeEncoder( ::pPtr )


METHOD QTextCodec:mibEnum()
   RETURN Qt_QTextCodec_mibEnum( ::pPtr )


METHOD QTextCodec:name()
   RETURN Qt_QTextCodec_name( ::pPtr )


METHOD QTextCodec:toUnicode( pA )
   RETURN Qt_QTextCodec_toUnicode( ::pPtr, hbqt_ptr( pA ) )


METHOD QTextCodec:toUnicode_1( pChars )
   RETURN Qt_QTextCodec_toUnicode_1( ::pPtr, hbqt_ptr( pChars ) )


METHOD QTextCodec:codecForCStrings()
   RETURN Qt_QTextCodec_codecForCStrings( ::pPtr )


METHOD QTextCodec:codecForHtml( pBa, pDefaultCodec )
   RETURN Qt_QTextCodec_codecForHtml( ::pPtr, hbqt_ptr( pBa ), hbqt_ptr( pDefaultCodec ) )


METHOD QTextCodec:codecForHtml_1( pBa )
   RETURN Qt_QTextCodec_codecForHtml_1( ::pPtr, hbqt_ptr( pBa ) )


METHOD QTextCodec:codecForLocale()
   RETURN Qt_QTextCodec_codecForLocale( ::pPtr )


METHOD QTextCodec:codecForMib( nMib )
   RETURN Qt_QTextCodec_codecForMib( ::pPtr, nMib )


METHOD QTextCodec:codecForName( pName )
   RETURN Qt_QTextCodec_codecForName( ::pPtr, hbqt_ptr( pName ) )


METHOD QTextCodec:codecForName_1( pName )
   RETURN Qt_QTextCodec_codecForName_1( ::pPtr, hbqt_ptr( pName ) )


METHOD QTextCodec:codecForTr()
   RETURN Qt_QTextCodec_codecForTr( ::pPtr )


METHOD QTextCodec:setCodecForCStrings( pCodec )
   RETURN Qt_QTextCodec_setCodecForCStrings( ::pPtr, hbqt_ptr( pCodec ) )


METHOD QTextCodec:setCodecForLocale( pC )
   RETURN Qt_QTextCodec_setCodecForLocale( ::pPtr, hbqt_ptr( pC ) )


METHOD QTextCodec:setCodecForTr( pC )
   RETURN Qt_QTextCodec_setCodecForTr( ::pPtr, hbqt_ptr( pC ) )

