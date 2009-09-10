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


CREATE CLASS QTextStream

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTextStream_destroy( ::pPtr )

   METHOD  atEnd()                             INLINE  Qt_QTextStream_atEnd( ::pPtr )
   METHOD  autoDetectUnicode()                 INLINE  Qt_QTextStream_autoDetectUnicode( ::pPtr )
   METHOD  codec()                             INLINE  Qt_QTextStream_codec( ::pPtr )
   METHOD  device()                            INLINE  Qt_QTextStream_device( ::pPtr )
   METHOD  fieldAlignment()                    INLINE  Qt_QTextStream_fieldAlignment( ::pPtr )
   METHOD  fieldWidth()                        INLINE  Qt_QTextStream_fieldWidth( ::pPtr )
   METHOD  flush()                             INLINE  Qt_QTextStream_flush( ::pPtr )
   METHOD  generateByteOrderMark()             INLINE  Qt_QTextStream_generateByteOrderMark( ::pPtr )
   METHOD  integerBase()                       INLINE  Qt_QTextStream_integerBase( ::pPtr )
   METHOD  locale()                            INLINE  Qt_QTextStream_locale( ::pPtr )
   METHOD  numberFlags()                       INLINE  Qt_QTextStream_numberFlags( ::pPtr )
   METHOD  pos()                               INLINE  Qt_QTextStream_pos( ::pPtr )
   METHOD  read( nMaxlen )                     INLINE  Qt_QTextStream_read( ::pPtr, nMaxlen )
   METHOD  readAll()                           INLINE  Qt_QTextStream_readAll( ::pPtr )
   METHOD  readLine( nMaxlen )                 INLINE  Qt_QTextStream_readLine( ::pPtr, nMaxlen )
   METHOD  realNumberNotation()                INLINE  Qt_QTextStream_realNumberNotation( ::pPtr )
   METHOD  realNumberPrecision()               INLINE  Qt_QTextStream_realNumberPrecision( ::pPtr )
   METHOD  reset()                             INLINE  Qt_QTextStream_reset( ::pPtr )
   METHOD  resetStatus()                       INLINE  Qt_QTextStream_resetStatus( ::pPtr )
   METHOD  seek( nPos )                        INLINE  Qt_QTextStream_seek( ::pPtr, nPos )
   METHOD  setAutoDetectUnicode( lEnabled )    INLINE  Qt_QTextStream_setAutoDetectUnicode( ::pPtr, lEnabled )
   METHOD  setCodec( pCodec )                  INLINE  Qt_QTextStream_setCodec( ::pPtr, pCodec )
   METHOD  setCodec_1( pCodecName )            INLINE  Qt_QTextStream_setCodec_1( ::pPtr, pCodecName )
   METHOD  setDevice( pDevice )                INLINE  Qt_QTextStream_setDevice( ::pPtr, pDevice )
   METHOD  setFieldAlignment( nMode )          INLINE  Qt_QTextStream_setFieldAlignment( ::pPtr, nMode )
   METHOD  setFieldWidth( nWidth )             INLINE  Qt_QTextStream_setFieldWidth( ::pPtr, nWidth )
   METHOD  setGenerateByteOrderMark( lGenerate )  INLINE  Qt_QTextStream_setGenerateByteOrderMark( ::pPtr, lGenerate )
   METHOD  setIntegerBase( nBase )             INLINE  Qt_QTextStream_setIntegerBase( ::pPtr, nBase )
   METHOD  setLocale( pLocale )                INLINE  Qt_QTextStream_setLocale( ::pPtr, pLocale )
   METHOD  setNumberFlags( nFlags )            INLINE  Qt_QTextStream_setNumberFlags( ::pPtr, nFlags )
   METHOD  setPadChar( nCh )                   INLINE  Qt_QTextStream_setPadChar( ::pPtr, nCh )
   METHOD  setRealNumberNotation( nNotation )  INLINE  Qt_QTextStream_setRealNumberNotation( ::pPtr, nNotation )
   METHOD  setRealNumberPrecision( nPrecision )  INLINE  Qt_QTextStream_setRealNumberPrecision( ::pPtr, nPrecision )
   METHOD  setStatus( nStatus )                INLINE  Qt_QTextStream_setStatus( ::pPtr, nStatus )
   METHOD  skipWhiteSpace()                    INLINE  Qt_QTextStream_skipWhiteSpace( ::pPtr )
   METHOD  status()                            INLINE  Qt_QTextStream_status( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextStream

   ::pParent := pParent

   ::pPtr := Qt_QTextStream( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTextStream

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
