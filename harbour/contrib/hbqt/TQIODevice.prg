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


CREATE CLASS QIODevice INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QIODevice_destroy( ::pPtr )

   METHOD  atEnd()                             INLINE  Qt_QIODevice_atEnd( ::pPtr )
   METHOD  bytesAvailable()                    INLINE  Qt_QIODevice_bytesAvailable( ::pPtr )
   METHOD  bytesToWrite()                      INLINE  Qt_QIODevice_bytesToWrite( ::pPtr )
   METHOD  canReadLine()                       INLINE  Qt_QIODevice_canReadLine( ::pPtr )
   METHOD  close()                             INLINE  Qt_QIODevice_close( ::pPtr )
   METHOD  errorString()                       INLINE  Qt_QIODevice_errorString( ::pPtr )
   METHOD  getChar( cC )                       INLINE  Qt_QIODevice_getChar( ::pPtr, cC )
   METHOD  isOpen()                            INLINE  Qt_QIODevice_isOpen( ::pPtr )
   METHOD  isReadable()                        INLINE  Qt_QIODevice_isReadable( ::pPtr )
   METHOD  isSequential()                      INLINE  Qt_QIODevice_isSequential( ::pPtr )
   METHOD  isTextModeEnabled()                 INLINE  Qt_QIODevice_isTextModeEnabled( ::pPtr )
   METHOD  isWritable()                        INLINE  Qt_QIODevice_isWritable( ::pPtr )
   METHOD  open( nMode )                       INLINE  Qt_QIODevice_open( ::pPtr, nMode )
   METHOD  openMode()                          INLINE  Qt_QIODevice_openMode( ::pPtr )
   METHOD  peek( cData, nMaxSize )             INLINE  Qt_QIODevice_peek( ::pPtr, cData, nMaxSize )
   METHOD  peek_1( nMaxSize )                  INLINE  Qt_QIODevice_peek_1( ::pPtr, nMaxSize )
   METHOD  pos()                               INLINE  Qt_QIODevice_pos( ::pPtr )
   METHOD  putChar( cC )                       INLINE  Qt_QIODevice_putChar( ::pPtr, cC )
   METHOD  read( cData, nMaxSize )             INLINE  Qt_QIODevice_read( ::pPtr, cData, nMaxSize )
   METHOD  read_1( nMaxSize )                  INLINE  Qt_QIODevice_read_1( ::pPtr, nMaxSize )
   METHOD  readAll()                           INLINE  Qt_QIODevice_readAll( ::pPtr )
   METHOD  readLine( cData, nMaxSize )         INLINE  Qt_QIODevice_readLine( ::pPtr, cData, nMaxSize )
   METHOD  readLine_1( nMaxSize )              INLINE  Qt_QIODevice_readLine_1( ::pPtr, nMaxSize )
   METHOD  reset()                             INLINE  Qt_QIODevice_reset( ::pPtr )
   METHOD  seek( nPos )                        INLINE  Qt_QIODevice_seek( ::pPtr, nPos )
   METHOD  setTextModeEnabled( lEnabled )      INLINE  Qt_QIODevice_setTextModeEnabled( ::pPtr, lEnabled )
   METHOD  size()                              INLINE  Qt_QIODevice_size( ::pPtr )
   METHOD  ungetChar( cC )                     INLINE  Qt_QIODevice_ungetChar( ::pPtr, cC )
   METHOD  waitForBytesWritten( nMsecs )       INLINE  Qt_QIODevice_waitForBytesWritten( ::pPtr, nMsecs )
   METHOD  waitForReadyRead( nMsecs )          INLINE  Qt_QIODevice_waitForReadyRead( ::pPtr, nMsecs )
   METHOD  write( pData, nMaxSize )            INLINE  Qt_QIODevice_write( ::pPtr, pData, nMaxSize )
   METHOD  write_1( pData )                    INLINE  Qt_QIODevice_write_1( ::pPtr, pData )
   METHOD  write_2( pByteArray )               INLINE  Qt_QIODevice_write_2( ::pPtr, pByteArray )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QIODevice

   ::pParent := pParent

   ::pPtr := Qt_QIODevice( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QIODevice

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
