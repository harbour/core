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


FUNCTION QIODevice( ... )
   RETURN HB_QIODevice():new( ... )


CREATE CLASS QIODevice INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QIODevice

   METHOD  new( ... )

   METHOD  atEnd()
   METHOD  bytesAvailable()
   METHOD  bytesToWrite()
   METHOD  canReadLine()
   METHOD  close()
   METHOD  errorString()
   METHOD  getChar( cC )
   METHOD  isOpen()
   METHOD  isReadable()
   METHOD  isSequential()
   METHOD  isTextModeEnabled()
   METHOD  isWritable()
   METHOD  open( nMode )
   METHOD  openMode()
   METHOD  peek( cData, nMaxSize )
   METHOD  peek_1( nMaxSize )
   METHOD  pos()
   METHOD  putChar( cC )
   METHOD  read( cData, nMaxSize )
   METHOD  read_1( nMaxSize )
   METHOD  readAll()
   METHOD  readLine( cData, nMaxSize )
   METHOD  readLine_1( nMaxSize )
   METHOD  reset()
   METHOD  seek( nPos )
   METHOD  setTextModeEnabled( lEnabled )
   METHOD  size()
   METHOD  ungetChar( cC )
   METHOD  waitForBytesWritten( nMsecs )
   METHOD  waitForReadyRead( nMsecs )
   METHOD  write( pData, nMaxSize )
   METHOD  write_1( pData )
   METHOD  write_2( pByteArray )

   ENDCLASS


METHOD QIODevice:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QIODevice( ... )
   RETURN Self


METHOD QIODevice:atEnd()
   RETURN Qt_QIODevice_atEnd( ::pPtr )


METHOD QIODevice:bytesAvailable()
   RETURN Qt_QIODevice_bytesAvailable( ::pPtr )


METHOD QIODevice:bytesToWrite()
   RETURN Qt_QIODevice_bytesToWrite( ::pPtr )


METHOD QIODevice:canReadLine()
   RETURN Qt_QIODevice_canReadLine( ::pPtr )


METHOD QIODevice:close()
   RETURN Qt_QIODevice_close( ::pPtr )


METHOD QIODevice:errorString()
   RETURN Qt_QIODevice_errorString( ::pPtr )


METHOD QIODevice:getChar( cC )
   RETURN Qt_QIODevice_getChar( ::pPtr, cC )


METHOD QIODevice:isOpen()
   RETURN Qt_QIODevice_isOpen( ::pPtr )


METHOD QIODevice:isReadable()
   RETURN Qt_QIODevice_isReadable( ::pPtr )


METHOD QIODevice:isSequential()
   RETURN Qt_QIODevice_isSequential( ::pPtr )


METHOD QIODevice:isTextModeEnabled()
   RETURN Qt_QIODevice_isTextModeEnabled( ::pPtr )


METHOD QIODevice:isWritable()
   RETURN Qt_QIODevice_isWritable( ::pPtr )


METHOD QIODevice:open( nMode )
   RETURN Qt_QIODevice_open( ::pPtr, nMode )


METHOD QIODevice:openMode()
   RETURN Qt_QIODevice_openMode( ::pPtr )


METHOD QIODevice:peek( cData, nMaxSize )
   RETURN Qt_QIODevice_peek( ::pPtr, cData, nMaxSize )


METHOD QIODevice:peek_1( nMaxSize )
   RETURN Qt_QIODevice_peek_1( ::pPtr, nMaxSize )


METHOD QIODevice:pos()
   RETURN Qt_QIODevice_pos( ::pPtr )


METHOD QIODevice:putChar( cC )
   RETURN Qt_QIODevice_putChar( ::pPtr, cC )


METHOD QIODevice:read( cData, nMaxSize )
   RETURN Qt_QIODevice_read( ::pPtr, cData, nMaxSize )


METHOD QIODevice:read_1( nMaxSize )
   RETURN Qt_QIODevice_read_1( ::pPtr, nMaxSize )


METHOD QIODevice:readAll()
   RETURN Qt_QIODevice_readAll( ::pPtr )


METHOD QIODevice:readLine( cData, nMaxSize )
   RETURN Qt_QIODevice_readLine( ::pPtr, cData, nMaxSize )


METHOD QIODevice:readLine_1( nMaxSize )
   RETURN Qt_QIODevice_readLine_1( ::pPtr, nMaxSize )


METHOD QIODevice:reset()
   RETURN Qt_QIODevice_reset( ::pPtr )


METHOD QIODevice:seek( nPos )
   RETURN Qt_QIODevice_seek( ::pPtr, nPos )


METHOD QIODevice:setTextModeEnabled( lEnabled )
   RETURN Qt_QIODevice_setTextModeEnabled( ::pPtr, lEnabled )


METHOD QIODevice:size()
   RETURN Qt_QIODevice_size( ::pPtr )


METHOD QIODevice:ungetChar( cC )
   RETURN Qt_QIODevice_ungetChar( ::pPtr, cC )


METHOD QIODevice:waitForBytesWritten( nMsecs )
   RETURN Qt_QIODevice_waitForBytesWritten( ::pPtr, nMsecs )


METHOD QIODevice:waitForReadyRead( nMsecs )
   RETURN Qt_QIODevice_waitForReadyRead( ::pPtr, nMsecs )


METHOD QIODevice:write( pData, nMaxSize )
   RETURN Qt_QIODevice_write( ::pPtr, hbqt_ptr( pData ), nMaxSize )


METHOD QIODevice:write_1( pData )
   RETURN Qt_QIODevice_write_1( ::pPtr, hbqt_ptr( pData ) )


METHOD QIODevice:write_2( pByteArray )
   RETURN Qt_QIODevice_write_2( ::pPtr, hbqt_ptr( pByteArray ) )

