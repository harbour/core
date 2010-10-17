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


FUNCTION QIODevice( ... )
   RETURN HB_QIODevice():new( ... )

FUNCTION QIODeviceFrom( ... )
   RETURN HB_QIODevice():from( ... )

FUNCTION QIODeviceFromPointer( ... )
   RETURN HB_QIODevice():fromPointer( ... )


CREATE CLASS QIODevice INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QIODevice

   METHOD  new( ... )

   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  bytesAvailable                // (  )                                               -> nQint64
   METHOD  bytesToWrite                  // (  )                                               -> nQint64
   METHOD  canReadLine                   // (  )                                               -> lBool
   METHOD  close                         // (  )                                               -> NIL
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  getChar                       // ( cC )                                             -> lBool
   METHOD  isOpen                        // (  )                                               -> lBool
   METHOD  isReadable                    // (  )                                               -> lBool
   METHOD  isSequential                  // (  )                                               -> lBool
   METHOD  isTextModeEnabled             // (  )                                               -> lBool
   METHOD  isWritable                    // (  )                                               -> lBool
   METHOD  open                          // ( nMode )                                          -> lBool
   METHOD  openMode                      // (  )                                               -> nOpenMode
   METHOD  peek                          // ( cData, nMaxSize )                                -> nQint64
                                         // ( nMaxSize )                                       -> oQByteArray
   METHOD  pos                           // (  )                                               -> nQint64
   METHOD  putChar                       // ( nC )                                             -> lBool
   METHOD  read                          // ( cData, nMaxSize )                                -> nQint64
                                         // ( nMaxSize )                                       -> oQByteArray
   METHOD  readAll                       // (  )                                               -> oQByteArray
   METHOD  readLine                      // ( cData, nMaxSize )                                -> nQint64
                                         // ( nMaxSize )                                       -> oQByteArray
   METHOD  reset                         // (  )                                               -> lBool
   METHOD  seek                          // ( nPos )                                           -> lBool
   METHOD  setTextModeEnabled            // ( lEnabled )                                       -> NIL
   METHOD  size                          // (  )                                               -> nQint64
   METHOD  ungetChar                     // ( nC )                                             -> NIL
   METHOD  waitForBytesWritten           // ( nMsecs )                                         -> lBool
   METHOD  waitForReadyRead              // ( nMsecs )                                         -> lBool
   METHOD  write                         // ( cData, nMaxSize )                                -> nQint64
                                         // ( cData )                                          -> nQint64
                                         // ( oQByteArray )                                    -> nQint64

   ENDCLASS


METHOD QIODevice:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QIODevice( ... )
   RETURN Self


METHOD QIODevice:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:bytesAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_bytesAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:bytesToWrite( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_bytesToWrite( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:canReadLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_canReadLine( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:getChar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_getChar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isOpen( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isOpen( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isReadable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isReadable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isSequential( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isSequential( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isTextModeEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isTextModeEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isWritable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isWritable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:open( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:openMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_openMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:peek( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_peek( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QIODevice_peek_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_pos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:putChar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_putChar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:read( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_read( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QIODevice_read_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:readAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QIODevice_readAll( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:readLine( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_readLine( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QIODevice_readLine_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QIODevice_readLine_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:seek( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_seek( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:setTextModeEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_setTextModeEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:ungetChar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_ungetChar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:waitForBytesWritten( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_waitForBytesWritten( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:waitForReadyRead( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_waitForReadyRead( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:write( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_write( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_write_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_write_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

