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


FUNCTION QHttp( ... )
   RETURN HB_QHttp():new( ... )

FUNCTION QHttpFromPointer( ... )
   RETURN HB_QHttp():fromPointer( ... )


CREATE CLASS QHttp INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QHttp

   METHOD  new( ... )

   METHOD  bytesAvailable                // (  )                                               -> nQint64
   METHOD  clearPendingRequests          // (  )                                               -> NIL
   METHOD  close                         // (  )                                               -> nInt
   METHOD  currentDestinationDevice      // (  )                                               -> oQIODevice
   METHOD  currentId                     // (  )                                               -> nInt
   METHOD  currentRequest                // (  )                                               -> oQHttpRequestHeader
   METHOD  currentSourceDevice           // (  )                                               -> oQIODevice
   METHOD  error                         // (  )                                               -> nError
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  get                           // ( cPath, oQIODevice )                              -> nInt
   METHOD  hasPendingRequests            // (  )                                               -> lBool
   METHOD  head                          // ( cPath )                                          -> nInt
   METHOD  lastResponse                  // (  )                                               -> oQHttpResponseHeader
   METHOD  post                          // ( cPath, oQIODevice, oQIODevice )                  -> nInt
                                         // ( cPath, oQByteArray, oQIODevice )                 -> nInt
   METHOD  readAll                       // (  )                                               -> oQByteArray
   METHOD  request                       // ( oQHttpRequestHeader, oQIODevice, oQIODevice )    -> nInt
                                         // ( oQHttpRequestHeader, oQByteArray, oQIODevice )   -> nInt
   METHOD  setHost                       // ( cHostName, nPort )                               -> nInt
                                         // ( cHostName, nMode, nPort )                        -> nInt
   METHOD  setProxy                      // ( cHost, nPort, cUsername, cPassword )             -> nInt
   METHOD  setUser                       // ( cUserName, cPassword )                           -> nInt
   METHOD  state                         // (  )                                               -> nState
   METHOD  abort                         // (  )                                               -> NIL

   ENDCLASS


METHOD QHttp:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHttp( ... )
   RETURN Self


METHOD QHttp:bytesAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_bytesAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:clearPendingRequests( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_clearPendingRequests( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:currentDestinationDevice( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QHttp_currentDestinationDevice( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:currentId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_currentId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:currentRequest( ... )
   SWITCH PCount()
   CASE 0
      RETURN QHttpRequestHeaderFromPointer( Qt_QHttp_currentRequest( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:currentSourceDevice( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QHttp_currentSourceDevice( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_error( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:get( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QHttp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:hasPendingRequests( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_hasPendingRequests( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:head( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttp_head( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:lastResponse( ... )
   SWITCH PCount()
   CASE 0
      RETURN QHttpResponseHeaderFromPointer( Qt_QHttp_lastResponse( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:post( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) ) + __objGetClsName( hb_pvalue( 3 ) )
         CASE "QIODEVICEQIODEVICE"
            RETURN Qt_QHttp_post( ::pPtr, ... )
         CASE "QBYTEARRAYQIODEVICE"
            RETURN Qt_QHttp_post_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) )
         CASE "QIODEVICE"
            RETURN Qt_QHttp_post( ::pPtr, ... )
         CASE "QBYTEARRAY"
            RETURN Qt_QHttp_post_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:readAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QHttp_readAll( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:request( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QHTTPREQUESTHEADERQIODEVICE"
            RETURN Qt_QHttp_request( ::pPtr, ... )
         CASE "QHTTPREQUESTHEADERQBYTEARRAY"
            RETURN Qt_QHttp_request_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) ) + __objGetClsName( hb_pvalue( 2 ) )
         CASE "QHTTPREQUESTHEADERQIODEVICE"
            RETURN Qt_QHttp_request( ::pPtr, ... )
         CASE "QHTTPREQUESTHEADERQBYTEARRAY"
            RETURN Qt_QHttp_request_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QHttp_request( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:setHost( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QHttp_setHost_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHttp_setHost( ::pPtr, ... )
         // RETURN Qt_QHttp_setHost_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttp_setHost( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:setProxy( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN Qt_QHttp_setProxy( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QHttp_setProxy( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHttp_setProxy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:setUser( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QHttp_setUser( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttp_setUser( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttp:abort( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttp_abort( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

