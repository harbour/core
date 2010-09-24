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


FUNCTION QHttp( ... )
   RETURN HB_QHttp():new( ... )


CREATE CLASS QHttp INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QHttp

   METHOD  new( ... )

   METHOD  bytesAvailable()
   METHOD  clearPendingRequests()
   METHOD  close()
   METHOD  currentDestinationDevice()
   METHOD  currentId()
   METHOD  currentRequest()
   METHOD  currentSourceDevice()
   METHOD  error()
   METHOD  errorString()
   METHOD  get( cPath, pTo )
   METHOD  hasPendingRequests()
   METHOD  head( cPath )
   METHOD  lastResponse()
   METHOD  post( ... )
   METHOD  readAll()
   METHOD  request( ... )
   METHOD  setHost( ... )
   METHOD  setProxy( cHost, nPort, cUsername, cPassword )
   METHOD  setUser( cUserName, cPassword )
   METHOD  state()
   METHOD  abort()

   ENDCLASS


METHOD QHttp:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHttp( ... )
   RETURN Self


METHOD QHttp:bytesAvailable()
   RETURN Qt_QHttp_bytesAvailable( ::pPtr )


METHOD QHttp:clearPendingRequests()
   RETURN Qt_QHttp_clearPendingRequests( ::pPtr )


METHOD QHttp:close()
   RETURN Qt_QHttp_close( ::pPtr )


METHOD QHttp:currentDestinationDevice()
   RETURN Qt_QHttp_currentDestinationDevice( ::pPtr )


METHOD QHttp:currentId()
   RETURN Qt_QHttp_currentId( ::pPtr )


METHOD QHttp:currentRequest()
   RETURN Qt_QHttp_currentRequest( ::pPtr )


METHOD QHttp:currentSourceDevice()
   RETURN Qt_QHttp_currentSourceDevice( ::pPtr )


METHOD QHttp:error()
   RETURN Qt_QHttp_error( ::pPtr )


METHOD QHttp:errorString()
   RETURN Qt_QHttp_errorString( ::pPtr )


METHOD QHttp:get( cPath, pTo )
   RETURN Qt_QHttp_get( ::pPtr, cPath, hbqt_ptr( pTo ) )


METHOD QHttp:hasPendingRequests()
   RETURN Qt_QHttp_hasPendingRequests( ::pPtr )


METHOD QHttp:head( cPath )
   RETURN Qt_QHttp_head( ::pPtr, cPath )


METHOD QHttp:lastResponse()
   RETURN Qt_QHttp_lastResponse( ::pPtr )


METHOD QHttp:post( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // int post ( const QString & path, QIODevice * data, QIODevice * to = 0 )
                // C c QString, PO p QIODevice, PO p QIODevice
         RETURN Qt_QHttp_post( ::pPtr, ... )
                // int post ( const QString & path, const QByteArray & data, QIODevice * to = 0 )
                // C c QString, PO p QByteArray, PO p QIODevice
         // RETURN Qt_QHttp_post_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "PO"
                // int post ( const QString & path, QIODevice * data, QIODevice * to = 0 )
                // C c QString, PO p QIODevice, PO p QIODevice
         RETURN Qt_QHttp_post( ::pPtr, ... )
                // int post ( const QString & path, const QByteArray & data, QIODevice * to = 0 )
                // C c QString, PO p QByteArray, PO p QIODevice
         // RETURN Qt_QHttp_post_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QHttp:readAll()
   RETURN Qt_QHttp_readAll( ::pPtr )


METHOD QHttp:request( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // int request ( const QHttpRequestHeader & header, QIODevice * data = 0, QIODevice * to = 0 )
                // PO p QHttpRequestHeader, PO p QIODevice, PO p QIODevice
         RETURN Qt_QHttp_request( ::pPtr, ... )
                // int request ( const QHttpRequestHeader & header, const QByteArray & data, QIODevice * to = 0 )
                // PO p QHttpRequestHeader, PO p QByteArray, PO p QIODevice
         // RETURN Qt_QHttp_request_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // int request ( const QHttpRequestHeader & header, const QByteArray & data, QIODevice * to = 0 )
                // PO p QHttpRequestHeader, PO p QByteArray, PO p QIODevice
         RETURN Qt_QHttp_request_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // int request ( const QHttpRequestHeader & header, QIODevice * data = 0, QIODevice * to = 0 )
                // PO p QHttpRequestHeader, PO p QIODevice, PO p QIODevice
         RETURN Qt_QHttp_request( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QHttp:setHost( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // int setHost ( const QString & hostName, ConnectionMode mode, quint16 port = 0 )
                // C c QString, N n QHttp::ConnectionMode, N n quint16
         RETURN Qt_QHttp_setHost_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // int setHost ( const QString & hostName, quint16 port = 80 )
                // C c QString, N n quint16
         RETURN Qt_QHttp_setHost( ::pPtr, ... )
                // int setHost ( const QString & hostName, ConnectionMode mode, quint16 port = 0 )
                // C c QString, N n QHttp::ConnectionMode, N n quint16
         // RETURN Qt_QHttp_setHost_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // int setHost ( const QString & hostName, quint16 port = 80 )
                // C c QString, N n quint16
         RETURN Qt_QHttp_setHost( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QHttp:setProxy( cHost, nPort, cUsername, cPassword )
   RETURN Qt_QHttp_setProxy( ::pPtr, cHost, nPort, cUsername, cPassword )


METHOD QHttp:setUser( cUserName, cPassword )
   RETURN Qt_QHttp_setUser( ::pPtr, cUserName, cPassword )


METHOD QHttp:state()
   RETURN Qt_QHttp_state( ::pPtr )


METHOD QHttp:abort()
   RETURN Qt_QHttp_abort( ::pPtr )

