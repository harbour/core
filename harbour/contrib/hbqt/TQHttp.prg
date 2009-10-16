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


CREATE CLASS QHttp INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  bytesAvailable()                    INLINE  Qt_QHttp_bytesAvailable( ::pPtr )
   METHOD  clearPendingRequests()              INLINE  Qt_QHttp_clearPendingRequests( ::pPtr )
   METHOD  close()                             INLINE  Qt_QHttp_close( ::pPtr )
   METHOD  currentDestinationDevice()          INLINE  Qt_QHttp_currentDestinationDevice( ::pPtr )
   METHOD  currentId()                         INLINE  Qt_QHttp_currentId( ::pPtr )
   METHOD  currentRequest()                    INLINE  Qt_QHttp_currentRequest( ::pPtr )
   METHOD  currentSourceDevice()               INLINE  Qt_QHttp_currentSourceDevice( ::pPtr )
   METHOD  error()                             INLINE  Qt_QHttp_error( ::pPtr )
   METHOD  errorString()                       INLINE  Qt_QHttp_errorString( ::pPtr )
   METHOD  get( cPath, pTo )                   INLINE  Qt_QHttp_get( ::pPtr, cPath, pTo )
   METHOD  hasPendingRequests()                INLINE  Qt_QHttp_hasPendingRequests( ::pPtr )
   METHOD  head( cPath )                       INLINE  Qt_QHttp_head( ::pPtr, cPath )
   METHOD  lastResponse()                      INLINE  Qt_QHttp_lastResponse( ::pPtr )
   METHOD  post( cPath, pData, pTo )           INLINE  Qt_QHttp_post( ::pPtr, cPath, pData, pTo )
   METHOD  post_1( cPath, pData, pTo )         INLINE  Qt_QHttp_post_1( ::pPtr, cPath, pData, pTo )
   METHOD  readAll()                           INLINE  Qt_QHttp_readAll( ::pPtr )
   METHOD  request( pHeader, pData, pTo )      INLINE  Qt_QHttp_request( ::pPtr, pHeader, pData, pTo )
   METHOD  request_1( pHeader, pData, pTo )    INLINE  Qt_QHttp_request_1( ::pPtr, pHeader, pData, pTo )
   METHOD  setHost( cHostName, nPort )         INLINE  Qt_QHttp_setHost( ::pPtr, cHostName, nPort )
   METHOD  setHost_1( cHostName, nMode, nPort )  INLINE  Qt_QHttp_setHost_1( ::pPtr, cHostName, nMode, nPort )
   METHOD  setProxy( cHost, nPort, cUsername, cPassword )  INLINE  Qt_QHttp_setProxy( ::pPtr, cHost, nPort, cUsername, cPassword )
   METHOD  setProxy_1( pProxy )                INLINE  Qt_QHttp_setProxy_1( ::pPtr, pProxy )
   METHOD  setSocket( pSocket )                INLINE  Qt_QHttp_setSocket( ::pPtr, pSocket )
   METHOD  setUser( cUserName, cPassword )     INLINE  Qt_QHttp_setUser( ::pPtr, cUserName, cPassword )
   METHOD  state()                             INLINE  Qt_QHttp_state( ::pPtr )
   METHOD  abort()                             INLINE  Qt_QHttp_abort( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QHttp

   ::pParent := pParent

   ::pPtr := Qt_QHttp( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QHttp

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
