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
   RETURN HB_QIODevice():from( Qt_QHttp_currentDestinationDevice( ::pPtr ) )


METHOD QHttp:currentId()
   RETURN Qt_QHttp_currentId( ::pPtr )


METHOD QHttp:currentRequest()
   RETURN HB_QHttpRequestHeader():from( Qt_QHttp_currentRequest( ::pPtr ) )


METHOD QHttp:currentSourceDevice()
   RETURN HB_QIODevice():from( Qt_QHttp_currentSourceDevice( ::pPtr ) )


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
   RETURN HB_QHttpResponseHeader():from( Qt_QHttp_lastResponse( ::pPtr ) )


METHOD QHttp:post( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QSTRING"
            RETURN Qt_QHttp_post( ::pPtr, ... )
         CASE "QSTRING"
            RETURN Qt_QHttp_post_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QSTRING"
            RETURN Qt_QHttp_post( ::pPtr, ... )
         CASE "QSTRING"
            RETURN Qt_QHttp_post_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QHttp:readAll()
   RETURN HB_QByteArray():from( Qt_QHttp_readAll( ::pPtr ) )


METHOD QHttp:request( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QHTTPREQUESTHEADER"
            RETURN Qt_QHttp_request( ::pPtr, ... )
         CASE "QHTTPREQUESTHEADER"
            RETURN Qt_QHttp_request_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QHttp_request_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QHttp_request( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QSTRING"
            RETURN Qt_QHttp_setHost( ::pPtr, ... )
         CASE "QSTRING"
            RETURN Qt_QHttp_setHost_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttp_setHost( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QHttp:setProxy( cHost, nPort, cUsername, cPassword )
   RETURN Qt_QHttp_setProxy( ::pPtr, cHost, nPort, cUsername, cPassword )


METHOD QHttp:setUser( cUserName, cPassword )
   RETURN Qt_QHttp_setUser( ::pPtr, cUserName, cPassword )


METHOD QHttp:state()
   RETURN Qt_QHttp_state( ::pPtr )


METHOD QHttp:abort()
   RETURN Qt_QHttp_abort( ::pPtr )

