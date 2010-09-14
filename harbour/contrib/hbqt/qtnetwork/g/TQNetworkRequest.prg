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


FUNCTION QNetworkRequest( ... )
   RETURN HB_QNetworkRequest():new( ... )


CREATE CLASS QNetworkRequest INHERIT HbQtObjectHandler FUNCTION HB_QNetworkRequest

   METHOD  new( ... )

   METHOD  attribute( nCode, pDefaultValue )
   METHOD  hasRawHeader( pHeaderName )
   METHOD  header( nHeader )
   METHOD  rawHeader( pHeaderName )
   METHOD  rawHeaderList()
   METHOD  setAttribute( nCode, pValue )
   METHOD  setHeader( nHeader, pValue )
   METHOD  setRawHeader( pHeaderName, pHeaderValue )
   METHOD  setUrl( pUrl )
   METHOD  url()

   ENDCLASS


METHOD QNetworkRequest:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QNetworkRequest( ... )
   RETURN Self


METHOD QNetworkRequest:attribute( nCode, pDefaultValue )
   RETURN Qt_QNetworkRequest_attribute( ::pPtr, nCode, hbqt_ptr( pDefaultValue ) )


METHOD QNetworkRequest:hasRawHeader( pHeaderName )
   RETURN Qt_QNetworkRequest_hasRawHeader( ::pPtr, hbqt_ptr( pHeaderName ) )


METHOD QNetworkRequest:header( nHeader )
   RETURN Qt_QNetworkRequest_header( ::pPtr, nHeader )


METHOD QNetworkRequest:rawHeader( pHeaderName )
   RETURN Qt_QNetworkRequest_rawHeader( ::pPtr, hbqt_ptr( pHeaderName ) )


METHOD QNetworkRequest:rawHeaderList()
   RETURN Qt_QNetworkRequest_rawHeaderList( ::pPtr )


METHOD QNetworkRequest:setAttribute( nCode, pValue )
   RETURN Qt_QNetworkRequest_setAttribute( ::pPtr, nCode, hbqt_ptr( pValue ) )


METHOD QNetworkRequest:setHeader( nHeader, pValue )
   RETURN Qt_QNetworkRequest_setHeader( ::pPtr, nHeader, hbqt_ptr( pValue ) )


METHOD QNetworkRequest:setRawHeader( pHeaderName, pHeaderValue )
   RETURN Qt_QNetworkRequest_setRawHeader( ::pPtr, hbqt_ptr( pHeaderName ), hbqt_ptr( pHeaderValue ) )


METHOD QNetworkRequest:setUrl( pUrl )
   RETURN Qt_QNetworkRequest_setUrl( ::pPtr, hbqt_ptr( pUrl ) )


METHOD QNetworkRequest:url()
   RETURN Qt_QNetworkRequest_url( ::pPtr )

