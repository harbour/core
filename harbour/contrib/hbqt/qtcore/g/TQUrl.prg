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


FUNCTION QUrl( ... )
   RETURN HB_QUrl():new( ... )


CREATE CLASS QUrl INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QUrl

   METHOD  new( ... )

   METHOD  addEncodedQueryItem( pKey, pValue )
   METHOD  addQueryItem( cKey, cValue )
   METHOD  allEncodedQueryItemValues( pKey )
   METHOD  allQueryItemValues( cKey )
   METHOD  authority()
   METHOD  clear()
   METHOD  encodedFragment()
   METHOD  encodedHost()
   METHOD  encodedPassword()
   METHOD  encodedPath()
   METHOD  encodedQuery()
   METHOD  encodedQueryItemValue( pKey )
   METHOD  encodedUserName()
   METHOD  errorString()
   METHOD  fragment()
   METHOD  hasEncodedQueryItem( pKey )
   METHOD  hasFragment()
   METHOD  hasQuery()
   METHOD  hasQueryItem( cKey )
   METHOD  host()
   METHOD  isEmpty()
   METHOD  isParentOf( pChildUrl )
   METHOD  isRelative()
   METHOD  isValid()
   METHOD  password()
   METHOD  path()
   METHOD  port()
   METHOD  port_1( nDefaultPort )
   METHOD  queryItemValue( cKey )
   METHOD  queryPairDelimiter()
   METHOD  queryValueDelimiter()
   METHOD  removeAllEncodedQueryItems( pKey )
   METHOD  removeAllQueryItems( cKey )
   METHOD  removeEncodedQueryItem( pKey )
   METHOD  removeQueryItem( cKey )
   METHOD  resolved( pRelative )
   METHOD  scheme()
   METHOD  setAuthority( cAuthority )
   METHOD  setEncodedFragment( pFragment )
   METHOD  setEncodedHost( pHost )
   METHOD  setEncodedPassword( pPassword )
   METHOD  setEncodedPath( pPath )
   METHOD  setEncodedQuery( pQuery )
   METHOD  setEncodedUrl( pEncodedUrl )
   METHOD  setEncodedUrl_1( pEncodedUrl, nParsingMode )
   METHOD  setEncodedUserName( pUserName )
   METHOD  setFragment( cFragment )
   METHOD  setHost( cHost )
   METHOD  setPassword( cPassword )
   METHOD  setPath( cPath )
   METHOD  setPort( nPort )
   METHOD  setScheme( cScheme )
   METHOD  setUrl( cUrl )
   METHOD  setUrl_1( cUrl, nParsingMode )
   METHOD  setUserInfo( cUserInfo )
   METHOD  setUserName( cUserName )
   METHOD  toEncoded( nOptions )
   METHOD  toLocalFile()
   METHOD  toString( nOptions )
   METHOD  userInfo()
   METHOD  userName()
   METHOD  fromAce( pDomain )
   METHOD  fromEncoded( pInput )
   METHOD  fromEncoded_1( pInput, nParsingMode )
   METHOD  fromLocalFile( cLocalFile )
   METHOD  fromPercentEncoding( pInput )
   METHOD  idnWhitelist()
   METHOD  setIdnWhitelist( pList )
   METHOD  toAce( cDomain )
   METHOD  toPercentEncoding( cInput, pExclude, pInclude )

   ENDCLASS


METHOD QUrl:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QUrl( ... )
   RETURN Self


METHOD QUrl:addEncodedQueryItem( pKey, pValue )
   RETURN Qt_QUrl_addEncodedQueryItem( ::pPtr, hbqt_ptr( pKey ), hbqt_ptr( pValue ) )


METHOD QUrl:addQueryItem( cKey, cValue )
   RETURN Qt_QUrl_addQueryItem( ::pPtr, cKey, cValue )


METHOD QUrl:allEncodedQueryItemValues( pKey )
   RETURN Qt_QUrl_allEncodedQueryItemValues( ::pPtr, hbqt_ptr( pKey ) )


METHOD QUrl:allQueryItemValues( cKey )
   RETURN Qt_QUrl_allQueryItemValues( ::pPtr, cKey )


METHOD QUrl:authority()
   RETURN Qt_QUrl_authority( ::pPtr )


METHOD QUrl:clear()
   RETURN Qt_QUrl_clear( ::pPtr )


METHOD QUrl:encodedFragment()
   RETURN Qt_QUrl_encodedFragment( ::pPtr )


METHOD QUrl:encodedHost()
   RETURN Qt_QUrl_encodedHost( ::pPtr )


METHOD QUrl:encodedPassword()
   RETURN Qt_QUrl_encodedPassword( ::pPtr )


METHOD QUrl:encodedPath()
   RETURN Qt_QUrl_encodedPath( ::pPtr )


METHOD QUrl:encodedQuery()
   RETURN Qt_QUrl_encodedQuery( ::pPtr )


METHOD QUrl:encodedQueryItemValue( pKey )
   RETURN Qt_QUrl_encodedQueryItemValue( ::pPtr, hbqt_ptr( pKey ) )


METHOD QUrl:encodedUserName()
   RETURN Qt_QUrl_encodedUserName( ::pPtr )


METHOD QUrl:errorString()
   RETURN Qt_QUrl_errorString( ::pPtr )


METHOD QUrl:fragment()
   RETURN Qt_QUrl_fragment( ::pPtr )


METHOD QUrl:hasEncodedQueryItem( pKey )
   RETURN Qt_QUrl_hasEncodedQueryItem( ::pPtr, hbqt_ptr( pKey ) )


METHOD QUrl:hasFragment()
   RETURN Qt_QUrl_hasFragment( ::pPtr )


METHOD QUrl:hasQuery()
   RETURN Qt_QUrl_hasQuery( ::pPtr )


METHOD QUrl:hasQueryItem( cKey )
   RETURN Qt_QUrl_hasQueryItem( ::pPtr, cKey )


METHOD QUrl:host()
   RETURN Qt_QUrl_host( ::pPtr )


METHOD QUrl:isEmpty()
   RETURN Qt_QUrl_isEmpty( ::pPtr )


METHOD QUrl:isParentOf( pChildUrl )
   RETURN Qt_QUrl_isParentOf( ::pPtr, hbqt_ptr( pChildUrl ) )


METHOD QUrl:isRelative()
   RETURN Qt_QUrl_isRelative( ::pPtr )


METHOD QUrl:isValid()
   RETURN Qt_QUrl_isValid( ::pPtr )


METHOD QUrl:password()
   RETURN Qt_QUrl_password( ::pPtr )


METHOD QUrl:path()
   RETURN Qt_QUrl_path( ::pPtr )


METHOD QUrl:port()
   RETURN Qt_QUrl_port( ::pPtr )


METHOD QUrl:port_1( nDefaultPort )
   RETURN Qt_QUrl_port_1( ::pPtr, nDefaultPort )


METHOD QUrl:queryItemValue( cKey )
   RETURN Qt_QUrl_queryItemValue( ::pPtr, cKey )


METHOD QUrl:queryPairDelimiter()
   RETURN Qt_QUrl_queryPairDelimiter( ::pPtr )


METHOD QUrl:queryValueDelimiter()
   RETURN Qt_QUrl_queryValueDelimiter( ::pPtr )


METHOD QUrl:removeAllEncodedQueryItems( pKey )
   RETURN Qt_QUrl_removeAllEncodedQueryItems( ::pPtr, hbqt_ptr( pKey ) )


METHOD QUrl:removeAllQueryItems( cKey )
   RETURN Qt_QUrl_removeAllQueryItems( ::pPtr, cKey )


METHOD QUrl:removeEncodedQueryItem( pKey )
   RETURN Qt_QUrl_removeEncodedQueryItem( ::pPtr, hbqt_ptr( pKey ) )


METHOD QUrl:removeQueryItem( cKey )
   RETURN Qt_QUrl_removeQueryItem( ::pPtr, cKey )


METHOD QUrl:resolved( pRelative )
   RETURN Qt_QUrl_resolved( ::pPtr, hbqt_ptr( pRelative ) )


METHOD QUrl:scheme()
   RETURN Qt_QUrl_scheme( ::pPtr )


METHOD QUrl:setAuthority( cAuthority )
   RETURN Qt_QUrl_setAuthority( ::pPtr, cAuthority )


METHOD QUrl:setEncodedFragment( pFragment )
   RETURN Qt_QUrl_setEncodedFragment( ::pPtr, hbqt_ptr( pFragment ) )


METHOD QUrl:setEncodedHost( pHost )
   RETURN Qt_QUrl_setEncodedHost( ::pPtr, hbqt_ptr( pHost ) )


METHOD QUrl:setEncodedPassword( pPassword )
   RETURN Qt_QUrl_setEncodedPassword( ::pPtr, hbqt_ptr( pPassword ) )


METHOD QUrl:setEncodedPath( pPath )
   RETURN Qt_QUrl_setEncodedPath( ::pPtr, hbqt_ptr( pPath ) )


METHOD QUrl:setEncodedQuery( pQuery )
   RETURN Qt_QUrl_setEncodedQuery( ::pPtr, hbqt_ptr( pQuery ) )


METHOD QUrl:setEncodedUrl( pEncodedUrl )
   RETURN Qt_QUrl_setEncodedUrl( ::pPtr, hbqt_ptr( pEncodedUrl ) )


METHOD QUrl:setEncodedUrl_1( pEncodedUrl, nParsingMode )
   RETURN Qt_QUrl_setEncodedUrl_1( ::pPtr, hbqt_ptr( pEncodedUrl ), nParsingMode )


METHOD QUrl:setEncodedUserName( pUserName )
   RETURN Qt_QUrl_setEncodedUserName( ::pPtr, hbqt_ptr( pUserName ) )


METHOD QUrl:setFragment( cFragment )
   RETURN Qt_QUrl_setFragment( ::pPtr, cFragment )


METHOD QUrl:setHost( cHost )
   RETURN Qt_QUrl_setHost( ::pPtr, cHost )


METHOD QUrl:setPassword( cPassword )
   RETURN Qt_QUrl_setPassword( ::pPtr, cPassword )


METHOD QUrl:setPath( cPath )
   RETURN Qt_QUrl_setPath( ::pPtr, cPath )


METHOD QUrl:setPort( nPort )
   RETURN Qt_QUrl_setPort( ::pPtr, nPort )


METHOD QUrl:setScheme( cScheme )
   RETURN Qt_QUrl_setScheme( ::pPtr, cScheme )


METHOD QUrl:setUrl( cUrl )
   RETURN Qt_QUrl_setUrl( ::pPtr, cUrl )


METHOD QUrl:setUrl_1( cUrl, nParsingMode )
   RETURN Qt_QUrl_setUrl_1( ::pPtr, cUrl, nParsingMode )


METHOD QUrl:setUserInfo( cUserInfo )
   RETURN Qt_QUrl_setUserInfo( ::pPtr, cUserInfo )


METHOD QUrl:setUserName( cUserName )
   RETURN Qt_QUrl_setUserName( ::pPtr, cUserName )


METHOD QUrl:toEncoded( nOptions )
   RETURN Qt_QUrl_toEncoded( ::pPtr, nOptions )


METHOD QUrl:toLocalFile()
   RETURN Qt_QUrl_toLocalFile( ::pPtr )


METHOD QUrl:toString( nOptions )
   RETURN Qt_QUrl_toString( ::pPtr, nOptions )


METHOD QUrl:userInfo()
   RETURN Qt_QUrl_userInfo( ::pPtr )


METHOD QUrl:userName()
   RETURN Qt_QUrl_userName( ::pPtr )


METHOD QUrl:fromAce( pDomain )
   RETURN Qt_QUrl_fromAce( ::pPtr, hbqt_ptr( pDomain ) )


METHOD QUrl:fromEncoded( pInput )
   RETURN Qt_QUrl_fromEncoded( ::pPtr, hbqt_ptr( pInput ) )


METHOD QUrl:fromEncoded_1( pInput, nParsingMode )
   RETURN Qt_QUrl_fromEncoded_1( ::pPtr, hbqt_ptr( pInput ), nParsingMode )


METHOD QUrl:fromLocalFile( cLocalFile )
   RETURN Qt_QUrl_fromLocalFile( ::pPtr, cLocalFile )


METHOD QUrl:fromPercentEncoding( pInput )
   RETURN Qt_QUrl_fromPercentEncoding( ::pPtr, hbqt_ptr( pInput ) )


METHOD QUrl:idnWhitelist()
   RETURN Qt_QUrl_idnWhitelist( ::pPtr )


METHOD QUrl:setIdnWhitelist( pList )
   RETURN Qt_QUrl_setIdnWhitelist( ::pPtr, hbqt_ptr( pList ) )


METHOD QUrl:toAce( cDomain )
   RETURN Qt_QUrl_toAce( ::pPtr, cDomain )


METHOD QUrl:toPercentEncoding( cInput, pExclude, pInclude )
   RETURN Qt_QUrl_toPercentEncoding( ::pPtr, cInput, hbqt_ptr( pExclude ), hbqt_ptr( pInclude ) )

