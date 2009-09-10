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


CREATE CLASS QUrl INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QUrl_destroy( ::pPtr )

   METHOD  addEncodedQueryItem( pKey, pValue )  INLINE  Qt_QUrl_addEncodedQueryItem( ::pPtr, pKey, pValue )
   METHOD  addQueryItem( cKey, cValue )        INLINE  Qt_QUrl_addQueryItem( ::pPtr, cKey, cValue )
   METHOD  allQueryItemValues( cKey )          INLINE  Qt_QUrl_allQueryItemValues( ::pPtr, cKey )
   METHOD  authority()                         INLINE  Qt_QUrl_authority( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QUrl_clear( ::pPtr )
   METHOD  encodedFragment()                   INLINE  Qt_QUrl_encodedFragment( ::pPtr )
   METHOD  encodedHost()                       INLINE  Qt_QUrl_encodedHost( ::pPtr )
   METHOD  encodedPassword()                   INLINE  Qt_QUrl_encodedPassword( ::pPtr )
   METHOD  encodedPath()                       INLINE  Qt_QUrl_encodedPath( ::pPtr )
   METHOD  encodedQuery()                      INLINE  Qt_QUrl_encodedQuery( ::pPtr )
   METHOD  encodedQueryItemValue( pKey )       INLINE  Qt_QUrl_encodedQueryItemValue( ::pPtr, pKey )
   METHOD  encodedUserName()                   INLINE  Qt_QUrl_encodedUserName( ::pPtr )
   METHOD  errorString()                       INLINE  Qt_QUrl_errorString( ::pPtr )
   METHOD  fragment()                          INLINE  Qt_QUrl_fragment( ::pPtr )
   METHOD  hasEncodedQueryItem( pKey )         INLINE  Qt_QUrl_hasEncodedQueryItem( ::pPtr, pKey )
   METHOD  hasFragment()                       INLINE  Qt_QUrl_hasFragment( ::pPtr )
   METHOD  hasQuery()                          INLINE  Qt_QUrl_hasQuery( ::pPtr )
   METHOD  hasQueryItem( cKey )                INLINE  Qt_QUrl_hasQueryItem( ::pPtr, cKey )
   METHOD  host()                              INLINE  Qt_QUrl_host( ::pPtr )
   METHOD  isEmpty()                           INLINE  Qt_QUrl_isEmpty( ::pPtr )
   METHOD  isParentOf( pChildUrl )             INLINE  Qt_QUrl_isParentOf( ::pPtr, pChildUrl )
   METHOD  isRelative()                        INLINE  Qt_QUrl_isRelative( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QUrl_isValid( ::pPtr )
   METHOD  password()                          INLINE  Qt_QUrl_password( ::pPtr )
   METHOD  path()                              INLINE  Qt_QUrl_path( ::pPtr )
   METHOD  port()                              INLINE  Qt_QUrl_port( ::pPtr )
   METHOD  port_1( nDefaultPort )              INLINE  Qt_QUrl_port_1( ::pPtr, nDefaultPort )
   METHOD  queryItemValue( cKey )              INLINE  Qt_QUrl_queryItemValue( ::pPtr, cKey )
   METHOD  queryPairDelimiter()                INLINE  Qt_QUrl_queryPairDelimiter( ::pPtr )
   METHOD  queryValueDelimiter()               INLINE  Qt_QUrl_queryValueDelimiter( ::pPtr )
   METHOD  removeAllEncodedQueryItems( pKey )  INLINE  Qt_QUrl_removeAllEncodedQueryItems( ::pPtr, pKey )
   METHOD  removeAllQueryItems( cKey )         INLINE  Qt_QUrl_removeAllQueryItems( ::pPtr, cKey )
   METHOD  removeEncodedQueryItem( pKey )      INLINE  Qt_QUrl_removeEncodedQueryItem( ::pPtr, pKey )
   METHOD  removeQueryItem( cKey )             INLINE  Qt_QUrl_removeQueryItem( ::pPtr, cKey )
   METHOD  resolved( pRelative )               INLINE  Qt_QUrl_resolved( ::pPtr, pRelative )
   METHOD  scheme()                            INLINE  Qt_QUrl_scheme( ::pPtr )
   METHOD  setAuthority( cAuthority )          INLINE  Qt_QUrl_setAuthority( ::pPtr, cAuthority )
   METHOD  setEncodedFragment( pFragment )     INLINE  Qt_QUrl_setEncodedFragment( ::pPtr, pFragment )
   METHOD  setEncodedHost( pHost )             INLINE  Qt_QUrl_setEncodedHost( ::pPtr, pHost )
   METHOD  setEncodedPassword( pPassword )     INLINE  Qt_QUrl_setEncodedPassword( ::pPtr, pPassword )
   METHOD  setEncodedPath( pPath )             INLINE  Qt_QUrl_setEncodedPath( ::pPtr, pPath )
   METHOD  setEncodedQuery( pQuery )           INLINE  Qt_QUrl_setEncodedQuery( ::pPtr, pQuery )
   METHOD  setEncodedUrl( pEncodedUrl )        INLINE  Qt_QUrl_setEncodedUrl( ::pPtr, pEncodedUrl )
   METHOD  setEncodedUrl_1( pEncodedUrl, nParsingMode )  INLINE  Qt_QUrl_setEncodedUrl_1( ::pPtr, pEncodedUrl, nParsingMode )
   METHOD  setEncodedUserName( pUserName )     INLINE  Qt_QUrl_setEncodedUserName( ::pPtr, pUserName )
   METHOD  setFragment( cFragment )            INLINE  Qt_QUrl_setFragment( ::pPtr, cFragment )
   METHOD  setHost( cHost )                    INLINE  Qt_QUrl_setHost( ::pPtr, cHost )
   METHOD  setPassword( cPassword )            INLINE  Qt_QUrl_setPassword( ::pPtr, cPassword )
   METHOD  setPath( cPath )                    INLINE  Qt_QUrl_setPath( ::pPtr, cPath )
   METHOD  setPort( nPort )                    INLINE  Qt_QUrl_setPort( ::pPtr, nPort )
   METHOD  setScheme( cScheme )                INLINE  Qt_QUrl_setScheme( ::pPtr, cScheme )
   METHOD  setUrl( cUrl )                      INLINE  Qt_QUrl_setUrl( ::pPtr, cUrl )
   METHOD  setUrl_1( cUrl, nParsingMode )      INLINE  Qt_QUrl_setUrl_1( ::pPtr, cUrl, nParsingMode )
   METHOD  setUserInfo( cUserInfo )            INLINE  Qt_QUrl_setUserInfo( ::pPtr, cUserInfo )
   METHOD  setUserName( cUserName )            INLINE  Qt_QUrl_setUserName( ::pPtr, cUserName )
   METHOD  toEncoded( nOptions )               INLINE  Qt_QUrl_toEncoded( ::pPtr, nOptions )
   METHOD  toLocalFile()                       INLINE  Qt_QUrl_toLocalFile( ::pPtr )
   METHOD  toString( nOptions )                INLINE  Qt_QUrl_toString( ::pPtr, nOptions )
   METHOD  userInfo()                          INLINE  Qt_QUrl_userInfo( ::pPtr )
   METHOD  userName()                          INLINE  Qt_QUrl_userName( ::pPtr )
   METHOD  fromAce( pDomain )                  INLINE  Qt_QUrl_fromAce( ::pPtr, pDomain )
   METHOD  fromEncoded( pInput )               INLINE  Qt_QUrl_fromEncoded( ::pPtr, pInput )
   METHOD  fromEncoded_1( pInput, nParsingMode )  INLINE  Qt_QUrl_fromEncoded_1( ::pPtr, pInput, nParsingMode )
   METHOD  fromLocalFile( cLocalFile )         INLINE  Qt_QUrl_fromLocalFile( ::pPtr, cLocalFile )
   METHOD  fromPercentEncoding( pInput )       INLINE  Qt_QUrl_fromPercentEncoding( ::pPtr, pInput )
   METHOD  idnWhitelist()                      INLINE  Qt_QUrl_idnWhitelist( ::pPtr )
   METHOD  setIdnWhitelist( pList )            INLINE  Qt_QUrl_setIdnWhitelist( ::pPtr, pList )
   METHOD  toAce( cDomain )                    INLINE  Qt_QUrl_toAce( ::pPtr, cDomain )
   METHOD  toPercentEncoding( cInput, pExclude, pInclude )  INLINE  Qt_QUrl_toPercentEncoding( ::pPtr, cInput, pExclude, pInclude )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QUrl

   ::pParent := pParent

   ::pPtr := Qt_QUrl( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QUrl

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
