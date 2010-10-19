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


FUNCTION QUrl( ... )
   RETURN HB_QUrl():new( ... )

FUNCTION QUrlFromPointer( ... )
   RETURN HB_QUrl():fromPointer( ... )


CREATE CLASS QUrl INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QUrl

   METHOD  new( ... )

   METHOD  addEncodedQueryItem           // ( oQByteArray, oQByteArray )                       -> NIL
   METHOD  addQueryItem                  // ( cKey, cValue )                                   -> NIL
   METHOD  allEncodedQueryItemValues     // ( oQByteArray )                                    -> oQList_QByteArray>
   METHOD  allQueryItemValues            // ( cKey )                                           -> oQStringList
   METHOD  authority                     // (  )                                               -> cQString
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  encodedFragment               // (  )                                               -> oQByteArray
   METHOD  encodedHost                   // (  )                                               -> oQByteArray
   METHOD  encodedPassword               // (  )                                               -> oQByteArray
   METHOD  encodedPath                   // (  )                                               -> oQByteArray
   METHOD  encodedQuery                  // (  )                                               -> oQByteArray
   METHOD  encodedQueryItemValue         // ( oQByteArray )                                    -> oQByteArray
   METHOD  encodedUserName               // (  )                                               -> oQByteArray
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  fragment                      // (  )                                               -> cQString
   METHOD  hasEncodedQueryItem           // ( oQByteArray )                                    -> lBool
   METHOD  hasFragment                   // (  )                                               -> lBool
   METHOD  hasQuery                      // (  )                                               -> lBool
   METHOD  hasQueryItem                  // ( cKey )                                           -> lBool
   METHOD  host                          // (  )                                               -> cQString
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isParentOf                    // ( oQUrl )                                          -> lBool
   METHOD  isRelative                    // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  password                      // (  )                                               -> cQString
   METHOD  path                          // (  )                                               -> cQString
   METHOD  port                          // (  )                                               -> nInt
                                         // ( nDefaultPort )                                   -> nInt
   METHOD  queryItemValue                // ( cKey )                                           -> cQString
   METHOD  queryPairDelimiter            // (  )                                               -> cChar
   METHOD  queryValueDelimiter           // (  )                                               -> cChar
   METHOD  removeAllEncodedQueryItems    // ( oQByteArray )                                    -> NIL
   METHOD  removeAllQueryItems           // ( cKey )                                           -> NIL
   METHOD  removeEncodedQueryItem        // ( oQByteArray )                                    -> NIL
   METHOD  removeQueryItem               // ( cKey )                                           -> NIL
   METHOD  resolved                      // ( oQUrl )                                          -> oQUrl
   METHOD  scheme                        // (  )                                               -> cQString
   METHOD  setAuthority                  // ( cAuthority )                                     -> NIL
   METHOD  setEncodedFragment            // ( oQByteArray )                                    -> NIL
   METHOD  setEncodedHost                // ( oQByteArray )                                    -> NIL
   METHOD  setEncodedPassword            // ( oQByteArray )                                    -> NIL
   METHOD  setEncodedPath                // ( oQByteArray )                                    -> NIL
   METHOD  setEncodedQuery               // ( oQByteArray )                                    -> NIL
   METHOD  setEncodedUrl                 // ( oQByteArray )                                    -> NIL
                                         // ( oQByteArray, nParsingMode )                      -> NIL
   METHOD  setEncodedUserName            // ( oQByteArray )                                    -> NIL
   METHOD  setFragment                   // ( cFragment )                                      -> NIL
   METHOD  setHost                       // ( cHost )                                          -> NIL
   METHOD  setPassword                   // ( cPassword )                                      -> NIL
   METHOD  setPath                       // ( cPath )                                          -> NIL
   METHOD  setPort                       // ( nPort )                                          -> NIL
   METHOD  setScheme                     // ( cScheme )                                        -> NIL
   METHOD  setUrl                        // ( cUrl )                                           -> NIL
                                         // ( cUrl, nParsingMode )                             -> NIL
   METHOD  setUserInfo                   // ( cUserInfo )                                      -> NIL
   METHOD  setUserName                   // ( cUserName )                                      -> NIL
   METHOD  toEncoded                     // ( nOptions )                                       -> oQByteArray
   METHOD  toLocalFile                   // (  )                                               -> cQString
   METHOD  toString                      // ( nOptions )                                       -> cQString
   METHOD  userInfo                      // (  )                                               -> cQString
   METHOD  userName                      // (  )                                               -> cQString
   METHOD  fromAce                       // ( oQByteArray )                                    -> cQString
   METHOD  fromEncoded                   // ( oQByteArray )                                    -> oQUrl
                                         // ( oQByteArray, nParsingMode )                      -> oQUrl
   METHOD  fromLocalFile                 // ( cLocalFile )                                     -> oQUrl
   METHOD  fromPercentEncoding           // ( oQByteArray )                                    -> cQString
   METHOD  idnWhitelist                  // (  )                                               -> oQStringList
   METHOD  setIdnWhitelist               // ( oQStringList )                                   -> NIL
   METHOD  toAce                         // ( cDomain )                                        -> oQByteArray
   METHOD  toPercentEncoding             // ( cInput, oQByteArray, oQByteArray )               -> oQByteArray

   ENDCLASS


METHOD QUrl:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QUrl( ... )
   RETURN Self


METHOD QUrl:addEncodedQueryItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QUrl_addEncodedQueryItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:addQueryItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QUrl_addQueryItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:allEncodedQueryItemValues( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QUrl_allEncodedQueryItemValues( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:allQueryItemValues( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QUrl_allQueryItemValues( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:authority( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_authority( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:encodedFragment( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QUrl_encodedFragment( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:encodedHost( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QUrl_encodedHost( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:encodedPassword( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QUrl_encodedPassword( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:encodedPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QUrl_encodedPath( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:encodedQuery( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QUrl_encodedQuery( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:encodedQueryItemValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QUrl_encodedQueryItemValue( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:encodedUserName( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QUrl_encodedUserName( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:fragment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_fragment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:hasEncodedQueryItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_hasEncodedQueryItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:hasFragment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_hasFragment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:hasQuery( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_hasQuery( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:hasQueryItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_hasQueryItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:host( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_host( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:isParentOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_isParentOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:isRelative( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_isRelative( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:password( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_password( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:path( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_path( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:port( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_port_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QUrl_port( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:queryItemValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_queryItemValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:queryPairDelimiter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_queryPairDelimiter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:queryValueDelimiter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_queryValueDelimiter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:removeAllEncodedQueryItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_removeAllEncodedQueryItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:removeAllQueryItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_removeAllQueryItems( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:removeEncodedQueryItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_removeEncodedQueryItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:removeQueryItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_removeQueryItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:resolved( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QUrlFromPointer( Qt_QUrl_resolved( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:scheme( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_scheme( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setAuthority( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setAuthority( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setEncodedFragment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setEncodedFragment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setEncodedHost( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setEncodedHost( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setEncodedPassword( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setEncodedPassword( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setEncodedPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setEncodedPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setEncodedQuery( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setEncodedQuery( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setEncodedUrl( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QUrl_setEncodedUrl_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setEncodedUrl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setEncodedUserName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setEncodedUserName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setFragment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setFragment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setHost( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setHost( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setPassword( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setPassword( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setPort( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setPort( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setScheme( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setScheme( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setUrl( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QUrl_setUrl_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setUrl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setUserInfo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setUserInfo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setUserName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setUserName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:toEncoded( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QUrl_toEncoded( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QUrl_toEncoded( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:toLocalFile( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_toLocalFile( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:toString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_toString( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QUrl_toString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:userInfo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_userInfo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:userName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUrl_userName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:fromAce( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_fromAce( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:fromEncoded( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QUrlFromPointer( Qt_QUrl_fromEncoded_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QUrlFromPointer( Qt_QUrl_fromEncoded( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:fromLocalFile( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QUrlFromPointer( Qt_QUrl_fromLocalFile( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:fromPercentEncoding( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_fromPercentEncoding( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:idnWhitelist( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QUrl_idnWhitelist( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:setIdnWhitelist( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUrl_setIdnWhitelist( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:toAce( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QUrl_toAce( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUrl:toPercentEncoding( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QByteArrayFromPointer( Qt_QUrl_toPercentEncoding( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QByteArrayFromPointer( Qt_QUrl_toPercentEncoding( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QUrl_toPercentEncoding( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

