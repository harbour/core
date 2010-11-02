/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTCORE


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

