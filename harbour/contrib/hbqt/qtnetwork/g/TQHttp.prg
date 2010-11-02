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


REQUEST __HBQTNETWORK


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

