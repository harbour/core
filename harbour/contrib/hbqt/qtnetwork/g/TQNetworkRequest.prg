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


FUNCTION QNetworkRequest( ... )
   RETURN HB_QNetworkRequest():new( ... )

FUNCTION QNetworkRequestFromPointer( ... )
   RETURN HB_QNetworkRequest():fromPointer( ... )


CREATE CLASS QNetworkRequest INHERIT HbQtObjectHandler FUNCTION HB_QNetworkRequest

   METHOD  new( ... )

   METHOD  attribute                     // ( nCode, oQVariant )                               -> oQVariant
   METHOD  hasRawHeader                  // ( oQByteArray )                                    -> lBool
   METHOD  header                        // ( nHeader )                                        -> oQVariant
   METHOD  rawHeader                     // ( oQByteArray )                                    -> oQByteArray
   METHOD  rawHeaderList                 // (  )                                               -> oQList_QByteArray>
   METHOD  setAttribute                  // ( nCode, oQVariant )                               -> NIL
   METHOD  setHeader                     // ( nHeader, oQVariant )                             -> NIL
   METHOD  setRawHeader                  // ( oQByteArray, oQByteArray )                       -> NIL
   METHOD  setUrl                        // ( oQUrl )                                          -> NIL
   METHOD  url                           // (  )                                               -> oQUrl

   ENDCLASS


METHOD QNetworkRequest:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QNetworkRequest( ... )
   RETURN Self


METHOD QNetworkRequest:attribute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QNetworkRequest_attribute( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QNetworkRequest_attribute( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:hasRawHeader( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QNetworkRequest_hasRawHeader( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:header( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QNetworkRequest_header( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:rawHeader( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QNetworkRequest_rawHeader( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:rawHeaderList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QNetworkRequest_rawHeaderList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:setAttribute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QNetworkRequest_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:setHeader( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QNetworkRequest_setHeader( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:setRawHeader( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QNetworkRequest_setRawHeader( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:setUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QNetworkRequest_setUrl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QNetworkRequest:url( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QNetworkRequest_url( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

