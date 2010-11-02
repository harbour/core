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


FUNCTION QHttpHeader( ... )
   RETURN HB_QHttpHeader():new( ... )

FUNCTION QHttpHeaderFromPointer( ... )
   RETURN HB_QHttpHeader():fromPointer( ... )


CREATE CLASS QHttpHeader INHERIT HbQtObjectHandler FUNCTION HB_QHttpHeader

   METHOD  new( ... )

   METHOD  addValue                      // ( cKey, cValue )                                   -> NIL
   METHOD  allValues                     // ( cKey )                                           -> oQStringList
   METHOD  contentLength                 // (  )                                               -> nUint
   METHOD  contentType                   // (  )                                               -> cQString
   METHOD  hasContentLength              // (  )                                               -> lBool
   METHOD  hasContentType                // (  )                                               -> lBool
   METHOD  hasKey                        // ( cKey )                                           -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  keys                          // (  )                                               -> oQStringList
   METHOD  majorVersion                  // (  )                                               -> nInt
   METHOD  minorVersion                  // (  )                                               -> nInt
   METHOD  removeAllValues               // ( cKey )                                           -> NIL
   METHOD  removeValue                   // ( cKey )                                           -> NIL
   METHOD  setContentLength              // ( nLen )                                           -> NIL
   METHOD  setContentType                // ( cType )                                          -> NIL
   METHOD  setValue                      // ( cKey, cValue )                                   -> NIL
   METHOD  toString                      // (  )                                               -> cQString
   METHOD  value                         // ( cKey )                                           -> cQString

   ENDCLASS


METHOD QHttpHeader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHttpHeader( ... )
   RETURN Self


METHOD QHttpHeader:addValue( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QHttpHeader_addValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:allValues( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QHttpHeader_allValues( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:contentLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_contentLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:contentType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_contentType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:hasContentLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_hasContentLength( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:hasContentType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_hasContentType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:hasKey( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttpHeader_hasKey( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:keys( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QHttpHeader_keys( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:majorVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_majorVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:minorVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_minorVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:removeAllValues( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttpHeader_removeAllValues( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:removeValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttpHeader_removeValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:setContentLength( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHttpHeader_setContentLength( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:setContentType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttpHeader_setContentType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:setValue( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QHttpHeader_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:toString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpHeader_toString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpHeader:value( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QHttpHeader_value( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

