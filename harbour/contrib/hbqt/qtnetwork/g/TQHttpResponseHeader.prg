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


FUNCTION QHttpResponseHeader( ... )
   RETURN HB_QHttpResponseHeader():new( ... )

FUNCTION QHttpResponseHeaderFromPointer( ... )
   RETURN HB_QHttpResponseHeader():fromPointer( ... )


CREATE CLASS QHttpResponseHeader INHERIT HbQtObjectHandler, HB_QHttpHeader FUNCTION HB_QHttpResponseHeader

   METHOD  new( ... )

   METHOD  majorVersion                  // (  )                                               -> nInt
   METHOD  minorVersion                  // (  )                                               -> nInt
   METHOD  reasonPhrase                  // (  )                                               -> cQString
   METHOD  setStatusLine                 // ( nCode, cText, nMajorVer, nMinorVer )             -> NIL
   METHOD  statusCode                    // (  )                                               -> nInt

   ENDCLASS


METHOD QHttpResponseHeader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHttpResponseHeader( ... )
   RETURN Self


METHOD QHttpResponseHeader:majorVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpResponseHeader_majorVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpResponseHeader:minorVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpResponseHeader_minorVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpResponseHeader:reasonPhrase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpResponseHeader_reasonPhrase( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpResponseHeader:setStatusLine( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QHttpResponseHeader_setStatusLine( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QHttpResponseHeader_setStatusLine( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QHttpResponseHeader_setStatusLine( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHttpResponseHeader_setStatusLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpResponseHeader:statusCode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpResponseHeader_statusCode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

