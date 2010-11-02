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


FUNCTION QHttpRequestHeader( ... )
   RETURN HB_QHttpRequestHeader():new( ... )

FUNCTION QHttpRequestHeaderFromPointer( ... )
   RETURN HB_QHttpRequestHeader():fromPointer( ... )


CREATE CLASS QHttpRequestHeader INHERIT HbQtObjectHandler, HB_QHttpHeader FUNCTION HB_QHttpRequestHeader

   METHOD  new( ... )

   METHOD  majorVersion                  // (  )                                               -> nInt
   METHOD  method                        // (  )                                               -> cQString
   METHOD  minorVersion                  // (  )                                               -> nInt
   METHOD  path                          // (  )                                               -> cQString
   METHOD  setRequest                    // ( cMethod, cPath, nMajorVer, nMinorVer )           -> NIL

   ENDCLASS


METHOD QHttpRequestHeader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHttpRequestHeader( ... )
   RETURN Self


METHOD QHttpRequestHeader:majorVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpRequestHeader_majorVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpRequestHeader:method( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpRequestHeader_method( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpRequestHeader:minorVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpRequestHeader_minorVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpRequestHeader:path( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHttpRequestHeader_path( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHttpRequestHeader:setRequest( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QHttpRequestHeader_setRequest( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QHttpRequestHeader_setRequest( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QHttpRequestHeader_setRequest( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

