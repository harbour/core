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


REQUEST __HBQTGUI


FUNCTION QDesktopServices( ... )
   RETURN HB_QDesktopServices():new( ... )

FUNCTION QDesktopServicesFromPointer( ... )
   RETURN HB_QDesktopServices():fromPointer( ... )


CREATE CLASS QDesktopServices INHERIT HbQtObjectHandler FUNCTION HB_QDesktopServices

   METHOD  new( ... )

   METHOD  displayName                   // ( nType )                                          -> cQString
   METHOD  openUrl                       // ( oQUrl )                                          -> lBool
   METHOD  setUrlHandler                 // ( cScheme, oQObject, cMethod )                     -> NIL
   METHOD  storageLocation               // ( nType )                                          -> cQString
   METHOD  unsetUrlHandler               // ( cScheme )                                        -> NIL

   ENDCLASS


METHOD QDesktopServices:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesktopServices( ... )
   RETURN Self


METHOD QDesktopServices:displayName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesktopServices_displayName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopServices:openUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesktopServices_openUrl( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopServices:setUrlHandler( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QDesktopServices_setUrlHandler( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopServices:storageLocation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesktopServices_storageLocation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopServices:unsetUrlHandler( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesktopServices_unsetUrlHandler( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

