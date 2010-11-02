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


REQUEST __HBQTWEBKIT


FUNCTION QWebHistoryItem( ... )
   RETURN HB_QWebHistoryItem():new( ... )

FUNCTION QWebHistoryItemFromPointer( ... )
   RETURN HB_QWebHistoryItem():fromPointer( ... )


CREATE CLASS QWebHistoryItem INHERIT HbQtObjectHandler FUNCTION HB_QWebHistoryItem

   METHOD  new( ... )

   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  lastVisited                   // (  )                                               -> oQDateTime
   METHOD  originalUrl                   // (  )                                               -> oQUrl
   METHOD  setUserData                   // ( oQVariant )                                      -> NIL
   METHOD  title                         // (  )                                               -> cQString
   METHOD  url                           // (  )                                               -> oQUrl
   METHOD  userData                      // (  )                                               -> oQVariant

   ENDCLASS


METHOD QWebHistoryItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebHistoryItem( ... )
   RETURN Self


METHOD QWebHistoryItem:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QWebHistoryItem_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryItem:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistoryItem_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryItem:lastVisited( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QWebHistoryItem_lastVisited( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryItem:originalUrl( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebHistoryItem_originalUrl( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryItem:setUserData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebHistoryItem_setUserData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryItem:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistoryItem_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryItem:url( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QWebHistoryItem_url( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryItem:userData( ... )
   SWITCH PCount()
   CASE 0
      RETURN QVariantFromPointer( Qt_QWebHistoryItem_userData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

