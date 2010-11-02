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


FUNCTION QWebHistoryInterface( ... )
   RETURN HB_QWebHistoryInterface():new( ... )

FUNCTION QWebHistoryInterfaceFromPointer( ... )
   RETURN HB_QWebHistoryInterface():fromPointer( ... )


CREATE CLASS QWebHistoryInterface INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QWebHistoryInterface

   METHOD  new( ... )

   METHOD  addHistoryEntry               // ( cUrl )                                           -> NIL
   METHOD  historyContains               // ( cUrl )                                           -> lBool
   METHOD  defaultInterface              // (  )                                               -> oQWebHistoryInterface
   METHOD  setDefaultInterface           // ( oQWebHistoryInterface )                          -> NIL

   ENDCLASS


METHOD QWebHistoryInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebHistoryInterface( ... )
   RETURN Self


METHOD QWebHistoryInterface:addHistoryEntry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebHistoryInterface_addHistoryEntry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryInterface:historyContains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWebHistoryInterface_historyContains( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryInterface:defaultInterface( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebHistoryInterfaceFromPointer( Qt_QWebHistoryInterface_defaultInterface( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistoryInterface:setDefaultInterface( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebHistoryInterface_setDefaultInterface( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

