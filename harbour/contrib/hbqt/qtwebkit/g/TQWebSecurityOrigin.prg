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


FUNCTION QWebSecurityOrigin( ... )
   RETURN HB_QWebSecurityOrigin():new( ... )

FUNCTION QWebSecurityOriginFromPointer( ... )
   RETURN HB_QWebSecurityOrigin():fromPointer( ... )


CREATE CLASS QWebSecurityOrigin INHERIT HbQtObjectHandler FUNCTION HB_QWebSecurityOrigin

   METHOD  new( ... )

   METHOD  databaseQuota                 // (  )                                               -> nQint64
   METHOD  databaseUsage                 // (  )                                               -> nQint64
   METHOD  host                          // (  )                                               -> cQString
   METHOD  port                          // (  )                                               -> nInt
   METHOD  scheme                        // (  )                                               -> cQString
   METHOD  setDatabaseQuota              // ( nQuota )                                         -> NIL
   METHOD  allOrigins                    // (  )                                               -> oQList_QWebSecurityOrigin>

   ENDCLASS


METHOD QWebSecurityOrigin:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebSecurityOrigin( ... )
   RETURN Self


METHOD QWebSecurityOrigin:databaseQuota( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSecurityOrigin_databaseQuota( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSecurityOrigin:databaseUsage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSecurityOrigin_databaseUsage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSecurityOrigin:host( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSecurityOrigin_host( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSecurityOrigin:port( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSecurityOrigin_port( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSecurityOrigin:scheme( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebSecurityOrigin_scheme( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSecurityOrigin:setDatabaseQuota( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebSecurityOrigin_setDatabaseQuota( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebSecurityOrigin:allOrigins( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWebSecurityOrigin_allOrigins( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

