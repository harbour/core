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


FUNCTION QTimer( ... )
   RETURN HB_QTimer():new( ... )

FUNCTION QTimerFromPointer( ... )
   RETURN HB_QTimer():fromPointer( ... )


CREATE CLASS QTimer INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QTimer

   METHOD  new( ... )

   METHOD  interval                      // (  )                                               -> nInt
   METHOD  isActive                      // (  )                                               -> lBool
   METHOD  isSingleShot                  // (  )                                               -> lBool
   METHOD  setInterval                   // ( nMsec )                                          -> NIL
   METHOD  setSingleShot                 // ( lSingleShot )                                    -> NIL
   METHOD  timerId                       // (  )                                               -> nInt
   METHOD  singleShot                    // ( nMsec, oQObject, cMember )                       -> NIL
   METHOD  start                         // (  )                                               -> NIL
   METHOD  stop                          // (  )                                               -> NIL
                                         // ( nMsec )                                          -> NIL

   ENDCLASS


METHOD QTimer:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTimer( ... )
   RETURN Self


METHOD QTimer:interval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimer_interval( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:isActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimer_isActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:isSingleShot( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimer_isSingleShot( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:setInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimer_setInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:setSingleShot( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTimer_setSingleShot( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:timerId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimer_timerId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:singleShot( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QTimer_singleShot( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:start( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimer_start_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTimer_start( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimer:stop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimer_stop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

