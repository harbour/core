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


FUNCTION QEventLoop( ... )
   RETURN HB_QEventLoop():new( ... )

FUNCTION QEventLoopFromPointer( ... )
   RETURN HB_QEventLoop():fromPointer( ... )


CREATE CLASS QEventLoop INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QEventLoop

   METHOD  new( ... )

   METHOD  exec                          // ( nFlags )                                         -> nInt
   METHOD  exit                          // ( nReturnCode )                                    -> NIL
   METHOD  isRunning                     // (  )                                               -> lBool
   METHOD  processEvents                 // ( nFlags )                                         -> lBool
                                         // ( nFlags, nMaxTime )                               -> NIL
   METHOD  wakeUp                        // (  )                                               -> NIL

   ENDCLASS


METHOD QEventLoop:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QEventLoop( ... )
   RETURN Self


METHOD QEventLoop:exec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QEventLoop_exec( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QEventLoop_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEventLoop:exit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QEventLoop_exit( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QEventLoop_exit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEventLoop:isRunning( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QEventLoop_isRunning( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEventLoop:processEvents( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QEventLoop_processEvents_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QEventLoop_processEvents( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QEventLoop_processEvents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QEventLoop:wakeUp( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QEventLoop_wakeUp( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

