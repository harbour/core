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


FUNCTION QThread( ... )
   RETURN HB_QThread():new( ... )

FUNCTION QThreadFromPointer( ... )
   RETURN HB_QThread():fromPointer( ... )


CREATE CLASS QThread INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QThread

   METHOD  new( ... )

   METHOD  exit                          // ( nReturnCode )                                    -> NIL
   METHOD  isFinished                    // (  )                                               -> lBool
   METHOD  isRunning                     // (  )                                               -> lBool
   METHOD  priority                      // (  )                                               -> nPriority
   METHOD  setPriority                   // ( nPriority )                                      -> NIL
   METHOD  setStackSize                  // ( nStackSize )                                     -> NIL
   METHOD  stackSize                     // (  )                                               -> nUint
   METHOD  wait                          // ( nTime )                                          -> lBool
   METHOD  currentThread                 // (  )                                               -> oQThread
   METHOD  idealThreadCount              // (  )                                               -> nInt
   METHOD  yieldCurrentThread            // (  )                                               -> NIL
   METHOD  quit                          // (  )                                               -> NIL
   METHOD  start                         // ( nPriority )                                      -> NIL
   METHOD  terminate                     // (  )                                               -> NIL

   ENDCLASS


METHOD QThread:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QThread( ... )
   RETURN Self


METHOD QThread:exit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QThread_exit( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QThread_exit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:isFinished( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_isFinished( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:isRunning( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_isRunning( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:priority( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_priority( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:setPriority( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QThread_setPriority( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:setStackSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QThread_setStackSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:stackSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_stackSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:wait( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QThread_wait( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QThread_wait( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:currentThread( ... )
   SWITCH PCount()
   CASE 0
      RETURN QThreadFromPointer( Qt_QThread_currentThread( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:idealThreadCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_idealThreadCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:yieldCurrentThread( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_yieldCurrentThread( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:quit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_quit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:start( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QThread_start( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QThread_start( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QThread:terminate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_terminate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

