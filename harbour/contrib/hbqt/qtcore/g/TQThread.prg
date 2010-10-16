/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QThread( ... )
   RETURN HB_QThread():new( ... )

FUNCTION QThreadFrom( ... )
   RETURN HB_QThread():from( ... )

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
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
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
   RETURN hbqt_error()


METHOD QThread:isFinished( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_isFinished( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:isRunning( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_isRunning( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:priority( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_priority( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:setPriority( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QThread_setPriority( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:setStackSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QThread_setStackSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:stackSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_stackSize( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QThread:currentThread( ... )
   SWITCH PCount()
   CASE 0
      RETURN QThreadFromPointer( Qt_QThread_currentThread( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:idealThreadCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_idealThreadCount( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:yieldCurrentThread( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_yieldCurrentThread( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QThread:quit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_quit( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


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
   RETURN hbqt_error()


METHOD QThread:terminate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QThread_terminate( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

