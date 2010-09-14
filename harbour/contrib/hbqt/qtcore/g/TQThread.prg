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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QThread( ... )
   RETURN HB_QThread():new( ... )


CREATE CLASS QThread INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QThread

   METHOD  new( ... )

   METHOD  exit( nReturnCode )
   METHOD  isFinished()
   METHOD  isRunning()
   METHOD  priority()
   METHOD  setPriority( nPriority )
   METHOD  setStackSize( nStackSize )
   METHOD  stackSize()
   METHOD  wait( nTime )
   METHOD  currentThread()
   METHOD  idealThreadCount()
   METHOD  yieldCurrentThread()
   METHOD  quit()
   METHOD  start( nPriority )
   METHOD  terminate()

   ENDCLASS


METHOD QThread:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QThread( ... )
   RETURN Self


METHOD QThread:exit( nReturnCode )
   RETURN Qt_QThread_exit( ::pPtr, nReturnCode )


METHOD QThread:isFinished()
   RETURN Qt_QThread_isFinished( ::pPtr )


METHOD QThread:isRunning()
   RETURN Qt_QThread_isRunning( ::pPtr )


METHOD QThread:priority()
   RETURN Qt_QThread_priority( ::pPtr )


METHOD QThread:setPriority( nPriority )
   RETURN Qt_QThread_setPriority( ::pPtr, nPriority )


METHOD QThread:setStackSize( nStackSize )
   RETURN Qt_QThread_setStackSize( ::pPtr, nStackSize )


METHOD QThread:stackSize()
   RETURN Qt_QThread_stackSize( ::pPtr )


METHOD QThread:wait( nTime )
   RETURN Qt_QThread_wait( ::pPtr, nTime )


METHOD QThread:currentThread()
   RETURN Qt_QThread_currentThread( ::pPtr )


METHOD QThread:idealThreadCount()
   RETURN Qt_QThread_idealThreadCount( ::pPtr )


METHOD QThread:yieldCurrentThread()
   RETURN Qt_QThread_yieldCurrentThread( ::pPtr )


METHOD QThread:quit()
   RETURN Qt_QThread_quit( ::pPtr )


METHOD QThread:start( nPriority )
   RETURN Qt_QThread_start( ::pPtr, nPriority )


METHOD QThread:terminate()
   RETURN Qt_QThread_terminate( ::pPtr )

