/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * XPP compatible classes to manage threads
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 * special thanks for Pritpal Bedi for class skeleton with info about
 * Xbase++ and to other contributors which I hope will finish and fix
 * this code
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

#include "common.ch"
#include "hbclass.ch"
#include "hbthread.ch"

#ifdef HB_COMPAT_XPP

/* I do not know Xbase++ values */
#define QUIT_NORESTART  1
#define QUIT_RESTART    2


/*
 * SIGNAL class
 */
CREATE CLASS TSignal FUNCTION Signal

   VAR cargo      AS USUAL EXPORTED
   VAR mutex      AS USUAL PROTECTED

EXPORTED:

   METHOD new()
   METHOD wait( nTimeOut )
   METHOD signal()

ENDCLASS

METHOD new( ... ) CLASS TSIGNAL
   ::mutex := hb_mutexCreate()
   ::Init( ... )
   RETURN Self

METHOD wait( nTimeOut ) CLASS TSIGNAL
   /* TOCHECK: I do not know if strict Xbase++ compatibility needs
    *          hb_mutexSubscribe() or hb_mutexSubscribeNow()
    *          Please change it if necessary
    */
   RETURN hb_mutexSubscribe( ::mutex, ;
                             iif( ISNUMBER( nTimeOut ), nTimeOut / 100, ) )

METHOD signal() CLASS TSIGNAL
   hb_mutexNotify( ::mutex )
   RETURN Self


/*
 * THREAD class
 */

CREATE CLASS TThread FUNCTION Thread

EXPORTED:
   VAR cargo            AS USUAL                      SYNC
   VAR active           AS LOGICAL READONLY  INIT .f.
   VAR deltaTime        AS NUMERIC READONLY  INIT 0
   VAR interval         AS USUAL   READONLY  INIT NIL
   VAR priority         AS NUMERIC READONLY  INIT 0
   VAR startCount       AS NUMERIC READONLY  INIT 0
   VAR startTime        AS USUAL   READONLY  INIT NIL
   VAR atEnd            AS USUAL             INIT NIL SYNC
   VAR atStart          AS USUAL             INIT NIL SYNC
   VAR result           AS USUAL             INIT NIL SYNC

PROTECTED:
   VAR maxStackSize     AS USUAL             INIT 50000

HIDDEN:
   VAR pThreadID        AS USUAL             INIT NIL SYNC


EXPORTED:
   METHOD new( nMaxStackSize )

PROTECTED:
   /* METHOD atEnd() */
   /* METHOD atStart() */
   METHOD execute()

EXPORTED:
   METHOD quit( xResult, nRestart )                   SYNC
   METHOD setInterval( nHSeconds )
   METHOD setPriority( nPriority )
   METHOD setStartTime( nSeconds )
   METHOD start()                                     SYNC
   METHOD synchronize( nTimeOut )
   METHOD threadSelf()
   METHOD threadID()

   METHOD thread INLINE ::TThread()

ENDCLASS

METHOD new( nMaxStackSize ) CLASS TTHREAD
   IF ISNUMBER( nMaxStackSize )
      ::maxStackSize := nMaxStackSize
   ENDIF
   ::Init()
   RETURN Self

METHOD execute() CLASS TTHREAD
   HB_SYMBOL_UNUSED( Self )
   RETURN NIL

METHOD quit( xResult, nRestart ) CLASS TTHREAD
   IF hb_threadSelf() == ::pThreadID
      IF ISNUMBER( nRestart )
         IF nRestart == QUIT_NORESTART
            ::interval := NIL
         ELSEIF nRestart == QUIT_RESTART
            IF ISNUMBER( ::interval )
               /* TODO: do not interrupt by QUIT but restart execution */
            ENDIF
         ENDIF
      ENDIF
      IF PCOUNT() > 0
         ::result := xResult
      ENDIF
      QUIT
   ENDIF
   RETURN NIL

METHOD setInterval( nHSeconds ) CLASS TTHREAD
   IF ISNUMBER( nHSeconds )
      ::interval := nHSeconds
   ENDIF
   RETURN .F.

METHOD setPriority( nPriority ) CLASS TTHREAD
   /* TODO: add thread priority setting */
   IF ISNUMBER( nPriority )
      ::priority := nPriority
   ENDIF
   RETURN .F.

METHOD setStartTime( nSeconds ) CLASS TTHREAD
   /* TODO: add such functionality, probably by special thread */
   IF ISNUMBER( nSeconds )
      ::startTime := nSeconds
   ELSEIF nSeconds == NIL
      ::startTime := NIL
   ENDIF
   RETURN .F.

METHOD start( xAction, ... ) CLASS TTHREAD
   /* TODO: thread stack size set by user ::maxStackSize */
   IF ::active
      RETURN .F.
   ELSEIF xAction == NIL
      ::active := .T.
      ::pThreadID := hb_threadStart( HB_THREAD_INHERIT_PUBLIC, ;
            { |...|
               ::startTime := Seconds()
               ThreadObject( Self )
               ::result := ::execute( ... )
               RETURN NIL
            }, ... )
   ELSEIF !Empty( xAction ) .AND. ValType( xAction ) $ "CBP"
      ::active := .T.
      ::pThreadID := hb_threadStart( HB_THREAD_INHERIT_PUBLIC, ;
            { |...|
               ::startTime := Seconds()
               ThreadObject( Self )
               IF ::atStart != NIL
                  EVAL( ::atStart, ... )
               ENDIF
               ::result := DO( xAction, ... )
               IF ::atEnd != NIL
                  EVAL( ::atEnd, ... )
               ENDIF
               ::startTime := NIL
               RETURN NIL
            }, ... )
   ELSE
      RETURN .F.
   ENDIF
   RETURN .T.

METHOD synchronize( nTimeOut ) CLASS TTHREAD
   LOCAL pThreadID := ::pThreadID

   IF hb_threadSelf() != pThreadID
      RETURN hb_threadWait( pThreadID, ;
                            iif( ISNUMBER( nTimeOut ) .AND. nTimeOut != 0, ;
                            nTimeOut / 100, ) )
   ENDIF
   RETURN .F.

METHOD threadSelf() CLASS TTHREAD
   RETURN ::pThreadID

METHOD threadID() CLASS TTHREAD
   LOCAL pThreadID := ::pThreadID
   RETURN IIF( pThreadID == NIL, 0, hb_threadID( pThreadID ) )

#endif
