/*
 * Harbour Project source code:
 *    xHarbour compatible MT helpers
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* real functions used as wrappers in above translations */

FUNCTION StartThread( p1, p2, ... )

   IF PCount() < 2
      RETURN hb_threadStart( p1 )
   ELSEIF HB_ISOBJECT( p1 ) .AND. HB_ISSTRING( p2 )
      RETURN hb_threadStart( {| ...| p1:&p2( ... ) }, ... )
   ENDIF

   RETURN hb_threadStart( p1, p2, ... )

FUNCTION Subscribe( mtx, nTimeOut, lSubscribed )

   LOCAL xSubscribed

   lSubscribed := hb_mutexSubscribe( mtx, ;
      iif( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, ), ;
      @xSubscribed )

   RETURN xSubscribed

FUNCTION SubscribeNow( mtx, nTimeOut, lSubscribed )

   LOCAL xSubscribed

   lSubscribed := hb_mutexSubscribeNow( mtx, ;
      iif( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, ), ;
      @xSubscribed )

   RETURN xSubscribed

FUNCTION IsSameThread( pThID1, pThID2 )
   RETURN hb_threadID( pThID1 ) == iif( PCount() < 2, hb_threadID(), ;
      hb_threadID( pThID2 ) )

FUNCTION IsValidThread( pThID )

   LOCAL lValid

   BEGIN SEQUENCE WITH {|| Break() }
      lValid := hb_threadID( pThID ) != 0
   RECOVER
      lValid := .F.
   END SEQUENCE

   RETURN lValid

FUNCTION KillThread( pThID )

   hb_threadQuitRequest( pThID )

   RETURN NIL

FUNCTION StopThread( pThID )

   hb_threadQuitRequest( pThID )
   hb_threadJoin( pThID )

   RETURN NIL

FUNCTION ThreadSleep( nTimeOut )
   RETURN hb_idleSleep( nTimeOut / 1000 )

FUNCTION hb_MutexTryLock( mtx )
   RETURN hb_mutexLock( mtx, 0 )

FUNCTION hb_MutexTimeOutLock( mtx, nTimeOut )
   RETURN hb_mutexLock( mtx, iif( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, 0 ) )

FUNCTION GetSystemThreadId( pThID )
   RETURN iif( PCount() < 1, hb_threadID(), hb_threadID( pThID ) )
