/*
 * $Id$
 */

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

/* real functions used as wrappers in above translations */

function StartThread( p1, p2, ... )
   if PCount() < 2
      return hb_threadStart( p1 )
   elseif HB_ISOBJECT( p1 ) .and. HB_ISSTRING( p2 )
      return hb_threadStart( {|...| p1:&p2( ... ) }, ... )
   endif
   return hb_threadStart( p1, p2, ... )


function Subscribe( mtx, nTimeOut, lSubscribed )
   local xSubscribed
   lSubscribed := hb_mutexSubscribe( mtx, ;
                                     iif( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, ), ;
                                     @xSubscribed )
   return xSubscribed


function SubscribeNow( mtx, nTimeOut, lSubscribed )
   local xSubscribed
   lSubscribed := hb_mutexSubscribeNow( mtx, ;
                                        iif( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, ), ;
                                        @xSubscribed )
   return xSubscribed


function IsSameThread( pThID1, pThID2 )
   return hb_threadId( pThID1 ) == iif( pcount() < 2, hb_threadId(), ;
                                                      hb_threadId( pThID2 ) )


function IsValidThread( pThID )
   local lValid

   begin sequence with {|| break() }
      lValid := hb_threadId( pThID ) != 0
   recover
      lValid := .F.
   end sequence

   return lValid


function KillThread( pThID )
   hb_threadQuitRequest( pThID )
   return NIL


function StopThread( pThID )
   hb_threadQuitRequest( pThID )
   hb_threadJoin( pThID )
   return NIL


function ThreadSleep( nTimeOut )
   return hb_idleSleep( nTimeOut / 1000 )


function hb_MutexTryLock( mtx )
   return hb_mutexLock( mtx, 0 )


function hb_MutexTimeOutLock( mtx, nTimeOut )
   return hb_mutexLock( mtx, IIF( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, 0 ) )


function GetSystemThreadId( pThID )
   return iif( PCount() < 1, hb_threadId(), hb_threadId( pThID ) )
