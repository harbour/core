/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library
 * C low level module for Windows serial communication
 *
 * Copyright 2004 - Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.harbour-project.org
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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#ifdef HB_OS_WIN_32

#include <stdio.h>

HB_FUNC( P_INITPORTSPEED )
{
   DCB dcb;
   char values[ 20 ];
   LPTSTR lpValues;

   FillMemory( &dcb, sizeof( dcb ), 0 );
   dcb.DCBlength = sizeof( dcb );

   hb_snprintf( values, sizeof( values ), "%lu,%1s,%1lu,%1lu", hb_parnl( 2 ), hb_parcx( 4 ), hb_parnl( 3 ), hb_parnl( 5 ) );
   lpValues = HB_TCHAR_CONVTO( values );

   if( BuildCommDCB( lpValues, &dcb ) )
   {
      if( SetCommState( ( HANDLE ) hb_parnint( 1 ), &dcb ) )
      {
         COMMTIMEOUTS timeouts;

         /* read/write operations return immediatly */
         timeouts.ReadIntervalTimeout = MAXDWORD;
         timeouts.ReadTotalTimeoutMultiplier = 0;
         timeouts.ReadTotalTimeoutConstant = 0;
         timeouts.WriteTotalTimeoutMultiplier = 0;
         timeouts.WriteTotalTimeoutConstant = 0;

         hb_retnl( SetCommTimeouts( ( HANDLE ) hb_parnint( 1 ), &timeouts ) ? 0 : -1 );
      }
      else
         hb_retnl( -1 );
   }
   else
      hb_retnl( -1 );

   HB_TCHAR_FREE( lpValues );
}

HB_FUNC( P_READPORT )
{
   char Buffer[ 512 ];
   DWORD nRead = 0;
   OVERLAPPED Overlapped;
   BOOL bRet;

   memset( &Overlapped, 0, sizeof( OVERLAPPED ) );
   bRet = ReadFile( ( HANDLE ) hb_parnint( 1 ), Buffer, sizeof( Buffer ), &nRead, &Overlapped );
   hb_retclen( bRet ? Buffer : NULL, nRead );
}

HB_FUNC( P_WRITEPORT )
{
   DWORD nWritten = 0;
   OVERLAPPED Overlapped;
   BOOL bRet;

   memset( &Overlapped, 0, sizeof( OVERLAPPED ) );
   bRet = WriteFile( ( HANDLE ) hb_parnint( 1 ), hb_parcx( 2 ), hb_parclen( 2 ), &nWritten, &Overlapped );
   hb_retnl( bRet ? ( long ) nWritten : -1 ); /* Put GetLastError() on error, or better a second byref param? */
}

#endif /* HB_OS_WIN_32 */
