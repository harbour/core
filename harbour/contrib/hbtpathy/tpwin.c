/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library
 * C low level module for Windows serial communication
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2004 Maurilio Longo <maurilio.longo@libero.it>
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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#if defined( HB_OS_WIN )

#include "telepath.ch"

#include <stdio.h>

HB_FUNC( __TP_INITPORTSPEED )
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
      if( SetCommState( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), &dcb ) )
      {
         COMMTIMEOUTS timeouts;

         /* read/write operations return immediatly */
         timeouts.ReadIntervalTimeout = MAXDWORD;
         timeouts.ReadTotalTimeoutMultiplier = 0;
         timeouts.ReadTotalTimeoutConstant = 0;
         timeouts.WriteTotalTimeoutMultiplier = 0;
         timeouts.WriteTotalTimeoutConstant = 0;

         hb_retnl( SetCommTimeouts( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), &timeouts ) ? 0 : -1 );
      }
      else
         hb_retnl( -1 );
   }
   else
      hb_retnl( -1 );

   HB_TCHAR_FREE( lpValues );
}

HB_FUNC( __TP_READPORT )
{
   char buffer[ 512 ];
   DWORD nRead = 0;
   OVERLAPPED Overlapped;

   memset( &Overlapped, 0, sizeof( OVERLAPPED ) );
   if( ReadFile( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), buffer, sizeof( buffer ), &nRead, &Overlapped ) )
      hb_retclen( buffer, nRead );
   else
      hb_retc_null();
}

HB_FUNC( __TP_WRITEPORT )
{
   DWORD nWritten = 0;
   OVERLAPPED Overlapped;

   memset( &Overlapped, 0, sizeof( OVERLAPPED ) );
   if( WriteFile( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), hb_parcx( 2 ), hb_parclen( 2 ), &nWritten, &Overlapped ) )
      hb_retnl( ( long ) nWritten ); /* Put GetLastError() on error, or better a second byref param? */
   else
      hb_retnl( -1 );
}

HB_FUNC( __TP_INFREE )
{
   HANDLE hPort = ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 );
   COMMPROP CommProp;
   if( GetCommProperties( hPort, &CommProp ) )
   {
      COMSTAT ComStat;
      if( ClearCommError( hPort, NULL, &ComStat ) && CommProp.dwCurrentRxQueue != 0 )
         hb_retnl( CommProp.dwCurrentRxQueue - ComStat.cbInQue );
      else
         hb_retnl( -1 );
   }
   else
      hb_retnl( -1 );
}

HB_FUNC( __TP_OUTFREE )
{
   HANDLE hPort = ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 );
   COMMPROP CommProp;
   if( GetCommProperties( hPort, &CommProp ) )
   {
      COMSTAT ComStat;
      if( ClearCommError( hPort, NULL, &ComStat ) && CommProp.dwCurrentTxQueue != 0 )
         hb_retnl( CommProp.dwCurrentTxQueue - ComStat.cbOutQue );
      else
         hb_retnl( -1 );
   }
   else
      hb_retnl( -1 );
}

HB_FUNC( __TP_CTRLCTS )
{
   hb_retni( 0 ); /* dummy */
}

HB_FUNC( __TP_ISDCD )
{
   DWORD dwModemStat = 0;
   BOOL bRet = GetCommModemStatus( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), &dwModemStat );

   hb_retl( bRet ? ( dwModemStat & MS_RLSD_ON ) != 0 : FALSE ); /* The RLSD (receive-line-signal-detect) signal is on. Also is DCD. */
}

HB_FUNC( __TP_ISRI )
{
   DWORD dwModemStat = 0;
   BOOL bRet = GetCommModemStatus( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), &dwModemStat );

   hb_retl( bRet ? ( dwModemStat & MS_RING_ON ) != 0 : FALSE );
}

HB_FUNC( __TP_ISDSR )
{
   DWORD dwModemStat = 0;
   BOOL bRet = GetCommModemStatus( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), &dwModemStat );

   hb_retl( bRet ? ( dwModemStat & MS_DSR_ON ) != 0 : FALSE );
}

HB_FUNC( __TP_ISCTS )
{
   DWORD dwModemStat = 0;
   BOOL bRet = GetCommModemStatus( ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 ), &dwModemStat );

   hb_retl( bRet ? ( dwModemStat & MS_CTS_ON ) != 0 : FALSE );
}

#endif /* HB_OS_WIN */
