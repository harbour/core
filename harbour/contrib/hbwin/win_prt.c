/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Windows communications library
 *
 * Copyright 2005 Alex Strickland <sscc@mweb.co.za>
 * www - http://www.xharbour.org
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
 * anyone as to the status o such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapierr.h"
#include "hbwin.h"

static struct
{
   HANDLE       hPort;
   int          iFunction;
   DWORD        dwError;
   DCB          OldDCB;
   COMMTIMEOUTS OldTimeouts;
} s_PortData[ 256 ];

/* -1 says use default calculation */
static long s_lReadIntervalTimeout = -1;
static long s_lReadTotalTimeoutMultiplier = -1;
static long s_lReadTotalTimeoutConstant = -1;
static long s_lWriteTotalTimeoutMultiplier = -1;
static long s_lWriteTotalTimeoutConstant = -1;

HB_FUNC( WIN_PORTOPEN )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      DWORD dwBaudRate = ( DWORD ) hb_parnl( 2 );
      int iParity = hb_parni( 3 );
      int iByteSize = hb_parni( 4 );
      int iStopBits = hb_parni( 5 );

      char szName[ 11 ];
      TCHAR tszName[ 11 ];

      HANDLE hCommPort;
      COMMTIMEOUTS NewTimeouts;
      DCB NewDCB;

      hb_snprintf( szName, sizeof( szName ), "\\\\.\\COM%d", iPort + 1 );
      HB_TCHAR_SETTO( tszName, szName, strlen( szName ) );

      s_PortData[ iPort ].hPort = INVALID_HANDLE_VALUE;
      s_PortData[ iPort ].iFunction = FCNCREATEFILE;
      s_PortData[ iPort ].dwError = 0;

      if( ( hCommPort = CreateFile( tszName,
                                    GENERIC_READ | GENERIC_WRITE,
                                    0,
                                    0,
                                    OPEN_EXISTING,
                                    FILE_FLAG_NO_BUFFERING, 0 ) ) == INVALID_HANDLE_VALUE )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retnl( -1 );
         return;
      }

      s_PortData[ iPort ].iFunction = FCNGETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;

      /* We'll put everything back */
      s_PortData[ iPort ].OldDCB.DCBlength = sizeof( DCB );
      if( ! GetCommState( hCommPort, &( s_PortData[ iPort ].OldDCB ) ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         CloseHandle( hCommPort );
         hb_retnl( -1 );
         return;
      }

      NewDCB.DCBlength = sizeof( DCB );
      if( ! GetCommState( hCommPort, &NewDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         CloseHandle( hCommPort );
         hb_retnl( -1 );
         return;
      }

      /* Initialised with NO flow control or control signals! */
      NewDCB.BaudRate = dwBaudRate;
      NewDCB.fBinary = 1;
      NewDCB.fParity = 0;
      NewDCB.fOutxCtsFlow = 0;
      NewDCB.fOutxDsrFlow = 0;
      NewDCB.fDtrControl = DTR_CONTROL_DISABLE;
      NewDCB.fDsrSensitivity = 0;
      NewDCB.fTXContinueOnXoff = 1;
      NewDCB.fOutX = 0;
      NewDCB.fInX = 0;
      NewDCB.fErrorChar = 1;
      NewDCB.fNull = 0;
      NewDCB.fRtsControl = RTS_CONTROL_DISABLE;
      NewDCB.fAbortOnError = 0;
    /*NewDCB.XonLim*/
    /*NewDCB.XoffLim*/
      NewDCB.ByteSize = ( BYTE ) iByteSize;
      NewDCB.Parity = ( BYTE ) iParity;
      NewDCB.StopBits = ( BYTE ) iStopBits;
    /*NewDCB.XonChar*/
    /*NewDCB.XoffChar*/
      NewDCB.ErrorChar = '?';
    /*NewDCB.EofChar*/
    /*NewDCB.EvtChar*/

      /* function reinitializes all hardware and control settings, but it does not empty output or input queues */
      s_PortData[ iPort ].iFunction = FCNSETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      if( ! SetCommState( hCommPort, &NewDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         CloseHandle( hCommPort );
         hb_retnl( -1 );
         return;
      }

      /* We'll leave this to Windows, unless you really want it changed! */
      if( HB_ISNUM( 6 ) &&
          HB_ISNUM( 7 ) )
      {
         s_PortData[ iPort ].iFunction = FCNSETUPCOMM;
         s_PortData[ iPort ].dwError = 0;
         if( ! SetupComm( hCommPort,
                          ( DWORD ) hb_parnl( 6 ) /* dwInQueue */,
                          ( DWORD ) hb_parnl( 7 ) /* dwOutQueue */ ) )
         {
            s_PortData[ iPort ].dwError = GetLastError();
            CloseHandle( hCommPort );
            hb_retnl( -1 );
            return;
         }
      }

      /* We'll put everything back */
      s_PortData[ iPort ].iFunction = FCNGETCOMMTIMEOUTS;
      s_PortData[ iPort ].dwError = 0;
      if( ! GetCommTimeouts( hCommPort, &( s_PortData[ iPort ].OldTimeouts ) ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         CloseHandle( hCommPort );
         hb_retnl( -1 );
         return;
      }

      /* Maximum time, in milliseconds, allowed to elapse between the arrival of two characters on
         the communications line. During a ReadFile operation, the time period begins when the first
         character is received. If the interval between the arrival of any two characters exceeds this
         amount, the ReadFile operation is completed and any buffered data is returned. A value of zero
         indicates that interval time-outs are not used. */

      /* A value of MAXDWORD, combined with zero values for both the s_lReadTotalTimeoutConstant and
         s_lReadTotalTimeoutMultiplier members, specifies that the read operation is to return
         immediately with the characters that have already been received, even if no characters
         have been received. */
      NewTimeouts.ReadIntervalTimeout = ( s_lReadIntervalTimeout == -1 ? MAXDWORD : ( DWORD ) s_lReadIntervalTimeout );

      /* Multiplier, in milliseconds, used to calculate the total time-out period for read operations.
         For each read operation, this value is multiplied by the requested number of bytes to be read. */
      NewTimeouts.ReadTotalTimeoutMultiplier = ( s_lReadTotalTimeoutMultiplier == -1 ? 0 : ( DWORD ) s_lReadTotalTimeoutMultiplier );

      /* Constant, in milliseconds, used to calculate the total time-out period for read operations.
         For each read operation, this value is added to the product of the s_lReadTotalTimeoutMultiplier
         member and the requested number of bytes. */
      NewTimeouts.ReadTotalTimeoutConstant = ( s_lReadTotalTimeoutConstant == -1 ? 0 : ( DWORD ) s_lReadTotalTimeoutConstant );

      /* A value of zero for both the s_lReadTotalTimeoutMultiplier and s_lReadTotalTimeoutConstant members
         indicates that total time-outs are not used for read operations ...
         and MAXDWORD, 0 and 0 are what we use by default */

      /* Multiplier, in milliseconds, used to calculate the total time-out period for write operations.
         For each write operation, this value is multiplied by the number of bytes to be written. */
      if( s_lWriteTotalTimeoutMultiplier == -1 )
      {
          /* float of 1.0 makes whole expression float */
          NewTimeouts.WriteTotalTimeoutMultiplier = HB_MIN( 1, ( DWORD ) ( ( 1.0 / dwBaudRate ) *
              ( iByteSize + 1 + ( iParity == NOPARITY ? 0 : 1 ) + ( iStopBits == ONESTOPBIT ? 1 : iStopBits == ONE5STOPBITS ? 1.5 : 2 ) ) * 1000 ) );
      }
      /* Constant, in milliseconds, used to calculate the total time-out period for write operations.
         For each write operation, this value is added to the product of the s_lWriteTotalTimeoutMultiplier member and the number of bytes to be written. */
      else
         NewTimeouts.WriteTotalTimeoutMultiplier = ( DWORD ) s_lWriteTotalTimeoutMultiplier;

      /* 50 ms is a thumbsuck - seems long enough and not too long! */
      NewTimeouts.WriteTotalTimeoutConstant = s_lWriteTotalTimeoutConstant == -1 ? 50 : ( DWORD ) s_lWriteTotalTimeoutConstant;

      /* A value of zero for both the s_lWriteTotalTimeoutMultiplier and s_lWriteTotalTimeoutConstant members
         indicates that total time-outs are not used for write operations ...
         and if flow control is enabled the program will "hang" or if it is not enabled the data will
         be lost (potentially), so we set a minimum of 1ms (baud rates higher than 4800) */

      s_PortData[ iPort ].iFunction = FCNSETCOMMTIMEOUTS;
      s_PortData[ iPort ].dwError = 0;
      if( ! SetCommTimeouts( hCommPort, &NewTimeouts ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         CloseHandle( hCommPort );
         hb_retnl( -1 );
      }
      else
      {
         s_PortData[ iPort ].hPort = hCommPort;
         hb_retnl( hCommPort == INVALID_HANDLE_VALUE ? -1 : 0 );
      }
   }
   else
      hb_retnl( -1 );
}

HB_FUNC( WIN_PORTCLOSE )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      long lDrain = hb_parnl( 2 );

      s_PortData[ iPort ].iFunction = FCNSETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      if( ! SetCommState( hCommPort, &( s_PortData[ iPort ].OldDCB ) ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         CloseHandle( hCommPort );
         hb_retl( FALSE );
         return;
      }

      s_PortData[ iPort ].iFunction = FCNSETCOMMTIMEOUTS;
      s_PortData[ iPort ].dwError = 0;
      if( ! SetCommTimeouts( hCommPort, &( s_PortData[ iPort ].OldTimeouts ) ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         CloseHandle( hCommPort );
         hb_retl( FALSE );
         return;
      }

      s_PortData[ iPort ].hPort = INVALID_HANDLE_VALUE;

      s_PortData[ iPort ].iFunction = FCNCLOSEHANDLE;
      s_PortData[ iPort ].dwError = 0;

      /* I honestly don't know if this helps */
      if( lDrain > 0 )
         Sleep( lDrain * 1000 );

      hb_retl( CloseHandle( hCommPort ) != 0 );
      s_PortData[ iPort ].dwError = GetLastError();
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTWRITE )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      const char * lpBuffer = hb_parcx( 2 );
      DWORD dwNumberofBytesToWrite = ( DWORD ) hb_parclen( 2 );
      DWORD dwNumberofBytesWritten;

      s_PortData[ iPort ].iFunction = FCNWRITEFILE;
      s_PortData[ iPort ].dwError = 0;
      if( WriteFile( hCommPort, lpBuffer, dwNumberofBytesToWrite, &dwNumberofBytesWritten, NULL ) )
         hb_retnl( dwNumberofBytesWritten );
      else
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retnl( -1 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTREAD )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      char * lpBuffer;
      DWORD dwNumberOfBytesToRead = ( DWORD ) hb_parclen( 2 );
      DWORD dwNumberOfBytesRead;

      lpBuffer = ( char * ) hb_xgrab( dwNumberOfBytesToRead + 1 );
      s_PortData[ iPort ].iFunction = FCNREADFILE;
      s_PortData[ iPort ].dwError = 0;
      if( ReadFile( hCommPort, lpBuffer, dwNumberOfBytesToRead, &dwNumberOfBytesRead, NULL ) )
      {
         if( ! hb_storclen_buffer( lpBuffer, dwNumberOfBytesRead, 2 ) )
            hb_xfree( lpBuffer );
         hb_retnl( dwNumberOfBytesRead );
      }
      else
      {
         hb_storc( NULL, 2 );
         hb_xfree( lpBuffer );
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retnl( -1 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTRECV )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      char * lpBuffer;
      DWORD dwNumberOfBytesToRead = ( DWORD ) hb_parnl( 2 );
      DWORD dwNumberOfBytesRead;

      lpBuffer = ( char * ) hb_xgrab( dwNumberOfBytesToRead + 1 );
      s_PortData[ iPort ].iFunction = FCNREADFILE;
      s_PortData[ iPort ].dwError = 0;
      if( ReadFile( hCommPort, lpBuffer, dwNumberOfBytesToRead, &dwNumberOfBytesRead, NULL ) )
      {
         hb_retclen_buffer( lpBuffer, dwNumberOfBytesRead );
         hb_stornl( dwNumberOfBytesRead, 3 );
      }
      else
      {
         hb_retc_null();
         hb_xfree( lpBuffer );
         s_PortData[ iPort ].dwError = GetLastError();
         hb_stornl( -1, 3 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTSTATUS )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DWORD dwModemStat = 0;

      s_PortData[ iPort ].iFunction = FCNGETCOMMMODEMSTATUS;
      s_PortData[ iPort ].dwError = 0;
      if( GetCommModemStatus( hCommPort, &dwModemStat ) )
      {
         hb_storl( ( dwModemStat & MS_CTS_ON )  != 0, 2 );     /* The CTS (clear-to-send) signal is on. */
         hb_storl( ( dwModemStat & MS_DSR_ON )  != 0, 3 );     /* The DSR (data-set-ready) signal is on. */
         hb_storl( ( dwModemStat & MS_RING_ON ) != 0, 4 );     /* The ring indicator signal is on. */
         hb_storl( ( dwModemStat & MS_RLSD_ON ) != 0, 5 );     /* The RLSD (receive-line-signal-detect) signal is on. Also is DCD. */

         hb_retl( TRUE );
      }
      else
      {
         s_PortData[ iPort ].dwError = GetLastError();

         hb_storl( FALSE, 2 );
         hb_storl( FALSE, 3 );
         hb_storl( FALSE, 4 );
         hb_storl( FALSE, 5 );

         hb_retl( FALSE );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTPURGE )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DWORD dwFlags;

      dwFlags = ( hb_parl( 2 ) ? PURGE_RXCLEAR : 0 ) | ( hb_parl( 3 ) ? PURGE_TXCLEAR : 0 );
      s_PortData[ iPort ].iFunction = FCNPURGECOMM;
      s_PortData[ iPort ].dwError = 0;
      if( PurgeComm( hCommPort, dwFlags ) )
         hb_retl( TRUE );
      else
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTQUEUESTATUS )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DWORD dwErrors = 0;
      COMSTAT ComStat;

      s_PortData[ iPort ].iFunction = FCNCLEARCOMMERROR;
      s_PortData[ iPort ].dwError = 0;
      if( ClearCommError( hCommPort, &dwErrors, &ComStat ) )
      {
         hb_storl( ComStat.fCtsHold, 2 );
         hb_storl( ComStat.fDsrHold, 3 );
         hb_storl( ComStat.fRlsdHold, 4 );
         hb_storl( ComStat.fXoffHold, 5 );
         hb_storl( ComStat.fXoffSent, 6 );
         hb_stornl( ComStat.cbInQue, 7 );
         hb_stornl( ComStat.cbOutQue, 8 ); /* This value will be zero for a nonoverlapped write */

         hb_retl( TRUE );
      }
      else
      {
         s_PortData[ iPort ].dwError = GetLastError();

         hb_storl( FALSE, 2 );
         hb_storl( FALSE, 3 );
         hb_storl( FALSE, 4 );
         hb_storl( FALSE, 5 );
         hb_storl( FALSE, 6 );
         hb_stornl( 0, 7 );
         hb_stornl( 0, 8 );

         hb_retl( FALSE );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* If handshaking is enabled, it is an error for the application to adjust the line by
   using the EscapeCommFunction function */

HB_FUNC( WIN_PORTSETRTS )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DWORD dwFunc = hb_parl( 2 ) ? SETRTS : CLRRTS;

      s_PortData[ iPort ].iFunction = ESCAPECOMMFUNCTION;
      s_PortData[ iPort ].dwError = 0;
      if( EscapeCommFunction( hCommPort, dwFunc ) )
         hb_retl( TRUE );
      else
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* If handshaking is enabled, it is an error for the application to adjust the line by
   using the EscapeCommFunction function */

HB_FUNC( WIN_PORTSETDTR )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DWORD dwFunc = hb_parl( 2 ) ? SETDTR : CLRDTR;

      s_PortData[ iPort ].iFunction = ESCAPECOMMFUNCTION;
      s_PortData[ iPort ].dwError = 0;
      if( EscapeCommFunction( hCommPort, dwFunc ) )
         hb_retl( TRUE );
      else
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTRTSFLOW )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DCB CurDCB;
      int iRtsControl = hb_parni( 2 );

      s_PortData[ iPort ].iFunction = FCNGETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      CurDCB.DCBlength = sizeof( DCB );
      if( ! GetCommState( hCommPort, &CurDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
         return;
      }

      if( iRtsControl == RTS_CONTROL_DISABLE )
      {
         CurDCB.fOutxCtsFlow = 0;
         CurDCB.fRtsControl = RTS_CONTROL_DISABLE;
      }
      else if( iRtsControl == RTS_CONTROL_ENABLE )
      {
         CurDCB.fOutxCtsFlow = 1;
         CurDCB.fRtsControl = RTS_CONTROL_ENABLE;
      }
      else if( iRtsControl == RTS_CONTROL_HANDSHAKE )
      {
         CurDCB.fOutxCtsFlow = 1;
         CurDCB.fRtsControl = RTS_CONTROL_HANDSHAKE;
      }
      else    /* RTS_CONTROL_TOGGLE - RS485? */
      {
         hb_retl( FALSE );
         return;
      }

      s_PortData[ iPort ].iFunction = FCNSETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      if( ! SetCommState( hCommPort, &CurDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
      }
      else
         hb_retl( TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTDTRFLOW )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DCB CurDCB;
      int DtrControl = hb_parni( 2 );

      s_PortData[ iPort ].iFunction = FCNGETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      CurDCB.DCBlength = sizeof( DCB );
      if( ! GetCommState( hCommPort, &CurDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
         return;
      }

      if( DtrControl == DTR_CONTROL_DISABLE )
      {
         CurDCB.fOutxDsrFlow = 0;
         CurDCB.fDtrControl = DTR_CONTROL_DISABLE;
      }
      else if( DtrControl == DTR_CONTROL_ENABLE )
      {
         CurDCB.fOutxDsrFlow = 1;
         CurDCB.fDtrControl = DTR_CONTROL_ENABLE;
      }
      else if( DtrControl == DTR_CONTROL_HANDSHAKE )
      {
         CurDCB.fOutxDsrFlow = 1;
         CurDCB.fDtrControl = DTR_CONTROL_HANDSHAKE;
      }
      else
      {
         hb_retl( FALSE );
         return;
      }

      s_PortData[ iPort ].iFunction = FCNSETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      if( ! SetCommState( hCommPort, &CurDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
      }
      else
         hb_retl( TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTXONXOFFFLOW )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DCB CurDCB;

      s_PortData[ iPort ].iFunction = FCNGETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      CurDCB.DCBlength = sizeof( DCB );
      if( ! GetCommState( hCommPort, &CurDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
         return;
      }

      if( hb_parl( 2 ) )
      {
         CurDCB.fInX = 1;
         CurDCB.fOutX = 1;
      }
      else
      {
         CurDCB.fInX = 0;
         CurDCB.fOutX = 0;
      }

      s_PortData[ iPort ].iFunction = FCNSETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      if( ! SetCommState( hCommPort, &CurDCB ) )
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retl( FALSE );
      }
      else
         hb_retl( TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* You can leave some out. If you pass them by reference you can save the current setting. */

HB_FUNC( WIN_PORTTIMEOUTS )
{
   long lTmp;

   if( HB_ISNUM( 1 ) )
   {
      lTmp = s_lReadIntervalTimeout;
      s_lReadIntervalTimeout = hb_parnl( 1 );
      hb_stornl( lTmp, 1 );
   }
   else
      s_lReadIntervalTimeout = -1;

   if( HB_ISNUM( 2 ) )
   {
      lTmp = s_lReadTotalTimeoutMultiplier;
      s_lReadTotalTimeoutMultiplier = hb_parnl( 2 );
      hb_stornl( lTmp, 2 );
   }
   else
      s_lReadTotalTimeoutMultiplier = -1;

   if( HB_ISNUM( 3 ) )
   {
      lTmp = s_lReadTotalTimeoutConstant;
      s_lReadTotalTimeoutConstant = hb_parnl( 3 );
      hb_stornl( lTmp, 3 );
   }
   else
      s_lReadTotalTimeoutConstant = -1;

   if( HB_ISNUM( 4 ) )
   {
      lTmp = s_lWriteTotalTimeoutMultiplier;
      s_lWriteTotalTimeoutMultiplier = hb_parnl( 4 );
      hb_stornl( lTmp, 4 );
   }
   else
      s_lWriteTotalTimeoutMultiplier = -1;

   if( HB_ISNUM( 5 ) )
   {
      lTmp = s_lWriteTotalTimeoutConstant;
      s_lWriteTotalTimeoutConstant = hb_parnl( 5 );
      hb_stornl( lTmp, 5 );
   }
   else
      s_lWriteTotalTimeoutConstant = -1;
}

HB_FUNC( WIN_PORTERROR )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      hb_retnl( s_PortData[ iPort ].dwError );
      s_PortData[ iPort ].dwError = 0; /* NOTE: reset */
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTFUNCLAST )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
      hb_retni( s_PortData[ iPort ].iFunction );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WIN_PORTDEBUGDCB )
{
   int iPort = hb_parni( 1 );

   if( iPort >= 0 && iPort < ( int ) HB_SIZEOFARRAY( s_PortData ) )
   {
      int iDebugLevel = HB_ISNUM( 2 ) ? hb_parni( 2 ) : WPDBGBASIC;
      HANDLE hCommPort = s_PortData[ iPort ].hPort;
      DCB CurDCB;
      COMMTIMEOUTS CurCOMMTIMEOUTS;
      COMMPROP CurCOMMPROP;
      char szDebugString[ 1024 ] = "";
      char buffer[ 80 ];

      s_PortData[ iPort ].iFunction = FCNGETCOMMSTATE;
      s_PortData[ iPort ].dwError = 0;
      CurDCB.DCBlength = sizeof( DCB );
      if( GetCommState( hCommPort, &CurDCB ) )
      {
         if( iDebugLevel & WPDBGBASIC )
         {
            hb_snprintf( buffer, sizeof( buffer ), "Baud     : %lu\n", CurDCB.BaudRate ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "ByteSize : %i\n" , CurDCB.ByteSize ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "Parity   : %i\n" , CurDCB.Parity   ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "StopBits : %i\n" , CurDCB.StopBits ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
         }
         if( iDebugLevel & WPDBGFLOW )
         {
            hb_strncat( szDebugString, "fRtsControl : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fRtsControl == RTS_CONTROL_DISABLE ? "RTS_CONTROL_DISABLE\n" :
                                       CurDCB.fRtsControl == RTS_CONTROL_ENABLE ? "RTS_CONTROL_ENABLE\n" :
                                       CurDCB.fRtsControl == RTS_CONTROL_HANDSHAKE ? "RTS_CONTROL_HANDSHAKE\n" : "RTS_CONTROL_TOGGLE\n", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, "fOutxCtsFlow : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fOutxCtsFlow ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, "fOutX : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fOutX ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, "fInX : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fInX ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, "fDtrControl : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fDtrControl == DTR_CONTROL_DISABLE ? "DTR_CONTROL_DISABLE\n" :
                                       CurDCB.fDtrControl == DTR_CONTROL_ENABLE ? "DTR_CONTROL_ENABLE\n" : "DTR_CONTROL_HANDSHAKE\n", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, "fOutxDsrFlow : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fOutxDsrFlow ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
         }
         if( iDebugLevel & WPDBGXTRAFLOW )
         {
            hb_strncat( szDebugString, "fDsrSensitivity : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fDsrSensitivity ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, "fTXContinueOnXoff : ", sizeof( szDebugString ) - 1 );
            hb_strncat( szDebugString, CurDCB.fTXContinueOnXoff ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "XonLim : %i\n"    , CurDCB.XonLim   ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "XoffLim : %i\n"   , CurDCB.XoffLim  ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "XonChar : 0x%i\n" , CurDCB.XonChar  ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "XoffChar : 0x%i\n", CurDCB.XoffChar ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
         }
         if( iDebugLevel & WPDBGOTHER )
         {
            hb_strncat(szDebugString, "fBinary : ", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, CurDCB.fBinary ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, "fParity : ", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, CurDCB.fParity ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, "fErrorChar : ", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, CurDCB.fErrorChar ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, "fNull : ", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, CurDCB.fNull ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, "fAbortOnError : ", sizeof( szDebugString ) - 1 );
            hb_strncat(szDebugString, CurDCB.fAbortOnError ? "true\n" : "false\n", sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "ErrorChar : 0x%i\n", CurDCB.ErrorChar ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "EofChar : 0x%i\n"  , CurDCB.EofChar   ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "EvtChar : 0x%i\n"  , CurDCB.EvtChar   ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
         }
      }
      else
      {
         s_PortData[ iPort ].dwError = GetLastError();
         hb_retc_null();
         return;
      }

      if( iDebugLevel & WPDBGTIMEOUTS )
      {
         s_PortData[ iPort ].iFunction = FCNGETCOMMTIMEOUTS;
         s_PortData[ iPort ].dwError = 0;
         if( GetCommTimeouts( hCommPort, &CurCOMMTIMEOUTS ) )
         {
            hb_snprintf( buffer, sizeof( buffer ), "ReadIntervalTimeout : %lu\n"        , CurCOMMTIMEOUTS.ReadIntervalTimeout         ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "ReadTotalTimeoutMultiplier : %ld\n" , CurCOMMTIMEOUTS.ReadTotalTimeoutMultiplier  ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "ReadTotalTimeoutConstant : %ld\n"   , CurCOMMTIMEOUTS.ReadTotalTimeoutConstant    ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "WriteTotalTimeoutMultiplier : %ld\n", CurCOMMTIMEOUTS.WriteTotalTimeoutMultiplier ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "WriteTotalTimeoutConstant : %ld\n"  , CurCOMMTIMEOUTS.WriteTotalTimeoutConstant   ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
         }
         else
         {
            s_PortData[ iPort ].dwError = GetLastError();
            hb_retc_null();
            return;
         }
      }

      if( iDebugLevel & WPDBGQUEUE )
      {
         s_PortData[ iPort ].iFunction = FCNGETCOMMPROPERTIES;
         s_PortData[ iPort ].dwError = 0;
         if( GetCommProperties( hCommPort, &CurCOMMPROP ) )
         {
            hb_snprintf( buffer, sizeof( buffer ), "dwCurrentTxQueue : %lu\n", CurCOMMPROP.dwCurrentTxQueue ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
            hb_snprintf( buffer, sizeof( buffer ), "dwCurrentRxQueue : %lu\n", CurCOMMPROP.dwCurrentRxQueue ) ; hb_strncat( szDebugString, buffer, sizeof( szDebugString ) - 1 );
         }
         else
         {
            s_PortData[ iPort ].dwError = GetLastError();
            hb_retc_null();
            return;
         }
      }

      hb_retc( szDebugString );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
