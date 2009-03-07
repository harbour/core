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
#include "hbwin.h"

#include <stdlib.h>


/* Waste some space ! */
static struct
{
    HANDLE Port;
    LPCTSTR Name;
} s_PortData[] =
{
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM1"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM2"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM3"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM4"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM5"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM6"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM7"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM8"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM9"  ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM10" ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM11" ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM12" ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM13" ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM14" ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM15" ) },
    { INVALID_HANDLE_VALUE, TEXT( "\\\\.\\COM16" ) }
};

static struct
{
    DCB OldDCB;
    COMMTIMEOUTS OldTimeouts;
} s_PortData2[ 16 ];


static int s_WinFcn = 0;
static DWORD s_WinError = 0;

static int s_ReadIntervalTimeout = -1;            /* -1 says use default calculation */
static int s_ReadTotalTimeoutMultiplier = -1;
static int s_ReadTotalTimeoutConstant = -1;
static int s_WriteTotalTimeoutMultiplier = -1;
static int s_WriteTotalTimeoutConstant = -1;

static int s_InQueue = -1;
static int s_OutQueue = -1;


HB_FUNC( WINPORTOPEN )
{
   int Port = hb_parni( 1 );
   LONG BaudRate = hb_parnl( 2 );
   int Parity = hb_parni( 3 );
   int ByteSize = hb_parni( 4 );
   int StopBits = hb_parni( 5 );
 /*LONG s_InQueue = hb_parnl( 6 ); */
 /*LONG s_OutQueue = hb_parnl( 7 ); */

   HANDLE hCommPort;
   COMMTIMEOUTS NewTimeouts;
   DCB NewDCB;

   s_WinFcn = FCNCREATEFILE;
   s_WinError = 0;
   if( ( hCommPort = CreateFile( s_PortData[ Port ].Name, 
                                 GENERIC_READ | GENERIC_WRITE,
                                 0,
                                 0,
                                 OPEN_EXISTING,
                                 FILE_FLAG_NO_BUFFERING, 0 ) ) == INVALID_HANDLE_VALUE )
   {
      s_WinError = GetLastError();
      hb_retnl( -1 );
      return;
   }

   s_WinFcn = FCNGETCOMMSTATE;
   s_WinError = 0;

   /* We'll put everything back */
   s_PortData2[ Port ].OldDCB.DCBlength = sizeof( DCB );
   if( ! GetCommState( hCommPort, &( s_PortData2[ Port ].OldDCB ) ) )
   {
      s_WinError = GetLastError();
      CloseHandle( hCommPort );
      hb_retnl( -1 );
      return;
   }

   NewDCB.DCBlength = sizeof( DCB );
   if( ! GetCommState( hCommPort, &NewDCB ) )
   {
      s_WinError = GetLastError();
      CloseHandle( hCommPort );
      hb_retnl( -1 );
      return;
   }

   /* Initialised with NO flow control or control signals! */
   NewDCB.BaudRate = ( DWORD ) BaudRate;
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
   NewDCB.ByteSize = ( BYTE ) ByteSize;
   NewDCB.Parity = ( BYTE ) Parity;
   NewDCB.StopBits = ( BYTE ) StopBits;
 /*NewDCB.XonChar*/
 /*NewDCB.XoffChar*/
   NewDCB.ErrorChar = '?';
 /*NewDCB.EofChar*/
 /*NewDCB.EvtChar*/

   /* function reinitializes all hardware and control settings, but it does not empty output or input queues */
   s_WinFcn = FCNSETCOMMSTATE;
   s_WinError = 0;
   if( ! SetCommState( hCommPort, &NewDCB ) )
   {
      s_WinError = GetLastError();
      CloseHandle( hCommPort );
      hb_retnl( -1 );
      return;
   }

   /* We'll leave this to Windows, unless you really want it changed! */
   if( s_InQueue != -1 )
   {
      s_WinFcn = FCNSETUPCOMM;
      s_WinError = 0;
      if( ! SetupComm( hCommPort, s_InQueue, s_OutQueue ) )
      {
         s_WinError = GetLastError();
         CloseHandle( hCommPort );
         hb_retnl( -1 );
         return;
      }
   }

   /* We'll put everything back */
   s_WinFcn = FCNGETCOMMTIMEOUTS;
   s_WinError = 0;
   if( ! GetCommTimeouts( hCommPort, &( s_PortData2[ Port ].OldTimeouts ) ) )
   {
      s_WinError = GetLastError();
      CloseHandle( hCommPort );
      hb_retnl( -1 );
      return;
   }

   /* Maximum time, in milliseconds, allowed to elapse between the arrival of two characters on
      the communications line. During a ReadFile operation, the time period begins when the first
      character is received. If the interval between the arrival of any two characters exceeds this
      amount, the ReadFile operation is completed and any buffered data is returned. A value of zero
      indicates that interval time-outs are not used. */

   /* A value of MAXDWORD, combined with zero values for both the s_ReadTotalTimeoutConstant and
      s_ReadTotalTimeoutMultiplier members, specifies that the read operation is to return
      immediately with the characters that have already been received, even if no characters
      have been received. */
   NewTimeouts.ReadIntervalTimeout = ( s_ReadIntervalTimeout == -1 ? MAXDWORD : ( DWORD ) s_ReadIntervalTimeout );

   /* Multiplier, in milliseconds, used to calculate the total time-out period for read operations.
      For each read operation, this value is multiplied by the requested number of bytes to be read. */
   NewTimeouts.ReadTotalTimeoutMultiplier = ( s_ReadTotalTimeoutMultiplier == -1 ? 0 : s_ReadTotalTimeoutMultiplier );

   /* Constant, in milliseconds, used to calculate the total time-out period for read operations.
      For each read operation, this value is added to the product of the s_ReadTotalTimeoutMultiplier
      member and the requested number of bytes. */
   NewTimeouts.ReadTotalTimeoutConstant = ( s_ReadIntervalTimeout == -1 ? 0 : s_ReadTotalTimeoutConstant );

   /* A value of zero for both the s_ReadTotalTimeoutMultiplier and s_ReadTotalTimeoutConstant members
      indicates that total time-outs are not used for read operations ...
      and MAXDWORD, 0 and 0 are what we use by default */

   /* Multiplier, in milliseconds, used to calculate the total time-out period for write operations.
      For each write operation, this value is multiplied by the number of bytes to be written. */
   if( s_WriteTotalTimeoutMultiplier == -1 )
   {
       /* float of 1.0 makes whole expression float */
       NewTimeouts.WriteTotalTimeoutMultiplier = HB_MIN( 1, ( DWORD ) ( ( 1.0 / BaudRate ) *
           ( ByteSize + 1 + ( Parity == NOPARITY ? 0 : 1 ) + ( StopBits == ONESTOPBIT ? 1 : StopBits == ONE5STOPBITS ? 1.5 : 2 ) ) * 1000 ) );
   }
   /* Constant, in milliseconds, used to calculate the total time-out period for write operations.
      For each write operation, this value is added to the product of the s_WriteTotalTimeoutMultiplier member and the number of bytes to be written. */
   else
       NewTimeouts.WriteTotalTimeoutMultiplier = s_WriteTotalTimeoutMultiplier;

   /* 50 ms is a thumbsuck - seems long enough and not too long! */
   NewTimeouts.WriteTotalTimeoutConstant = s_WriteTotalTimeoutConstant == -1 ? 50 : s_WriteTotalTimeoutConstant;

   /* A value of zero for both the s_WriteTotalTimeoutMultiplier and s_WriteTotalTimeoutConstant members
      indicates that total time-outs are not used for write operations ...
      and if flow control is enabled the program will "hang" or if it is not enabled the data will
      be lost (potentially), so we set a minimum of 1ms (baud rates higher than 4800) */

   s_WinFcn = FCNSETCOMMTIMEOUTS;
   s_WinError = 0;
   if( ! SetCommTimeouts( hCommPort, &NewTimeouts ) )
   {
      s_WinError = GetLastError();
      CloseHandle( hCommPort );
      hb_retnl( -1 );
   }
   else
   {
      s_PortData[ Port ].Port = hCommPort;
      hb_retnl( hCommPort == INVALID_HANDLE_VALUE ? -1 : 0 );
   }
}


HB_FUNC( WINPORTCLOSE )
{
   int Port = hb_parni( 1 );
   long Drain = hb_parni( 2 );
   HANDLE hCommPort = s_PortData[ Port ].Port;

   s_WinFcn = FCNSETCOMMSTATE;
   s_WinError = 0;
   if( ! SetCommState( hCommPort, &( s_PortData2[ Port ].OldDCB ) ) )
   {
      s_WinError = GetLastError();
      CloseHandle( hCommPort );
      hb_retl( FALSE );
      return;
   }

   s_WinFcn = FCNSETCOMMTIMEOUTS;
   s_WinError = 0;
   if( ! SetCommTimeouts( hCommPort, &( s_PortData2[ Port ].OldTimeouts ) ) )
   {
      s_WinError = GetLastError();
      CloseHandle( hCommPort );
      hb_retl( FALSE );
      return;
   }

   s_PortData[ Port ].Port = INVALID_HANDLE_VALUE;

   s_WinFcn = FCNCLOSEHANDLE;
   s_WinError = 0;

   /* I honestly don't know if this helps */
   if( Drain > 0 )
      Sleep( Drain * 1000 );

   hb_retl( CloseHandle( hCommPort ) != 0 );
   s_WinError = GetLastError();
}


HB_FUNC( WINPORTWRITE )
{
   int Port = hb_parni( 1 );
   char * lpBuffer = hb_parcx( 2 );
   LONG NumberofBytesToWrite = hb_parclen( 2 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DWORD NumberofBytesWritten;

   s_WinFcn = FCNWRITEFILE;
   s_WinError = 0;
   if( ! WriteFile( hCommPort, lpBuffer, NumberofBytesToWrite, &NumberofBytesWritten, NULL ) )
   {
      s_WinError = GetLastError();
      hb_retnl( -1 );
   }
   else
      hb_retnl( NumberofBytesWritten );
}


HB_FUNC( WINPORTREAD )
{
   int Port = hb_parni( 1 );
   char * lpBuffer;
   LONG NumberOfBytesToRead = hb_parclen( 2 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DWORD NumberOfBytesRead;

   lpBuffer = ( char * ) hb_xgrab( NumberOfBytesToRead );
   s_WinFcn = FCNREADFILE;
   s_WinError = 0;
   if( ! ReadFile( hCommPort, lpBuffer, NumberOfBytesToRead, &NumberOfBytesRead, NULL ) )
   {
      s_WinError = GetLastError();
      hb_retnl( -1 );
   }
   else
   {
      if( ! hb_storclen_buffer( lpBuffer, NumberOfBytesRead, 2 ) )
         hb_xfree( lpBuffer );
      hb_retnl( NumberOfBytesRead );
   }
}


HB_FUNC( WINPORTSTATUS )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DWORD ModemStat;

   s_WinFcn = FCNGETCOMMMODEMSTATUS;
   s_WinError = 0;
   if( ! GetCommModemStatus( hCommPort, &ModemStat ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
   {
      hb_storl( ( ModemStat & MS_CTS_ON )  != 0, 2 );     /* The CTS (clear-to-send) signal is on. */
      hb_storl( ( ModemStat & MS_DSR_ON )  != 0, 3 );     /* The DSR (data-set-ready) signal is on. */
      hb_storl( ( ModemStat & MS_RING_ON ) != 0, 4 );     /* The ring indicator signal is on. */
      hb_storl( ( ModemStat & MS_RLSD_ON ) != 0, 5 );     /* The RLSD (receive-line-signal-detect) signal is on. Also is DCD. */

      hb_retl( TRUE );
   }
}


HB_FUNC( WINPORTPURGE )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DWORD Flags;

   Flags = ( hb_parl( 2 ) ? PURGE_RXCLEAR : 0 ) | ( hb_parl( 3 ) ? PURGE_TXCLEAR : 0 );
   s_WinFcn = FCNPURGECOMM;
   s_WinError = 0;
   if( ! PurgeComm( hCommPort, Flags ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
      hb_retl( TRUE );
}


HB_FUNC( WINPORTQUEUESTATUS )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DWORD Errors;
   COMSTAT ComStat;

   s_WinFcn = FCNCLEARCOMMERROR;
   s_WinError = 0;
   if( ! ClearCommError( hCommPort, &Errors, &ComStat ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
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
}


/* If handshaking is enabled, it is an error for the application to adjust the line by
   using the EscapeCommFunction function */

HB_FUNC( WINPORTSETRTS )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DWORD Func = hb_parl( 2 ) ? SETRTS : CLRRTS;

   s_WinFcn = ESCAPECOMMFUNCTION;
   s_WinError = 0;
   if( ! EscapeCommFunction( hCommPort, Func ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
      hb_retl( TRUE );
}


/* If handshaking is enabled, it is an error for the application to adjust the line by
   using the EscapeCommFunction function */

HB_FUNC( WINPORTSETDTR )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DWORD Func = hb_parl( 2 ) ? SETDTR : CLRDTR;

   s_WinFcn = ESCAPECOMMFUNCTION;
   s_WinError = 0;
   if( ! EscapeCommFunction( hCommPort, Func ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
      hb_retl( TRUE );
}


HB_FUNC( WINPORTRTSFLOW )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DCB CurDCB;
   int RtsControl = hb_parni( 2 );

   s_WinFcn = FCNGETCOMMSTATE;
   s_WinError = 0;
   CurDCB.DCBlength = sizeof( DCB );
   if( ! GetCommState( hCommPort, &CurDCB ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
      return;
   }

   if( RtsControl == RTS_CONTROL_DISABLE )
   {
      CurDCB.fOutxCtsFlow = 0;
      CurDCB.fRtsControl = RTS_CONTROL_DISABLE;
   }
   else if( RtsControl == RTS_CONTROL_ENABLE )
   {
      CurDCB.fOutxCtsFlow = 1;
      CurDCB.fRtsControl = RTS_CONTROL_ENABLE;
   }
   else if( RtsControl == RTS_CONTROL_HANDSHAKE )
   {
      CurDCB.fOutxCtsFlow = 1;
      CurDCB.fRtsControl = RTS_CONTROL_HANDSHAKE;
   }
   else    /* RTS_CONTROL_TOGGLE - RS485? */
   {
      hb_retl( FALSE );
      return;
   }

   s_WinFcn = FCNSETCOMMSTATE;
   s_WinError = 0;
   if( ! SetCommState( hCommPort, &CurDCB ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
      hb_retl( TRUE );
}


HB_FUNC( WINPORTDTRFLOW )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DCB CurDCB;
   int DtrControl = hb_parni( 2 );

   s_WinFcn = FCNGETCOMMSTATE;
   s_WinError = 0;
   CurDCB.DCBlength = sizeof( DCB );
   if( ! GetCommState( hCommPort, &CurDCB ) )
   {
      s_WinError = GetLastError();
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

   s_WinFcn = FCNSETCOMMSTATE;
   s_WinError = 0;
   if( ! SetCommState( hCommPort, &CurDCB ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
      hb_retl( TRUE );
}


HB_FUNC( WINPORTXONXOFFFLOW )
{
   int Port = hb_parni( 1 );
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DCB CurDCB;

   s_WinFcn = FCNGETCOMMSTATE;
   s_WinError = 0;
   CurDCB.DCBlength = sizeof( DCB );
   if( ! GetCommState( hCommPort, &CurDCB ) )
   {
      s_WinError = GetLastError();
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

   s_WinFcn = FCNSETCOMMSTATE;
   s_WinError = 0;
   if( ! SetCommState( hCommPort, &CurDCB ) )
   {
      s_WinError = GetLastError();
      hb_retl( FALSE );
   }
   else
      hb_retl( TRUE );
}


/* You can leave some out. If you pass them by reference you can save the current setting. */

HB_FUNC( WINPORTTIMEOUTS )
{
   long Tmp;

   if( ISNUM( 1 ) )
   {
      Tmp = s_ReadIntervalTimeout;
      s_ReadIntervalTimeout = hb_parnl( 1 );
      hb_stornl( Tmp, 1 );
   }
   else
      s_ReadIntervalTimeout = -1;

   if( ISNUM( 2 ) )
   {
      Tmp = s_ReadTotalTimeoutMultiplier;
      s_ReadTotalTimeoutMultiplier = hb_parnl( 2 );
      hb_stornl( Tmp, 2 );
   }
   else
      s_ReadTotalTimeoutMultiplier = -1;

   if( ISNUM( 3 ) )
   {
      Tmp = s_ReadTotalTimeoutConstant;
      s_ReadTotalTimeoutConstant = hb_parnl( 3 );
      hb_stornl( Tmp, 3 );
   }
   else
      s_ReadTotalTimeoutConstant = -1;

   if( ISNUM( 4 ) )
   {
      Tmp = s_WriteTotalTimeoutMultiplier;
      s_WriteTotalTimeoutMultiplier = hb_parnl( 4 );
      hb_stornl( Tmp, 4 );
   }
   else
      s_WriteTotalTimeoutMultiplier = -1;

   if( ISNUM( 5 ) )
   {
      Tmp = s_WriteTotalTimeoutConstant;
      s_WriteTotalTimeoutConstant = hb_parnl( 5 );
      hb_stornl( Tmp, 5 );
   }
   else
      s_WriteTotalTimeoutConstant = -1;
}


/* You must set both! */

HB_FUNC( WINPORTBUFFERS )
{
   s_InQueue = hb_parnl( 1 );
   s_OutQueue = hb_parnl( 2 );
}


HB_FUNC( WINPORTERROR )
{
   hb_retnl( s_WinError );
   s_WinError = 0;       /* Note - reset */
}


HB_FUNC( WINPORTFCN )
{
   hb_retni( s_WinFcn );
}

HB_FUNC( FORMATMESSAGE )
{
   char Buffer[ 256 ] = "";
   DWORD Messageid = ISNUM( 1 ) ? ( DWORD ) hb_parnl( 1 ) : GetLastError();

   if( FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM, NULL, Messageid, MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ), /* Default language */
           ( LPTSTR ) Buffer, sizeof( Buffer ), NULL) == 0 )
   {
      hb_snprintf( Buffer, sizeof( Buffer ), "FormatMessage() failed for message %ld.", Messageid );
   }

   hb_retc( Buffer );
}


HB_FUNC( WINPORTDEBUGDCB )
{
   int Port = hb_parni( 1 );
   int DebugLevel = ISNUM(2) ? hb_parni( 2 ) : WPDBGBASIC;
   HANDLE hCommPort = s_PortData[ Port ].Port;
   DCB CurDCB;
   COMMTIMEOUTS CurCOMMTIMEOUTS;
   COMMPROP CurCOMMPROP;
   char DebugString[ 1024 ] = "";
   char Buffer[ 80 ];

   s_WinFcn = FCNGETCOMMSTATE;
   s_WinError = 0;
   CurDCB.DCBlength = sizeof( DCB );
   if( GetCommState( hCommPort, &CurDCB ) )
   {
      if( DebugLevel & WPDBGBASIC )
      {
         hb_snprintf( Buffer, sizeof( Buffer ), "Baud     : %lu\n", CurDCB.BaudRate ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "ByteSize : %i\n" , CurDCB.ByteSize ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "Parity   : %i\n" , CurDCB.Parity   ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "StopBits : %i\n" , CurDCB.StopBits ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
      }
      if( DebugLevel & WPDBGFLOW )
      {
         hb_strncat( DebugString, "fRtsControl : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fRtsControl == RTS_CONTROL_DISABLE ? "RTS_CONTROL_DISABLE\n" :
                                  CurDCB.fRtsControl == RTS_CONTROL_ENABLE ? "RTS_CONTROL_ENABLE\n" :
                                  CurDCB.fRtsControl == RTS_CONTROL_HANDSHAKE ? "RTS_CONTROL_HANDSHAKE\n" : "RTS_CONTROL_TOGGLE\n", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, "fOutxCtsFlow : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fOutxCtsFlow ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, "fOutX : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fOutX ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, "fInX : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fInX ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, "fDtrControl : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fDtrControl == DTR_CONTROL_DISABLE ? "DTR_CONTROL_DISABLE\n" :
                                  CurDCB.fDtrControl == DTR_CONTROL_ENABLE ? "DTR_CONTROL_ENABLE\n" : "DTR_CONTROL_HANDSHAKE\n", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, "fOutxDsrFlow : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fOutxDsrFlow ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
      }
      if( DebugLevel & WPDBGXTRAFLOW )
      {
         hb_strncat( DebugString, "fDsrSensitivity : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fDsrSensitivity ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, "fTXContinueOnXoff : ", sizeof( DebugString ) - 1 );
         hb_strncat( DebugString, CurDCB.fTXContinueOnXoff ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "XonLim : %i\n"    , CurDCB.XonLim   ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "XoffLim : %i\n"   , CurDCB.XoffLim  ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "XonChar : 0x%i\n" , CurDCB.XonChar  ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "XoffChar : 0x%i\n", CurDCB.XoffChar ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
      }
      if( DebugLevel & WPDBGOTHER )
      {
         hb_strncat(DebugString, "fBinary : ", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, CurDCB.fBinary ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, "fParity : ", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, CurDCB.fParity ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, "fErrorChar : ", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, CurDCB.fErrorChar ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, "fNull : ", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, CurDCB.fNull ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, "fAbortOnError : ", sizeof( DebugString ) - 1 );
         hb_strncat(DebugString, CurDCB.fAbortOnError ? "true\n" : "false\n", sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "ErrorChar : 0x%i\n", CurDCB.ErrorChar ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "EofChar : 0x%i\n"  , CurDCB.EofChar   ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "EvtChar : 0x%i\n"  , CurDCB.EvtChar   ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
      }
   }
   else
   {
      s_WinError = GetLastError();
      hb_retc_null();
      return;
   }

   if( DebugLevel & WPDBGTIMEOUTS )
   {
      s_WinFcn = FCNGETCOMMTIMEOUTS;
      s_WinError = 0;
      if( GetCommTimeouts( hCommPort, &CurCOMMTIMEOUTS ) )
      {
         hb_snprintf( Buffer, sizeof( Buffer ), "ReadIntervalTimeout : %lu\n"        , CurCOMMTIMEOUTS.ReadIntervalTimeout         ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "ReadTotalTimeoutMultiplier : %ld\n" , CurCOMMTIMEOUTS.ReadTotalTimeoutMultiplier  ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "ReadTotalTimeoutConstant : %ld\n"   , CurCOMMTIMEOUTS.ReadTotalTimeoutConstant    ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "WriteTotalTimeoutMultiplier : %ld\n", CurCOMMTIMEOUTS.WriteTotalTimeoutMultiplier ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "WriteTotalTimeoutConstant : %ld\n"  , CurCOMMTIMEOUTS.WriteTotalTimeoutConstant   ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
      }
      else
      {
         s_WinError = GetLastError();
         hb_retc_null();
         return;
      }
   }

   if( DebugLevel & WPDBGQUEUE )
   {
      s_WinFcn = FCNGETCOMMPROPERTIES;
      s_WinError = 0;
      if( GetCommProperties( hCommPort, &CurCOMMPROP ) )
      {
         hb_snprintf( Buffer, sizeof( Buffer ), "dwCurrentTxQueue : %lu\n", CurCOMMPROP.dwCurrentTxQueue ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
         hb_snprintf( Buffer, sizeof( Buffer ), "dwCurrentRxQueue : %lu\n", CurCOMMPROP.dwCurrentRxQueue ) ; hb_strncat( DebugString, Buffer, sizeof( DebugString ) - 1 );
      }
      else
      {
         s_WinError = GetLastError();
         hb_retc_null();
         return;
      }
   }

   hb_retc( DebugString );
}
