/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library
 * C low level module for OS/2 serial communication
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

#define INCL_BASE
#define INCL_DOS
#define INCL_DOSERROR
#define INCL_DOSDEVICES
#define INCL_DOSDEVIOCTL

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#if defined( HB_OS_OS2 )

#include "telepath.ch"

#include <os2.h>

HB_FUNC( __TP_INITPORTSPEED )
{
   LINECONTROL lctl;
   DCBINFO dcb;
   USHORT Baud = ( USHORT ) hb_parnl( 2 );
   const char * ptr = hb_parcx( 4 );

   memset( &dcb, 0, sizeof( dcb ) );
   memset( &lctl, 0, sizeof( lctl ) );

   /* OS/2 has Mark and Space parity options */
   switch( *ptr )
   {
      case 'N':
      case 'n':
         lctl.bParity = 0;
         break;
      case 'O':
      case 'o':
         lctl.bParity = 1;
         break;
      case 'E':
      case 'e':
         lctl.bParity = 2;
         break;
      case 'M':
      case 'm':
         lctl.bParity = 3;
         break;
      case 'S':
      case 's':
         lctl.bParity = 4;
   }

   lctl.bDataBits = hb_parnl( 3 );
   lctl.bStopBits = hb_parnl( 5 ) == 1 ? 0 : hb_parnl( 5 );    /* 1 == 1.5 stop bits only valid with 5 data bits */
   lctl.fTransBreak = 0;

   if( DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_SETBAUDRATE, &Baud,
                    sizeof( USHORT ), NULL, NULL, 0L, NULL ) == NO_ERROR )
   {
      if( DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_SETLINECTRL,
                       &lctl, sizeof( LINECONTROL ), NULL, NULL, 0L, NULL ) == NO_ERROR )
      {
         /* tp_ help says: on port open
         DTR      ON   (value 1)
         CTS      OFF
         DCD      IGNORE
         DSR      OFF
         RTS      ON   (value 1)
         XON/XOFF OFF
         */
         dcb.fbCtlHndShake = MODE_DTR_HANDSHAKE | MODE_RTS_HANDSHAKE;

         /* 0x20 == full duplex */
         dcb.fbFlowReplace = MODE_RTS_HANDSHAKE | 0x20;

         dcb.fbTimeout = MODE_NO_WRITE_TIMEOUT | MODE_NOWAIT_READ_TIMEOUT;

         if( DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_SETDCBINFO, &dcb,
                          sizeof(DCBINFO), 0L, NULL, 0L, NULL ) == NO_ERROR )
            hb_retnl( 0 );
         else
            hb_retnl( -3 );
      }
      else
         hb_retnl( -2 );
   }
   else
      hb_retnl( -1 );
}

HB_FUNC( __TP_READPORT )
{
   char Buffer[ 512 ];
   ULONG nRead = 0;
   APIRET rc = DosRead( ( HFILE ) hb_parnl( 1 ), Buffer, sizeof( Buffer ), &nRead );

   hb_retclen( rc == NO_ERROR ? Buffer : NULL, nRead );
}

HB_FUNC( __TP_WRITEPORT )
{
   ULONG nWritten = 0;
   APIRET rc = DosWrite( ( HFILE ) hb_parnl( 1 ), ( void * ) hb_parcx( 2 ), hb_parclen( 2 ), &nWritten );

   hb_retnl( rc == NO_ERROR ? ( long ) nWritten : -1 ); /* Put GetLastError() on error, or better a second byref param? */
}

HB_FUNC( __TP_INFREE )
{
   APIRET rc;
   RXQUEUE rxqueue;

   memset( &rxqueue, 0, sizeof( rxqueue ) );

   if( ( rc = DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_GETINQUECOUNT,
                           NULL, 0L, NULL, &rxqueue, sizeof( RXQUEUE ), NULL ) ) == NO_ERROR )
      hb_retnl( rxqueue.cb - rxqueue.cch );
   else
      hb_retnl( -1 ); /* Put GetLastError() here, or better a second byref param? */
}

HB_FUNC( __TP_OUTFREE )
{
   APIRET rc;
   RXQUEUE rxqueue;

   memset( &rxqueue, 0, sizeof( rxqueue ) );

   if( ( rc = DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_GETOUTQUECOUNT,
                           NULL, 0L, NULL, &rxqueue, sizeof( RXQUEUE ), NULL ) ) == NO_ERROR )
      hb_retnl( rxqueue.cb - rxqueue.cch );
   else
      hb_retnl( -1 ); /* Put GetLastError() here, or better a second byref param? */
}

HB_FUNC( __TP_ISDCD )
{
   BYTE instat;

   /* if DosDevIOCtl() returns an error, return no DCD */
   if( DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_GETMODEMINPUT,
                    NULL, 0, NULL, &instat, sizeof( instat ), NULL ) == NO_ERROR )
      hb_retl( ( instat & DCD_ON ) == DCD_ON );
   else
      hb_retl( FALSE );
}

HB_FUNC( __TP_ISRI )
{
   BYTE instat;

   if( DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_GETMODEMINPUT,
                    NULL, 0, NULL, &instat, sizeof( instat ), NULL ) == NO_ERROR )
      hb_retl( ( instat & RI_ON ) == RI_ON );
   else
      hb_retl( FALSE );
}

HB_FUNC( __TP_ISDSR )
{
   BYTE instat;

   if( DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_GETMODEMINPUT,
                    NULL, 0, NULL, &instat, sizeof( instat ), NULL ) == NO_ERROR )
      hb_retl( ( instat & DSR_ON ) == DSR_ON );
   else
      hb_retl( FALSE );
}

HB_FUNC( __TP_ISCTS )
{
   BYTE instat;

   if( DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_GETMODEMINPUT,
                    NULL, 0, NULL, &instat, sizeof( instat ), NULL ) == NO_ERROR )
      hb_retl( ( instat & CTS_ON ) == CTS_ON );
   else
      hb_retl( FALSE );
}

HB_FUNC( __TP_CTRLCTS )
{
   hb_retni( 0 );
}

#endif /* HB_OS_OS2 */
