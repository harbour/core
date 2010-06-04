/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library
 * C low level module for *nix serial communication
 *
 * Copyright 2005 - Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.harbour-project.org
 *
 * Lots of code from http://www.easysw.com/~mike/serial/serial.html
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#if defined( HB_OS_UNIX )

#include "telepath.ch"

#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <sys/ioctl.h>

HB_FUNC( __TP_OPEN )
{
   int fd = open( hb_parcx( 1 ), O_RDWR | O_NOCTTY | O_NDELAY ); /* File descriptor for the port */

   if( fd != -1 )
      fcntl( fd, F_SETFL, 0 );

   hb_retnl( fd );
}

/*
p_InitPortSpeed( aPorts[ nPort, TPFP_HANDLE ] ,;
                 aPorts[ nPort, TPFP_BAUD   ] ,;
                 aPorts[ nPort, TPFP_DBITS  ] ,;
                 aPorts[ nPort, TPFP_PARITY ] ,;
                 aPorts[ nPort, TPFP_SBITS  ] )
*/
HB_FUNC( __TP_INITPORTSPEED )
{
   struct termios options;
   int port = hb_parnl( 1 );
   int baud = B300;
   const char * ptr = hb_parcx( 4 );
   int rc;

   tcgetattr( port, &options );

   /* let's set baud rate */
   switch( hb_parnl( 2 ) )
   {
      case 0:       baud = B0;       break; /* Drop line */
      case 50:      baud = B50;      break;
      case 75:      baud = B75;      break;
      case 110:     baud = B110;     break;
      case 150:     baud = B150;     break;
      case 200:     baud = B200;     break;
      case 300:     baud = B300;     break;
      case 600:     baud = B600;     break;
      case 1200:    baud = B1200;    break;
      case 1800:    baud = B1800;    break;
      case 2400:    baud = B2400;    break;
      case 4800:    baud = B4800;    break;
      case 9600:    baud = B9600;    break;
      case 19200:   baud = B19200;   break;
      case 38400:   baud = B38400;   break;
#if defined( B57600 )
      case 57600:   baud = B57600;   break;
#endif
#if defined( B115200 )
      case 115200:  baud = B115200;  break;
#endif
#if defined( B230400 )
      case 230400:  baud = B230400;  break;
#endif
#ifdef B460800
      case 460800:  baud = B460800;  break;
#endif
#ifdef B500000
      case 500000:  baud = B500000;  break;
#endif
#ifdef B576000
      case 576000:  baud = B576000;  break;
#endif
#ifdef B921600
      case 921600:  baud = B921600;  break;
#endif
   }

   cfsetispeed( &options, baud );
   cfsetospeed( &options, baud );

   /* Enable the receiver and set local mode... */
   options.c_cflag |= ( CLOCAL | CREAD );

#if defined( cfmakeraw )
   /* Raw input from device */
   cfmakeraw( &options );
#else
   options.c_iflag &= ~( IGNBRK | BRKINT | PARMRK | ISTRIP | \
                         INLCR | IGNCR | ICRNL | IXON );
   options.c_oflag &= ~OPOST;
   options.c_lflag &= ~( ECHO | ECHONL | ICANON | ISIG | IEXTEN );
   options.c_cflag &= ~( CSIZE | PARENB );
   options.c_cflag |= CS8;
#endif

   /* Reset data bits ( cfmakeraw() puts it to CS8 ) */
   options.c_cflag &= ~CSIZE;

   /* Data bits */
   if( hb_parni( 3 ) == 8 )
      options.c_cflag |= CS8;
   else
      options.c_cflag |= CS7;

   /* Stop bits */
   if( hb_parni( 5 ) == 1 )
      options.c_cflag &= ~CSTOPB;

   /* Parity, only No, Even, Odd supported */
   switch ( *ptr )
   {
      case 'N':
      case 'n':
         options.c_cflag &= ~PARENB;

         options.c_iflag &= ~(INPCK);   /* disable input parity checking */
         break;

      case 'O':
      case 'o':
         options.c_cflag |= PARENB;
         options.c_cflag |= PARODD;

         options.c_iflag |= INPCK;
         break;

      case 'E':
      case 'e':
         options.c_cflag |= PARENB;
         options.c_cflag &= ~PARODD;

         options.c_iflag |= INPCK;
         break;
   }

   /* Every read() call returns as soon as a char is available OR after 3 tenths of a second */
   options.c_cc[ VMIN ] = 0;
   options.c_cc[ VTIME ] = 3;

   /* Set the new options for the port... */
   rc = tcsetattr( port, TCSAFLUSH, &options );

   hb_retnl( rc );
}

HB_FUNC( __TP_READPORT )
{
   char Buffer[ 512 ];
   int nRead = read( hb_parnl( 1 ), Buffer, sizeof( Buffer ) );

   hb_retclen( nRead > 0 ? Buffer : NULL, nRead );
}

HB_FUNC( __TP_WRITEPORT )
{
   long n = write( hb_parnl( 1 ), hb_parcx( 2 ), hb_parclen( 2 ) );

   hb_retnl( n < 0 ? -1 : n );
}

HB_FUNC( __TP_DRAIN )
{
   hb_retnl( tcdrain( hb_parnl( 1 ) ) );
}

HB_FUNC( __TP_INFREE )
{
   hb_retnl( -1 );
}

HB_FUNC( __TP_OUTFREE )
{
#if 0
   APIRET rc;
   RXQUEUE rxqueue = { 0 };

   if ( ( rc = DosDevIOCtl( ( HFILE ) hb_parnl( 1 ), IOCTL_ASYNC, ASYNC_GETOUTQUECOUNT,
                            NULL, 0L, NULL, &rxqueue, sizeof( RXQUEUE ), NULL ) ) == NO_ERROR )
      hb_retnl( rxqueue.cb - rxqueue.cch );
   else
      hb_retnl( -1 ); /* Put GetLastError() here, or better a second byref param? */
#endif
}

HB_FUNC( __TP_ISDCD )
{
#if defined( TIOCMGET )
   int status;

   if ( ioctl( hb_parnl( 1 ), TIOCMGET, &status ) == 0 )
      hb_retl( ( status & TIOCM_CD ) == TIOCM_CD );
   else
#endif
      hb_retl( HB_FALSE );
}

HB_FUNC( __TP_ISRI )
{
#if defined( TIOCMGET )
   int status;

   if ( ioctl( hb_parnl( 1 ), TIOCMGET, &status ) == 0 )
      hb_retl( ( status & TIOCM_RI ) == TIOCM_RI );
   else
#endif
      hb_retl( HB_FALSE );
}

HB_FUNC( __TP_ISDSR )
{
#if defined( TIOCMGET )
   int status;

   if ( ioctl( hb_parnl( 1 ), TIOCMGET, &status ) == 0 )
      hb_retl( ( status & TIOCM_DSR ) == TIOCM_DSR );
   else
#endif
      hb_retl( HB_FALSE );
}

HB_FUNC( __TP_ISCTS )
{
#if defined( TIOCMGET )
   int status;

   if ( ioctl( hb_parnl( 1 ), TIOCMGET, &status ) == 0 )
      hb_retl( ( status & TIOCM_CTS ) == TIOCM_CTS );
   else
#endif
      hb_retl( HB_FALSE );
}

#if ! defined( CRTSCTS )
   /* if you find compiler which does not support it then please check
    * if such flow control is supported. If yes then check exact value
    * for this switch on given OS and define it only for this compiler
    * and OS
    */
   #if defined( HB_OS_LINUX ) && defined( __WATCOMC__ )
      #define CRTSCTS   020000000000
   #endif
#endif

HB_FUNC( __TP_CTRLCTS )
{
#if defined( CRTSCTS )
   struct termios options;
   int port = hb_parnl( 1 );
   int newvalue = hb_pcount() == 2 ? hb_parnl( 2 ) : -1;
   int curvalue;
   int rc;

   tcgetattr( port, &options );
   curvalue = ( options.c_cflag & CRTSCTS ) == CRTSCTS;

   if( newvalue == 0 )
      options.c_cflag &= ~CRTSCTS;
   else if( newvalue == 1 )
      options.c_cflag |= CRTSCTS;

   if( newvalue >= 0 )
      rc = tcsetattr( port, TCSAFLUSH, &options );

   hb_retni( curvalue ? 1 : 0 );
#else
   int iTODO;
   hb_retni( 0 );
#endif
}

#if 0

/* Inline function moved here from telepath.prg */
HB_FUNC( __TP_CTRLDTR )
{
   double nph = hb_parnd( 1 );
   double nnewval, noldval;
   unsigned int result = 0;

   ioctl( nph, TIOCMGET, &result );

   if( result & TIOCM_DTR )
      noldval = 1;
   else
      noldval = 0;

   if( noldval != nnewval )
   {
      if( nnewval == 0 )
         result &= ~TIOCM_DTR;
      else
         result |= TIOCM_DTR;

      ioctl( nph, TIOCMSET, &result );
   }

   hb_stornd( nnewval, 2 );
   hb_stornd( noldval, 3 );
}

#endif

#endif /* HB_OS_UNIX */
