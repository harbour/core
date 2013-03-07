/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Serial communication functions
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapicom.h"
#include "hbvm.h"
#include "hbinit.h"
#include "hbdate.h"

#if defined( HB_OS_UNIX ) && ( !defined( __WATCOMC__ ) || __WATCOMC__ > 1290 ) && \
    !defined( HB_OS_SYMBIAN ) /* || defined( __DJGPP__ ) */
#  if defined( HB_OS_VXWORKS )
#     if ! defined( HB_HAS_SIOLIB )
#        define HB_HAS_SIOLIB
#     endif
#  else
#     if ! defined( HB_HAS_TERMIOS )
#        define HB_HAS_TERMIOS
#     endif
#  endif
#  if defined( HB_OS_SUNOS )
#     if ! defined( BSD_COMP )
#        define BSD_COMP
#     endif
#  endif
#elif defined( HB_OS_DOS )
#  define HB_HAS_PMCOM
#endif


#if defined( HB_HAS_TERMIOS )
#  include <termios.h>
#  include <fcntl.h>
#  include <sys/ioctl.h>
#  include <unistd.h>
#  include <errno.h>
#  if defined( HB_OS_UNIX )
#     include <sys/time.h>
#     include <sys/types.h>
#  endif
#  if defined( HB_OS_HPUX )
#     include <sys/modem.h>
#  endif
#elif defined( HB_HAS_SIOLIB )
#  include <sioLib.h>
#elif defined( HB_HAS_PMCOM )
#  include "../../src/3rd/hbpmcom/com.h"
#elif defined( HB_OS_WIN )
#  include <windows.h>
#  include "hbwinuni.h"
#elif defined( HB_OS_OS2 )
#  define INCL_BASE
#  define INCL_DOS
#  define INCL_DOSERROR
#  define INCL_DOSDEVICES
#  define INCL_DOSDEVIOCTL
#  include <os2.h>
#endif

typedef struct
{
#if defined( HB_HAS_TERMIOS )
   HB_FHANDLE     fd;
#  if !defined( HB_OS_UNIX )
   HB_MAXINT      rdtimeout;
#  endif
#elif defined( HB_OS_WIN )
   HANDLE         hComm;
   HB_MAXINT      rdtimeout;
   HB_MAXINT      wrtimeout;
#elif defined( HB_OS_OS2 )
   HFILE          hFile;
   USHORT         rdtimeout;
   USHORT         wrtimeout;
#endif
   int            status;
   int            error;
   int            oserr;
   int            port;
   char *         name;
/* struct termios tio; */
}
HB_COM, * PHB_COM;

static HB_COM s_comList[ HB_COM_PORT_MAX ];

static void hb_comCloseAll( void )
{
   int iPort;

   for( iPort = 0; iPort < HB_COM_PORT_MAX; ++iPort )
   {
      if( s_comList[ iPort ].status & HB_COM_OPEN )
         hb_comClose( iPort + 1 );

      if( s_comList[ iPort ].name )
      {
         hb_xfree( s_comList[ iPort ].name );
         s_comList[ iPort ].name = NULL;
      }
   }
}

static void hb_comSetComError( PHB_COM pCom, int iError )
{
   pCom->error = iError;
   pCom->oserr = 0;
}

static PHB_COM hb_comGetPort( int iPort, int iStatus )
{
   if( iPort >= 1 && iPort <= HB_COM_PORT_MAX )
   {
      PHB_COM pCom = &s_comList[ iPort - 1 ];
      if( iStatus == HB_COM_ANY || ( iStatus & pCom->status ) != 0 )
         return pCom;
      if( iStatus & HB_COM_ENABLED )
         hb_comSetComError( pCom, HB_COM_ERR_WRONGPORT );
      else
         hb_comSetComError( pCom, HB_COM_ERR_CLOSED );
   }
   return NULL;
}

static const char * hb_comGetName( PHB_COM pCom, char * buffer, int size )
{
   const char * name = pCom->name;

   if( name == NULL )
   {
#if defined( HB_OS_UNIX )
#  if defined( HB_OS_SUNOS )
      hb_snprintf( buffer, size, "/dev/tty%c", pCom->port + 'a' - 1 );
#  elif defined( HB_OS_HPUX )
      hb_snprintf( buffer, size, "/dev/tty%dp0", pCom->port );
#  elif defined( HB_OS_AIX )
      hb_snprintf( buffer, size, "/dev/tty%d", pCom->port );
#  elif defined( HB_OS_IRIX )
      hb_snprintf( buffer, size, "/dev/ttyf%d", pCom->port );
#  elif defined( HB_OS_DIGITAL_UNIX )
      hb_snprintf( buffer, size, "/dev/ttyf%02d", pCom->port );
#  elif defined( HB_OS_DARWIN )
      hb_snprintf( buffer, size, "/dev/cuaa%d", pCom->port - 1 );
#  elif defined( HB_OS_MINIX )
      hb_snprintf( buffer, size, "/dev/tty%02d", pCom->port - 1 );
#  else /* defined( HB_OS_LINUX ) || defined( HB_OS_CYGWIN ) || ... */
      hb_snprintf( buffer, size, "/dev/ttyS%d", pCom->port - 1 );
#  endif
#else
      if( hb_iswinnt() )
         hb_snprintf( buffer, size, "\\\\.\\COM%d", pCom->port );
      else
         hb_snprintf( buffer, size, "COM%d", pCom->port );
#endif
      name = buffer;
   }
   return name;
}

const char * hb_comGetDevice( int iPort, char * buffer, int size )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );
   const char * pszName = NULL;

   if( pCom )
   {
      if( buffer && size > 0 )
         pszName = hb_comGetName( pCom, buffer, size );
      else
         pszName = pCom->name;
   }

   return pszName;
}

int hb_comSetDevice( int iPort, const char * szDevName )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   if( pCom )
   {
      if( pCom->name )
         hb_xfree( pCom->name );
      pCom->name = szDevName ? hb_strdup( szDevName ) : NULL;
   }

   return pCom ? 0 : -1;
}

HB_FHANDLE hb_comGetDeviceHandle( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );
   HB_FHANDLE hFile = FS_ERROR;

   if( pCom )
   {
#if defined( HB_HAS_TERMIOS )
      hFile = pCom->fd;
#elif defined( HB_OS_WIN )
      hFile = ( HB_FHANDLE ) pCom->hComm;
#elif defined( HB_OS_OS2 )
      hFile = ( HB_FHANDLE ) pCom->hFile;
#endif
   }

   return hFile;
}

void hb_comSetError( int iPort, int iError )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   if( pCom )
      pCom->error = iError;
}

int hb_comGetError( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   return pCom ? pCom->error : HB_COM_ERR_WRONGPORT;
}

int hb_comGetOsError( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   return pCom ? pCom->oserr : 0;
}

int hb_comLastNum( void )
{
   int iPort;

   for( iPort = HB_COM_PORT_MAX; iPort; --iPort )
   {
      if( s_comList[ iPort - 1 ].status & HB_COM_ENABLED )
         break;
   }
   return iPort;
}

#if defined( HB_HAS_TERMIOS )

#define HB_COM_IS_EINTR()  ( errno == EINTR )
#define HB_COM_IS_EBADF()  ( errno == EBADF )
#define HB_COM_GETERROR()  ( errno )

#if defined( HB_OS_LINUX )
#  define HB_HAS_SELECT_TIMER
#endif

static void hb_comSetOsError( PHB_COM pCom, HB_BOOL fError )
{
   pCom->oserr = fError ? HB_COM_GETERROR() : 0;
   switch( pCom->oserr )
   {
      case 0:
         pCom->error = 0;
         break;
      case EIO:
         pCom->error = HB_COM_ERR_IO;
         break;
      case EPIPE:
         pCom->error = HB_COM_ERR_PIPE;
         break;
      case EBUSY:
         pCom->error = HB_COM_ERR_BUSY;
         break;
      case EAGAIN:
         pCom->error = HB_COM_ERR_TIMEOUT;
         break;
      case EACCES:
#if defined( ETXTBSY )
      case ETXTBSY:
#endif
#if defined( EPERM )
      case EPERM:
#endif
         pCom->error = HB_COM_ERR_ACCESS;
         break;
      case ENOTTY:
      case ENOENT:
#if defined( ENOTDIR )
      case ENOTDIR:
#endif
         pCom->error = HB_COM_ERR_NOCOM;
         break;
      default:
         pCom->error = HB_COM_ERR_OTHER;
         break;
   }
}

#if defined( HB_OS_UNIX )
static int hb_comCanRead( PHB_COM pCom, HB_MAXINT timeout )
{
   struct timeval tv;
   fd_set rfds;
   int iResult;

#if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();
#else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#endif

   for( ;; )
   {
      FD_ZERO( &rfds );
      FD_SET( pCom->fd, &rfds );

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#if ! defined( HB_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#endif

      iResult = select( ( int ) ( pCom->fd + 1 ), &rfds, NULL, NULL, &tv );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 && timeout < 0 )
         continue;
      if( iResult != -1 || timeout == 0 || ! HB_COM_IS_EINTR() ||
          hb_vmRequestQuery() != 0 )
         break;
#if ! defined( HB_HAS_SELECT_TIMER )
      else if( timeout > 0 )
      {
         HB_MAXUINT timecurr = hb_dateMilliSeconds();
         if( timecurr > timer )
         {
            if( ( timeout -= timecurr - timer ) <= 0 )
               break;
            timer = timecurr;
         }
      }
#endif
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( pCom->fd, &rfds ) ? 1 : 0 );
}

static int hb_comCanWrite( PHB_COM pCom, HB_MAXINT timeout )
{
   struct timeval tv;
   fd_set wfds;
   int iResult;

#if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();
#else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#endif

   for( ;; )
   {
      FD_ZERO( &wfds );
      FD_SET( pCom->fd, &wfds );

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#if ! defined( HB_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#endif

      iResult = select( ( int ) ( pCom->fd + 1 ), NULL, &wfds, NULL, &tv );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 && timeout < 0 )
         continue;
      if( iResult != -1 || timeout == 0 || ! HB_COM_IS_EINTR() ||
          hb_vmRequestQuery() != 0 )
         break;
#if ! defined( HB_HAS_SELECT_TIMER )
      else if( timeout > 0 )
      {
         HB_MAXUINT timecurr = hb_dateMilliSeconds();
         if( timecurr > timer )
         {
            if( ( timeout -= timecurr - timer ) <= 0 )
               break;
            timer = timecurr;
         }
      }
#endif
      break;
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( pCom->fd, &wfds ) ? 1 : 0 );
}
#endif

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
#if defined( TIOCINQ )
      int iResult = ioctl( pCom->fd, TIOCINQ, &iCount );
      if( iResult == -1 )
         iCount = 0;
      hb_comSetOsError( pCom, iResult == -1 );
#elif defined( FIONREAD ) && ! defined( HB_OS_CYGWIN )
      /* Cygwin sys/termios.h explicitly says that "TIOCINQ is
       * utilized instead of FIONREAD which has been accupied for
       * other purposes under CYGWIN", so don't give Cygwin
       * even a chance to hit this code path. */
      int iResult = ioctl( pCom->fd, FIONREAD, &iCount );
      if( iResult == -1 )
         iCount = 0;
      hb_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCINQ;
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }
   else
      iCount = -1;

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
#if defined( TIOCOUTQ )
      int iResult = ioctl( pCom->fd, TIOCOUTQ, &iCount );
      if( iResult == -1 )
         iCount = 0;
      hb_comSetOsError( pCom, iResult == -1 );
#elif defined( FIONWRITE )
      int iResult = ioctl( pCom->fd, FIONWRITE, &iCount );
      if( iResult == -1 )
         iCount = 0;
      hb_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCOUTQ;
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }
   else
      iCount = -1;

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      switch( iType )
      {
         case HB_COM_IFLUSH:
            iResult = tcflush( pCom->fd, TCIFLUSH );
            hb_comSetOsError( pCom, iResult == -1 );
            break;
         case HB_COM_OFLUSH:
            iResult = tcflush( pCom->fd, TCOFLUSH );
            hb_comSetOsError( pCom, iResult == -1 );
            break;
         case HB_COM_IOFLUSH:
            iResult = tcflush( pCom->fd, TCIOFLUSH );
            hb_comSetOsError( pCom, iResult == -1 );
            break;
         default:
            iResult = -1;
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
            break;
      }
   }
   return iResult;
}

/*
   TIOCM_LE          DSR (data set ready/line enable)
   TIOCM_DTR         DTR (data terminal ready)
   TIOCM_RTS         RTS (request to send)
   TIOCM_ST          Secondary TXD (transmit)
   TIOCM_SR          Secondary RXD (receive)
   TIOCM_CTS         CTS (clear to send)
   TIOCM_CAR         DCD (data carrier detect)
   TIOCM_CD           see TIOCM_CAR
   TIOCM_RNG         RNG (ring)
   TIOCM_RI           see TIOCM_RNG
   TIOCM_DSR         DSR (data set ready)

   supported only by few platforms (i.e. newer Linux kernels >= 2.4)
   TIOCM_OUT1        OUT 1 (auxiliary user-designated output 2)
   TIOCM_OUT2        OUT 2 (auxiliary user-designated output 1)
   TIOCM_LOOP        LOOP (loopback mode)
 */

#ifdef HB_OS_LINUX
   /* hack for missing defintions in standard header files */
#  ifndef TIOCM_OUT1
#     define TIOCM_OUT1    0x2000
#  endif
#  ifndef TIOCM_OUT2
#     define TIOCM_OUT2    0x4000
#  endif
#  ifndef TIOCM_LOOP
#     define TIOCM_LOOP    0x8000
#  endif
#endif

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#if defined( TIOCMGET ) && defined( TIOCMSET )
      int iRawVal, iOldVal;

      iResult = ioctl( pCom->fd, TIOCMGET, &iRawVal );
      if( iResult == 0 )
      {
         if( iRawVal & TIOCM_DTR )
            iValue |= HB_COM_MCR_DTR;
         if( iRawVal & TIOCM_RTS )
            iValue |= HB_COM_MCR_RTS;
#ifdef TIOCM_OUT1
         if( iRawVal & TIOCM_OUT1 )
            iValue |= HB_COM_MCR_OUT1;
#endif
#ifdef TIOCM_OUT2
         if( iRawVal & TIOCM_OUT2 )
            iValue |= HB_COM_MCR_OUT2;
#endif
#ifdef TIOCM_LOOP
         if( iRawVal & TIOCM_LOOP )
            iValue |= HB_COM_MCR_LOOP;
#endif

         iOldVal = iRawVal;

         if( iSet & HB_COM_MCR_DTR )
            iRawVal |= TIOCM_DTR;
         else if( iClr & HB_COM_MCR_DTR )
            iRawVal &= ~TIOCM_DTR;

         if( iSet & HB_COM_MCR_RTS )
            iRawVal |= TIOCM_RTS;
         else if( iClr & HB_COM_MCR_RTS )
            iRawVal &= ~TIOCM_RTS;

#ifdef TIOCM_OUT1
         if( iSet & HB_COM_MCR_OUT1 )
            iRawVal |= TIOCM_OUT1;
         else if( iClr & HB_COM_MCR_OUT1 )
            iRawVal &= ~TIOCM_OUT1;
#endif
#ifdef TIOCM_OUT2
         if( iSet & HB_COM_MCR_OUT2 )
            iRawVal |= TIOCM_OUT2;
         else if( iClr & HB_COM_MCR_OUT2 )
            iRawVal &= ~TIOCM_OUT2;
#endif
#ifdef TIOCM_LOOP
         if( iSet & HB_COM_MCR_LOOP )
            iRawVal |= TIOCM_LOOP;
         else if( iClr & HB_COM_MCR_LOOP )
            iRawVal &= ~TIOCM_LOOP;
#endif

         if( iRawVal != iOldVal )
            iResult = ioctl( pCom->fd, TIOCMSET, &iRawVal );
      }
      hb_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCMGET_MCR;
      HB_SYMBOL_UNUSED( iClr );
      HB_SYMBOL_UNUSED( iSet );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#if defined( TIOCMGET ) && defined( TIOCMSET )
      int iRawVal;

      iResult = ioctl( pCom->fd, TIOCMGET, &iRawVal );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         if( iRawVal & TIOCM_CTS )
            iValue |= HB_COM_MSR_CTS;
         if( iRawVal & TIOCM_DSR )
            iValue |= HB_COM_MSR_DSR;
         if( iRawVal & TIOCM_RI )
            iValue |= HB_COM_MSR_RI;
         if( iRawVal & TIOCM_CD )
            iValue |= HB_COM_MSR_DCD;
      }
#else
      int TODO_TIOCMGET_MSR;
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#ifdef TIOCSERGETLSR
      iResult = ioctl( pCom->fd, TIOCSERGETLSR, &iValue );
      hb_comSetOsError( pCom, iResult == -1 );
#else
      /* NOTE: most of systems do not give access to the
       *       Line Status Register (LSR)
       */
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: duration is implementation defined non portable extension
       *       we use 0 what means 'transmit zero-valued bits for at
       *       least 0.25 seconds, and not more that 0.5 seconds'
       */
      HB_SYMBOL_UNUSED( iDurationInMilliSecs );

      hb_vmUnlock();

      iResult = tcsendbreak( pCom->fd, 0 );
      hb_comSetOsError( pCom, iResult == -1 );

      hb_vmLock();
   }
   return iResult;
}

#if defined( CCTS_OFLOW ) && defined( CRTS_IFLOW )
   #define _HB_OCRTSCTS       CCTS_OFLOW
   #define _HB_ICRTSCTS       CRTS_IFLOW
#elif defined( CRTSCTS ) && defined( CRTSXOFF )
   #define _HB_OCRTSCTS       CRTSCTS
   #define _HB_ICRTSCTS       CRTSXOFF
#elif defined( CRTSCTS )
   #define _HB_OCRTSCTS       CRTSCTS
   #define _HB_ICRTSCTS       CRTSCTS
#elif defined( CNEW_RTSCTS )
   #define _HB_OCRTSCTS       CNEW_RTSCTS
   #define _HB_ICRTSCTS       CNEW_RTSCTS
#elif defined( CCTS_OFLOW )
   #define _HB_OCRTSCTS       CCTS_OFLOW
   #define _HB_ICRTSCTS       0
#elif defined( CRTS_IFLOW )
   #define _HB_OCRTSCTS       0
   #define _HB_ICRTSCTS       CCTS_IFLOW
#elif defined( CRTSXOFF )
   #define _HB_OCRTSCTS       0
   #define _HB_ICRTSCTS       CRTSXOFF
#else
   /* if you find compiler which does not support it then please check
    * if such flow control is supported by OS. If yes then check exact
    * value for this switch on given OS and define it only for given
    * compiler and OS
    */
   #if defined( HB_OS_LINUX ) && defined( __WATCOMC__ )
      #define _HB_OCRTSCTS    020000000000
      #define _HB_ICRTSCTS    020000000000
   #endif
#endif

int hb_comFlowControl( int iPort, int * piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      /* NOTE: there is no support for DTR/DSR so we cannot use
       *       DTR/DSR handshake instead of the RTS/CTS handshake
       *       BSD systems support MDMBUF flags which enable output
       *       flow control using CD (Carrier Detect) flag.
       *       In SunOS TIOCSSOFTCAR can be used to control CLOCAL flag.
       *       CLOCAL termios structure c_cflag can be used to enable CD
       *       flow control in most portable way.
       */
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         tcflag_t c_cflag = tio.c_cflag;
         tcflag_t c_iflag = tio.c_iflag;

#if defined( _HB_OCRTSCTS )
         if( ( tio.c_cflag & _HB_OCRTSCTS ) == _HB_OCRTSCTS )
            iValue |= HB_COM_FLOW_ORTSCTS;
         if( ( tio.c_cflag & _HB_ICRTSCTS ) == _HB_ICRTSCTS )
            iValue |= HB_COM_FLOW_IRTSCTS;

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_ORTSCTS )
               tio.c_cflag |= _HB_OCRTSCTS;
            else
               tio.c_cflag &= ~_HB_OCRTSCTS;
            if( iFlow & HB_COM_FLOW_IRTSCTS )
               tio.c_cflag |= _HB_ICRTSCTS;
            else
               tio.c_cflag &= ~_HB_ICRTSCTS;
         }
#else
         {
            int TODO_CRTSCTS;
         }
#endif

         if( ( tio.c_cflag & CLOCAL ) != CLOCAL )
            iValue |= HB_COM_FLOW_DCD;

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_DCD )
               tio.c_cflag &= ~CLOCAL;
            else
               tio.c_cflag |= CLOCAL;
         }


         if( ( tio.c_iflag & IXON ) == IXON )
            iValue |= HB_COM_FLOW_XON;
         if( ( tio.c_iflag & IXOFF ) == IXOFF )
            iValue |= HB_COM_FLOW_XOFF;

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_XON )
               tio.c_iflag |= IXON;
            else
               tio.c_iflag &= ~IXON;
            if( iFlow & HB_COM_FLOW_XOFF )
               tio.c_iflag |= IXOFF;
            else
               tio.c_iflag &= ~IXOFF;
         }

         if( c_cflag != tio.c_cflag || c_iflag != tio.c_iflag )
         {
            iResult = tcsetattr( pCom->fd, TCSANOW, &tio );
            hb_comSetOsError( pCom, iResult == -1 );
         }
      }
   }

   if( piFlow )
      *piFlow = iValue;

   return iResult;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: HB_COM_FL_SOFT is ignored, we assume that user chose
       *       correct hardware/software flow control type which is
       *       the same as set in terminal device parameters
       */
      if( iFlow & HB_COM_FL_OON )
         iResult = tcflow( pCom->fd, TCOON );
      else if( iFlow & HB_COM_FL_OOFF )
         iResult = tcflow( pCom->fd, TCOOFF );
      else
         iResult = 0;

      if( iFlow & HB_COM_FL_ION )
      {
         if( tcflow( pCom->fd, TCION ) == -1 )
            iResult = -1;
      }
      else if( iFlow & HB_COM_FL_IOFF )
      {
         if( tcflow( pCom->fd, TCIOFF ) == -1 )
            iResult = -1;
      }
      hb_comSetOsError( pCom, iResult == -1 );
   }

   return iResult;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = 0;
      if( iXONchar >= 0 || iXOFFchar >= 0 )
      {
         struct termios tio;

         iResult = tcgetattr( pCom->fd, &tio );
         if( iResult == 0 )
         {
            if( iXONchar >= 0 )
               tio.c_cc[ VSTART ] = iXONchar;
            if( iXOFFchar >= 0 )
               tio.c_cc[ VSTOP ] = iXOFFchar;
            iResult = tcsetattr( pCom->fd, TCSANOW, &tio );
         }
      }
      hb_comSetOsError( pCom, iResult == -1 );
   }
   return iResult;
}

#if ! defined( _POSIX_VDISABLE )
#  define _POSIX_VDISABLE  '\0'
#endif

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
#if defined( VDISCARD ) && defined( IEXTEN )
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         if( ( tio.c_lflag & IEXTEN ) != 0 &&
             tio.c_cc[ VDISCARD ] != _POSIX_VDISABLE )
            iResult = 1;

         if( iChar == -1 ? iResult != 0 :
             ( iResult == 0 || tio.c_cc[ VDISCARD ] != iChar ) )
         {
            if( iChar == -1 )
            {
               tio.c_lflag &= ~IEXTEN;
               tio.c_cc[ VDISCARD ] = _POSIX_VDISABLE;
            }
            else
            {
               tio.c_lflag |= IEXTEN;
               tio.c_cc[ VDISCARD ] = iChar;
#if defined( VLNEXT )
               tio.c_cc[ VLNEXT ] = _POSIX_VDISABLE;
#endif
            }
            if( tcsetattr( pCom->fd, TCSANOW, &tio ) == -1 )
            {
               hb_comSetOsError( pCom, HB_TRUE );
               iResult = -1;
            }
         }
      }
#else
      int TODO_VDISCARD;
      HB_SYMBOL_UNUSED( iChar );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   return iResult;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: there is no support for setting user defined error character
       */

      HB_SYMBOL_UNUSED( iChar );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int hb_comOutputState( int iPort )
{
   /* NOTE: checking HB_COM_TX_* output flow states is unsupported */
   int iResult = hb_comOutputCount( iPort );

   if( iResult == 0 )
      iResult = HB_COM_TX_EMPTY;
   else if( iResult > 0 )
      iResult = 0;

   return iResult;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: checking HB_COM_RX_* input flow states is unsupported */
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      hb_vmUnlock();

#if defined( HB_OS_UNIX )
      if( timeout >= 0 )
      {
         lSent = hb_comCanWrite( pCom, timeout );
         if( lSent == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lSent = -1;
         }
      }
      else
         lSent = 0;
#else
      /* NOTE: write timeout is unsupported */
      HB_SYMBOL_UNUSED( timeout );
      lSent = 0;
#endif

      if( lSent >= 0 )
      {
         do
         {
            lSent = write( pCom->fd, data, len );
            hb_comSetOsError( pCom, lSent == -1 );
         }
         while( lSent == -1 && HB_COM_IS_EINTR() && hb_vmRequestQuery() == 0 );
      }
      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      hb_vmUnlock();

#if defined( HB_OS_UNIX )
      if( timeout >= 0 )
      {
         lReceived = hb_comCanRead( pCom, timeout );
         if( lReceived == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lReceived = -1;
         }
      }
      else
         lReceived = 0;
#else
      if( timeout != pCom->rdtimeout )
      {
         /* TODO: implent timeout settings
          *          tio.c_cc[ VTIME ] = ( timeout + 50 ) / 100;
          *          tio.c_cc[ VMIN ]  = 0;
          *       in DJGPP builds
          */
      }
      lReceived = 0;
#endif

      if( lReceived >= 0 )
      {
         do
         {
            lReceived = read( pCom->fd, ( char * ) data, len );
            hb_comSetOsError( pCom, lReceived == -1 );
         }
         while( lReceived == -1 && HB_COM_IS_EINTR() && hb_vmRequestQuery() == 0 );
      }
      hb_vmLock();
   }

   return lReceived;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
#if defined( cfmakeraw ) || defined( HB_OS_LINUX )
         /* Raw input from device */
         cfmakeraw( &tio );
#endif
         tio.c_iflag &= ~( IGNBRK | IGNPAR | BRKINT | PARMRK | ISTRIP |
                           INLCR | IGNCR | ICRNL | IXON | IXANY | IXOFF );
         tio.c_oflag &= ~OPOST;
         tio.c_lflag &= ~( ECHO | ECHONL | ICANON | ISIG | IEXTEN );
         tio.c_cflag &= ~( CSIZE | PARENB );
         /* Enable the receiver and set local mode... */
         tio.c_cflag |= ( CLOCAL | CREAD );

         tio.c_cc[ VTIME ] = 0;  /* inter-character timer in 1/10 sec. */
         tio.c_cc[ VMIN ]  = 0;  /* minimum number of characters for read */

         if( iBaud )
         {
            switch( iBaud )
            {
               case        0: iBaud =      B0; break;
               case       50: iBaud =     B50; break;
               case       75: iBaud =     B75; break;
               case      110: iBaud =    B110; break;
               case      150: iBaud =    B150; break;
               case      200: iBaud =    B200; break;
               case      300: iBaud =    B300; break;
               case      600: iBaud =    B600; break;
               case     1200: iBaud =   B1200; break;
               case     1800: iBaud =   B1800; break;
               case     2400: iBaud =   B2400; break;
               case     4800: iBaud =   B4800; break;
               case     9600: iBaud =   B9600; break;
               case    19200: iBaud =  B19200; break;
               case    38400: iBaud =  B38400; break;
#ifdef B57600
               case    57600: iBaud =  B57600; break;
#endif
#ifdef B115200
               case   115200: iBaud = B115200; break;
#endif
#ifdef B230400
               case   230400: iBaud = B230400; break;
#endif
#ifdef B460800
               case   460800: iBaud = B460800; break;
#endif
#ifdef B500000
               case   500000: iBaud = B500000; break;
#endif
#ifdef B576000
               case   576000: iBaud = B576000; break;
#endif
#ifdef B921600
               case   921600: iBaud = B921600; break;
#endif
               default:
                  iResult = -1;
            }
         }
         switch( iParity )
         {
            case 0:
            case 'N':
            case 'n':
               tio.c_cflag &= ~( PARENB | PARODD );
               tio.c_iflag &= ~INPCK;
               break;
            case 'E':
            case 'e':
               tio.c_cflag |= PARENB;
               tio.c_cflag &= ~PARODD;
               tio.c_iflag |= INPCK;
               break;
            case 'O':
            case 'o':
               tio.c_cflag |= PARENB | PARODD;
               tio.c_iflag |= INPCK;
               break;
#if defined( CMSPAR )
            case 'S':
            case 's':
               tio.c_cflag |= CMSPAR | PARENB;
               tio.c_cflag &= ~PARODD;
               tio.c_iflag |= INPCK;
               break;
            case 'M':
            case 'm':
               tio.c_cflag |= CMSPAR | PARENB | PARODD;
               tio.c_iflag |= INPCK;
               break;
#endif
            default:
               iResult = -1;
         }
         switch( iSize )
         {
            case 0:
            case 8: tio.c_cflag |= CS8; break;
            case 7: tio.c_cflag |= CS7; break;
            case 6: tio.c_cflag |= CS6; break;
            case 5: tio.c_cflag |= CS5; break;
            default:
               iResult = -1;
         }
         switch( iStop )
         {
            case 0:
            case 1: tio.c_cflag &= ~CSTOPB; break;
            case 2: tio.c_cflag |= CSTOPB; break;
            default:
               iResult = -1;
         }

         if( iResult == 0 )
         {
            if( iBaud )
            {
               cfsetispeed( &tio, iBaud );
               cfsetospeed( &tio, iBaud );
            }

            iResult = tcsetattr( pCom->fd, TCSAFLUSH, &tio );
#if ! defined( HB_OS_UNIX )
            if( iResult == 0 )
               pCom->rdtimeout = 0;
#endif
            hb_comSetOsError( pCom, iResult == -1 );
         }
         else
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
      }
   }

   return iResult;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      hb_vmUnlock();
#if defined( TIOCNXCL )
      ioctl( pCom->fd, TIOCNXCL );
#endif
      do
      {
         iResult = close( pCom->fd );
         hb_comSetOsError( pCom, iResult == -1 );
      }
      while( iResult == -1 && HB_COM_IS_EINTR() && hb_vmRequestQuery() == 0 );

      if( iResult != -1 || HB_COM_IS_EBADF() )
      {
         pCom->fd = ( HB_FHANDLE ) FS_ERROR;
         pCom->status &= ~HB_COM_OPEN;
      }
      hb_vmLock();
   }

   return iResult;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   int iResult = -1;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         char buffer[ HB_COM_DEV_NAME_MAX ];
         const char * name = hb_comGetName( pCom, buffer, sizeof( buffer ) );

         hb_vmUnlock();

         pCom->fd = open( name, O_RDWR | O_NOCTTY );
         if( pCom->fd != -1 )
         {
#if defined( TIOCEXCL ) /* TIOCNXCL */
            iResult = ioctl( pCom->fd, TIOCEXCL );
            if( iResult != 0 )
            {
               close( pCom->fd );
               pCom->fd = -1;
               hb_comSetComError( pCom, HB_COM_ERR_BUSY );
            }
            else
#else
            iResult = 0;
#endif
            pCom->status |= HB_COM_OPEN;
         }
         hb_comSetOsError( pCom, iResult == -1 );

         hb_vmLock();
      }
   }

   return iResult;
}

/* end of HB_HAS_TERMIOS */

#elif defined( HB_OS_WIN )

static void hb_comSetOsError( PHB_COM pCom, BOOL fError )
{
   pCom->oserr = fError ? GetLastError() : 0;

   switch( pCom->oserr )
   {
      case 0:
         pCom->error = 0;
         break;
      case ERROR_TIMEOUT:
         pCom->error = HB_COM_ERR_TIMEOUT;
         break;
      case ERROR_ACCESS_DENIED:
      case ERROR_SHARING_VIOLATION:
         pCom->error = HB_COM_ERR_BUSY;
         break;
      case ERROR_FILE_NOT_FOUND:
      case ERROR_PATH_NOT_FOUND:
         pCom->error = HB_COM_ERR_NOCOM;
         break;
      default:
         pCom->error = HB_COM_ERR_OTHER;
         break;
   }
}

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
      COMSTAT comStat;

      if( ClearCommError( pCom->hComm, NULL, &comStat ) )
      {
         iCount = comStat.cbInQue;
         hb_comSetOsError( pCom, HB_FALSE );
      }
      else
         hb_comSetOsError( pCom, HB_TRUE );
   }
   else
      iCount = -1;

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
      COMSTAT comStat;

      if( ClearCommError( pCom->hComm, NULL, &comStat ) )
      {
         iCount = comStat.cbOutQue;
         hb_comSetOsError( pCom, HB_FALSE );
      }
      else
         hb_comSetOsError( pCom, HB_TRUE );
   }
   else
      iCount = -1;

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      switch( iType )
      {
         case HB_COM_IFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_RXCLEAR );
            hb_comSetOsError( pCom, ! fResult );
            break;
         case HB_COM_OFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_TXCLEAR );
            hb_comSetOsError( pCom, ! fResult );
            break;
         case HB_COM_IOFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_TXCLEAR | PURGE_RXCLEAR );
            hb_comSetOsError( pCom, ! fResult );
            break;
         default:
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
            break;
      }
   }
   return fResult ? 0 : -1;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      if( iSet & HB_COM_MCR_DTR )
         fResult = EscapeCommFunction( pCom->hComm, SETDTR );
      else if( iClr & HB_COM_MCR_DTR )
         fResult = EscapeCommFunction( pCom->hComm, CLRDTR );

      if( iSet & HB_COM_MCR_RTS )
         fResult = EscapeCommFunction( pCom->hComm, SETRTS );
      else if( iClr & HB_COM_MCR_RTS )
         fResult = EscapeCommFunction( pCom->hComm, CLRRTS );

      /* MCR_OUT1, MCR_OUT2, MCR_LOOP and reading current state
       * is unsupported
       */
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
      *piValue = iValue;

   return fResult ? 0 : -1;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DWORD dwModemStat = 0;

      fResult = GetCommModemStatus( pCom->hComm, &dwModemStat );
      if( fResult )
      {
         if( dwModemStat & MS_CTS_ON )
            iValue |= HB_COM_MSR_CTS;
         if( dwModemStat & MS_DSR_ON )
            iValue |= HB_COM_MSR_DSR;
         if( dwModemStat & MS_RING_ON )
            iValue |= HB_COM_MSR_RI;
         if( dwModemStat & MS_RLSD_ON )
            iValue |= HB_COM_MSR_DCD;

         /* MSR_DELTA_CTS, MSR_DELTA_DSR, MSR_TERI, MSR_DELTA_DCD
          * are unsupported
          */

      }
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
      *piValue = iValue;

   return fResult ? 0 : -1;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DWORD dwErrors = 0;

      fResult = ClearCommError( pCom->hComm, &dwErrors, NULL );
      if( fResult )
      {
         if( dwErrors & CE_BREAK )
            iValue |= HB_COM_LSR_BREAK;
         if( dwErrors & CE_FRAME )
            iValue |= HB_COM_LSR_FRAMING_ERR;
         if( dwErrors & CE_OVERRUN )
            iValue |= HB_COM_LSR_OVERRUN_ERR;
         if( dwErrors & CE_RXPARITY )
            iValue |= HB_COM_LSR_PARITY_ERR;

         /* LSR_DATA_READY, LSR_TRANS_HOLD_EMPTY, LSR_TRANS_EMPTY
          * are unsupported
          */
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
      *piValue = iValue;

   return fResult ? 0 : -1;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      hb_vmUnlock();

      fResult = SetCommBreak( pCom->hComm );
      if( fResult )
      {
         Sleep( iDurationInMilliSecs );
         fResult = ClearCommBreak( pCom->hComm );
      }
      hb_comSetOsError( pCom, ! fResult );

      hb_vmLock();
   }
   return fResult ? 0 : -1;
}

int hb_comFlowControl( int iPort, int * piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof( DCB );
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         if( dcb.fRtsControl == RTS_CONTROL_HANDSHAKE )
            iValue |= HB_COM_FLOW_IRTSCTS;
         if( dcb.fOutxCtsFlow )
            iValue |= HB_COM_FLOW_ORTSCTS;

         if( dcb.fDtrControl == DTR_CONTROL_HANDSHAKE )
            iValue |= HB_COM_FLOW_IDTRDSR;
         if( dcb.fOutxDsrFlow )
            iValue |= HB_COM_FLOW_ODTRDSR;

         if( dcb.fInX )
            iValue |= HB_COM_FLOW_XOFF;
         if( dcb.fOutX )
            iValue |= HB_COM_FLOW_XON;

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_IRTSCTS )
               dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;
            else if( dcb.fRtsControl == RTS_CONTROL_HANDSHAKE )
               dcb.fRtsControl = RTS_CONTROL_ENABLE;
            dcb.fOutxCtsFlow = ( iFlow & HB_COM_FLOW_ORTSCTS ) != 0;

            if( iFlow & HB_COM_FLOW_IDTRDSR )
               dcb.fDtrControl = DTR_CONTROL_HANDSHAKE;
            else if( dcb.fDtrControl == DTR_CONTROL_HANDSHAKE )
               dcb.fDtrControl = DTR_CONTROL_ENABLE;
            dcb.fOutxDsrFlow = ( iFlow & HB_COM_FLOW_ODTRDSR ) != 0;

            dcb.fInX = ( iFlow & HB_COM_FLOW_XOFF ) != 0;
            dcb.fOutX = ( iFlow & HB_COM_FLOW_XON ) != 0;

            fResult = SetCommState( pCom->hComm, &dcb );
         }
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piFlow )
      *piFlow = iValue;

   return fResult ? 0 : -1;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE, fNotSup = FALSE;

   if( pCom )
   {
      if( iFlow & HB_COM_FL_SOFT )
      {
         if( iFlow & HB_COM_FL_OOFF )
            fResult = EscapeCommFunction( pCom->hComm, SETXOFF );
         else if( iFlow & HB_COM_FL_OON )
            fResult = EscapeCommFunction( pCom->hComm, SETXON );
         else
            fNotSup = TRUE;
         hb_comSetOsError( pCom, ! fResult );
      }
      else if( iFlow & HB_COM_FL_RTSCTS )
      {
         if( iFlow & HB_COM_FL_IOFF )
            fResult = EscapeCommFunction( pCom->hComm, CLRRTS );
         else if( iFlow & HB_COM_FL_ION )
            fResult = EscapeCommFunction( pCom->hComm, SETRTS );
         else
            fNotSup = TRUE;
      }
      else if( iFlow & HB_COM_FL_DTRDSR )
      {
         if( iFlow & HB_COM_FL_IOFF )
            fResult = EscapeCommFunction( pCom->hComm, CLRDTR );
         else if( iFlow & HB_COM_FL_ION )
            fResult = EscapeCommFunction( pCom->hComm, SETDTR );
         else
            fNotSup = TRUE;
      }
      else
         fNotSup = TRUE;

      if( fNotSup )
         hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
      else
         hb_comSetOsError( pCom, ! fResult );
   }

   return fResult ? 0 : -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      fResult = TRUE;
      if( iXONchar >= 0 || iXOFFchar >= 0 )
      {
         DCB dcb;

         dcb.DCBlength = sizeof( DCB );
         fResult = GetCommState( pCom->hComm, &dcb );
         if( fResult )
         {
            if( iXONchar >= 0 )
               dcb.XonChar = ( char ) iXONchar;
            if( iXOFFchar >= 0 )
               dcb.XoffChar = ( char ) iXOFFchar;
            fResult = SetCommState( pCom->hComm, &dcb );
         }
      }
      hb_comSetOsError( pCom, ! fResult );
   }
   return fResult ? 0 : -1;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: there is no support for setting user defined character
       * discarding input buffer
       */
      HB_SYMBOL_UNUSED( iChar );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof( DCB );
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         if( iChar >= 0 )
         {
            dcb.fErrorChar = TRUE;
            dcb.ErrorChar = ( char ) iChar;
         }
         else
            dcb.fErrorChar = FALSE;
         fResult = SetCommState( pCom->hComm, &dcb );
      }
      hb_comSetOsError( pCom, ! fResult );
   }
   return fResult ? 0 : -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      COMSTAT comStat;

      fResult = ClearCommError( pCom->hComm, NULL, &comStat );
      if( fResult )
      {
         /* NOTE: HB_COM_TX_RFLUSH is unsupported */

         if( comStat.fCtsHold )
            iValue |= HB_COM_TX_CTS;
         if( comStat.fDsrHold )
            iValue |= HB_COM_TX_DSR;
         if( comStat.fRlsdHold )
            iValue |= HB_COM_TX_DCD;
         if( comStat.fXoffHold )
            iValue |= HB_COM_TX_XOFF;
         if( comStat.cbOutQue == 0 )
            iValue |= HB_COM_TX_EMPTY;
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   return fResult ? iValue : -1;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      COMSTAT comStat;

      fResult = ClearCommError( pCom->hComm, NULL, &comStat );
      if( fResult )
      {
         if( comStat.fXoffSent )
            iValue |= HB_COM_RX_XOFF;
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   return fResult ? iValue : -1;
}

static BOOL hb_comSetTimeouts( PHB_COM pCom, HB_MAXINT rdtimeout,
                                             HB_MAXINT wrtimeout )
{
   COMMTIMEOUTS timeouts;
   BOOL fResult;

   if( rdtimeout == 0 )
   {
      timeouts.ReadIntervalTimeout = MAXDWORD;
      timeouts.ReadTotalTimeoutMultiplier = 0;
      timeouts.ReadTotalTimeoutConstant = 0;
   }
   else
   {
      timeouts.ReadIntervalTimeout = MAXDWORD;
      timeouts.ReadTotalTimeoutMultiplier = MAXDWORD;
      timeouts.ReadTotalTimeoutConstant = ( DWORD ) rdtimeout;
   }
   timeouts.WriteTotalTimeoutMultiplier = 0;
   timeouts.WriteTotalTimeoutConstant = ( DWORD ) HB_MAX( wrtimeout, 1 );

   fResult = SetCommTimeouts( pCom->hComm, &timeouts );
   if( fResult )
   {
      pCom->rdtimeout = rdtimeout;
      pCom->wrtimeout = wrtimeout;
   }

   return fResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      hb_vmUnlock();

      if( timeout < 0 )
         timeout = 0;

      if( pCom->wrtimeout == timeout ||
          hb_comSetTimeouts( pCom, pCom->rdtimeout, timeout ) )
      {
         DWORD dwWritten = 0;
         BOOL fResult;

         fResult = WriteFile( pCom->hComm, data, ( DWORD ) len, &dwWritten, NULL );
         lSent = fResult ? ( long ) dwWritten : -1;
         if( lSent == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lSent = -1;
         }
         else
            hb_comSetOsError( pCom, ! fResult );
      }
      else
         hb_comSetOsError( pCom, HB_TRUE );

      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      hb_vmUnlock();

      if( timeout < 0 )
         timeout = 0;

      if( pCom->rdtimeout == timeout ||
          hb_comSetTimeouts( pCom, timeout, pCom->wrtimeout ) )
      {
         DWORD dwRead = 0;
         BOOL fResult;

         fResult = ReadFile( pCom->hComm, data, ( DWORD ) len, &dwRead, NULL );
         lReceived = fResult ? ( long ) dwRead : -1;
         if( lReceived == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lReceived = -1;
         }
         else
            hb_comSetOsError( pCom, ! fResult );
      }
      else
         hb_comSetOsError( pCom, HB_TRUE );

      hb_vmLock();
   }

   return lReceived;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof( DCB );
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         switch( iParity )
         {
            case 0:
            case 'N':
            case 'n':
               iParity = NOPARITY;
               break;
            case 'E':
            case 'e':
               iParity = EVENPARITY;
               break;
            case 'O':
            case 'o':
               iParity = ODDPARITY;
               break;
            case 'S':
            case 's':
               iParity = SPACEPARITY;
               break;
            case 'M':
            case 'm':
               iParity = MARKPARITY;
               break;
            default:
               fResult = FALSE;
         }
         switch( iSize )
         {
            case 0:
               iSize = 8;
            case 8:
            case 7:
            case 6:
            case 5:
               break;
            default:
               fResult = FALSE;
         }
         switch( iStop )
         {
            case 0:
            case 1: iStop = ONESTOPBIT; break;
            case 2: iStop = TWOSTOPBITS; break;
            default:
               fResult = FALSE;
         }
         if( fResult )
         {
            if( iBaud )
               dcb.BaudRate = ( DWORD ) iBaud;
            dcb.fBinary = 1;
            dcb.fParity = 0;
            dcb.fOutxCtsFlow = 0;
            dcb.fOutxDsrFlow = 0;
            dcb.fDtrControl = DTR_CONTROL_ENABLE;
            dcb.fDsrSensitivity = 0;
            dcb.fTXContinueOnXoff = 1;
            dcb.fOutX = 0;
            dcb.fInX = 0;
            dcb.fErrorChar = 0;
            dcb.fNull = 0;
            dcb.fRtsControl = RTS_CONTROL_ENABLE;
            dcb.fAbortOnError = 0;
          /*dcb.XonLim*/
          /*dcb.XoffLim*/
            dcb.ByteSize = ( BYTE ) iSize;
            dcb.Parity = ( BYTE ) iParity;
            dcb.StopBits = ( BYTE ) iStop;
          /*dcb.XonChar*/
          /*dcb.XoffChar*/
            dcb.ErrorChar = '?';
          /*dcb.EofChar*/
          /*dcb.EvtChar*/

            fResult = SetCommState( pCom->hComm, &dcb );
            if( fResult )
               fResult = hb_comSetTimeouts( pCom, 0, 0 );

            hb_comSetOsError( pCom, ! fResult );
         }
         else
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
      }
      else
         hb_comSetOsError( pCom, ! fResult );
   }

   return fResult ? 0 : -1;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      hb_vmUnlock();
      /* FlushFileBuffers( pCom->hComm ); */
      fResult = CloseHandle( pCom->hComm );
      pCom->hComm = INVALID_HANDLE_VALUE;
      pCom->status &= ~HB_COM_OPEN;
      hb_comSetOsError( pCom, !fResult );
      hb_vmLock();
   }

   return fResult ? 0 : -1;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   BOOL fResult = FALSE;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         char buffer[ HB_COM_DEV_NAME_MAX ];
         const char * szName = hb_comGetName( pCom, buffer, sizeof( buffer ) );
         LPCTSTR lpName;
         LPTSTR lpFree;

         lpName = HB_FSNAMECONV( szName, &lpFree );

         hb_vmUnlock();

         pCom->hComm = CreateFile( lpName,
                                   GENERIC_READ | GENERIC_WRITE,
                                   0,
                                   NULL,
                                   OPEN_EXISTING,
                                   FILE_FLAG_NO_BUFFERING, NULL );
         if( pCom->hComm != INVALID_HANDLE_VALUE )
         {
            fResult = TRUE;
            pCom->status |= HB_COM_OPEN;
         }
         hb_comSetOsError( pCom, ! fResult );

         hb_vmLock();

         if( lpFree )
            hb_xfree( lpFree );
      }
   }

   return fResult ? 0 : -1;
}

/* end of HB_OS_WIN */

#elif defined( HB_OS_OS2 )

static void hb_comSetOsError( PHB_COM pCom, APIRET rc )
{
   pCom->oserr = rc;
   switch( pCom->oserr )
   {
      case NO_ERROR:
         pCom->error = 0;
         break;
      case ERROR_TIMEOUT:
         pCom->error = HB_COM_ERR_TIMEOUT;
         break;
      case ERROR_ACCESS_DENIED:
      case ERROR_SHARING_VIOLATION:
         pCom->error = HB_COM_ERR_BUSY;
         break;
      case ERROR_FILE_NOT_FOUND:
      case ERROR_PATH_NOT_FOUND:
         pCom->error = HB_COM_ERR_NOCOM;
         break;
      default:
         pCom->error = HB_COM_ERR_OTHER;
         break;
   }
}

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
   {
      APIRET rc;
      RXQUEUE rxqueue;

      memset( &rxqueue, 0, sizeof( rxqueue ) );
      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETINQUECOUNT,
                        NULL, 0, NULL, &rxqueue, sizeof( RXQUEUE ), NULL );
      if( rc == NO_ERROR )
         iCount = rxqueue.cch;

      hb_comSetOsError( pCom, rc );
   }

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
   {
      APIRET rc;
      RXQUEUE rxqueue;

      memset( &rxqueue, 0, sizeof( rxqueue ) );
      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETOUTQUECOUNT,
                        NULL, 0, NULL, &rxqueue, sizeof( RXQUEUE ), NULL );
      if( rc == NO_ERROR )
         iCount = rxqueue.cch;

      hb_comSetOsError( pCom, rc );
   }

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE;

   if( pCom )
   {
      UCHAR cmdinfo = 0;
      USHORT reserved = 0;

      switch( iType )
      {
         case HB_COM_IFLUSH:
         case HB_COM_IOFLUSH:
            rc = DosDevIOCtl( pCom->hFile, IOCTL_GENERAL, DEV_FLUSHINPUT,
                              &cmdinfo, sizeof( cmdinfo ), NULL,
                              &reserved, sizeof( reserved ), NULL );
            if( iType == HB_COM_IFLUSH )
               break;
            cmdinfo = 0;
            reserved = 0;
         case HB_COM_OFLUSH:
            rc = DosDevIOCtl( pCom->hFile, IOCTL_GENERAL, DEV_FLUSHOUTPUT,
                              &cmdinfo, sizeof( cmdinfo ), NULL,
                              &reserved, sizeof( reserved ), NULL );
            break;
         default:
            rc = ERROR_INVALID_PARAMETER;
            break;
      }
   }
   hb_comSetOsError( pCom, rc );

   return ( rc == NO_ERROR ) ? 0 : -1;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      APIRET rc;
      UCHAR mcos = 0;

      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETMODEMOUTPUT,
                        NULL, 0, NULL, &mcos, sizeof( mcos ), NULL );
      if( rc == NO_ERROR )
      {
         MODEMSTATUS ms;

         /* MCR_OUT1, MCR_OUT2, MCR_LOOP are unsupported
          */

         if( mcos & DTR_ON )
            iValue |= HB_COM_MCR_DTR;
         if( mcos & RTS_ON )
            iValue |= HB_COM_MCR_RTS;

         ms.fbModemOn = 0x00;
         ms.fbModemOff = 0xFF;
         if( iSet & HB_COM_MCR_DTR )
            ms.fbModemOn |= DTR_ON;
         else if( iClr & HB_COM_MCR_DTR )
            ms.fbModemOff &= DTR_OFF;
         if( iSet & HB_COM_MCR_RTS )
            ms.fbModemOn |= RTS_ON;
         else if( iClr & HB_COM_MCR_RTS )
            ms.fbModemOff &= RTS_OFF;

         if( ms.fbModemOn != 0x00 || ms.fbModemOff != 0xFF )
         {
            USHORT comError = 0;
            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETMODEMCTRL,
                              &ms, sizeof( ms ), NULL,
                              &comError, sizeof( comError ), NULL );
         }
      }
      hb_comSetOsError( pCom, rc );
      if( rc == NO_ERROR )
         iResult = 0;
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      APIRET rc;
      UCHAR mcis = 0;
      USHORT comEvent = 0;

      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETMODEMINPUT,
                        NULL, 0, NULL, &mcis, sizeof( mcis ), NULL );
      if( rc == NO_ERROR )
      {
         if( mcis & CTS_ON )
            iValue |= HB_COM_MSR_CTS;
         if( mcis & DSR_ON )
            iValue |= HB_COM_MSR_DSR;
         if( mcis & RI_ON )
            iValue |= HB_COM_MSR_RI;
         if( mcis & DCD_ON )
            iValue |= HB_COM_MSR_DCD;

         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETCOMMEVENT,
                           NULL, 0, NULL, &comEvent, sizeof( comEvent ), NULL );

         if( rc == NO_ERROR )
         {
            if( comEvent & CTS_CHANGED )
               iValue |= HB_COM_MSR_DELTA_CTS;
            if( comEvent & DSR_CHANGED )
               iValue |= HB_COM_MSR_DELTA_DSR;
            if( comEvent & DCD_CHANGED )
               iValue |= HB_COM_MSR_DELTA_DCD;
            if( comEvent & RI_DETECTED )
               iValue |= HB_COM_MSR_TERI;
            iResult = 0;
         }
      }
      hb_comSetOsError( pCom, rc );
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      APIRET rc;
      USHORT comError = 0;

      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETCOMMERROR,
                        NULL, 0, NULL, &comError, sizeof( comError ), NULL );
      if( rc == NO_ERROR )
      {
         if( comError & ( RX_QUE_OVERRUN | RX_HARDWARE_OVERRUN ) )
            iValue |= HB_COM_LSR_OVERRUN_ERR;
         if( comError & PARITY_ERROR )
            iValue |= HB_COM_LSR_PARITY_ERR;
         if( comError & FRAMING_ERROR )
            iValue |= HB_COM_LSR_FRAMING_ERR;

         /* LSR_DATA_READY, LSR_TRANS_HOLD_EMPTY, LSR_TRANS_EMPTY, LSR_BREAK
          * are unsupported
          */
         iResult = 0;
      }
      hb_comSetOsError( pCom, rc );
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      APIRET rc;
      USHORT comError = 0;

      hb_vmUnlock();

      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETBREAKON,
                        NULL, 0, NULL, &comError, sizeof( comError ), NULL );
      if( rc == NO_ERROR )
      {
         DosSleep( iDurationInMilliSecs );
         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETBREAKOFF,
                           NULL, 0, NULL, &comError, sizeof( comError ), NULL );

         if( rc == NO_ERROR )
            iResult = 0;
      }
      hb_comSetOsError( pCom, rc );

      hb_vmLock();
   }
   return iResult;
}

/* some missing defines in bsedev.h */
#define MODE_RTS_MASK         ( MODE_RTS_CONTROL | MODE_RTS_HANDSHAKE )
#define MODE_DTR_MASK         ( MODE_DTR_CONTROL | MODE_RTS_HANDSHAKE )
#define MODE_FULL_DUPLEX      0x20

int hb_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE;
   int iValue = 0;

   if( pCom )
   {
      DCBINFO dcb;

      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETDCBINFO,
                        NULL, 0, NULL, &dcb, sizeof( dcb ), NULL );
      if( rc == NO_ERROR )
      {
         if( ( dcb.fbFlowReplace & MODE_RTS_MASK ) == MODE_RTS_HANDSHAKE )
            iValue |= HB_COM_FLOW_IRTSCTS;

         if( dcb.fbCtlHndShake & MODE_CTS_HANDSHAKE )
            iValue |= HB_COM_FLOW_ORTSCTS;

         if( ( dcb.fbCtlHndShake & MODE_DTR_MASK ) == MODE_DTR_HANDSHAKE )
            iValue |= HB_COM_FLOW_IDTRDSR;

         if( dcb.fbCtlHndShake & MODE_DSR_HANDSHAKE )
            iValue |= HB_COM_FLOW_ODTRDSR;

         if( dcb.fbCtlHndShake & MODE_DCD_HANDSHAKE )
            iValue |= HB_COM_FLOW_DCD;

         if( dcb.fbFlowReplace & MODE_AUTO_TRANSMIT )
            iValue |= HB_COM_FLOW_XON;

         if( dcb.fbFlowReplace & MODE_AUTO_RECEIVE )
            iValue |= HB_COM_FLOW_XOFF;

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_IRTSCTS )
            {
               dcb.fbFlowReplace &= ~MODE_RTS_MASK;
               dcb.fbFlowReplace |= MODE_RTS_HANDSHAKE;
            }
            else
               dcb.fbFlowReplace &= ~MODE_RTS_HANDSHAKE;

            if( iFlow & HB_COM_FLOW_ORTSCTS )
               dcb.fbCtlHndShake |= MODE_CTS_HANDSHAKE;
            else
               dcb.fbCtlHndShake &= ~MODE_CTS_HANDSHAKE;

            if( iFlow & HB_COM_FLOW_IDTRDSR )
            {
               dcb.fbCtlHndShake &= ~MODE_DTR_MASK;
               dcb.fbCtlHndShake |= MODE_DTR_HANDSHAKE;
            }
            else
               dcb.fbCtlHndShake &= ~MODE_DTR_HANDSHAKE;

            if( iFlow & HB_COM_FLOW_ODTRDSR )
               dcb.fbCtlHndShake |= MODE_DSR_HANDSHAKE;
            else
               dcb.fbCtlHndShake &= ~MODE_DSR_HANDSHAKE;

            if( iFlow & HB_COM_FLOW_DCD )
               dcb.fbCtlHndShake |= MODE_DCD_HANDSHAKE;
            else
               dcb.fbCtlHndShake &= ~MODE_DCD_HANDSHAKE;

            if( iFlow & HB_COM_FLOW_XON )
               dcb.fbFlowReplace |= MODE_AUTO_TRANSMIT;
            else
               dcb.fbFlowReplace &= ~MODE_AUTO_TRANSMIT;

            if( iFlow & HB_COM_FLOW_XOFF )
               dcb.fbFlowReplace |= MODE_AUTO_RECEIVE;
            else
               dcb.fbFlowReplace &= ~MODE_AUTO_RECEIVE;

            dcb.fbCtlHndShake &= ~MODE_DSR_SENSITIVITY;
            dcb.fbFlowReplace |= MODE_FULL_DUPLEX;

            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETDCBINFO,
                              &dcb, sizeof( dcb ), NULL, NULL, 0, NULL );
         }
      }
      hb_comSetOsError( pCom, rc );
   }

   if( piFlow )
      *piFlow = iValue;

   return ( rc == NO_ERROR ) ? 0 : -1;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE, fNotSup = FALSE;

   if( pCom )
   {
      if( iFlow & HB_COM_FL_SOFT )
      {
         if( iFlow & HB_COM_FL_OOFF )
            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_STOPTRANSMIT,
                              NULL, 0, NULL, NULL, 0, NULL );
         else if( iFlow & HB_COM_FL_OON )
            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_STARTTRANSMIT,
                              NULL, 0, NULL, NULL, 0, NULL );
         else
            fNotSup = TRUE;
      }
      else
         fNotSup = TRUE;

      if( fNotSup )
         hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
      else
         hb_comSetOsError( pCom, rc );
   }

   return ( rc == NO_ERROR ) ? 0 : -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE;

   if( pCom )
   {
      rc = NO_ERROR;
      if( iXONchar >= 0 || iXOFFchar >= 0 )
      {
         DCBINFO dcb;

         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETDCBINFO,
                           NULL, 0, NULL, &dcb, sizeof( dcb ), NULL );
         if( rc == NO_ERROR )
         {
            if( iXONchar >= 0 )
               dcb.bXONChar = ( BYTE ) iXONchar;
            if( iXOFFchar >= 0 )
               dcb.bXOFFChar = ( BYTE ) iXOFFchar;
            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETDCBINFO,
                              &dcb, sizeof( dcb ), NULL, NULL, 0, NULL );
         }
      }
      hb_comSetOsError( pCom, rc );
   }
   return ( rc == NO_ERROR ) ? 0 : -1;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: there is no support for setting user defined character
       * discarding input buffer
       */
      HB_SYMBOL_UNUSED( iChar );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE;

   if( pCom )
   {
      DCBINFO dcb;

      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETDCBINFO,
                        NULL, 0, NULL, &dcb, sizeof( dcb ), NULL );
      if( rc == NO_ERROR )
      {
         if( iChar >= 0 )
         {
            dcb.fbFlowReplace |= MODE_ERROR_CHAR;
            dcb.bErrorReplacementChar = ( BYTE ) iChar;
         }
         else
            dcb.fbFlowReplace &= ~MODE_ERROR_CHAR;
         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETDCBINFO,
                           &dcb, sizeof( dcb ), NULL, NULL, 0, NULL );
      }
      hb_comSetOsError( pCom, rc );
   }
   return ( rc == NO_ERROR ) ? 0 : -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE;
   int iValue = 0;

   if( pCom )
   {
      UCHAR comStatus = 0;

      rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETCOMMSTATUS,
                        NULL, 0, NULL, &comStatus, sizeof( comStatus ), NULL );
      if( rc == NO_ERROR )
      {
         /* NOTE: HB_COM_TX_RFLUSH is unsupported */

         if( comStatus & TX_WAITING_FOR_CTS )
            iValue |= HB_COM_TX_CTS;
         if( comStatus & TX_WAITING_FOR_DSR )
            iValue |= HB_COM_TX_DSR;
         if( comStatus & TX_WAITING_FOR_DCD )
            iValue |= HB_COM_TX_DCD;
         if( comStatus & TX_WAITING_FOR_XON )
            iValue |= HB_COM_TX_XOFF;

         if( hb_comOutputCount( iPort ) == 0 )
            iValue |= HB_COM_TX_EMPTY;
      }

      hb_comSetOsError( pCom, rc );
   }

   return ( rc == NO_ERROR ) ? iValue : -1;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* TODO: checking HB_COM_RX_* input flow states is unsupported */
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      APIRET rc = NO_ERROR;

      hb_vmUnlock();

      if( timeout < 0 )
         timeout = 0;
      else if( timeout > 0 )
      {
         timeout = ( timeout / 10 ) + 1;
         if( timeout > USHRT_MAX )
            timeout = USHRT_MAX;
      }

      if( pCom->wrtimeout != ( USHORT ) timeout )
      {
         DCBINFO dcb;

         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETDCBINFO,
                           NULL, 0, NULL, &dcb, sizeof( dcb ), NULL );
         if( rc == NO_ERROR )
         {
            dcb.fbTimeout &= ~MODE_NO_WRITE_TIMEOUT;
            /* dcb.usWriteTimeout is 0 based (0 = 0.01 sec.) and this is
             * the minimal write timeout what seems to be reasonable.
             */
            if( timeout )
               dcb.usWriteTimeout = ( USHORT ) ( timeout - 1 );
            else
               dcb.usWriteTimeout = 0;

            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETDCBINFO,
                              &dcb, sizeof( dcb ), NULL, NULL, 0, NULL );
            if( rc == NO_ERROR )
               pCom->wrtimeout = ( USHORT ) timeout;
         }
      }

      if( rc == NO_ERROR )
      {
         ULONG ulWritten = 0;

         rc = DosWrite( pCom->hFile, ( void * ) data, len, &ulWritten );
         if( rc == NO_ERROR )
            lSent = ( long ) ulWritten;
      }

      if( lSent == 0 )
      {
         hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
         lSent = -1;
      }
      else
         hb_comSetOsError( pCom, rc );

      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      APIRET rc = NO_ERROR;

      hb_vmUnlock();

      if( timeout < 0 )
         timeout = 0;
      else if( timeout > 0 )
      {
         timeout = ( timeout / 10 ) + 1;
         if( timeout > USHRT_MAX )
            timeout = USHRT_MAX;
      }

      if( pCom->rdtimeout != ( USHORT ) timeout )
      {
         DCBINFO dcb;

         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETDCBINFO,
                           NULL, 0, NULL, &dcb, sizeof( dcb ), NULL );
         if( rc == NO_ERROR )
         {
            dcb.fbTimeout &= ~( MODE_READ_TIMEOUT | MODE_NOWAIT_READ_TIMEOUT );
            if( timeout )
            {
               dcb.fbTimeout |= MODE_READ_TIMEOUT;
               dcb.usReadTimeout = ( USHORT ) ( timeout - 1 );
            }
            else
            {
               dcb.fbTimeout |= MODE_NOWAIT_READ_TIMEOUT;
               dcb.usReadTimeout = 0;
            }

            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETDCBINFO,
                              &dcb, sizeof( dcb ), NULL, NULL, 0, NULL );
            if( rc == NO_ERROR )
               pCom->rdtimeout = ( USHORT ) timeout;
         }
      }

      if( rc == NO_ERROR )
      {
         ULONG ulRead = 0;

         rc = DosRead( pCom->hFile, data, len, &ulRead );
         if( rc == NO_ERROR )
            lReceived = ( long ) ulRead;
      }

      if( lReceived == 0 )
      {
         hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
         lReceived = -1;
      }
      else
         hb_comSetOsError( pCom, rc );

      hb_vmLock();
   }

   return lReceived;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE;

   if( pCom )
   {
      LINECONTROL lctrl;

      rc = NO_ERROR;
      switch( iSize )
      {
         case 0:
            iSize = 8;
         case 8:
         case 7:
         case 6:
         case 5:
            lctrl.bDataBits = iSize;
            break;
         default:
            rc = ERROR_INVALID_PARAMETER;
      }
      switch( iParity )
      {
         case 0:
         case 'N':
         case 'n':
            lctrl.bParity = 0; break;
         case 'O':
         case 'o':
            lctrl.bParity = 1; break;
         case 'E':
         case 'e':
            lctrl.bParity = 2; break;
         case 'M':
         case 'm':
            lctrl.bParity = 3; break;
         case 'S':
         case 's':
            lctrl.bParity = 4; break;
         default:
            rc = ERROR_INVALID_PARAMETER;
      }
      switch( iStop )
      {
         case 0:
         case 1: lctrl.bStopBits = 0; break;
         case 2: lctrl.bStopBits = 2; break;
         default:
            rc = ERROR_INVALID_PARAMETER;
      }
      lctrl.fTransBreak = 0;

      if( iBaud < 0 || iBaud > USHRT_MAX )
         rc = ERROR_INVALID_PARAMETER;

      if( rc == NO_ERROR )
      {
         if( iBaud )
         {
            USHORT baud = ( USHORT ) iBaud;

            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETBAUDRATE,
                              &baud, sizeof( baud ), NULL, NULL, 0L, NULL );
         }
      }
      if( rc == NO_ERROR )
         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETLINECTRL,
                           &lctrl, sizeof( lctrl ), NULL, NULL, 0L, NULL );

      if( rc == NO_ERROR )
      {
         DCBINFO dcb;

         rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_GETDCBINFO,
                           NULL, 0, NULL, &dcb, sizeof( dcb ), NULL );
         if( rc == NO_ERROR )
         {
            dcb.usWriteTimeout = 0;
            dcb.usReadTimeout  = 0;

            dcb.fbCtlHndShake = 0x00;
            dcb.fbFlowReplace = MODE_FULL_DUPLEX;
            dcb.fbTimeout &= ~MODE_NO_WRITE_TIMEOUT;
            dcb.fbTimeout |= MODE_NOWAIT_READ_TIMEOUT;

            rc = DosDevIOCtl( pCom->hFile, IOCTL_ASYNC, ASYNC_SETDCBINFO,
                              &dcb, sizeof( dcb ), NULL, NULL, 0, NULL );
            pCom->rdtimeout = pCom->wrtimeout = 0;
         }
      }
      hb_comSetOsError( pCom, rc );
   }

   return ( rc == NO_ERROR ) ? 0 : -1;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   APIRET rc = ERROR_INVALID_HANDLE;

   if( pCom )
   {
      hb_vmUnlock();
      /* DosResetBuffer( pCom->hFile ); */
      rc = DosClose( pCom->hFile );
      pCom->hFile = 0;
      pCom->status &= ~HB_COM_OPEN;
      hb_comSetOsError( pCom, rc );
      hb_vmLock();
   }

   return ( rc == NO_ERROR ) ? 0 : -1;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   APIRET rc = ERROR_INVALID_HANDLE;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         char buffer[ HB_COM_DEV_NAME_MAX ];
         const char * pszName = hb_comGetName( pCom, buffer, sizeof( buffer ) );
         ULONG ulAction = 0;

         hb_vmUnlock();

         rc = DosOpen( ( PSZ ) pszName,
                       &pCom->hFile,
                       &ulAction,
                       0L,
                       FILE_NORMAL,
                       OPEN_ACTION_OPEN_IF_EXISTS,
                       OPEN_ACCESS_READWRITE | OPEN_SHARE_DENYREADWRITE,
                       0L );
         if( rc == NO_ERROR )
            pCom->status |= HB_COM_OPEN;

         hb_comSetOsError( pCom, rc );

         hb_vmLock();
      }
   }

   return ( rc == NO_ERROR ) ? 0 : -1;
}

/* end of HB_OS_OS2 */

#elif defined( HB_HAS_PMCOM )

static void hb_comSetOsError( PHB_COM pCom, int iError )
{
   pCom->oserr = iError;

   switch( iError )
   {
      case COMERR_NOCHIP:
         pCom->error = HB_COM_ERR_WRONGPORT;
         break;
      case COMERR_RXOVERFLOW:
      case COMERR_NOMEMORY:
      case COMERR_GENERAL:
      default:
         pCom->error = HB_COM_ERR_OTHER;
   }
}

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
      iCount = COMTXBufferUsed( iPort - 1 );

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
      iCount = COMRXBufferUsed( iPort - 1 );

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = 0;
      hb_comSetOsError( pCom, 0 );
      switch( iType )
      {
         case HB_COM_IFLUSH:
         case HB_COM_IOFLUSH:
            COMClearRXBuffer( iPort - 1 );
            if( iType == HB_COM_IFLUSH )
               break;
            break;
         case HB_COM_OFLUSH:
            COMClearTXBuffer( iPort - 1 );
            break;
         default:
            iResult = -1;
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
            break;
      }
   }

   return iResult;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      /* MCR_OUT1, MCR_OUT2, MCR_LOOP and reading current state
       * is unsupported
       */
      if( iSet & HB_COM_MCR_DTR )
         COMSetDtr( iPort - 1, 1 );
      else if( iClr & HB_COM_MCR_DTR )
         COMSetDtr( iPort - 1, 0 );

      if( iSet & HB_COM_MCR_RTS )
         COMSetRts( iPort - 1, 1 );
      else if( iClr & HB_COM_MCR_RTS )
         COMSetRts( iPort - 1, 0 );

      iResult = 0;
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      int iMSR = COMGetModemStatus( iPort - 1 );

      if( iMSR & DELTA_CTS )
         iValue |= HB_COM_MSR_DELTA_CTS;
      if( iMSR & DELTA_DSR )
         iValue |= HB_COM_MSR_DELTA_DSR;
      if( iMSR & DELTA_RI )
         iValue |= HB_COM_MSR_TERI;
      if( iMSR & DELTA_CD )
         iValue |= HB_COM_MSR_DELTA_DCD;

      if( iMSR & CTS_LINE )
         iValue |= HB_COM_MSR_CTS;
      if( iMSR & DSR_LINE )
         iValue |= HB_COM_MSR_DSR;
      if( iMSR & RI_LINE )
         iValue |= HB_COM_MSR_RI;
      if( iMSR & CD_LINE )
         iValue |= HB_COM_MSR_DCD;

      iResult = 0;
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iDurationInMilliSecs );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      int iFlowVal = COMGetFlowControl( iPort - 1 );

      if( iFlowVal & FLOW_RTS )
         iValue |= HB_COM_FLOW_IRTSCTS;
      if( iFlowVal & FLOW_CTS )
         iValue |= HB_COM_FLOW_ORTSCTS;
      if( iFlowVal & FLOW_DTR )
         iValue |= HB_COM_FLOW_IDTRDSR;
      if( iFlowVal & FLOW_DSR )
         iValue |= HB_COM_FLOW_ODTRDSR;
      if( iFlowVal & FLOW_DCD )
         iValue |= HB_COM_FLOW_DCD;
      if( iFlowVal & FLOW_XOFF )
         iValue |= HB_COM_FLOW_XOFF;
      if( iFlowVal & FLOW_XON )
         iValue |= HB_COM_FLOW_XON;

      if( iFlow >= 0 )
      {
         iFlowVal = 0;
         if( iFlow & HB_COM_FLOW_IRTSCTS )
            iFlowVal |= FLOW_RTS;
         if( iFlow & HB_COM_FLOW_ORTSCTS )
            iFlowVal |= FLOW_CTS;
         if( iFlow & HB_COM_FLOW_IDTRDSR )
            iFlowVal |= FLOW_DTR;
         if( iFlow & HB_COM_FLOW_ODTRDSR )
            iFlowVal |= FLOW_DSR;
         if( iFlow & HB_COM_FLOW_DCD )
            iFlowVal |= FLOW_DCD;
         if( iFlow & HB_COM_FLOW_XOFF )
            iFlowVal |= FLOW_XOFF;
         if( iFlow & HB_COM_FLOW_XON )
            iFlowVal |= FLOW_XON;

         COMSetFlowControl( iPort - 1, iFlowVal );
      }

      iResult = 0;
   }

   if( piFlow )
      *piFlow = iValue;

   return iResult;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iFlow );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      COMSetFlowChars( iPort - 1, iXONchar, iXOFFchar );
      iResult = 0;
   }

   return iResult;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = COMRXBufferUsed( iPort - 1 );
      if( iResult == 0 )
         iResult = HB_COM_TX_EMPTY;
      else if( iResult > 0 )
         iResult = 0;
   }

   return iResult;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return iResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      const char * buffer = ( const char * ) data;
      HB_MAXUINT timer = timeout <= 0 ? 0 : ( hb_dateMilliSeconds() + timeout );

      hb_comSetOsError( pCom, 0 );
      lSent = 0;

      hb_vmUnlock();

      while( len > 0 )
      {
         int iSent, iErr;

         iErr = COMWriteBuffer( iPort - 1, buffer, NULL, len, &iSent );
         lSent += iSent;
         if( iErr == COMERR_TXOVERFLOW )
         {
            buffer += iSent;
            len -= iSent;
            if( timer == 0 || timer < hb_dateMilliSeconds() )
            {
               if( lSent == 0 )
               {
                  hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
                  lSent = -1;
               }
               break;
            }
            hb_releaseCPU();
         }
         else
            break;
      }

      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      char * buffer = ( char * ) data;
      HB_MAXUINT timer = timeout <= 0 ? 0 : ( hb_dateMilliSeconds() + timeout );

      hb_comSetOsError( pCom, 0 );
      lReceived = 0;

      hb_vmUnlock();

      while( len > 0 )
      {
         int iErr = COMReadChar( iPort - 1, buffer, NULL );

         if( iErr == 0 )
         {
            ++buffer;
            --len;
            ++lReceived;
         }
         else if( iErr == COM_BUFEMPTY )
         {
            if( lReceived > 0 || timer == 0 || timer < hb_dateMilliSeconds() )
            {
               if( lReceived == 0 )
               {
                  hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
                  lReceived = -1;
               }
               break;
            }
            hb_releaseCPU();
         }
         else
         {
            hb_comSetOsError( pCom, iErr );
            break;
         }
      }

      hb_vmLock();
   }

   return lReceived;
}

static int s_comChkPortParam( int *piBaud, int *piParity,
                              int *piSize, int *piStop )
{
   int iResult = 0;

   if( *piBaud == 0 )
      *piBaud = 9600;

   *piParity = HB_TOUPPER( *piParity );
   switch( *piParity )
   {
      case 0:
         *piParity = 'N';
      case 'N':
      case 'E':
      case 'O':
      case 'S':
      case 'M':
         break;

      default:
         iResult = -1;
   }

   switch( *piSize )
   {
      case 0:
         *piSize = 8;
      case 8:
      case 7:
      case 6:
      case 5:
         break;
      default:
         iResult = -1;
   }

   switch( *piStop )
   {
      case 0:
         *piStop = 1;
      case 1:
      case 2:
         break;
      default:
         iResult = -1;
   }

   return iResult;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
      if( iResult == 0 )
      {
         COMSetTransmitParameters( iPort - 1, iBaud, iSize, iParity, iStop );
         hb_comSetOsError( pCom, 0 );
      }
      else
         hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
   }

   return iResult;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      hb_vmUnlock();
      COMPortClose( iPort - 1 );
      pCom->status &= ~HB_COM_OPEN;
      hb_comSetOsError( pCom, 0 );
      iResult = 0;
      hb_vmLock();
   }

   return iResult;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   int iResult = -1;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         int iBaud, iParity, iSize, iStop, iFlowControl;

         hb_vmUnlock();

         iBaud = iParity = iSize = iStop = 0;
         iFlowControl = 0;
         s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
         iResult = COMPortOpen( iPort - 1, iBaud, iSize, iParity, iStop,
                                iFlowControl, NULL );
         if( iResult == 0 )
         {
            pCom->status |= HB_COM_OPEN;
            hb_comSetOsError( pCom, 0 );
         }
         else
         {
            hb_comSetOsError( pCom, iResult );
            iResult = -1;
         }

         hb_vmLock();
      }
   }

   return iResult;
}

/* end of HB_HAS_PMCOM */

#else

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iType );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piValue );
   HB_SYMBOL_UNUSED( iClr );
   HB_SYMBOL_UNUSED( iSet );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piValue );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piValue );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iDurationInMilliSecs );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piFlow );
   HB_SYMBOL_UNUSED( iFlow );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iFlow );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iXONchar );
   HB_SYMBOL_UNUSED( iXOFFchar );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( timeout );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( timeout );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iBaud );
   HB_SYMBOL_UNUSED( iParity );
   HB_SYMBOL_UNUSED( iSize );
   HB_SYMBOL_UNUSED( iStop );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   int iTODO_serial_port_support;

   if( pCom )
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );

   return -1;
}

#endif


static int s_iComInit = 0;

static void hb_com_exit( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_iComInit )
   {
      hb_comCloseAll();
      s_iComInit = 0;
   }
}

static void hb_com_init( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( ! s_iComInit )
   {
      int iPort;

      for( iPort = 0; iPort < HB_COM_PORT_MAX; ++iPort )
      {
         s_comList[ iPort ].port = iPort + 1;
         s_comList[ iPort ].status = HB_COM_ENABLED;
      }

      hb_vmAtQuit( hb_com_exit, NULL );

      s_iComInit = 1;
   }
}

HB_CALL_ON_STARTUP_BEGIN( _hb_com_init_ )
   hb_vmAtInit( hb_com_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_com_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_com_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_com_init_ )
   #include "hbiniseg.h"
#endif
