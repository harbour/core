/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *    The internet protocol / TCP support
 *
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
 *                Ron Pinkas [Ron@RonPinkas.com]
 *                Marcelo Lombardo [marcelo.lombardo@newage-software.com.br]
 * www - http://www.xharbour.org
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    updated and ported to Harbour
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* Compile in Unix mode under Cygwin */
#ifdef OS_UNIX_COMPATIBLE
  #undef HB_OS_WIN_32
#endif

/* HB_INET_H_ */
#if defined( HB_OS_DOS )

   #ifndef HB_NO_DEFAULT_INET
      #define HB_NO_DEFAULT_INET
   #endif

#else

   #include <string.h>

   #if defined( HB_OS_WIN_32 )
      #define _WINSOCKAPI_  /* Prevents inclusion of Winsock.h in Windows.h */
      #define HB_SOCKET_T SOCKET
      #include <winsock2.h>
      #include <windows.h>

      #define HB_INET_CLOSE( x )    closesocket( x )
   #else

      #if defined( HB_OS_HPUX )
         #define _XOPEN_SOURCE_EXTENDED
      #endif

      #define HB_SOCKET_T int
      #include <unistd.h>
      #include <sys/types.h>
      #include <sys/socket.h>
      #include <netdb.h>
      #include <netinet/in.h>
      #include <arpa/inet.h>

      #if defined(__WATCOMC__)
         #define h_errno errno
      #else
         extern int h_errno;
      #endif
      #define HB_INET_CLOSE( x )    close( x )
      #include <errno.h>
   #endif

   #define HB_SENDRECV_BUFFER_SIZE         1400

   typedef struct _HB_SOCKET_STRUCT
   {
       HB_SOCKET_T com;
       char *errorDesc;
       int errorCode;
       struct sockaddr_in remote;
       LONG count;
       int timeout;
       int timelimit;
       PHB_ITEM caPeriodic;
   } HB_SOCKET_STRUCT;

   #define HB_PARSOCKET( n )     ( ( HB_SOCKET_STRUCT * ) hb_parptrGC( hb_inetSocketFinalize, n ) )

   #define HB_SOCKET_ZERO_ERROR( s )   \
               do { s->errorCode = 0; s->errorDesc = ""; } while( 0 )

   #if defined( HB_OS_WIN_32 )
       #define HB_SOCKET_SET_ERROR( s )   \
               do { \
                  s->errorCode = WSAGetLastError(); \
                  s->errorDesc = strerror( s->errorCode );\
                  WSASetLastError( 0 ); \
               } while( 0 )

   #else
       #define HB_SOCKET_SET_ERROR( s )      \
               do { s->errorCode = errno; s->errorDesc = strerror( errno ); } while( 0 )
   #endif

   #define HB_SOCKET_SET_ERROR1( s, code )   \
               do { s->errorCode = code; s->errorDesc = strerror( code ); } while( 0 )
   #define HB_SOCKET_SET_ERROR2( s, code, desc )   \
               do { s->errorCode = code; s->errorDesc = desc; } while( 0 )

   #define HB_SOCKET_INIT( s, p ) \
      do { \
         s = ( HB_SOCKET_STRUCT *) hb_gcAlloc( sizeof( HB_SOCKET_STRUCT ), hb_inetSocketFinalize );\
         p = hb_itemPutPtrGC( p, s );\
         HB_SOCKET_ZERO_ERROR( s );\
         s->com = 0;\
         s->count = 0;\
         s->timeout = -1;\
         s->timelimit = -1;\
         s->caPeriodic = NULL;\
      } while( 0 )

   #ifndef MSG_NOSIGNAL
      #define MSG_NOSIGNAL 0
   #endif

   #ifndef MSG_DONTWAIT
      /* #define MSG_DONTWAIT 0x80 */
      #define MSG_DONTWAIT 0
   #endif

   #ifndef MSG_WAITALL
      #define MSG_WAITALL  0
   #endif

#endif
/* HB_INET_H_ */


#if !defined( HB_NO_DEFAULT_INET )

#if !defined( HB_WINCE )
   #include <fcntl.h>
   #include <errno.h>
#endif

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE ) || defined( HB_OS_BSD ) || defined(HB_OS_OS2)
   #include <sys/time.h>
#endif

#if defined(HB_OS_OS2)
   #include <sys/socket.h>
   #include <sys/select.h>
   #include <sys/ioctl.h>
   /* NET_SIZE_T exists because of shortsightedness on the POSIX committee.  BSD
    * systems used "int *" as the parameter to accept(), getsockname(),
    * getpeername() et al.  Consequently many unixes took an int * for that
    * parameter.  The POSIX committee decided that "int" was just too generic and
    * had to be replaced with size_t almost everywhere.  There's no problem with
    * that when you're passing by value.  But when you're passing by reference
    * this creates a gross source incompatibility with existing programs.  On
    * 32-bit architectures it creates only a warning.  On 64-bit architectures it
    * creates broken code -- because "int *" is a pointer to a 64-bit quantity and
    * "size_t *" is frequently a pointer to a 32-bit quantity.
    *
    * Some Unixes adopted "size_t *" for the sake of POSIX compliance.  Others
    * ignored it because it was such a broken interface.  Chaos ensued.  POSIX
    * finally woke up and decided that it was wrong and created a new type
    * socklen_t.  The only useful value for socklen_t is int, and that's how
    * everyone who has a clue implements it.  It is almost always the case that
    * NET_SIZE_T should be defined to be an int, unless the system being compiled
    * for was created in the window of POSIX madness.
    */
   #define socklen_t int
#endif

#ifdef HB_OS_LINUX
#include <signal.h>
#define HB_INET_LINUX_INTERRUPT     SIGUSR1+90
static void hb_inetLinuxSigusrHandle( int sig )
{
   /* nothing to do */
   HB_SYMBOL_UNUSED( sig );
}
#endif

/* JC1: we need it volatile to be minimally thread safe. */
static volatile int s_iSessions = 0;

/* Useful utility function to have a timeout; */

static int hb_selectReadSocket( HB_SOCKET_STRUCT *Socket )
{
   fd_set set;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET(Socket->com, &set);

   if( Socket->timeout == -1 )
   {
      if( select( Socket->com + 1, &set, NULL, NULL, NULL ) < 0 )
         return 0;
   }
   else
   {
      tv.tv_sec = Socket->timeout/ 1000;
      tv.tv_usec = (Socket->timeout % 1000) * 1000;
      if( select( Socket->com + 1, &set, NULL, NULL, &tv ) < 0 )
         return 0;
   }

   return FD_ISSET( Socket->com, &set );
}

static int hb_selectWriteSocket( HB_SOCKET_STRUCT *Socket )
{
   fd_set set;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET(Socket->com, &set);

   if( Socket->timeout == -1 )
   {
      if( select( Socket->com + 1, NULL, &set, NULL, NULL ) < 0 )
         return 0;
   }
   else
   {
      tv.tv_sec = Socket->timeout/ 1000;
      tv.tv_usec = (Socket->timeout % 1000) * 1000;
      if( select( Socket->com + 1, NULL, &set, NULL, &tv ) < 0 )
         return 0;
   }

   return FD_ISSET( Socket->com, &set );
}

#if defined(HB_OS_WIN_32)
static int hb_selectWriteExceptSocket( HB_SOCKET_STRUCT *Socket )
{
   fd_set set, eset;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET(Socket->com, &set);
   FD_ZERO( &eset );
   FD_SET(Socket->com, &eset);

   if( Socket->timeout == -1 )
   {
      if( select( Socket->com + 1, NULL, &set, &eset, NULL ) < 0 )
         return 2;
   }
   else
   {
      tv.tv_sec = Socket->timeout/ 1000;
      tv.tv_usec = (Socket->timeout % 1000) * 1000;
      if( select(Socket->com + 1, NULL, &set, &eset, &tv) < 0 )
         return 2;
   }

   if( FD_ISSET( Socket->com, &eset) )
   {
      return 2;
   }

   if( FD_ISSET( Socket->com, &set ) )
   {
      return 1;
   }
   return 0;
}
#endif

/*** Utilty to access host DNS */
static struct hostent * hb_getHosts( char * name, HB_SOCKET_STRUCT *Socket )
{
   struct hostent *Host = NULL;

   /* let's see if name is an IP address; not necessary on linux */
#if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
   ULONG ulAddr;

   ulAddr = inet_addr( name );
   if( ulAddr == INADDR_NONE )
   {
      if( strcmp( "255.255.255.255", name ) == 0 )
      {
         Host = gethostbyaddr( (const char*) &ulAddr, sizeof( ulAddr ), AF_INET );
      }
   }
   else
   {
      Host = gethostbyaddr( (const char*)  &ulAddr, sizeof( ulAddr ), AF_INET );
   }
#endif

   if( Host == NULL )
   {
      Host = gethostbyname( name );
   }

   if( Host == NULL && Socket != NULL )
   {
#if defined(HB_OS_WIN_32)
      HB_SOCKET_SET_ERROR2( Socket, WSAGetLastError() , "Generic error in GetHostByName()" );
      WSASetLastError( 0 );
#elif defined(HB_OS_OS2) || defined(HB_OS_HPUX) || defined(__WATCOMC__)
      HB_SOCKET_SET_ERROR2( Socket, h_errno, "Generic error in GetHostByName()" );
#else
      HB_SOCKET_SET_ERROR2( Socket, h_errno, (char *) hstrerror( h_errno ) );
#endif
   }
   return Host;
}


/*** Setup the non-blocking method **/

static void hb_socketSetNonBlocking( HB_SOCKET_STRUCT *Socket )
{
#ifdef HB_OS_WIN_32
   ULONG mode = 1;
   ioctlsocket( Socket->com, FIONBIO, &mode );

#else
   int flags = fcntl( Socket->com, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags |= O_NONBLOCK;
      fcntl( Socket->com, F_SETFL, (LONG) flags );
   }
#endif
}


/*** Setup the blocking method **/

static void hb_socketSetBlocking( HB_SOCKET_STRUCT *Socket )
{
#ifdef HB_OS_WIN_32
   ULONG mode = 0;
   ioctlsocket( Socket->com, FIONBIO, &mode );
#else
   int flags = fcntl( Socket->com, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags &= ~O_NONBLOCK;
      fcntl( Socket->com, F_SETFL, ( long ) flags );
   }
#endif
}

/*** Utility to connect to a defined remote address ***/

static int hb_socketConnect( HB_SOCKET_STRUCT *Socket )
{
   int iErr1;
   #if ! defined(HB_OS_WIN_32)
      int iErrval;
      socklen_t iErrvalLen;
   #endif
   int iOpt = 1;

   setsockopt( Socket->com, SOL_SOCKET, SO_KEEPALIVE, (const char *) &iOpt , sizeof( iOpt ));

   /* we'll be using a nonblocking function */
   hb_socketSetNonBlocking( Socket );

   iErr1 = connect( Socket->com, (struct sockaddr *) &Socket->remote, sizeof(Socket->remote) );
   if( iErr1 != 0 )
   {
#if defined(HB_OS_WIN_32)
      if( WSAGetLastError() != WSAEWOULDBLOCK )
#else
      if( errno != EINPROGRESS )
#endif
      {
         HB_SOCKET_SET_ERROR( Socket );
      }
      else
      {
         /* Now we wait for socket connection or timeout */

#if defined(HB_OS_WIN_32)
         iErr1 = hb_selectWriteExceptSocket( Socket );
         if( iErr1 == 2 )
         {
            HB_SOCKET_SET_ERROR2( Socket, 2, "Connection failed" );
         }
         else if( iErr1 == 1 )
         {
            /* success */
         }
#else
         if( hb_selectWriteSocket( Socket ) )
         {
            /* Connection has been completed with a failure or a success */
            iErrvalLen = sizeof( iErrval );
            iErr1 = getsockopt( Socket->com,
               SOL_SOCKET,
               SO_ERROR,
               (void *) &iErrval,
               &iErrvalLen
            );

            if( iErr1 )
            {
               HB_SOCKET_SET_ERROR1( Socket, iErr1 );
            }
            else if( iErrval )
            {
               HB_SOCKET_SET_ERROR1( Socket, iErrval );
            }
            /* Success! */
         }
#endif
         /* Timed out */
         else
         {
            HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
         }
      }
   }

   hb_socketSetBlocking( Socket );

   return Socket->errorCode == 0;
}


static HB_GARBAGE_FUNC( hb_inetSocketFinalize )
{
   HB_SOCKET_STRUCT *Socket = ( HB_SOCKET_STRUCT *) Cargo;

   if( Socket->com > 0 )
   {
      #if defined( HB_OS_WIN_32 )
         shutdown( Socket->com, SD_BOTH );
      #elif defined(HB_OS_OS2)
         shutdown( Socket->com, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
      #elif !defined(__WATCOMC__)
         shutdown( Socket->com, SHUT_RDWR );
      #endif

      HB_INET_CLOSE( Socket->com );
   }

   if( Socket->caPeriodic != NULL )
   {
      hb_itemRelease( Socket->caPeriodic );
      Socket->caPeriodic = NULL;
   }
}

/*****************************************************
* Socket Initialization
***/

HB_FUNC( HB_INETINIT )
{
   if( s_iSessions )
   {
      s_iSessions++;
   }
   else
   {
      #if defined(HB_OS_WIN_32)
         WSADATA wsadata;
         WSAStartup( MAKEWORD(1,1), &wsadata );
      #elif defined( HB_OS_LINUX )
         signal( HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle );
      #endif
      s_iSessions = 1;
   }
}

HB_FUNC( HB_INETCLEANUP )
{
   if( --s_iSessions == 0 )
   {
      #if defined(HB_OS_WIN_32)
         WSACleanup();
      #endif
   }
}

/*****************************************************
* Socket Creation and destruction
***/

HB_FUNC( HB_INETCREATE )
{
   PHB_ITEM pSocket = NULL;
   HB_SOCKET_STRUCT *Socket;
   HB_SOCKET_INIT( Socket, pSocket );

   if( ISNUM( 1 ) )
      Socket->timeout = hb_parni(1);
   hb_itemRelease( hb_itemReturnForward( pSocket ) );
}

HB_FUNC( HB_INETCLOSE )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else if( Socket->com )
   {
      #if defined( HB_OS_WIN_32 )
         shutdown( Socket->com, SD_BOTH );
      #elif defined(HB_OS_OS2)
         shutdown( Socket->com, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
      #elif !defined(__WATCOMC__)
         shutdown( Socket->com, SHUT_RDWR );
      #endif

      hb_retni( HB_INET_CLOSE( Socket->com ) );

      Socket->com = 0;
      #ifdef HB_OS_LINUX
         kill( 0, HB_INET_LINUX_INTERRUPT );
      #endif
   }
   else
      hb_retni( -1 );
}

HB_FUNC( HB_INETFD )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
   {
      hb_retnint( Socket->com );
      if( ISLOG( 2 ) && hb_parl( 2 ) )
         Socket->com = 0;
   }
}

/************************************************
* Socket data access & management
***/

HB_FUNC( HB_INETSTATUS )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
      /* TODO: hb_retni( Socket->status ); */
      hb_retni( Socket->com == 0 ? -1 : 1 );
}

/* Prepared, but still not used; being in wait for comments
HB_FUNC( HB_INETSTATUSDESC )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   switch( Socket->status )
   {
      case 0: hb_retc( "Connection not opened" ); return;
      case 1: hb_retc( "Connection alive" ); return;
      case 2: hb_retc( "Last operation error" ); return;
      case 3: hb_retc( "Last operation timeout" ); return;
   }
}
*/

HB_FUNC( HB_INETERRORCODE )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
      hb_retni( Socket->errorCode );
}

HB_FUNC( HB_INETERRORDESC )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   hb_retc( Socket->errorDesc );
}

HB_FUNC( HB_INETCLEARERROR )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
      HB_SOCKET_ZERO_ERROR( Socket );
}


HB_FUNC( HB_INETCOUNT )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
      hb_retni( Socket->count );
}

HB_FUNC( HB_INETADDRESS )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   char *addr;

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
   {
      addr = inet_ntoa( Socket->remote.sin_addr );
      hb_retc( addr );
   }
}

HB_FUNC( HB_INETPORT )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
      hb_retni( ntohs( Socket->remote.sin_port ) );
}


HB_FUNC( HB_INETTIMEOUT )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket != NULL )
   {
      hb_retni( Socket->timeout );
      if( ISNUM( 2 ) )
         Socket->timeout = hb_parni( 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCLEARTIMEOUT )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
      Socket->timeout = -1;
}

HB_FUNC( HB_INETTIMELIMIT )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
   {
      hb_retnl( Socket->timelimit );
      if( ISNUM( 2 ) )
         Socket->timelimit = hb_parnl(2);
   }
}

HB_FUNC( HB_INETCLEARTIMELIMIT )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
      Socket->timelimit = -1;
}

HB_FUNC( HB_INETPERIODCALLBACK )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pExec = hb_param( 2, HB_IT_ARRAY | HB_IT_BLOCK | HB_IT_SYMBOL );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else
   {
      if( Socket->caPeriodic )
         hb_itemReturn( Socket->caPeriodic );
      if( pExec )
      {
         if( Socket->caPeriodic )
            hb_itemRelease( Socket->caPeriodic );
         Socket->caPeriodic = hb_itemClone( pExec );
      }
   }
}

HB_FUNC( HB_INETCLEARPERIODCALLBACK )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );

   if( Socket == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else if( Socket->caPeriodic )
   {
      hb_itemRelease( Socket->caPeriodic );
      Socket->caPeriodic = NULL;
   }
}

/********************************************************************
* TCP receive and send functions
***/

static void s_inetRecvInternal( int iMode )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   char *Buffer;
   int iLen, iMaxLen, iReceived, iBufferLen;
   int iTimeElapsed;

   if( Socket == NULL || pBuffer == NULL || !ISBYREF( 2 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   pBuffer = hb_itemUnShare( pBuffer );
   Buffer = hb_itemGetCPtr( pBuffer );
   iLen = hb_itemGetCLen( pBuffer );

   if( ISNIL( 3 ) )
   {
      iMaxLen = iLen;
   }
   else
   {
      iMaxLen = hb_parni( 3 );
      if( iLen < iMaxLen )
         iMaxLen = iLen;
   }

   iReceived = 0;
   iTimeElapsed = 0;
   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      if( iMode == 1 )
         iBufferLen = HB_SENDRECV_BUFFER_SIZE > iMaxLen - iReceived ? iMaxLen - iReceived : HB_SENDRECV_BUFFER_SIZE;
      else
         iBufferLen = iMaxLen;

      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recv( Socket->com, Buffer + iReceived, iBufferLen, MSG_NOSIGNAL );

         if( iLen > 0 )
            iReceived += iLen;

         /* Called from InetRecv()? */
         if( iMode == 0 )
            break;
      }
      else
      {
         /* timed out; let's see if we have to run a cb routine */
         iTimeElapsed += Socket->timeout;

         /* if we have a caPeriodic, timeLimit is our REAL timeout */
         if( Socket->caPeriodic != NULL )
         {
            hb_execFromArray( Socket->caPeriodic );

            /* do we continue? */
            if( ! hb_parl( -1 ) || ( Socket->timelimit != -1 && iTimeElapsed >= Socket->timelimit ) )
            {
               HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
               hb_retni( iReceived );
               return;
            }

            /* Declare success to continue loop */
            iLen = 1;
         }
         else /* the timeout has gone, and we have no recovery routine */
         {
            HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
            hb_retni( iReceived );
            return;
         }
      }
   }
   while( iReceived < iMaxLen && iLen > 0 );

   Socket->count = iReceived;

   if( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      hb_retni( iLen );
   }
   else if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_retni( iLen );
   }
   else
      hb_retni( iReceived );
}

HB_FUNC( HB_INETRECV )
{
   s_inetRecvInternal( 0 );
}


HB_FUNC( HB_INETRECVALL )
{
   s_inetRecvInternal( 1 );
}


static void s_inetRecvPattern( char *szPattern )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pResult     = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 4, HB_IT_NUMERIC );

   char cChar;
   char *Buffer;
   int iAllocated, iBufferSize, iMax;
   int iLen = 0;
   int iPos = 0, iTimeElapsed;
   ULONG ulPatPos;


   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( pBufferSize )
   {
      iBufferSize = hb_itemGetNI( pBufferSize );
   }
   else
   {
      iBufferSize = 80;
   }

   if( pMaxSize )
   {
      iMax = hb_itemGetNI( pMaxSize );
   }
   else
   {
      iMax = 0;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   Buffer = (char *) hb_xgrab( iBufferSize );
   iAllocated = iBufferSize;
   iTimeElapsed = 0;

   ulPatPos = 0;
   do
   {
      if( iPos == iAllocated - 1 )
      {
         iAllocated += iBufferSize;
         Buffer = ( char * ) hb_xrealloc( Buffer, iAllocated );
      }

      if( hb_selectReadSocket( Socket ) )
         iLen = recv( Socket->com, &cChar, 1, MSG_NOSIGNAL );
      else
      {
         iTimeElapsed += Socket->timeout;

         if( Socket->caPeriodic != NULL )
         {
            hb_execFromArray( Socket->caPeriodic );
            /* do we continue? */
            if( hb_parl( -1 ) &&
               ( Socket->timelimit == -1 || iTimeElapsed < Socket->timelimit ) )
            {
               continue;
            }
         }

         /* this signals timeout */
         iLen = -2;
      }

      if( iLen > 0 )
      {
         /* verify endsequence recognition automata status */
         if( cChar == szPattern[ ulPatPos ] )
         {
            ulPatPos ++;
            if( ! szPattern[ ulPatPos ] )
            {
               break;
            }
         }
         else
         {
            ulPatPos = 0;
         }

         Buffer[ iPos++ ] = cChar;
      }
      else
      {
         break;
      }
   }
   while( iMax == 0 || iPos < iMax );

   if( iLen <= 0 )
   {
      if( pResult )
      {
         hb_itemPutNI( pResult, iLen );
      }

      if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      }
      else if( iLen == -2 ) {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      }
      else
      {
         HB_SOCKET_SET_ERROR( Socket );
      }

      hb_xfree( (void *) Buffer );
      hb_ret();
   }
   else
   {
      if( iMax == 0 || iPos < iMax )
      {
         iPos--;
         Socket->count = iPos;

         if( pResult )
         {
            hb_itemPutNI( pResult, iPos );
         }

         hb_retclen_buffer( Buffer, iPos );
      }
      else
      {
         HB_SOCKET_SET_ERROR2( Socket, -3, "Buffer overrun" );

         if( pResult )
         {
            hb_itemPutNI( pResult, -2 );
         }

         hb_xfree( (void *) Buffer );
         hb_retc( NULL );
      }
   }
}

HB_FUNC( HB_INETRECVLINE )
{
   s_inetRecvPattern( "\r\n" );
}

HB_FUNC( HB_INETRECVENDBLOCK )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pProto      = hb_param( 2, HB_IT_ARRAY | HB_IT_STRING );
   PHB_ITEM pResult     = hb_param( 3, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 5, HB_IT_NUMERIC );

   char cChar;
   char *Buffer;
   char **Proto;
   int iAllocated, iBufferSize, iMax;
   int iLen;
   int iPos = 0;
   int iPosProto;
   int iTimeElapsed = 0;
   int iprotos;
   int i;
   int *iprotosize;
   int ifindproto = 0;
   BOOL bProtoFound;


   if( Socket == NULL  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( pProto )
   {
      if( HB_IS_ARRAY( pProto ) )
      {
         iprotos = (int) hb_arrayLen( pProto );
         if( iprotos <= 0 )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
            return;
         }

         Proto   = (char**) hb_xgrab( sizeof(char*) * iprotos );
         iprotosize = (int *) hb_xgrab( sizeof(int) * iprotos );

         for( i = 0; i < iprotos; i++ )
         {
            PHB_ITEM pItem  = hb_arrayGetItemPtr( pProto, i + 1 );
            Proto[ i ]      = hb_itemGetCPtr( pItem );
            iprotosize[ i ] = hb_itemGetCLen( pItem );
         }
      }
      else
      {
         Proto         = (char**) hb_xgrab( sizeof(char*) );
         iprotosize    = (int *) hb_xgrab( sizeof(int) );
         Proto[0]      = hb_itemGetCPtr( pProto );
         iprotosize[0] = hb_itemGetCLen( pProto );
         iprotos       = 1;
      }
   }
   else
   {
      Proto         = (char**) hb_xgrab( sizeof(char*) );
      iprotosize    = (int *) hb_xgrab( sizeof(int) );
      Proto[0]      = (char *) "\r\n";
      iprotos       = 1;
      iprotosize[0] = 2;
   }

   iBufferSize = pBufferSize ? hb_itemGetNI( pBufferSize ) : 80;
   iMax = pMaxSize ? hb_itemGetNI( pMaxSize ) : 0;

   HB_SOCKET_ZERO_ERROR( Socket );

   Buffer = (char *) hb_xgrab( iBufferSize );
   iAllocated = iBufferSize;

   do
   {
      if( iPos == iAllocated - 1 )
      {
         iAllocated += iBufferSize;
         Buffer = ( char * ) hb_xrealloc( Buffer, iAllocated );
      }

      iLen = 0;

      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recv( Socket->com, &cChar, 1, MSG_NOSIGNAL );
      }
      else
      {
         iTimeElapsed += Socket->timeout;
         if( Socket->caPeriodic != NULL )
         {
            hb_execFromArray( Socket->caPeriodic );

            if( hb_parl( -1 ) &&
                ( Socket->timelimit == -1 || iTimeElapsed < Socket->timelimit ) )
            {
               continue;
            }
         }

         iLen = -2;
      }

      if( iLen > 0 )
      {
         int protos;
         bProtoFound = 0;

         for( protos=0; protos < iprotos; protos++ )
         {
            if( cChar == Proto[protos][iprotosize[protos]-1] && iprotosize[protos] <= iPos )
            {
               bProtoFound = 1;
               for(iPosProto=0; iPosProto < (iprotosize[protos]-1); iPosProto++)
               {
                  if(Proto[protos][iPosProto] != Buffer[ (iPos-iprotosize[protos])+iPosProto+1 ])
                  {
                     bProtoFound = 0;
                     break;
                  }
               }
               if(bProtoFound)
               {
                  ifindproto = protos;
                  break;
               }
            }
         }

         if(bProtoFound)
         {
            break;
         }

         Buffer[ iPos++ ] = cChar;
      }
      else
      {
         break;
      }
   }
   while( iMax == 0 || iPos < iMax );

   if( iLen <= 0 )
   {
      if( pResult )
      {
         hb_itemPutNI( pResult, iLen );
      }

      if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      }
      else if( iLen == -2 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      }
      else
      {
         HB_SOCKET_SET_ERROR( Socket );
      }

      hb_xfree( ( void * ) Buffer );
      hb_retc( NULL );
   }
   else
   {
      if( iMax == 0 || iPos < iMax )
      {
         Socket->count = iPos;

         if( pResult )
         {
            hb_itemPutNI( pResult, iPos  - (iprotosize[ifindproto]-1) );
         }

         hb_retclen_buffer( Buffer, iPos  - (iprotosize[ifindproto]-1) );
      }
      else
      {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Buffer overrun" );

         if( pResult )
         {
            hb_itemPutNI( pResult, -2 );
         }

         hb_xfree( (void *) Buffer );
         hb_retc( NULL );
      }
   }

   hb_xfree( Proto );
   hb_xfree( iprotosize );
}

HB_FUNC( HB_INETDATAREADY )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   int iLen;
   fd_set rfds;
   struct timeval tv = {0,0};

   if( Socket == NULL || ( hb_pcount() == 2 && ! ISNUM( 2 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   /* Watch our socket. */
   if( hb_pcount() == 2 )
   {
      iLen = hb_parni( 2 );
      tv.tv_sec = iLen / 1000;
      tv.tv_usec = (iLen % 1000) * 1000;
   }

   FD_ZERO(&rfds);
   FD_SET(Socket->com, &rfds);

   iLen = select(Socket->com + 1, &rfds, NULL, NULL, &tv);
   /* Don't rely on the value of tv now! */

   if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }

   hb_retni( iLen );
}


static void s_inetSendInternal( int iMode )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   char *Buffer;
   int iLen, iSent, iSend, iBufferLen;

   if( Socket == NULL || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   Buffer = hb_itemGetCPtr( pBuffer );
   iSend = ( int ) hb_itemGetCLen( pBuffer );
   if( ISNUM( 3 ) )
   {
      iLen = hb_parni( 3 );
      if( iLen < iSend )
         iSend = iLen;
   }
   iSent = 0;

   HB_SOCKET_ZERO_ERROR( Socket );

   iLen = 0;
   while( iSent < iSend )
   {
      if( iMode == 1 )
         iBufferLen = HB_SENDRECV_BUFFER_SIZE > iSend - iSent ? iSend - iSent : HB_SENDRECV_BUFFER_SIZE;
      else
         iBufferLen = iSend;

      iLen = 0;
      if( hb_selectWriteSocket( Socket ) )
         iLen = send( Socket->com, Buffer + iSent, iBufferLen, MSG_NOSIGNAL );

      if( iLen > 0 )
      {
         iSent += iLen;
      }
      else if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -1 , "Timeout" );
         break;
      }
      else
      {
         HB_SOCKET_SET_ERROR( Socket );
         break;
      }
      if( iMode == 0 )
         break;
   }

   Socket->count = iSent;

   if( iLen > 0 )
   {
      hb_retni( iSent );
   }
   else
   {
      hb_retni( -1 );
   }
}

HB_FUNC( HB_INETSEND )
{
   s_inetSendInternal( 0 );
}

HB_FUNC( HB_INETSENDALL )
{
   s_inetSendInternal( 1 );
}


/*******************************************
* Name resolution interface functions
***/

HB_FUNC( HB_INETGETHOSTS )
{
   char * szHost = hb_parc( 1 );
   struct hostent *Host;
   char ** cHosts;
   int iCount = 0;

   if( szHost == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   Host = hb_getHosts( szHost, NULL );
   if( Host )
   {
      cHosts = Host->h_addr_list;
      while( *cHosts )
      {
         iCount++;
         cHosts++;
      }
   }

   if( iCount == 0 )
      hb_reta( 0 );
   else
   {
      PHB_ITEM pHosts = hb_itemArrayNew( iCount );
      iCount = 0;
      cHosts = Host->h_addr_list;
      while( *cHosts )
      {
         hb_itemPutC( hb_arrayGetItemPtr( pHosts, ++iCount ),
                      inet_ntoa( *( ( struct in_addr * ) * cHosts ) ) );
         cHosts++;
      }
      hb_itemRelease( hb_itemReturnForward( pHosts ) );
   }
}


HB_FUNC( HB_INETGETALIAS )
{
   char * szHost = hb_parc( 1 );
   struct hostent *Host;
   char ** cHosts;
   int iCount = 0;

   if( szHost == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   Host = hb_getHosts( szHost, NULL );
   if( Host )
   {
      cHosts = Host->h_aliases;
      while( *cHosts )
      {
         iCount++;
         cHosts++;
      }
   }

   if( iCount == 0 )
      hb_reta( 0 );
   else
   {
      PHB_ITEM pHosts = hb_itemArrayNew( iCount );
      iCount = 0;
      cHosts = Host->h_aliases;
      while( *cHosts )
      {
         hb_itemPutC( hb_arrayGetItemPtr( pHosts, ++iCount ),
                      inet_ntoa( *( ( struct in_addr * ) * cHosts ) ) );
         cHosts++;
      }
      hb_itemRelease( hb_itemReturnForward( pHosts ) );
   }
}


/**********************************************
* Server Specific functions
****/

HB_FUNC( HB_INETSERVER )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 2 );
   PHB_ITEM pSocket = NULL;
   char * szAddress;
   int iPort;
   int iOpt = 1;
   int iListen;

   /* Parameter error checking */
   if( ! ISNUM( 1 ) || ( Socket == NULL && !ISNIL( 2 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket != NULL )
   {
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
   {
      HB_SOCKET_INIT( Socket, pSocket );
   }

   /* Creates comm socket */
#if defined(HB_OS_WIN_32)
   Socket->com = socket( AF_INET, SOCK_STREAM, 0 );
#else
   Socket->com = socket( PF_INET, SOCK_STREAM, 0 );
#endif

   if( Socket->com == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->com = 0;
      if( pSocket )
         hb_itemRelease( hb_itemReturn( pSocket ) );
      else
         hb_itemReturn( hb_param( 2, HB_IT_ANY ) );
   }

   /* we'll be using only nonblocking sockets */
   /* hb_socketSetNonBlocking( Socket ); */

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( Socket->com, SOL_SOCKET, SO_REUSEADDR, (const char *) &iOpt, sizeof( iOpt ));

   iPort  = htons( hb_parni( 1 ) );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = iPort;

   szAddress = hb_parc( 2 );
   Socket->remote.sin_addr.s_addr = szAddress ? inet_addr( szAddress ) : INADDR_ANY;

   iListen = ISNUM( 3 ) ? hb_parni( 3 ) : 10;

   if( bind( Socket->com, (struct sockaddr *) &Socket->remote, sizeof(Socket->remote) ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
   }
   else if( listen( Socket->com, iListen ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
   }

   if( pSocket )
      hb_itemRelease( hb_itemReturnForward( pSocket ) );
   else
      hb_itemReturn( hb_param( 2, HB_IT_ANY ) );
}

HB_FUNC( HB_INETACCEPT )
{
#if !defined(EAGAIN)
#define EAGAIN -1
#endif
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   HB_SOCKET_STRUCT *NewSocket;
   HB_SOCKET_T incoming = 0;
   int iError = EAGAIN;
   struct sockaddr_in si_remote;
#if defined(_XOPEN_SOURCE_EXTENDED)
   socklen_t Len;
#elif defined(HB_OS_WIN_32)
   int Len;
#else
   unsigned int Len;
#endif

   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   Len = sizeof( struct sockaddr_in );

   /*
   * Accept can (and should) be asynchronously stopped by closing the
   * accepting socket. this will make the wait to terminate, and the
   * calling program will be notivfied through the status of the
   * returned socket.
   */

   HB_SOCKET_ZERO_ERROR( Socket );

   /* Connection incoming */
   while( iError == EAGAIN )
   {
      if( hb_selectReadSocket( Socket ) )
      {
         /* On error (e.g. async connection closed) , com will be -1 and
            errno == 22 (invalid argument ) */
         incoming = accept( Socket->com, (struct sockaddr *) &si_remote, &Len );

         if( incoming == ( HB_SOCKET_T ) -1 )
         {
#if defined(HB_OS_WIN_32)
            iError = WSAGetLastError();
#else
            iError = errno;
#endif
         }
         else
            iError = 0;
      }
      /* Timeout expired */
      else
         iError = -1;
   }

   if( iError == -1 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      hb_ret();
   }
   else if( iError > 0 )
   {
      HB_SOCKET_SET_ERROR1( Socket, iError );
      hb_ret();
   }
   else
   {
      PHB_ITEM pSocket = NULL;
      /* we'll be using only nonblocking sockets */
      HB_SOCKET_INIT( NewSocket, pSocket );
      memcpy( &NewSocket->remote, &si_remote, Len );
      NewSocket->com = incoming;
      /* hb_socketSetNonBlocking( NewSocket ); */
      hb_itemRelease( hb_itemReturnForward( pSocket ) );
   }
}


/**********************************************
* Client specific (connection functions)
****/

HB_FUNC( HB_INETCONNECT )
{
   char * szHost = hb_parc( 1 );
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 3 );
   PHB_ITEM pSocket = NULL;
   struct hostent *Host;
   int iPort;

   if( szHost == NULL || !ISNUM( 2 ) || ( Socket == NULL && !ISNIL( 3 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket != NULL )
   {
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
   {
      HB_SOCKET_INIT( Socket, pSocket );
   }

   Host = hb_getHosts( szHost, Socket );

   /* error had been set by get hosts */

   if( Host != NULL )
   {
      /* Creates comm socket */
#if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_STREAM, 0);
#else
      Socket->com = socket( PF_INET, SOCK_STREAM, 0);
#endif

      if( Socket->com == ( HB_SOCKET_T ) -1 )
      {
         HB_SOCKET_SET_ERROR( Socket );
      }
      else
      {
         iPort = htons( hb_parni( 2 ) );

         Socket->remote.sin_family = AF_INET;
         Socket->remote.sin_port= iPort;
         Socket->remote.sin_addr.s_addr = (*(UINT *)Host->h_addr_list[0]);

         hb_socketConnect( Socket );
      }
   }

   if( pSocket )
      hb_itemRelease( hb_itemReturnForward( pSocket ) );
   else
      hb_itemReturn( hb_param( 3, HB_IT_ANY ) );
}


HB_FUNC( HB_INETCONNECTIP )
{
   char * szHost = hb_parc( 1 );
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 3 );
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 2 );

   if( szHost == NULL || iPort == 0 || ( Socket == NULL && !ISNIL( 3 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket != NULL )
   {
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
   {
      HB_SOCKET_INIT( Socket, pSocket );
   }

   /* Creates comm socket */
#if defined(HB_OS_WIN_32)
   Socket->com = socket( AF_INET, SOCK_STREAM, 0);
#else
   Socket->com = socket( PF_INET, SOCK_STREAM, 0);
#endif

   if( Socket->com == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }
   else
   {
      iPort = htons( iPort );

      Socket->remote.sin_family = AF_INET;
      Socket->remote.sin_port= iPort;
      Socket->remote.sin_addr.s_addr = inet_addr( szHost );

      hb_socketConnect( Socket );
   }

   if( pSocket )
      hb_itemRelease( hb_itemReturnForward( pSocket ) );
   else
      hb_itemReturn( hb_param( 3, HB_IT_ANY ) );
}

/***********************************************************
* Datagram functions
************************************************************/

HB_FUNC( HB_INETDGRAMBIND )
{
   HB_SOCKET_STRUCT *Socket;
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni(1);
   int iOpt = 1;
   char * szAddress;

   /* Parameter error checking */
   if( iPort == 0 || ( hb_pcount() == 4 && ! ISCHAR(4) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );
   #else
      Socket->com = socket( PF_INET, SOCK_DGRAM, 0 );
   #endif

   if( Socket->com == (HB_SOCKET_T)-1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->com = 0;
      hb_itemRelease( hb_itemReturnForward( pSocket ) );
      return;
   }

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( Socket->com, SOL_SOCKET, SO_REUSEADDR, (const char *) &iOpt, sizeof( iOpt ));

   /* Setting broadcast if needed. */
   if( hb_parl( 3 ) )
   {
      iOpt = 1;
      setsockopt( Socket->com, SOL_SOCKET, SO_BROADCAST, (const char *) &iOpt, sizeof( iOpt ));
   }

   /* Binding here */
   iPort = htons( iPort );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = iPort;

   szAddress = hb_parc( 2 );
   Socket->remote.sin_addr.s_addr = szAddress ? inet_addr( szAddress ) : INADDR_ANY;

   if( bind( Socket->com, (struct sockaddr *) &Socket->remote, sizeof(Socket->remote) ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
   }

   if( hb_pcount() == 4 )
   {
      #ifndef IP_ADD_MEMBERSHIP
         #define IP_ADD_MEMBERSHIP  5     /* which header should this be in? */
      #endif

      /* this structure should be define in a header file.  The MS SDK indicates that */
      /* it is in Ws2tcpip.h but I'm not sure I know where it should go in xHb */
      struct ip_mreq
      {
         struct in_addr imr_multiaddr;  /* IP multicast address of group */
         struct in_addr imr_interface;  /* local IP address of interface */
      };

      struct ip_mreq mreq ;

      mreq.imr_multiaddr.s_addr = inet_addr( hb_parc( 4 ) ); /* HELLO_GROUP */
      mreq.imr_interface.s_addr = htonl( INADDR_ANY );

#ifndef IPPROTO_IP
      /*
       * some systems may not have this definitions, it should
       * be 0 what works with TCP/UDP sockets or explicitly set
       * to IPPROTO_TCP/IPPROTO_UDP
       */
#     define IPPROTO_IP 0
#endif

      if( setsockopt( Socket->com, IPPROTO_IP, IP_ADD_MEMBERSHIP, (const char *) &mreq, sizeof( mreq )) < 0)
      {
         HB_SOCKET_SET_ERROR( Socket );
         Socket->com = 0;
         hb_itemRelease( hb_itemReturnForward( pSocket ) );
         return;
      }
   }

   hb_itemRelease( hb_itemReturnForward( pSocket ) );
}

HB_FUNC( HB_INETDGRAM )
{
   HB_SOCKET_STRUCT *Socket;
   PHB_ITEM pSocket = NULL;
   int iOpt = 1;

   HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );
   #else
      Socket->com = socket( PF_INET, SOCK_DGRAM, 0 );
   #endif

   if( Socket->com == (HB_SOCKET_T)-1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->com = 0;
      hb_itemRelease( hb_itemReturnForward( pSocket ) );
      return;
   }

   /* Setting broadcast if needed. */
   if( hb_parl( 1 ) )
   {
      iOpt = 1;
      setsockopt( Socket->com, SOL_SOCKET, SO_BROADCAST, (const char *) &iOpt, sizeof( iOpt ));
   }
   /* we'll be using non blocking sockets in all functions */
   /* hb_socketSetNonBlocking( Socket ); */

   hb_itemRelease( hb_itemReturnForward( pSocket ) );
}

HB_FUNC( HB_INETDGRAMSEND )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   char * szAddress = hb_parc( 2 );
   int iPort = hb_parni( 3 );
   PHB_ITEM pBuffer = hb_param( 4, HB_IT_STRING );
   int iLen;
   char *szBuffer ;

   if( Socket == NULL ||
       szAddress == NULL || iPort == 0 || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = htons( iPort );
   Socket->remote.sin_addr.s_addr = inet_addr( szAddress );
   szBuffer = hb_itemGetCPtr( pBuffer );

   if( ISNUM( 5 ) )
   {
      iLen = hb_parni( 5 );
   }
   else
   {
      iLen = ( int ) hb_itemGetCLen( pBuffer );
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   Socket->count = 0;
   if( hb_selectWriteSocket( Socket ) )
   {
      Socket->count = sendto( Socket->com, szBuffer, iLen, 0,
            (const struct sockaddr *) &Socket->remote, sizeof( Socket->remote ) );
   }

   hb_retni( Socket->count );

   if( Socket->count == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
   }
   else if( Socket->count < 0 )
   {
      Socket->count = 0;
      HB_SOCKET_SET_ERROR( Socket );
   }

}


HB_FUNC( HB_INETDGRAMRECV )
{
   HB_SOCKET_STRUCT *Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   int iTimeElapsed = 0;
   int iLen, iMaxLen;
   char *Buffer;
   BOOL fRepeat;
#if defined(HB_OS_WIN_32)
   int iDtLen = sizeof( struct sockaddr );
#else
   socklen_t iDtLen = (socklen_t) sizeof( struct sockaddr );
#endif

   if( Socket == NULL || pBuffer == NULL || !ISBYREF( 2 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   pBuffer = hb_itemUnShare( pBuffer );
   Buffer = hb_itemGetCPtr( pBuffer );

   if( ISNUM( 3 ) )
   {
      iMaxLen = hb_parni( 3 );
   }
   else
   {
      iMaxLen = ( int ) hb_itemGetCLen( pBuffer );
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      fRepeat = FALSE;
      iLen = -2;
      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recvfrom( Socket->com, Buffer, iMaxLen, 0,
               (struct sockaddr *) &Socket->remote, &iDtLen );
      }
      iTimeElapsed += Socket->timeout;
      if( Socket->caPeriodic != NULL )
      {
         hb_execFromArray( Socket->caPeriodic );
         /* do we continue? */
         fRepeat = hb_parl( -1 ) &&
                   ( Socket->timelimit == -1 || iTimeElapsed < Socket->timelimit );
      }
   }
   while( fRepeat );

   if( iLen == -2 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      Socket->count = 0;
      iLen = -1;
   }
   else if( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      Socket->count = 0;
   }
   else if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->count = 0;
   }
   else
   {
      Socket->count = iLen;
   }
   hb_retni( iLen );
}


/***********************************************************
* Generic utility(?) functions
************************************************************/

HB_FUNC( HB_INETCRLF )
{
   hb_retc( "\r\n" );
}

HB_FUNC( HB_INETISSOCKET )
{
   hb_retl( HB_PARSOCKET( 1 ) != NULL );
}

#endif /* !HB_NO_DEFAULT_INET */
