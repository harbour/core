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
 * Copyright 2008 Miguel Angel marchuet <miguelangel@marchuet.net>
 *    added dinamic system buffer
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
#include "hbvm.h"

/* Compile in Unix mode under Cygwin */
#if defined( HB_OS_UNIX_COMPATIBLE )
   #undef HB_OS_WIN
#endif

/* HB_INET_H_ */
#if defined( HB_OS_DOS )

   #ifndef HB_NO_DEFAULT_INET
      #define HB_NO_DEFAULT_INET
   #endif

#else

   #include <string.h>

   #if defined( HB_OS_WIN )
      #define _WINSOCKAPI_  /* Prevents inclusion of winsock.h in windows.h */
      #define HB_SOCKET_T SOCKET
      #include <winsock2.h>
      #include <windows.h>

      #define HB_INET_CLOSE( x )    closesocket( x )
   #else

      #if defined( HB_OS_HPUX )
         #define _XOPEN_SOURCE_EXTENDED
      #endif

      #define HB_SOCKET_T int

      #include <errno.h>
#if defined( HB_OS_OS2 )
      #if defined( __WATCOMC__ )
         #include <types.h>
         #include <nerrno.h>
      #endif
      #include <sys/types.h>
      #include <sys/socket.h>
      #include <sys/select.h>
      #include <sys/ioctl.h>
#else
      #include <sys/types.h>
      #include <sys/socket.h>
#endif
      #include <netdb.h>
      #include <netinet/in.h>
      #include <arpa/inet.h>
      #include <unistd.h>

      #if ! defined( h_errno )
         #if defined( __WATCOMC__ )
            #define h_errno errno
         #else
            extern int h_errno;
         #endif
      #endif
      #define HB_INET_CLOSE( x )    close( x )
   #endif

   typedef struct _HB_SOCKET_STRUCT
   {
      HB_SOCKET_T        com;
      struct sockaddr_in remote;
      char               szErrorText[ 128 ];
      const char *       pszErrorText;
      int                iErrorCode;
      int                iCount;
      int                iTimeout;
      int                iTimeLimit;
      PHB_ITEM           pPeriodicBlock;
   } HB_SOCKET_STRUCT;

   #define HB_PARSOCKET( n )     ( ( HB_SOCKET_STRUCT * ) hb_parptrGC( hb_inetSocketFinalize, n ) )

   #define HB_SOCKET_ZERO_ERROR( s )   \
      do { \
         s->iErrorCode = 0; \
         s->pszErrorText = ""; \
      } while( 0 )

   #if defined( HB_OS_WIN )
      #if defined( _MSC_VER ) && _MSC_VER >= 1400
         #define HB_SOCKET_SET_ERROR( s )   \
            do { \
               s->iErrorCode = WSAGetLastError(); \
               strerror_s( s->szErrorText, sizeof( s->szErrorText ), s->iErrorCode ); \
               s->pszErrorText = s->szErrorText; \
               WSASetLastError( 0 ); \
            } while( 0 )
      #else
         #define HB_SOCKET_SET_ERROR( s )   \
            do { \
               s->iErrorCode = WSAGetLastError(); \
               s->pszErrorText = strerror( s->iErrorCode ); \
               WSASetLastError( 0 ); \
            } while( 0 )
      #endif
   #else
      #define HB_SOCKET_SET_ERROR( s )      \
         do { \
            s->iErrorCode = errno; \
            s->pszErrorText = strerror( errno ); \
         } while( 0 )
   #endif

   #if defined( _MSC_VER ) && _MSC_VER >= 1400
      #define HB_SOCKET_SET_ERROR1( s, code )   \
         do { \
            s->iErrorCode = code; \
            strerror_s( s->szErrorText, sizeof( s->szErrorText ), code ); \
            s->pszErrorText = s->szErrorText; \
         } while( 0 )
   #else
      #define HB_SOCKET_SET_ERROR1( s, code )   \
         do { \
            s->iErrorCode = code; \
            s->pszErrorText = strerror( code ); \
         } while( 0 )
   #endif
   #define HB_SOCKET_SET_ERROR2( s, code, desc )   \
      do { \
         s->iErrorCode = code; \
         s->pszErrorText = desc; \
      } while( 0 )

   #define HB_SOCKET_INIT( s, p ) \
      do { \
         s = ( HB_SOCKET_STRUCT * ) hb_gcAlloc( sizeof( HB_SOCKET_STRUCT ), hb_inetSocketFinalize ); \
         memset( s, '\0', sizeof( HB_SOCKET_STRUCT ) ); \
         s->com = ( HB_SOCKET_T ) -1; \
         s->iTimeout = -1; \
         s->iTimeLimit = -1; \
         s->pszErrorText = ""; \
         p = hb_itemPutPtrGC( p, s ); \
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

#if !defined( HB_OS_WIN_CE )
   #include <fcntl.h>
   #include <errno.h>
#endif

#if defined( HB_OS_UNIX ) || defined( HB_OS_UNIX_COMPATIBLE ) || defined( HB_OS_OS2 )
   #include <sys/time.h>
#endif


#if defined( HB_OS_OS2 ) || defined( HB_OS_WIN )
   /* NET_SIZE_T exists because of shortsightedness on the POSIX committee.  BSD
    * systems used "int *" as the parameter to accept(), getsockname(),
    * getpeername() et al.  Consequently many unixes took an int * for that
    * parameter.  The POSIX committee decided that "int" was just too generic and
    * had to be replaced with size_t almost everywhere.  There's no problem with
    * that when you're passing by value.  But when you're passing by reference
    * this creates a gross source incompatibility with existing programs.  On
    * 32-bit architectures it creates only a warning.  On 64-bit architectures it
    * creates broken code -- because "int *" is a pointer to a 32-bit quantity and
    * "size_t *" is frequently a pointer to a 64-bit quantity.
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

#if defined( __POCC__ ) && ( __POCC__ >= 500 ) && defined( HB_OS_WIN_64 )
   /* TOFIX: Bad workaround for the '__WSAFDIsSet unresolved' problem
             in Pelles C 5.00.13 AMD64 mode, to make final executables
             link at all. Some hbinet.c features (or the whole module)
             won't properly work though. [vszakats] */
   #undef FD_ISSET
   #define FD_ISSET( s, f ) ( 0 )
#endif

#if defined( HB_OS_LINUX )
#include <signal.h>
/* #define HB_INET_LINUX_INTERRUPT     SIGUSR1 + 90 */
#  ifdef HB_INET_LINUX_INTERRUPT
static void hb_inetLinuxSigusrHandle( int sig )
{
   /* nothing to do */
   HB_SYMBOL_UNUSED( sig );
}
#  endif
#endif

static const char * s_inetCRLF = "\r\n";

/* JC1: we need it volatile to be minimally thread safe. */
static volatile int s_iSessions = 0;

static BOOL hb_inetIsOpen( HB_SOCKET_STRUCT * Socket )
{
   if( Socket->com == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -4, "Closed socket" );
      return FALSE;
   }
   return TRUE;
}

/* Useful utility function to have a timeout; */

static int hb_selectReadSocket( HB_SOCKET_STRUCT * Socket )
{
   fd_set set;
   struct timeval tv;
   int iResult;

   hb_vmUnlock();

   FD_ZERO( &set );
   FD_SET( Socket->com, &set );

   if( Socket->iTimeout == -1 )
      iResult = select( Socket->com + 1, &set, NULL, NULL, NULL );
   else
   {
      tv.tv_sec = Socket->iTimeout / 1000;
      tv.tv_usec = ( Socket->iTimeout % 1000 ) * 1000;
      iResult = select( Socket->com + 1, &set, NULL, NULL, &tv );
   }

   hb_vmLock();

   return iResult > 0 ? FD_ISSET( Socket->com, &set ) : 0;
}

static int hb_selectWriteSocket( HB_SOCKET_STRUCT * Socket )
{
   fd_set set;
   struct timeval tv;
   int iResult;

   hb_vmUnlock();

   FD_ZERO( &set );
   FD_SET( Socket->com, &set );

   if( Socket->iTimeout == -1 )
      iResult = select( Socket->com + 1, NULL, &set, NULL, NULL );
   else
   {
      tv.tv_sec = Socket->iTimeout / 1000;
      tv.tv_usec = ( Socket->iTimeout % 1000 ) * 1000;
      iResult = select( Socket->com + 1, NULL, &set, NULL, &tv );
   }

   hb_vmLock();

   return iResult > 0 ? FD_ISSET( Socket->com, &set ) : 0;
}

#if defined( HB_OS_WIN )
static int hb_selectWriteExceptSocket( HB_SOCKET_STRUCT * Socket )
{
   fd_set set, eset;
   struct timeval tv;
   int iResult;

   hb_vmUnlock();

   FD_ZERO( &set );
   FD_SET( Socket->com, &set );
   FD_ZERO( &eset );
   FD_SET( Socket->com, &eset );

   if( Socket->iTimeout == -1 )
      iResult = select( Socket->com + 1, NULL, &set, &eset, NULL );
   else
   {
      tv.tv_sec = Socket->iTimeout / 1000;
      tv.tv_usec = ( Socket->iTimeout % 1000 ) * 1000;
      iResult = select( Socket->com + 1, NULL, &set, &eset, &tv );
   }

   hb_vmLock();

   if( iResult < 0 || FD_ISSET( Socket->com, &eset) )
      return 2;
   else if( FD_ISSET( Socket->com, &set ) )
      return 1;
   else
      return 0;
}
#endif

/*** Utilty to access host DNS */
static struct hostent * hb_getHosts( const char * name, HB_SOCKET_STRUCT * Socket )
{
   struct hostent * Host = NULL;

   hb_vmUnlock();

   /* TOFIX: make it MT safe */

   /* let's see if name is an IP address; not necessary on Linux */
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   {
      ULONG ulAddr;

      ulAddr = inet_addr( name );
      if( ulAddr == INADDR_NONE )
      {
         if( strcmp( "255.255.255.255", name ) == 0 )
            Host = gethostbyaddr( ( const char * ) &ulAddr, sizeof( ulAddr ), AF_INET );
      }
      else
         Host = gethostbyaddr( ( const char * )  &ulAddr, sizeof( ulAddr ), AF_INET );
   }
#endif

   if( Host == NULL )
      Host = gethostbyname( name );

   if( Host == NULL && Socket )
   {
#if defined( HB_OS_WIN )
      HB_SOCKET_SET_ERROR2( Socket, WSAGetLastError() , "Generic error in gethostbyname()" );
      WSASetLastError( 0 );
#elif defined( HB_OS_OS2 ) || defined( HB_OS_HPUX ) || defined( __WATCOMC__ )
      HB_SOCKET_SET_ERROR2( Socket, h_errno, "Generic error in gethostbyname()" );
#else
      HB_SOCKET_SET_ERROR2( Socket, h_errno, ( char * ) hstrerror( h_errno ) );
#endif
   }

   hb_vmLock();

   return Host;
}


/*** Setup the non-blocking method **/

static void hb_socketSetNonBlocking( HB_SOCKET_STRUCT * Socket )
{
#if defined( HB_OS_WIN )
   u_long mode = 1;
   ioctlsocket( Socket->com, FIONBIO, &mode );
#elif defined( O_NONBLOCK )
   int flags = fcntl( Socket->com, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags |= O_NONBLOCK;
      fcntl( Socket->com, F_SETFL, ( long ) flags );
   }
#else
   HB_SYMBOL_UNUSED( Socket );
#endif
}


/*** Setup the blocking method **/

static void hb_socketSetBlocking( HB_SOCKET_STRUCT * Socket )
{
#if defined( HB_OS_WIN )
   u_long mode = 0;
   ioctlsocket( Socket->com, FIONBIO, &mode );
#elif defined( O_NONBLOCK )
   int flags = fcntl( Socket->com, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags &= ~O_NONBLOCK;
      fcntl( Socket->com, F_SETFL, ( long ) flags );
   }
#else
   HB_SYMBOL_UNUSED( Socket );
#endif
}

/*** Utility to connect to a defined remote address ***/

static int hb_socketConnect( HB_SOCKET_STRUCT * Socket )
{
   int iErr1;
   #if ! defined( HB_OS_WIN )
      int iErrval;
      socklen_t iErrvalLen;
   #endif
   int iOpt = 1;

   hb_vmUnlock();

   setsockopt( Socket->com, SOL_SOCKET, SO_KEEPALIVE, ( const char * ) &iOpt, sizeof( iOpt ) );

   /* we'll be using a nonblocking function */
   hb_socketSetNonBlocking( Socket );

   iErr1 = connect( Socket->com, ( struct sockaddr * ) ( void * ) &Socket->remote, sizeof( Socket->remote ) );
   if( iErr1 != 0 )
   {
#if defined( HB_OS_WIN )
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

#if defined( HB_OS_WIN )
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
               ( void * ) &iErrval,
               &iErrvalLen
            );

            if( iErr1 )
               HB_SOCKET_SET_ERROR1( Socket, iErr1 );
            else if( iErrval )
               HB_SOCKET_SET_ERROR1( Socket, iErrval );
            /* Success! */
         }
#endif
         else
            HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      }
   }

   hb_socketSetBlocking( Socket );

   hb_vmLock();

   return Socket->iErrorCode == 0;
}


static HB_GARBAGE_FUNC( hb_inetSocketFinalize )
{
   HB_SOCKET_STRUCT * Socket = ( HB_SOCKET_STRUCT * ) Cargo;

   if( Socket->com != ( HB_SOCKET_T ) -1 )
   {
      #if defined( HB_OS_WIN )
         shutdown( Socket->com, SD_BOTH );
      #elif defined( HB_OS_OS2 )
         shutdown( Socket->com, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
      #elif !defined( __WATCOMC__ )
         shutdown( Socket->com, SHUT_RDWR );
      #endif

      HB_INET_CLOSE( Socket->com );
      Socket->com = ( HB_SOCKET_T ) -1;
   }

   if( Socket->pPeriodicBlock )
   {
      hb_itemRelease( Socket->pPeriodicBlock );
      Socket->pPeriodicBlock = NULL;
   }
}

/*****************************************************
* Socket Initialization
***/

HB_FUNC( HB_INETINIT )
{
   if( s_iSessions )
      s_iSessions++;
   else
   {
      #if defined( HB_OS_WIN )
         #define HB_MKWORD( l, h ) ((WORD)(((BYTE)(l)) | (((WORD)((BYTE)(h))) << 8)))
         WSADATA wsadata;
         WSAStartup( HB_MKWORD( 1, 1 ), &wsadata );
      #elif defined( HB_INET_LINUX_INTERRUPT )
         signal( HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle );
      #endif
      s_iSessions = 1;
   }
}

HB_FUNC( HB_INETCLEANUP )
{
   if( --s_iSessions == 0 )
   {
      #if defined( HB_OS_WIN )
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
   HB_SOCKET_STRUCT * Socket;
   HB_SOCKET_INIT( Socket, pSocket );

   if( HB_ISNUM( 1 ) )
      Socket->iTimeout = hb_parni( 1 );

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETCLOSE )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      if( Socket->com != ( HB_SOCKET_T ) -1 )
      {
         hb_vmUnlock();

         #if defined( HB_OS_WIN )
            shutdown( Socket->com, SD_BOTH );
         #elif defined( HB_OS_OS2 )
            shutdown( Socket->com, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
         #elif !defined( __WATCOMC__ )
            shutdown( Socket->com, SHUT_RDWR );
         #endif

         hb_retni( HB_INET_CLOSE( Socket->com ) );
         Socket->com = ( HB_SOCKET_T ) -1;

         #ifdef HB_INET_LINUX_INTERRUPT
            kill( 0, HB_INET_LINUX_INTERRUPT );
         #endif

         hb_vmLock();
      }
      else
         hb_retni( -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETFD )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      hb_retnint( Socket->com );

      if( hb_parl( 2 ) )
         Socket->com = ( HB_SOCKET_T ) -1;
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/************************************************
* Socket data access & management
***/

HB_FUNC( HB_INETSTATUS )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( Socket->com == ( HB_SOCKET_T ) -1 ? -1 : 1 ); /* TODO: hb_retni( Socket->status ); */
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Prepared, but still not used; being in wait for comments
HB_FUNC( HB_INETSTATUSDESC )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      switch( Socket->status )
      {
         case 0:  hb_retc_const( "Connection not opened" ); return;
         case 1:  hb_retc_const( "Connection alive" ); return;
         case 2:  hb_retc_const( "Last operation error" ); return;
         case 3:  hb_retc_const( "Last operation timeout" ); return;
         default: hb_retc_const( "unknown" );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
*/

HB_FUNC( HB_INETERRORCODE )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( Socket->iErrorCode );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETERRORDESC )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retc( Socket->pszErrorText );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCLEARERROR )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      HB_SOCKET_ZERO_ERROR( Socket );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCOUNT )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( Socket->iCount );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETADDRESS )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retc( inet_ntoa( Socket->remote.sin_addr ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETPORT )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( ntohs( Socket->remote.sin_port ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( HB_INETTIMEOUT )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      hb_retni( Socket->iTimeout );

      if( HB_ISNUM( 2 ) )
         Socket->iTimeout = hb_parni( 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCLEARTIMEOUT )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      Socket->iTimeout = -1;
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETTIMELIMIT )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      hb_retnl( Socket->iTimeLimit );

      if( HB_ISNUM( 2 ) )
         Socket->iTimeLimit = hb_parnl( 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCLEARTIMELIMIT )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
      Socket->iTimeLimit = -1;
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETPERIODCALLBACK )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      PHB_ITEM pExec = hb_param( 2, HB_IT_ARRAY | HB_IT_BLOCK | HB_IT_SYMBOL );

      if( Socket->pPeriodicBlock )
         hb_itemReturn( Socket->pPeriodicBlock );

      if( pExec )
      {
         if( Socket->pPeriodicBlock )
            hb_itemRelease( Socket->pPeriodicBlock );
         Socket->pPeriodicBlock = hb_itemClone( pExec );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCLEARPERIODCALLBACK )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      if( Socket->pPeriodicBlock )
      {
         hb_itemRelease( Socket->pPeriodicBlock );
         Socket->pPeriodicBlock = NULL;
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETGETSNDBUFSIZE )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      if( hb_inetIsOpen( Socket ) )
      {
         int value;
         socklen_t len = sizeof( value );
#if defined( HB_OS_WIN )
         getsockopt( Socket->com, SOL_SOCKET, SO_SNDBUF, ( char * ) &value, &len );
#else
         getsockopt( Socket->com, SOL_SOCKET, SO_SNDBUF, ( void * ) &value, &len );
#endif
         hb_retni( value );
      }
      else
         hb_retni( -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETGETRCVBUFSIZE )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      if( hb_inetIsOpen( Socket ) )
      {
         int value;
         socklen_t len = sizeof( value );
#if defined( HB_OS_WIN )
         getsockopt( Socket->com, SOL_SOCKET, SO_RCVBUF, ( char * ) &value, &len );
#else
         getsockopt( Socket->com, SOL_SOCKET, SO_RCVBUF, ( void * ) &value, &len );
#endif
         hb_retni( value );
      }
      else
         hb_retni( -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETSETSNDBUFSIZE )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      if( hb_inetIsOpen( Socket ) )
      {
         int value = hb_parni( 2 );
#if defined( HB_OS_WIN )
         setsockopt( Socket->com, SOL_SOCKET, SO_SNDBUF, ( char * ) &value, sizeof( value ) );
#else
         setsockopt( Socket->com, SOL_SOCKET, SO_SNDBUF, ( void * ) &value, sizeof( value ) );
#endif
         hb_retni( value );
      }
      else
         hb_retni( -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETSETRCVBUFSIZE )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      if( hb_inetIsOpen( Socket ) )
      {
         int value = hb_parni( 2 );
#if defined( HB_OS_WIN )
         setsockopt( Socket->com, SOL_SOCKET, SO_RCVBUF, ( char * ) &value, sizeof( value ) );
#else
         setsockopt( Socket->com, SOL_SOCKET, SO_RCVBUF, ( void * ) &value, sizeof( value ) );
#endif
         hb_retni( value );
      }
      else
         hb_retni( -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}



/********************************************************************
* TCP receive and send functions
***/

static void s_inetRecvInternal( int iMode )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   char * buffer;
   ULONG ulLen;
   int iLen, iMaxLen, iReceived;
   int iTimeElapsed;

   if( Socket == NULL || pBuffer == NULL || ! HB_ISBYREF( 2 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
   {
      hb_retni( 0 );
      return;
   }

   if( hb_itemGetWriteCL( pBuffer, &buffer, &ulLen ) )
      iLen = ( int ) ulLen;
   else
   {
      iLen = 0;
      buffer = NULL;
   }

   if( HB_ISNUM( 3 ) )
   {
      iMaxLen = hb_parni( 3 );
      if( iLen < iMaxLen )
         iMaxLen = iLen;
   }
   else
      iMaxLen = iLen;

   hb_vmUnlock();

   iReceived = 0;
   iTimeElapsed = 0;
   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recv( Socket->com, buffer + iReceived, iMaxLen - iReceived, MSG_NOSIGNAL );
         if( iLen > 0 )
            iReceived += iLen;

         /* Called from InetRecv()? */
         if( iMode == 0 )
            break;
      }
      else
      {
         /* timed out; let's see if we have to run a cb routine */
         iTimeElapsed += Socket->iTimeout;

         hb_vmLock();

         /* if we have a pPeriodicBlock, timeLimit is our REAL timeout */
         if( Socket->pPeriodicBlock )
         {
            hb_execFromArray( Socket->pPeriodicBlock );

            /* do we continue? */
            if( ! hb_parl( -1 ) || hb_vmRequestQuery() != 0 ||
                ( Socket->iTimeLimit != -1 && iTimeElapsed >= Socket->iTimeLimit ) )
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

         hb_vmUnlock();
      }
   }
   while( iReceived < iMaxLen && iLen > 0 );

   Socket->iCount = iReceived;

   hb_vmLock();

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


static void s_inetRecvPattern( const char * szPattern )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pResult     = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 4, HB_IT_NUMERIC );

   char cChar = '\0';
   char * Buffer;
   int iAllocated, iBufferSize, iMax;
   int iLen = 0, iPatLen;
   int iPos = 0, iTimeElapsed;

   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
   {
      hb_retni( 0 );
      return;
   }

   iBufferSize = pBufferSize ? hb_itemGetNI( pBufferSize ) : 80;
   iMax = pMaxSize ? hb_itemGetNI( pMaxSize ) : 0;

   hb_vmUnlock();

   HB_SOCKET_ZERO_ERROR( Socket );

   Buffer = ( char * ) hb_xgrab( iBufferSize );
   iAllocated = iBufferSize;
   iTimeElapsed = 0;
   iPatLen = ( int ) strlen( szPattern );

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
         iTimeElapsed += Socket->iTimeout;

         if( Socket->pPeriodicBlock )
         {
            BOOL fResult;

            hb_vmLock();
            hb_execFromArray( Socket->pPeriodicBlock );
            fResult = hb_parl( -1 ) && hb_vmRequestQuery() == 0;
            hb_vmUnlock();

            /* do we continue? */
            if( fResult &&
                ( Socket->iTimeLimit == -1 || iTimeElapsed < Socket->iTimeLimit ) )
            {
               continue;
            }
         }

         /* this signals timeout */
         iLen = -2;
      }

      if( iLen > 0 )
      {
         Buffer[ iPos++ ] = cChar;
         /* verify endsequence recognition automata status */
         if( iPos >= iPatLen &&
             memcmp( Buffer + iPos - iPatLen, szPattern, iPatLen ) == 0 )
         {
            break;
         }
      }
      else
         break;
   }
   while( iMax == 0 || iPos < iMax );

   hb_vmLock();

   if( iLen <= 0 )
   {
      if( pResult )
         hb_itemPutNI( pResult, iLen );

      if( iLen == 0 )
         HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      else if( iLen == -2 )
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      else
         HB_SOCKET_SET_ERROR( Socket );

      hb_xfree( Buffer );
   }
   else
   {
      if( iMax == 0 || iPos < iMax )
      {
         iPos -= iPatLen;
         Socket->iCount = iPos;

         if( pResult )
            hb_itemPutNI( pResult, iPos );

         hb_retclen_buffer( Buffer, iPos );
      }
      else
      {
         HB_SOCKET_SET_ERROR2( Socket, -3, "Buffer overrun" );

         if( pResult )
            hb_itemPutNI( pResult, -2 );

         hb_xfree( Buffer );
         hb_retc_null();
      }
   }
}

HB_FUNC( HB_INETRECVLINE )
{
   s_inetRecvPattern( s_inetCRLF );
}

HB_FUNC( HB_INETRECVENDBLOCK )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pProto      = hb_param( 2, HB_IT_ARRAY | HB_IT_STRING );
   PHB_ITEM pResult     = hb_param( 3, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 5, HB_IT_NUMERIC );

   char cChar = '\0';
   char * Buffer;
   const char ** Proto;
   int iAllocated, iBufferSize, iMax;
   int iLen;
   int iPos = 0;
   int iPosProto;
   int iTimeElapsed = 0;
   int iprotos;
   int i;
   int * iprotosize;
   int ifindproto = 0;
   BOOL bProtoFound;

   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
   {
      if( pResult )
         hb_itemPutNI( pResult, -1 );
      hb_retc_null();
      return;
   }

   if( pProto )
   {
      if( HB_IS_ARRAY( pProto ) )
      {
         iprotos = ( int ) hb_arrayLen( pProto );
         if( iprotos <= 0 )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            return;
         }

         Proto = ( const char ** ) hb_xgrab( sizeof( char * ) * iprotos );
         iprotosize = ( int * ) hb_xgrab( sizeof( int ) * iprotos );

         for( i = 0; i < iprotos; i++ )
         {
            Proto[ i ]      = hb_arrayGetCPtr( pProto, i + 1 );
            iprotosize[ i ] = hb_arrayGetCLen( pProto, i + 1 );
         }
      }
      else
      {
         Proto           = ( const char ** ) hb_xgrab( sizeof( char * ) );
         iprotosize      = ( int * ) hb_xgrab( sizeof( int ) );
         Proto[ 0 ]      = hb_itemGetCPtr( pProto );
         iprotosize[ 0 ] = hb_itemGetCLen( pProto );
         iprotos         = 1;
      }
   }
   else
   {
      Proto           = ( const char ** ) hb_xgrab( sizeof( char * ) );
      iprotosize      = ( int * ) hb_xgrab( sizeof( int ) );
      Proto[ 0 ]      = ( char * ) "\r\n";
      iprotos         = 1;
      iprotosize[ 0 ] = 2;
   }

   iBufferSize = pBufferSize ? hb_itemGetNI( pBufferSize ) : 80;
   iMax = pMaxSize ? hb_itemGetNI( pMaxSize ) : 0;

   hb_vmUnlock();

   HB_SOCKET_ZERO_ERROR( Socket );

   Buffer = ( char * ) hb_xgrab( iBufferSize );
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
         iTimeElapsed += Socket->iTimeout;
         if( Socket->pPeriodicBlock )
         {
            BOOL fResult;

            hb_vmLock();
            hb_execFromArray( Socket->pPeriodicBlock );
            fResult = hb_parl( -1 ) && hb_vmRequestQuery() == 0;
            hb_vmUnlock();

            if( fResult &&
                ( Socket->iTimeLimit == -1 || iTimeElapsed < Socket->iTimeLimit ) )
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

         for( protos = 0; protos < iprotos; protos++ )
         {
            if( cChar == Proto[ protos ][ iprotosize[ protos ] - 1 ] && iprotosize[ protos ] <= iPos )
            {
               bProtoFound = 1;
               for( iPosProto = 0; iPosProto < ( iprotosize[ protos ] - 1 ); iPosProto++ )
               {
                  if( Proto[ protos ][ iPosProto ] != Buffer[ ( iPos - iprotosize[ protos ] ) + iPosProto + 1 ] )
                  {
                     bProtoFound = 0;
                     break;
                  }
               }

               if( bProtoFound )
               {
                  ifindproto = protos;
                  break;
               }
            }
         }

         if( bProtoFound )
            break;

         Buffer[ iPos++ ] = cChar;
      }
      else
         break;
   }
   while( iMax == 0 || iPos < iMax );

   hb_vmLock();

   if( iLen <= 0 )
   {
      if( pResult )
         hb_itemPutNI( pResult, iLen );

      if( iLen == 0 )
         HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      else if( iLen == -2 )
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      else
         HB_SOCKET_SET_ERROR( Socket );

      hb_xfree( Buffer );
      hb_retc_null();
   }
   else
   {
      if( iMax == 0 || iPos < iMax )
      {
         Socket->iCount = iPos;

         if( pResult )
            hb_itemPutNI( pResult, iPos - ( iprotosize[ ifindproto ] - 1 ) );

         hb_retclen_buffer( Buffer, iPos - ( iprotosize[ ifindproto ] - 1 ) );
      }
      else
      {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Buffer overrun" );

         if( pResult )
            hb_itemPutNI( pResult, -2 );

         hb_xfree( Buffer );
         hb_retc_null();
      }
   }

   hb_xfree( Proto );
   hb_xfree( iprotosize );
}

HB_FUNC( HB_INETDATAREADY )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   int iVal;
   fd_set rfds;
   struct timeval tv = { 0, 0 };

   if( Socket == NULL || ( hb_pcount() >= 2 && ! HB_ISNUM( 2 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
   {
      hb_retl( FALSE );
      return;
   }

   if( HB_ISNUM( 2 ) )
   {
      iVal = hb_parni( 2 );
      tv.tv_sec = iVal / 1000;
      tv.tv_usec = ( iVal % 1000 ) * 1000;
   }

   hb_vmUnlock();

   HB_SOCKET_ZERO_ERROR( Socket );

   FD_ZERO( &rfds );
   FD_SET( Socket->com, &rfds );

   iVal = select( Socket->com + 1, &rfds, NULL, NULL, &tv );
   /* Don't rely on the value of tv now! */

   if( iVal < 0 )
      HB_SOCKET_SET_ERROR( Socket );

   hb_vmLock();

   hb_retni( iVal );
}


static void s_inetSendInternal( int iMode )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   const char * Buffer;
   int iLen, iSent, iSend;

   if( Socket == NULL || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
   {
      hb_retni( 0 );
      return;
   }

   Buffer = hb_itemGetCPtr( pBuffer );
   iSend = ( int ) hb_itemGetCLen( pBuffer );

   if( HB_ISNUM( 3 ) )
   {
      iLen = hb_parni( 3 );
      if( iLen < iSend )
         iSend = iLen;
   }

   hb_vmUnlock();

   HB_SOCKET_ZERO_ERROR( Socket );

   iSent = 0;
   iLen = 0;
   while( iSent < iSend )
   {
      if( hb_selectWriteSocket( Socket ) )
         iLen = send( Socket->com, Buffer + iSent, iSend - iSent, MSG_NOSIGNAL );
      else
         iLen = 0;

      if( iLen > 0 )
      {
         iSent += iLen;
      }
      else if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
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

   Socket->iCount = iSent;

   hb_vmLock();

   hb_retni( iLen > 0 ? iSent : -1 );
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
   const char * szHost = hb_parc( 1 );

   if( szHost )
   {
      struct hostent * Host = hb_getHosts( szHost, NULL );

      char ** cHosts;
      int iCount = 0;

      if( Host )
      {
         cHosts = Host->h_addr_list;
         while( *cHosts )
         {
            iCount++;
            cHosts++;
         }
      }

      if( iCount )
      {
         PHB_ITEM pHosts = hb_itemArrayNew( iCount );
         iCount = 0;
         cHosts = Host->h_addr_list;
         while( *cHosts )
         {
            hb_arraySetC( pHosts, ++iCount,
                          inet_ntoa( *( ( struct in_addr * ) * cHosts ) ) );
            cHosts++;
         }
         hb_itemReturnRelease( pHosts );
      }
      else
         hb_reta( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( HB_INETGETALIAS )
{
   const char * szHost = hb_parc( 1 );

   if( szHost )
   {
      struct hostent * Host = hb_getHosts( szHost, NULL );

      char ** cHosts;
      int iCount = 0;

      if( Host )
      {
         cHosts = Host->h_aliases;
         while( *cHosts )
         {
            iCount++;
            cHosts++;
         }
      }

      if( iCount )
      {
         PHB_ITEM pHosts = hb_itemArrayNew( iCount );
         iCount = 0;
         cHosts = Host->h_aliases;
         while( *cHosts )
         {
            hb_arraySetC( pHosts, ++iCount,
                          inet_ntoa( *( ( struct in_addr * ) * cHosts ) ) );
            cHosts++;
         }
         hb_itemReturnRelease( pHosts );
      }
      else
         hb_reta( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/**********************************************
* Server Specific functions
****/

HB_FUNC( HB_INETSERVER )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 2 );
   PHB_ITEM pSocket = NULL;
   const char * szAddress;
   int iPort;
   int iOpt = 1;
   int iListen;

   /* Parameter error checking */
   if( ! HB_ISNUM( 1 ) || ( Socket == NULL && ! HB_ISNIL( 2 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket )
      HB_SOCKET_ZERO_ERROR( Socket );
   else
      HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
#if defined( HB_OS_WIN )
   Socket->com = socket( AF_INET, SOCK_STREAM, 0 );
#else
   Socket->com = socket( PF_INET, SOCK_STREAM, 0 );
#endif

   if( Socket->com == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      if( pSocket )
         hb_itemReturnRelease( pSocket );
      else
         hb_itemReturn( hb_param( 2, HB_IT_ANY ) );
      return;
   }

   /* we'll be using only nonblocking sockets */
   /* hb_socketSetNonBlocking( Socket ); */

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( Socket->com, SOL_SOCKET, SO_REUSEADDR, ( const char * ) &iOpt, sizeof( iOpt ) );

   iPort = htons( ( hbU16 ) hb_parni( 1 ) );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = ( hbU16 ) iPort;

   szAddress = hb_parc( 2 );
   Socket->remote.sin_addr.s_addr = szAddress ? inet_addr( szAddress ) : INADDR_ANY;

   iListen = HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10;

   hb_vmUnlock();

   if( bind( Socket->com, ( struct sockaddr * ) ( void * ) &Socket->remote, sizeof( Socket->remote ) ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
      Socket->com = ( HB_SOCKET_T ) -1;
   }
   else if( listen( Socket->com, iListen ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
      Socket->com = ( HB_SOCKET_T ) -1;
   }

   hb_vmLock();

   if( pSocket )
      hb_itemReturnRelease( pSocket );
   else
      hb_itemReturn( hb_param( 2, HB_IT_ANY ) );
}

#ifndef EAGAIN
   #define EAGAIN -1
#endif

HB_FUNC( HB_INETACCEPT )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   HB_SOCKET_STRUCT * NewSocket;
   HB_SOCKET_T incoming = 0;
   int iError = EAGAIN;
   struct sockaddr_in si_remote;
   socklen_t Len;

   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
      return;

   hb_vmUnlock();

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
         incoming = accept( Socket->com, ( struct sockaddr * ) ( void * ) &si_remote, &Len );

         if( incoming == ( HB_SOCKET_T ) -1 )
         {
#if defined( HB_OS_WIN )
            iError = WSAGetLastError();
#else
            iError = errno;
#endif
         }
         else
            iError = 0;
      }
      else
         iError = -1; /* Timeout expired */
   }

   hb_vmLock();

   if( iError == -1 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
   }
   else if( iError > 0 )
   {
      HB_SOCKET_SET_ERROR1( Socket, iError );
   }
   else
   {
      PHB_ITEM pSocket = NULL;
      /* we'll be using only nonblocking sockets */
      HB_SOCKET_INIT( NewSocket, pSocket );
      memcpy( &NewSocket->remote, &si_remote, Len );
      NewSocket->com = incoming;
      /* hb_socketSetNonBlocking( NewSocket ); */
      hb_itemReturnRelease( pSocket );
   }
}

/**********************************************
* Client specific (connection functions)
****/

HB_FUNC( HB_INETCONNECT )
{
   const char * szHost = hb_parc( 1 );
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 3 );
   PHB_ITEM pSocket = NULL;
   struct hostent * Host;
   int iPort;

   if( szHost == NULL || ! HB_ISNUM( 2 ) || ( Socket == NULL && ! HB_ISNIL( 3 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket )
   {
      if( Socket->com != ( HB_SOCKET_T ) -1 )
      {
         HB_INET_CLOSE( Socket->com );
         Socket->com = ( HB_SOCKET_T ) -1;
      }
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
      HB_SOCKET_INIT( Socket, pSocket );

   Host = hb_getHosts( szHost, Socket );

   /* error had been set by get hosts */

   if( Host )
   {
      /* Creates comm socket */
#if defined( HB_OS_WIN )
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
         iPort = htons( ( hbU16 ) hb_parni( 2 ) );

         Socket->remote.sin_family = AF_INET;
         Socket->remote.sin_port = ( hbU16 ) iPort;
         Socket->remote.sin_addr.s_addr = ( * ( UINT * ) Host->h_addr_list[ 0 ] );

         hb_socketConnect( Socket );
      }
   }

   if( pSocket )
      hb_itemReturnRelease( pSocket );
   else
      hb_itemReturn( hb_param( 3, HB_IT_ANY ) );
}

HB_FUNC( HB_INETCONNECTIP )
{
   const char * szHost = hb_parc( 1 );
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 3 );
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 2 );

   if( szHost == NULL || iPort == 0 || ( Socket == NULL && ! HB_ISNIL( 3 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket )
   {
      if( Socket->com != ( HB_SOCKET_T ) -1 )
      {
         HB_INET_CLOSE( Socket->com );
         Socket->com = ( HB_SOCKET_T ) -1;
      }
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
   {
      HB_SOCKET_INIT( Socket, pSocket );
   }

   /* Creates comm socket */
#if defined( HB_OS_WIN )
   Socket->com = socket( AF_INET, SOCK_STREAM, 0 );
#else
   Socket->com = socket( PF_INET, SOCK_STREAM, 0 );
#endif

   if( Socket->com == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }
   else
   {
      iPort = htons( ( hbU16 ) iPort );

      Socket->remote.sin_family = AF_INET;
      Socket->remote.sin_port = ( hbU16 ) iPort;
      Socket->remote.sin_addr.s_addr = inet_addr( szHost );

      hb_socketConnect( Socket );
   }

   if( pSocket )
      hb_itemReturnRelease( pSocket );
   else
      hb_itemReturn( hb_param( 3, HB_IT_ANY ) );
}

/***********************************************************
* Datagram functions
************************************************************/

HB_FUNC( HB_INETDGRAMBIND )
{
   HB_SOCKET_STRUCT * Socket;
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 1 );
   int iOpt = 1;
   const char * szAddress;

   /* Parameter error checking */
   if( iPort == 0 || ( hb_pcount() >= 4 && ! HB_ISCHAR( 4 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
#if defined( HB_OS_WIN )
   Socket->com = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );
#else
   Socket->com = socket( PF_INET, SOCK_DGRAM, 0 );
#endif

   if( Socket->com == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_itemReturnRelease( pSocket );
      return;
   }

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( Socket->com, SOL_SOCKET, SO_REUSEADDR, ( const char * ) &iOpt, sizeof( iOpt ) );

   /* Setting broadcast if needed. */
   if( hb_parl( 3 ) )
   {
      iOpt = 1;
      setsockopt( Socket->com, SOL_SOCKET, SO_BROADCAST, ( const char * ) &iOpt, sizeof( iOpt ) );
   }

   /* Binding here */
   iPort = htons( ( hbU16 ) iPort );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = ( hbU16 ) iPort;

   szAddress = hb_parc( 2 );
   Socket->remote.sin_addr.s_addr = szAddress ? inet_addr( szAddress ) : INADDR_ANY;

   hb_vmUnlock();

   if( bind( Socket->com, ( struct sockaddr * ) ( void * ) &Socket->remote, sizeof( Socket->remote ) ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
      Socket->com = ( HB_SOCKET_T ) -1;
   }
   else if( hb_pcount() >= 4 )
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

      struct ip_mreq mreq;

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

      if( setsockopt( Socket->com, IPPROTO_IP, IP_ADD_MEMBERSHIP, ( const char * ) &mreq, sizeof( mreq ) ) < 0 )
      {
         HB_SOCKET_SET_ERROR( Socket );
      }
   }

   hb_vmLock();

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETDGRAM )
{
   HB_SOCKET_STRUCT * Socket;
   PHB_ITEM pSocket = NULL;
   int iOpt = 1;

   HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
#if defined( HB_OS_WIN )
   Socket->com = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );
#else
   Socket->com = socket( PF_INET, SOCK_DGRAM, 0 );
#endif

   if( Socket->com == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_itemReturnRelease( pSocket );
      return;
   }

   /* Setting broadcast if needed. */
   if( hb_parl( 1 ) )
   {
      iOpt = 1;
      setsockopt( Socket->com, SOL_SOCKET, SO_BROADCAST, ( const char * ) &iOpt, sizeof( iOpt ) );
   }
   /* we'll be using non blocking sockets in all functions */
   /* hb_socketSetNonBlocking( Socket ); */

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETDGRAMSEND )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   const char * szAddress = hb_parc( 2 );
   int iPort = hb_parni( 3 );
   PHB_ITEM pBuffer = hb_param( 4, HB_IT_STRING );
   int iLen;
   const char * szBuffer;

   if( Socket == NULL || szAddress == NULL || iPort == 0 || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
   {
      Socket->iCount = 0;
      hb_retni( 0 );
      return;
   }

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = htons( ( hbU16 ) iPort );
   Socket->remote.sin_addr.s_addr = inet_addr( szAddress );
   szBuffer = hb_itemGetCPtr( pBuffer );

   iLen = ( int ) hb_itemGetCLen( pBuffer );
   if( HB_ISNUM( 5 ) )
   {
      int iMaxLen = hb_parni( 5 );

      if( iMaxLen < iLen )
         iLen = iMaxLen;
   }

   hb_vmUnlock();

   HB_SOCKET_ZERO_ERROR( Socket );

   Socket->iCount = 0;
   if( hb_selectWriteSocket( Socket ) )
   {
      Socket->iCount = ( int ) sendto( Socket->com, szBuffer, iLen, 0,
            ( const struct sockaddr * ) ( void * ) &Socket->remote, sizeof( Socket->remote ) );
   }

   hb_vmLock();

   hb_retni( Socket->iCount );

   if( Socket->iCount == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
   }
   else if( Socket->iCount < 0 )
   {
      Socket->iCount = 0;
      HB_SOCKET_SET_ERROR( Socket );
   }
}

HB_FUNC( HB_INETDGRAMRECV )
{
   HB_SOCKET_STRUCT * Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   int iTimeElapsed = 0;
   int iLen, iMaxLen;
   char * Buffer;
   ULONG ulLen;
   BOOL fRepeat;
   socklen_t iDtLen = ( socklen_t ) sizeof( struct sockaddr );

   if( Socket == NULL || pBuffer == NULL || ! HB_ISBYREF( 2 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
   {
      Socket->iCount = 0;
      hb_retni( -1 );
      return;
   }

   if( hb_itemGetWriteCL( pBuffer, &Buffer, &ulLen ) )
      iLen = ( int ) ulLen;
   else
   {
      iLen = 0;
      Buffer = NULL;
   }

   if( HB_ISNUM( 3 ) )
   {
      iMaxLen = hb_parni( 3 );
      if( iLen < iMaxLen )
         iMaxLen = iLen;
   }
   else
      iMaxLen = iLen;

   hb_vmUnlock();

   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      fRepeat = FALSE;
      iLen = -2;
      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recvfrom( Socket->com, Buffer, iMaxLen, 0,
               ( struct sockaddr * ) ( void * ) &Socket->remote, &iDtLen );
      }
      iTimeElapsed += Socket->iTimeout;
      if( Socket->pPeriodicBlock )
      {
         hb_vmLock();
         hb_execFromArray( Socket->pPeriodicBlock );
         /* do we continue? */
         fRepeat = hb_parl( -1 ) && hb_vmRequestQuery() == 0 &&
                   ( Socket->iTimeLimit == -1 || iTimeElapsed < Socket->iTimeLimit );
         hb_vmUnlock();
      }
   }
   while( fRepeat );

   if( iLen == -2 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      Socket->iCount = 0;
      iLen = -1;
   }
   else if( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      Socket->iCount = 0;
   }
   else if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->iCount = 0;
   }
   else
      Socket->iCount = iLen;

   hb_vmLock();

   hb_retni( iLen );
}


/***********************************************************
* Generic utility(?) functions
************************************************************/

HB_FUNC( HB_INETCRLF )
{
   hb_retc_const( s_inetCRLF );
}

HB_FUNC( HB_INETISSOCKET )
{
   hb_retl( HB_PARSOCKET( 1 ) != NULL );
}

#endif /* !HB_NO_DEFAULT_INET */
