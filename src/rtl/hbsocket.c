/*
 * Harbour Project source code:
 *    socket C API
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

#include "hbsocket.h"

#if ( defined( HB_OS_DOS ) && ! defined( HB_HAS_WATT ) ) || \
   defined( HB_OS_SYMBIAN ) || defined( __TINYC__ )
#  if ! defined( HB_SOCKET_OFF )
#     define HB_SOCKET_OFF
#  endif
#endif

#if ! defined( HB_SOCKET_OFF )

/* we do not use autoconf so we can only guess what is supported
 * by platform and/or CRTL :-(
 */
/* we are using following macros:

   platform supports inet_aton() function:
      #define HB_HAS_INET_ATON

   platform supports inet_pton() function:
      #define HB_HAS_INET_PTON

   platform supports inet_ntop() function:
      #define HB_HAS_INET_NTOP

   platform supports thread safe (using TLS) inet_ntoa() function:
      #define HB_IS_INET_NTOA_MT_SAFE

   platform supports getaddrinfo()/freeaddrinfo() functions:
      #define HB_HAS_ADDRINFO

   platform supports getnameinfo() function:
      #define HB_HAS_NAMEINFO

   platform supports gethostbyaddr function:
      #define HB_HAS_GETHOSTBYADDR

   platform uses sockaddr structure which contains sa_len member:
      #define HB_HAS_SOCKADDR_SA_LEN

   platform supports constant inet6 addresses in6addr_any and in6addr_loopback:
      #define HB_HAS_INET6_ADDR_CONST

   platform supports IP6 protocol:
      #define HB_HAS_INET6

   platform supports unix/local protocol:
      #define HB_HAS_UNIX

   platform supports 'struct sockaddr_storage' which can be used as holder
   for all socket address structures in all supported protocols,
   simple 'struct sockaddr' is not large enough for such usage:
      #define HB_HAS_SOCKADDR_STORAGE

   timeval parameter used in Select() function is updated by kernel/CRTL
   and decreased by the amount of time the function was waiting:
      #define HB_HAS_SELECT_TIMER

   all implementations should use BSD compatible constant values but
   if it's not guarantied then these two macros can be used.
   protocol families have to be translated:
      #define HB_SOCKET_TRANSLATE_DOMAIN

   protocol types have to be translated:
      #define HB_SOCKET_TRANSLATE_TYPE
*/

#if defined( HB_OS_HPUX )
#  define _XOPEN_SOURCE_EXTENDED
#endif

#if defined( HB_OS_UNIX )
#  define HB_HAS_UNIX
#  if ! defined( __WATCOMC__ )
#     define HB_HAS_INET_ATON
#     define HB_HAS_INET_PTON
#     define HB_HAS_INET_NTOP
#     define HB_HAS_SOCKADDR_STORAGE
#     define HB_HAS_ADDRINFO
#     define HB_HAS_NAMEINFO
#     define HB_HAS_GETHOSTBYADDR
#  endif
#  if ! defined( __WATCOMC__ ) && ! defined( HB_OS_BEOS ) && ! defined( HB_OS_MINIX )
#     define HB_HAS_INET6
#     if ! defined( HB_OS_VXWORKS )
#        define HB_HAS_INET6_ADDR_CONST
#     endif
#  endif
#  if defined( HB_OS_BEOS )
#     define HB_SOCKET_TRANSLATE_DOMAIN
#     define HB_SOCKET_TRANSLATE_TYPE
#     define HB_HAS_SOCKADDR_SA_LEN
#  endif
#  if defined( HB_OS_LINUX )
#     define HB_HAS_SELECT_TIMER
#  endif
#  if defined( HB_OS_SUNOS )
#     if ! defined( BSD_COMP )
#        define BSD_COMP
#     endif
#     define HB_SOCKET_TRANSLATE_DOMAIN
#     define HB_SOCKET_TRANSLATE_TYPE
#  endif
#  if defined( HB_OS_BSD )
#     define HB_SOCKET_TRANSLATE_DOMAIN
#     define HB_HAS_SOCKADDR_SA_LEN
#  endif
#elif defined( HB_OS_WIN )
#  if defined( __WATCOMC__ )
#     if ( NTDDI_VERSION >= 0x06000000 )
#        define HB_HAS_INET_PTON
#        define HB_HAS_INET_NTOP
#     endif
#     define HB_HAS_SOCKADDR_STORAGE
/* #     define HB_HAS_INET6 */
#  elif defined( __MINGW32__ )
#     define HB_HAS_SOCKADDR_STORAGE
#  elif defined( __POCC__ ) && ! defined( __XCC__ )
#     define HB_HAS_SOCKADDR_STORAGE
#  endif
#  define HB_IS_INET_NTOA_MT_SAFE
#  define HB_HAS_GETHOSTBYADDR
#elif defined( HB_OS_OS2 )
#  if defined( __WATCOMC__ )
#     define HB_HAS_INET_PTON
#     define HB_HAS_INET_NTOP
#     define HB_HAS_SOCKADDR_SA_LEN
#  else
#     if ! defined( TCPV40HDRS )
#        define HB_HAS_INET_ATON
#        define HB_HAS_INET_PTON
#        define HB_HAS_INET_NTOP
#        define HB_HAS_SOCKADDR_SA_LEN
#     endif
#  endif
#  define HB_HAS_GETHOSTBYADDR
#elif defined( HB_OS_DOS )
#  define HB_HAS_INET_ATON
#  define HB_HAS_INET_PTON
#  define HB_HAS_INET_NTOP
#  define HB_HAS_SOCKADDR_STORAGE
#  define HB_HAS_ADDRINFO
#  define HB_HAS_NAMEINFO
#  define HB_HAS_GETHOSTBYADDR
#  define HB_HAS_INET6_ADDR_CONST
/* #  define HB_HAS_INET6 */
#endif


#if defined( HB_OS_WIN )
#  include <winsock2.h>
#  include <ws2tcpip.h>
#else
#  include <errno.h>
#  if defined( HB_OS_DOS )
#     if defined( __WATCOMC__ )
         /* workaround for declaration conflicts in tcp.h */
#        define _GETOPT_H
#     endif
#     include <tcp.h>
#  elif defined( HB_OS_OS2 )
#     if defined( __WATCOMC__ )
#        include <types.h>
#        include <nerrno.h>
#     endif
#     include <sys/socket.h>
#     include <sys/select.h>
#     include <arpa/inet.h>
#  endif
#  if ! ( defined( HB_OS_DOS ) && defined( __WATCOMC__ ) )
#     include <sys/time.h>
#  endif
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <sys/ioctl.h>
#  if defined( HB_OS_BEOS )
#     include <sys/sockio.h>
#  endif
#  if defined( HB_OS_VXWORKS )
#     include <sockLib.h>
#     include <ioLib.h>
#  endif
#  include <netdb.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  if defined( HB_HAS_UNIX )
#     include <sys/un.h>
#  endif
#  include <netinet/tcp.h>
#  if ! ( defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) )
#     include <net/if.h>
#  endif
#  include <unistd.h>
#  include <fcntl.h>
#  if defined( HB_OS_DOS )
#     define select          select_s
#  endif
#endif

#if defined( HB_OS_OS2 ) || defined( HB_OS_WIN ) || defined( HB_OS_DOS ) || \
    defined( HB_OS_VXWORKS )
#  define socklen_t int
#endif

#if ! defined( INET_ADDRSTRLEN )
#  define INET_ADDRSTRLEN  16
#endif

#if defined( HB_OS_DOS ) && ! defined( SHUT_RD )
#  define SHUT_RD       0
#  define SHUT_WR       1
#  define SHUT_RDWR     2
#endif

#if defined( __WATCOMC__ ) && defined( HB_OS_LINUX ) && ! defined( IP_ADD_MEMBERSHIP )
   /* it's missed in OpenWatcom 1.8 Linux header files :-( */
#  define IP_ADD_MEMBERSHIP   35
   struct ip_mreq
   {
      struct in_addr imr_multiaddr;    /* IP multicast address of group */
      struct in_addr imr_interface;    /* Local IP address of interface */
   };
#endif

#if defined( __POCC__ ) && ( __POCC__ >= 500 ) && defined( HB_OS_WIN_64 )
   /* TOFIX: Bad workaround for the '__WSAFDIsSet unresolved' problem
             in Pelles C 5.00.13 AMD64 mode, to make final executables
             link at all. Some hbsocket.c features (or the whole module)
             won't properly work though. [vszakats] */
   #undef FD_ISSET
   #define FD_ISSET( s, f ) ( 0 )
#endif

#if defined( HB_OS_WIN )
   typedef SOCKET       HB_SOCKET_T;
#else
   typedef HB_SOCKET    HB_SOCKET_T;
#endif

#endif /* HB_SOCKET_OFF */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbthread.h"
#include "hbdate.h"

/* TODO change error description to sth more user friendly */
static const char * s_socketErrors[] = {
   "OK",
   "EPIPE",
   "ETIMEOUT",
   "EWRONGADDR",
   "EAFNOSUPPORT",
   "EPFNOSUPPORT",
   "EPROTONOSUPPORT",
   "EPARAMVALUE",
   "ENOSUPPORT",
   "ENORESOURCE",
   "EACCESS",
   "EADDRINUSE",
   "EINTERRUPT",
   "EALREADYCONNECTED",
   "ECONNREFUSED",
   "ECONNABORTED",
   "ECONNRESET",
   "ENETUNREACH",
   "ENETDOWN",
   "ENETRESET",
   "EINPROGRESS",
   "EALREADY",
   "EADDRNOTAVAIL",
   "EREADONLY",
   "EAGAIN",
   "EINVALIDHANDLE",
   "EINVAL",
   "EPROTO",
   "EPROTOTYPE",
   "ENOFILE",
   "ENOBUFS",
   "ENOMEM",
   "EFAULT",
   "ENAMETOOLONG",
   "ENOENT",
   "ENOTDIR",
   "ELOOP",
   "EMSGSIZE",
   "EDESTADDRREQ",
   "ENOPROTOOPT",
   "ENOTCONN",
   "ESHUTDOWN",
   "ETOOMANYREFS",
   "ERESTARTSYS",
   "ENOSR",
   "EHOSTDOWN",
   "EHOSTUNREACH",
   "ENOTEMPTY",
   "EUSERS",
   "EDQUOT",
   "ESTALE",
   "EREMOTE",
   "EPROCLIM",
   "EDISCON",
   "ENOMORE",
   "ECANCELLED",
   "EINVALIDPROCTABLE",
   "EINVALIDPROVIDER",
   "EPROVIDERFAILEDINIT",
   "EREFUSED",
   "ESYSNOTREADY",
   "EVERNOTSUPPORTED",
   "ENOTINITIALISED",
   "TRYAGAIN",
   "HOSTNOTFOUND",
   "NORECOVERY",
   "NODATA",
   "ESYSCALLFAILURE",
   "ESERVICENOTFOUND",
   "ETYPENOTFOUND",
   "EOTHER"
};

int hb_socketGetError( void )
{
   return hb_stackIOErrors()->uiSocketError;
}

int hb_socketGetOsError( void )
{
   return hb_stackIOErrors()->iSocketOsError;
}

const char * hb_socketErrorStr( int iError )
{
   if( iError >= 0 && iError <= HB_SOCKET_ERR_OTHER )
      return s_socketErrors[ iError ];
   else
      return "";
}

static void hb_socketSetRawError( int err )
{
   PHB_IOERRORS pError = hb_stackIOErrors();

   pError->uiSocketError = ( HB_ERRCODE ) err;
   pError->iSocketOsError = 0;
}

#if defined( HB_SOCKET_OFF )

int hb_socketInit( void ) { return -1; }

void hb_socketCleanup( void ) { ; }

int hb_socketGetAddrFamily( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   return -1;
}

HB_BOOL hb_socketLocalAddr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr )
{
   HB_SYMBOL_UNUSED( szAddr );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

HB_BOOL hb_socketInetAddr( void ** pSockAddr, unsigned * puiLen,
                           const char * szAddr, int iPort )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

HB_BOOL hb_socketInet6Addr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr, int iPort )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

char * hb_socketAddrGetName( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

int hb_socketAddrGetPort( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return -1;
}

HB_BOOL hb_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PHB_ITEM pAddrItm )
{
   HB_SYMBOL_UNUSED( pAddrItm );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

PHB_ITEM hb_socketAddrToItem( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

int hb_socketGetSockName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   HB_SYMBOL_UNUSED( sd );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return -1;
}

int hb_socketGetPeerName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   HB_SYMBOL_UNUSED( sd );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return -1;
}

HB_SOCKET hb_socketOpen( int domain, int type, int protocol )
{
   HB_SYMBOL_UNUSED( domain );
   HB_SYMBOL_UNUSED( type );
   HB_SYMBOL_UNUSED( protocol );
   hb_socketSetRawError( HB_SOCKET_ERR_PFNOSUPPORT );
   return HB_NO_SOCKET;
}

int hb_socketClose( HB_SOCKET sd )
{
   HB_SYMBOL_UNUSED( sd );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketShutdown( HB_SOCKET sd, int iMode )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iMode );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketBind( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( uiLen );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketListen( HB_SOCKET sd, int iBacklog )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iBacklog );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

HB_SOCKET hb_socketAccept( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( puiLen );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return HB_NO_SOCKET;
}

int hb_socketConnect( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( uiLen );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long hb_socketSend( HB_SOCKET sd, const void * data, long len, int flags, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( flags );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long hb_socketSendTo( HB_SOCKET sd, const void * data, long len, int flags, const void * pSockAddr, unsigned uiSockLen, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( flags );
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( uiSockLen );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long hb_socketRecv( HB_SOCKET sd, void * data, long len, int flags, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( flags );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long hb_socketRecvFrom( HB_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( flags );
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( puiSockLen );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetBlockingIO( HB_SOCKET sd, HB_BOOL fBlocking )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fBlocking );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetNoDelay( HB_SOCKET sd, HB_BOOL fNoDelay )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fNoDelay );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetExclusiveAddr( HB_SOCKET sd, HB_BOOL fExclusive )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fExclusive );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetReuseAddr( HB_SOCKET sd, HB_BOOL fReuse )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fReuse );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetKeepAlive( HB_SOCKET sd, HB_BOOL fKeepAlive )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fKeepAlive );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetBroadcast( HB_SOCKET sd, HB_BOOL fBroadcast )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fBroadcast );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetSndBufSize( HB_SOCKET sd, int iSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iSize );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetRcvBufSize( HB_SOCKET sd, int iSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iSize );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketGetSndBufSize( HB_SOCKET sd, int * piSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( piSize );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketGetRcvBufSize( HB_SOCKET sd, int * piSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( piSize );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetMulticast( HB_SOCKET sd, int af, const char * szAddr )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( af );
   HB_SYMBOL_UNUSED( szAddr );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSelectRead( HB_SOCKET sd, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSelectWrite( HB_SOCKET sd, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}


int hb_socketSelectWriteEx( HB_SOCKET sd, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetRawError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSelect( PHB_ITEM pArrayRD, HB_BOOL fSetRD,
                     PHB_ITEM pArrayWR, HB_BOOL fSetWR,
                     PHB_ITEM pArrayEX, HB_BOOL fSetEX,
                     HB_MAXINT timeout, HB_SOCKET_FUNC pFunc )
{
   HB_SYMBOL_UNUSED( pArrayRD );
   HB_SYMBOL_UNUSED( fSetRD );
   HB_SYMBOL_UNUSED( pArrayWR );
   HB_SYMBOL_UNUSED( fSetWR );
   HB_SYMBOL_UNUSED( pArrayEX );
   HB_SYMBOL_UNUSED( fSetEX );
   HB_SYMBOL_UNUSED( timeout );
   HB_SYMBOL_UNUSED( pFunc );
   hb_socketSetRawError( HB_SOCKET_ERR_NOSUPPORT );
   return -1;
}

HB_BOOL hb_socketResolveInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

char * hb_socketResolveAddr( const char * szAddr, int af )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( af );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PHB_ITEM hb_socketGetHosts( const char * szAddr, int af )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( af );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PHB_ITEM hb_socketGetAliases( const char * szAddr, int af )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( af );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

char * hb_socketGetHostName( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PHB_ITEM hb_socketGetIFaces( int af, HB_BOOL fNoAliases )
{
   HB_SYMBOL_UNUSED( af );
   HB_SYMBOL_UNUSED( fNoAliases );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

#else

#define HB_SOCKADDR_MAX_LEN   256

#if defined( HB_OS_WIN )
#  define HB_SOCK_GETERROR()              WSAGetLastError()
#  define HB_SOCK_IS_EINTR( err )         ( (err) == WSAEINTR )
#  define HB_SOCK_IS_EINPROGRES( err )    ( (err) == WSAEWOULDBLOCK )
#elif defined( HB_OS_OS2 ) && defined( __WATCOMC__ )
#  define HB_SOCK_GETERROR()              sock_errno()
#  define HB_SOCK_IS_EINTR( err )         ( (err) == EINTR )
#  define HB_SOCK_IS_EINPROGRES( err )    ( (err) == EINPROGRESS )
#else
#  define HB_SOCK_GETERROR()              errno
#  define HB_SOCK_IS_EINTR( err )         ( (err) == EINTR )
#  define HB_SOCK_IS_EINPROGRES( err )    ( (err) == EINPROGRESS )
#endif

typedef union
{
#if defined( HB_HAS_SOCKADDR_STORAGE )
   struct sockaddr_storage st;
#else
   char st[ HB_SOCKADDR_MAX_LEN ];
#endif
   struct sockaddr sa;
} HB_SOCKADDR_STORAGE;

/* MT macros */
#define HB_SOCKET_LOCK()        hb_threadEnterCriticalSection( &s_sockMtx )
#define HB_SOCKET_UNLOCK()      hb_threadLeaveCriticalSection( &s_sockMtx )
static HB_CRITICAL_NEW( s_sockMtx );

static int s_iSessions;


#if defined( HB_HAS_INET6 ) && ! defined( HB_HAS_INET6_ADDR_CONST ) && \
    defined( IN6ADDR_ANY_INIT )
   static const struct in6_addr s_in6addr_any = IN6ADDR_ANY_INIT;
#endif

#if ! defined( HB_HAS_INET_NTOP ) && ! defined( HB_IS_INET_NTOA_MT_SAFE ) && defined( AF_INET )
static const char * hb_inet_ntoa( const struct in_addr * addr, char * pBuffer )
{
   /* dirty hack to make inet_ntoa() MT safe,
    * in many systems inet_ntoa() returns pointer to
    * static buffer and is not MT safe.
    */
   HB_ULONG u = ntohl( addr->s_addr );

   hb_snprintf( pBuffer, INET_ADDRSTRLEN, "%hd.%hd.%hd.%hd",
                HB_UHBYTE( u ), HB_ULBYTE( u ), HB_HIBYTE( u ), HB_LOBYTE( u ) );
   return pBuffer;
}
#endif

int hb_socketInit( void )
{
   int ret = 0;

   HB_SOCKET_LOCK();
   if( ++s_iSessions == 1 )
   {
#if defined( HB_OS_WIN )
      WSADATA wsadata;
      ret = WSAStartup( HB_MKUSHORT( 1, 1 ), &wsadata );
#elif defined( HB_OS_DOS )
      ret = sock_init();
#endif
   }
   HB_SOCKET_UNLOCK();

   return ret;
}

void hb_socketCleanup( void )
{
   HB_SOCKET_LOCK();
   if( --s_iSessions == 0 )
   {
#if defined( HB_OS_WIN )
      WSACleanup();
#elif defined( HB_OS_DOS )
      sock_exit();
#endif
   }
   HB_SOCKET_UNLOCK();
}

static void hb_socketSetOsError( int err )
{
   PHB_IOERRORS pError = hb_stackIOErrors();
   HB_ERRCODE uiErr;

#if defined( HB_OS_WIN )
   switch( err )
   {
      case 0:
         uiErr = 0;
         break;
      case WSAEINTR:
         uiErr = HB_SOCKET_ERR_INTERRUPT;
         break;
      case WSAEBADF:
      case WSAENOTSOCK:
         uiErr = HB_SOCKET_ERR_INVALIDHANDLE;
         break;
      case WSAEACCES:
         uiErr = HB_SOCKET_ERR_ACCESS;
         break;
      case WSAEFAULT:
         uiErr = HB_SOCKET_ERR_FAULT;
         break;
      case WSAEINVAL:
         uiErr = HB_SOCKET_ERR_INVAL;
         break;
      case WSAEMFILE:
         uiErr = HB_SOCKET_ERR_NOFILE;
         break;
      case WSAEWOULDBLOCK:
         uiErr = HB_SOCKET_ERR_AGAIN;
         break;
      case WSAEINPROGRESS:
         uiErr = HB_SOCKET_ERR_INPROGRESS;
         break;
      case WSAEALREADY:
         uiErr = HB_SOCKET_ERR_ALREADY;
         break;
      case WSAEDESTADDRREQ:
         uiErr = HB_SOCKET_ERR_DESTADDRREQ;
         break;
      case WSAEMSGSIZE:
         uiErr = HB_SOCKET_ERR_MSGSIZE;
         break;
      case WSAEPROTOTYPE:
         uiErr = HB_SOCKET_ERR_PROTOTYPE;
         break;
      case WSAENOPROTOOPT:
         uiErr = HB_SOCKET_ERR_NOPROTOOPT;
         break;
      case WSAEPROTONOSUPPORT:
         uiErr = HB_SOCKET_ERR_PROTONOSUPPORT;
         break;
      case WSAEOPNOTSUPP:
      case WSAESOCKTNOSUPPORT:
         uiErr = HB_SOCKET_ERR_NOSUPPORT;
         break;
      case WSAEPFNOSUPPORT:
         uiErr = HB_SOCKET_ERR_PFNOSUPPORT;
         break;
      case WSAEAFNOSUPPORT:
         uiErr = HB_SOCKET_ERR_AFNOSUPPORT;
         break;
      case WSAEADDRINUSE:
         uiErr = HB_SOCKET_ERR_ADDRINUSE;
         break;
      case WSAEADDRNOTAVAIL:
         uiErr = HB_SOCKET_ERR_ADDRNOTAVAIL;
         break;
      case WSAENETDOWN:
         uiErr = HB_SOCKET_ERR_NETDOWN;
         break;
      case WSAENETUNREACH:
         uiErr = HB_SOCKET_ERR_NETUNREACH;
         break;
      case WSAENETRESET:
         uiErr = HB_SOCKET_ERR_NETRESET;
         break;
      case WSAECONNREFUSED:
         uiErr = HB_SOCKET_ERR_CONNREFUSED;
         break;
      case WSAECONNABORTED:
         uiErr = HB_SOCKET_ERR_CONNABORTED;
         break;
      case WSAECONNRESET:
         uiErr = HB_SOCKET_ERR_CONNRESET;
         break;
      case WSAENOBUFS:
         uiErr = HB_SOCKET_ERR_NOBUFS;
         break;
      case WSAEISCONN:
         uiErr = HB_SOCKET_ERR_ALREADYCONNECTED;
         break;
      case WSAENOTCONN:
         uiErr = HB_SOCKET_ERR_NOTCONN;
         break;
      case WSAESHUTDOWN:
         uiErr = HB_SOCKET_ERR_SHUTDOWN;
         break;
      case WSAETOOMANYREFS:
         uiErr = HB_SOCKET_ERR_TOOMANYREFS;
         break;
      case WSAETIMEDOUT:
         uiErr = HB_SOCKET_ERR_TIMEOUT;
         break;
      case WSAELOOP:
         uiErr = HB_SOCKET_ERR_LOOP;
         break;
      case WSAENAMETOOLONG:
         uiErr = HB_SOCKET_ERR_NAMETOOLONG;
         break;
      case WSAEHOSTDOWN:
         uiErr = HB_SOCKET_ERR_HOSTDOWN;
         break;
      case WSAEHOSTUNREACH:
         uiErr = HB_SOCKET_ERR_HOSTUNREACH;
         break;
      case WSAENOTEMPTY:
         uiErr = HB_SOCKET_ERR_NOTEMPTY;
         break;
      case WSAEUSERS:
         uiErr = HB_SOCKET_ERR_USERS;
         break;
      case WSAEDQUOT:
         uiErr = HB_SOCKET_ERR_DQUOT;
         break;
      case WSAESTALE:
         uiErr = HB_SOCKET_ERR_STALE;
         break;
      case WSAEREMOTE:
         uiErr = HB_SOCKET_ERR_REMOTE;
         break;
      case WSAEPROCLIM:
         uiErr = HB_SOCKET_ERR_PROCLIM;
         break;
      case WSAEDISCON:
         uiErr = HB_SOCKET_ERR_DISCON;
         break;
      case WSAENOMORE:
         uiErr = HB_SOCKET_ERR_NOMORE;
         break;
      case WSAECANCELLED:
         uiErr = HB_SOCKET_ERR_CANCELLED;
         break;
      case WSAEINVALIDPROCTABLE:
         uiErr = HB_SOCKET_ERR_INVALIDPROCTABLE;
         break;
      case WSAEINVALIDPROVIDER:
         uiErr = HB_SOCKET_ERR_INVALIDPROVIDER;
         break;
      case WSAEPROVIDERFAILEDINIT:
         uiErr = HB_SOCKET_ERR_PROVIDERFAILEDINIT;
         break;
      case WSAEREFUSED:
         uiErr = HB_SOCKET_ERR_REFUSED;
         break;
      case WSATRY_AGAIN:
         uiErr = HB_SOCKET_ERR_TRYAGAIN;
         break;
      case WSASYSNOTREADY:
         uiErr = HB_SOCKET_ERR_SYSNOTREADY;
         break;
      case WSAVERNOTSUPPORTED:
         uiErr = HB_SOCKET_ERR_VERNOTSUPPORTED;
         break;
      case WSANOTINITIALISED:
         uiErr = HB_SOCKET_ERR_NOTINITIALISED;
         break;
      case WSAHOST_NOT_FOUND:
         uiErr = HB_SOCKET_ERR_HOSTNOTFOUND;
         break;
      case WSANO_RECOVERY:
         uiErr = HB_SOCKET_ERR_NORECOVERY;
         break;
      case WSANO_DATA:
         uiErr = HB_SOCKET_ERR_NODATA;
         break;
      case WSASYSCALLFAILURE:
         uiErr = HB_SOCKET_ERR_SYSCALLFAILURE;
         break;
      case WSASERVICE_NOT_FOUND:
         uiErr = HB_SOCKET_ERR_SERVICENOTFOUND;
         break;
      case WSATYPE_NOT_FOUND:
         uiErr = HB_SOCKET_ERR_TYPENOTFOUND;
         break;
      case WSA_E_NO_MORE:
         uiErr = HB_SOCKET_ERR_NOMORE;
         break;
      case WSA_E_CANCELLED:
         uiErr = HB_SOCKET_ERR_CANCELLED;
         break;
      default:
         uiErr = HB_SOCKET_ERR_OTHER;
         break;
   }
#else
   switch( err )
   {
      case 0:
         uiErr = 0;
         break;
#if defined( EPFNOSUPPORT )
      case EPFNOSUPPORT:
         uiErr = HB_SOCKET_ERR_PFNOSUPPORT;
         break;
#endif
#if defined( EAFNOSUPPORT )
      case EAFNOSUPPORT:
         uiErr = HB_SOCKET_ERR_AFNOSUPPORT;
         break;
#endif
#if defined( EPROTONOSUPPORT )
      case EPROTONOSUPPORT:
         uiErr = HB_SOCKET_ERR_PROTONOSUPPORT;
         break;
#endif
      case EADDRINUSE:
         uiErr = HB_SOCKET_ERR_ADDRINUSE;
         break;
      case EINTR:
         uiErr = HB_SOCKET_ERR_INTERRUPT;
         break;
      case ETIMEDOUT:
         uiErr = HB_SOCKET_ERR_TIMEOUT;
         break;
      case EISCONN:
         uiErr = HB_SOCKET_ERR_ALREADYCONNECTED;
         break;
      case ENOTCONN:
         uiErr = HB_SOCKET_ERR_NOTCONN;
         break;
#if defined( ECONNABORTED )
      case ECONNABORTED:
         uiErr = HB_SOCKET_ERR_CONNABORTED;
         break;
#endif
      case ECONNRESET:
         uiErr = HB_SOCKET_ERR_CONNRESET;
         break;
      case ECONNREFUSED:
         uiErr = HB_SOCKET_ERR_CONNREFUSED;
         break;
      case ENETUNREACH:
         uiErr = HB_SOCKET_ERR_NETUNREACH;
         break;
      case ENETDOWN:
         uiErr = HB_SOCKET_ERR_NETDOWN;
         break;
#if defined( ENETRESET )
      case ENETRESET:
         uiErr = HB_SOCKET_ERR_NETRESET;
         break;
#endif
      case EINPROGRESS:
         uiErr = HB_SOCKET_ERR_INPROGRESS;
         break;
      case EALREADY:
         uiErr = HB_SOCKET_ERR_ALREADY;
         break;
      case EADDRNOTAVAIL:
         uiErr = HB_SOCKET_ERR_ADDRNOTAVAIL;
         break;
      case EROFS:
         uiErr = HB_SOCKET_ERR_READONLY;
         break;
      case EAGAIN:
#if defined( EWOULDBLOCK )
#  if EWOULDBLOCK != EAGAIN
      case EWOULDBLOCK:
#  endif
#endif
         uiErr = HB_SOCKET_ERR_AGAIN;
         break;
      case EPIPE:
         uiErr = HB_SOCKET_ERR_PIPE;
         break;
      case EPERM:
      case EACCES:
         uiErr = HB_SOCKET_ERR_ACCESS;
         break;
      case EBADF:
      case ENOTSOCK:
         uiErr = HB_SOCKET_ERR_INVALIDHANDLE;
         break;
      case EINVAL:
         uiErr = HB_SOCKET_ERR_INVAL;
         break;
#if defined( EPROTO )
      case EPROTO:
         uiErr = HB_SOCKET_ERR_PROTO;
         break;
#endif
      case EPROTOTYPE:
         uiErr = HB_SOCKET_ERR_PROTOTYPE;
         break;
      case EOPNOTSUPP:
#if defined( ESOCKTNOSUPPORT )
      case ESOCKTNOSUPPORT:
#endif
         uiErr = HB_SOCKET_ERR_NOSUPPORT;
         break;
      case EMFILE:
      case ENFILE:
         uiErr = HB_SOCKET_ERR_NOFILE;
         break;
      case ENOBUFS:
         uiErr = HB_SOCKET_ERR_NOBUFS;
         break;
      case ENOMEM:
         uiErr = HB_SOCKET_ERR_NOMEM;
         break;
      case EFAULT:
         uiErr = HB_SOCKET_ERR_FAULT;
         break;
      case ENAMETOOLONG:
         uiErr = HB_SOCKET_ERR_NAMETOOLONG;
         break;
      case ENOENT:
         uiErr = HB_SOCKET_ERR_NOENT;
         break;
      case ENOTDIR:
         uiErr = HB_SOCKET_ERR_NOTDIR;
         break;
      case ELOOP:
         uiErr = HB_SOCKET_ERR_LOOP;
         break;
#if defined( ENOSR )
      case ENOSR:
         uiErr = HB_SOCKET_ERR_NOSR;
         break;
#endif
#if defined( ERESTARTSYS )
      case ERESTARTSYS:
         uiErr = HB_SOCKET_ERR_RESTARTSYS;
         break;
#endif
      case EDESTADDRREQ:
         uiErr = HB_SOCKET_ERR_DESTADDRREQ;
         break;
      case EMSGSIZE:
         uiErr = HB_SOCKET_ERR_MSGSIZE;
         break;
      case ENOPROTOOPT:
         uiErr = HB_SOCKET_ERR_NOPROTOOPT;
         break;
      case ESHUTDOWN:
         uiErr = HB_SOCKET_ERR_SHUTDOWN;
         break;
#if defined( ETOOMANYREFS )
      case ETOOMANYREFS:
         uiErr = HB_SOCKET_ERR_TOOMANYREFS;
         break;
#endif
#if defined( EHOSTDOWN )
      case EHOSTDOWN:
         uiErr = HB_SOCKET_ERR_HOSTDOWN;
         break;
#endif
      case EHOSTUNREACH:
         uiErr = HB_SOCKET_ERR_HOSTUNREACH;
         break;
      case ENOTEMPTY:
         uiErr = HB_SOCKET_ERR_NOTEMPTY;
         break;
#if defined( EUSERS )
      case EUSERS:
         uiErr = HB_SOCKET_ERR_USERS;
         break;
#endif
#if defined( EDQUOT )
      case EDQUOT:
         uiErr = HB_SOCKET_ERR_DQUOT;
         break;
#endif
#if defined( ESTALE )
      case ESTALE:
         uiErr = HB_SOCKET_ERR_STALE;
         break;
#endif
#if defined( EREMOTE )
      case EREMOTE:
         uiErr = HB_SOCKET_ERR_REMOTE;
         break;
#endif
#if defined( EPROCLIM )
      case EPROCLIM:
         uiErr = HB_SOCKET_ERR_PROCLIM;
         break;
#endif
#if defined( EDISCON )
      case EDISCON:
         uiErr = HB_SOCKET_ERR_DISCON;
         break;
#endif
#if defined( ENOMORE )
      case ENOMORE:
         uiErr = HB_SOCKET_ERR_NOMORE;
         break;
#endif
#if defined( ECANCELLED )
      case ECANCELLED:
         uiErr = HB_SOCKET_ERR_CANCELLED;
         break;
#endif
#if defined( EINVALIDPROCTABLE )
      case EINVALIDPROCTABLE:
         uiErr = HB_SOCKET_ERR_INVALIDPROCTABLE;
         break;
#endif
#if defined( EINVALIDPROVIDER )
      case EINVALIDPROVIDER:
         uiErr = HB_SOCKET_ERR_INVALIDPROVIDER;
         break;
#endif
#if defined( EPROVIDERFAILEDINIT )
      case EPROVIDERFAILEDINIT:
         uiErr = HB_SOCKET_ERR_PROVIDERFAILEDINIT;
         break;
#endif
#if defined( EREFUSED )
#  if EREFUSED != ECONNREFUSED
      case EREFUSED:
         uiErr = HB_SOCKET_ERR_REFUSED;
         break;
#  endif
#endif
/*
#if defined( TRY_AGAIN )
      case TRY_AGAIN:
         uiErr = HB_SOCKET_ERR_TRYAGAIN;
         break;
#endif
#if defined( HOST_NOT_FOUND )
      case HOST_NOT_FOUND:
         uiErr = HB_SOCKET_ERR_HOSTNOTFOUND;
         break;
#endif
#if defined( NO_RECOVERY )
      case NO_RECOVERY:
         uiErr = HB_SOCKET_ERR_NORECOVERY;
         break;
#endif
#if defined( NO_DATA ) || defined( NO_ADDRESS )
#if defined( NO_DATA )
      case NO_DATA:
#endif
#if defined( NO_ADDRESS ) && \
    ( ! defined( NO_DATA ) || NO_ADDRESS != NO_DATA )
      case NO_ADDRESS:
#endif
         uiErr = HB_SOCKET_ERR_NODATA;
         break;
#endif
*/
      default:
         uiErr = HB_SOCKET_ERR_OTHER;
         break;
   }
#endif

   pError->uiSocketError = uiErr;
   pError->iSocketOsError = err;
}

#if defined( HB_SOCKET_TRANSLATE_DOMAIN )
static int hb_socketTransDomain( int domain, int *err )
{
   switch( domain )
   {
      case HB_SOCKET_AF_INET:
#if   defined( AF_INET )
         domain = AF_INET;
#elif defined( PF_INET )
         domain = PF_INET;
#else
         if( err )
            *err = HB_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case HB_SOCKET_AF_INET6:
#if   defined( AF_INET6 )
         domain = AF_INET6;
#elif defined( PF_INET6 )
         domain = PF_INET6;
#else
         if( err )
            *err = HB_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case HB_SOCKET_AF_LOCAL:
#if   defined( AF_LOCAL )
         domain = AF_LOCAL;
#elif defined( AF_UNIX )
         domain = AF_UNIX;
#elif defined( PF_LOCAL )
         domain = PF_LOCAL;
#elif defined( PF_UNIX )
         domain = PF_UNIX;
#else
         if( err )
            *err = HB_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case HB_SOCKET_AF_PACKET:
#if   defined( AF_PACKET )
         domain = AF_PACKET;
#elif defined( PF_PACKET )
         domain = PF_PACKET;
#else
         if( err )
            *err = HB_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case HB_SOCKET_AF_IPX:
#if   defined( AF_IPX )
         domain = AF_IPX;
#elif defined( PF_IPX )
         domain = PF_IPX;
#else
         if( err )
            *err = HB_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      default:
         if( err )
            *err = HB_SOCKET_ERR_PFNOSUPPORT;
   }
   return domain;
}
#endif

#if defined( HB_SOCKET_TRANSLATE_TYPE )
static int hb_socketTransType( int type, int *err )
{
   switch( type )
   {
      case HB_SOCKET_PT_STREAM:
#if   defined( SOCK_STREAM )
         type = SOCK_STREAM;
#else
         if( err )
            *err = HB_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case HB_SOCKET_PT_DGRAM:
#if   defined( SOCK_DGRAM )
         type = SOCK_DGRAM;
#else
         if( err )
            *err = HB_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case HB_SOCKET_PT_SEQPACKET:
#if   defined( SOCK_SEQPACKET )
         type = SOCK_SEQPACKET;
#else
         if( err )
            *err = HB_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case HB_SOCKET_PT_RAW:
#if   defined( SOCK_RAW )
         type = SOCK_RAW;
#else
         if( err )
            *err = HB_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case HB_SOCKET_PT_RDM:
#if   defined( SOCK_RDM )
         type = SOCK_RDM;
#else
         if( err )
            *err = HB_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      default:
         if( err )
            *err = HB_SOCKET_ERR_PROTONOSUPPORT;
   }
   return type;
}
#endif

static int hb_socketSelectRD( HB_SOCKET sd, HB_MAXINT timeout )
{
   struct timeval tv, * ptv;
   fd_set rfds;
   int iResult, iError;

#if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();
#endif

   if( timeout >= 0 )
   {
      tv.tv_sec = ( long ) ( timeout / 1000 );
      tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      ptv = &tv;
   }
   else
      ptv = NULL;

   for( ;; )
   {
      FD_ZERO( &rfds );
      FD_SET( ( HB_SOCKET_T ) sd, &rfds );

      iResult = select( ( int ) ( sd + 1 ), &rfds, NULL, NULL, ptv );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      hb_socketSetOsError( iError );
      if( iResult == -1 && timeout > 0 && HB_SOCK_IS_EINTR( iError ) &&
          hb_vmRequestQuery() == 0 )
#if defined( HB_HAS_SELECT_TIMER )
         continue;
#else
      {
         HB_MAXUINT timecurr = hb_dateMilliSeconds();
         if( timecurr > timer )
         {
            timeout -= timecurr - timer;
            if( timeout > 0 )
            {
               timer = timecurr;
               tv.tv_sec = ( long ) ( timeout / 1000 );
               tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
               continue;
            }
         }
      }
#endif
      break;
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, &rfds ) ? 1 : 0 );
}

static int hb_socketSelectWR( HB_SOCKET sd, HB_MAXINT timeout )
{
   struct timeval tv, * ptv;
   fd_set wfds;
   int iResult, iError;

#if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();
#endif

   if( timeout >= 0 )
   {
      tv.tv_sec = ( long ) ( timeout / 1000 );
      tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      ptv = &tv;
   }
   else
      ptv = NULL;

   for( ;; )
   {
      FD_ZERO( &wfds );
      FD_SET( ( HB_SOCKET_T ) sd, &wfds );

      iResult = select( ( int ) ( sd + 1 ), NULL, &wfds, NULL, ptv );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      hb_socketSetOsError( iError );
      if( iResult == -1 && timeout > 0 && HB_SOCK_IS_EINTR( iError ) &&
          hb_vmRequestQuery() == 0 )
#if defined( HB_HAS_SELECT_TIMER )
         continue;
#else
      {
         HB_MAXUINT timecurr = hb_dateMilliSeconds();
         if( timecurr > timer )
         {
            timeout -= timecurr - timer;
            if( timeout > 0 )
            {
               timer = timecurr;
               tv.tv_sec = ( long ) ( timeout / 1000 );
               tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
               continue;
            }
         }
      }
#endif
      break;
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, &wfds ) ? 1 : 0 );
}

static int hb_socketSelectWRE( HB_SOCKET sd, HB_MAXINT timeout )
{
   struct timeval tv, * ptv;
   fd_set wfds, * pefds;

#if defined( HB_OS_WIN )
   fd_set efds;
#endif
   int iResult, iError;
   socklen_t len;
#if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();
#endif

   if( timeout >= 0 )
   {
      tv.tv_sec = ( long ) ( timeout / 1000 );
      tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      ptv = &tv;
   }
   else
      ptv = NULL;

   for( ;; )
   {
      FD_ZERO( &wfds );
      FD_SET( ( HB_SOCKET_T ) sd, &wfds );
#if defined( HB_OS_WIN )
      FD_ZERO( &efds );
      FD_SET( ( HB_SOCKET_T ) sd, &efds );
      pefds = &efds;
#else
      pefds = NULL;
#endif

      iResult = select( ( int ) ( sd + 1 ), NULL, &wfds, pefds, ptv );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      hb_socketSetOsError( iError );
#if defined( HB_OS_WIN )
      if( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, pefds ) )
      {
         iResult = -1;
         len = sizeof( iError );
         if( getsockopt( sd, SOL_SOCKET, SO_ERROR, ( char * ) &iError, &len ) != 0 )
            iError = HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
      }
      else
#endif
      if( iResult == -1 && timeout > 0 && HB_SOCK_IS_EINTR( iError ) &&
          hb_vmRequestQuery() == 0 )
#if defined( HB_HAS_SELECT_TIMER )
         continue;
#else
      {
         HB_MAXUINT timecurr = hb_dateMilliSeconds();
         if( timecurr > timer )
         {
            timeout -= timecurr - timer;
            if( timeout > 0 )
            {
               timer = timecurr;
               tv.tv_sec = ( long ) ( timeout / 1000 );
               tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
               continue;
            }
         }
      }
#endif
      break;
   }
#if ! defined( HB_OS_WIN )
   if( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, &wfds ) )
   {
      len = sizeof( iError );
      if( getsockopt( sd, SOL_SOCKET, SO_ERROR, ( char * ) &iError, &len ) != 0 )
      {
         iResult = -1;
         iError = HB_SOCK_GETERROR();
      }
#if defined( HB_OS_DOS )
      else if( iError == EISCONN )
         iError = 0;
#endif
      else if( iError != 0 )
         iResult = -1;

      hb_socketSetOsError( iError );
   }
#endif

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, &wfds ) ? 1 : 0 );
}

int hb_socketGetAddrFamily( const void * pSockAddr, unsigned len )
{
   return pSockAddr && len ? ( ( const struct sockaddr * ) pSockAddr )->sa_family : -1;
}

HB_BOOL hb_socketLocalAddr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr )
{
#if defined( HB_HAS_UNIX )
   struct sockaddr_un sa;
   memset( &sa, 0, sizeof( sa ) );
#if defined( AF_UNIX )
   sa.sun_family = AF_UNIX;
#else
   sa.sun_family = AF_LOCAL;
#endif
   hb_strncpy( sa.sun_path, szAddr, sizeof( sa.sun_path ) - 1 );
   *pSockAddr = memcpy( hb_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
   *puiLen = ( unsigned ) sizeof( sa );
   return HB_TRUE;
#else
   HB_SYMBOL_UNUSED( szAddr );
   *pSockAddr = NULL;
   *puiLen = 0;
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return HB_FALSE;
#endif
}

HB_BOOL hb_socketInetAddr( void ** pSockAddr, unsigned * puiLen,
                           const char * szAddr, int iPort )
{
#if defined( AF_INET )
   struct sockaddr_in sa;

   memset( &sa, 0, sizeof( sa ) );
   sa.sin_family = AF_INET;
   sa.sin_port = htons( ( HB_U16 ) iPort );
   if( ! szAddr || ! *szAddr )
   {
      sa.sin_addr.s_addr = htonl( INADDR_ANY );
      *pSockAddr = memcpy( hb_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return HB_TRUE;
   }
   else
   {
#if defined( HB_HAS_INET_PTON )
      if( inet_pton( AF_INET, szAddr, &sa.sin_addr ) > 0 )
#elif defined( HB_HAS_INET_ATON )
      if( inet_aton( szAddr, &sa.sin_addr ) != 0 )
#else
      sa.sin_addr.s_addr = inet_addr( szAddr );
      if( sa.sin_addr.s_addr != INADDR_NONE ||
          strcmp( "255.255.255.255", szAddr ) == 0 )  /* dirty hack */
#endif
      {
         *pSockAddr = memcpy( hb_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
         *puiLen = ( unsigned ) sizeof( sa );
         return HB_TRUE;
      }
      else
         hb_socketSetRawError( HB_SOCKET_ERR_WRONGADDR );
   }
#else
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
#endif
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

HB_BOOL hb_socketInet6Addr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr, int iPort )
{
#if defined( HB_HAS_INET6 )
   struct sockaddr_in6 sa;

   memset( &sa, 0, sizeof( sa ) );
   sa.sin6_family = AF_INET6;
   sa.sin6_port = htons( ( HB_U16 ) iPort );
   if( ! szAddr || ! *szAddr )
   {
#if defined( HB_HAS_INET6_ADDR_CONST )
      memcpy( &sa.sin6_addr, &in6addr_any, sizeof( struct in6_addr ) );
#elif defined( IN6ADDR_ANY_INIT )
      memcpy( &sa.sin6_addr, &s_in6addr_any, sizeof( struct in6_addr ) );
#else
      int iTODO;
#endif
      *pSockAddr = memcpy( hb_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return HB_TRUE;
   }
   else
   {
#if defined( HB_HAS_INET_PTON )
      int err = inet_pton( AF_INET6, szAddr, &sa.sin6_addr );
      if( err > 0 )
      {
         *pSockAddr = memcpy( hb_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
         *puiLen = ( unsigned ) sizeof( sa );
         return HB_TRUE;
      }
      else if( err == 0 )
         hb_socketSetRawError( HB_SOCKET_ERR_WRONGADDR );
      else
         hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
#else
      int iTODO;
#endif
   }
#else
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
#endif
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

/* caller must free the buffer if not NULL */
char * hb_socketAddrGetName( const void * pSockAddr, unsigned len )
{
   char * szName = NULL;

   switch( hb_socketGetAddrFamily( pSockAddr, len ) )
   {
#if defined( AF_INET )
      case AF_INET:
         if( len >= sizeof( struct sockaddr_in ) )
         {
            const struct sockaddr_in * sa = ( const struct sockaddr_in * ) pSockAddr;
            const char * szAddr;
#  if defined( HB_HAS_INET_NTOP )
            char buf[ INET_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET, &sa->sin_addr, buf, sizeof( buf ) );
#  elif defined( HB_IS_INET_NTOA_MT_SAFE )
            szAddr = inet_ntoa( sa->sin_addr );
#  else
            char buf[ INET_ADDRSTRLEN ];
            szAddr = hb_inet_ntoa( &sa->sin_addr, buf );
#  endif
            if( szAddr )
               szName = hb_strdup( szAddr );
         }
         break;
#endif
#if defined( HB_HAS_INET6 )
      case AF_INET6:
         if( len >= sizeof( struct sockaddr_in6 ) )
         {
            const struct sockaddr_in6 * sa = ( const struct sockaddr_in6 * ) pSockAddr;
            const char * szAddr;
#  if defined( HB_HAS_INET_NTOP )
            char buf[ INET6_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET6, &sa->sin6_addr, buf, sizeof( buf ) );
#  else
            {
               int iTODO;
               szAddr = NULL;
            }
#  endif
            if( szAddr )
               szName = hb_strdup( szAddr );
         }
         break;
#endif
#if defined( HB_HAS_UNIX )
#  if defined( AF_UNIX )
      case AF_UNIX:
#  else
      case AF_LOCAL:
#  endif
         if( len >= sizeof( struct sockaddr_un ) )
         {
            const struct sockaddr_un * sa = ( const struct sockaddr_un * ) pSockAddr;
            szName = hb_strdup( sa->sun_path );
         }
         break;
#endif
#if defined( AF_IPX )
      case AF_IPX:
         break;
#endif
#if defined( AF_PACKET )
      case AF_PACKET:
         break;
#endif
   }
   hb_socketSetRawError( szName ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
   return szName;
}

int hb_socketAddrGetPort( const void * pSockAddr, unsigned len )
{
   int iPort = -1;

   switch( hb_socketGetAddrFamily( pSockAddr, len ) )
   {
#if defined( AF_INET )
      case AF_INET:
         if( len >= sizeof( struct sockaddr_in ) )
            iPort = ntohs( ( ( const struct sockaddr_in * ) pSockAddr )->sin_port );
         break;
#endif
#if defined( HB_HAS_INET6 )
      case AF_INET6:
         if( len >= sizeof( struct sockaddr_in6 ) )
            iPort = ntohs( ( ( const struct sockaddr_in6 * ) pSockAddr )->sin6_port );
         break;
#endif
#if defined( HB_HAS_UNIX )
#  if defined( AF_UNIX )
      case AF_UNIX:
#  else
      case AF_LOCAL:
#  endif
         break;
#endif
#if defined( AF_IPX )
      case AF_IPX:
         break;
#endif
#if defined( AF_PACKET )
      case AF_PACKET:
         break;
#endif
   }
   hb_socketSetRawError( iPort != -1 ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
   return iPort;
}

HB_BOOL hb_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PHB_ITEM pAddrItm )
{
   HB_BOOL fOK = HB_FALSE;

   *pSockAddr = NULL;
   *puiLen = 0;

   if( pAddrItm && HB_IS_ARRAY( pAddrItm ) )
   {
      if( hb_arrayLen( pAddrItm ) >= 2 &&
          ( hb_arrayGetType( pAddrItm, 1 ) & HB_IT_NUMERIC ) != 0 )
      {
         switch( hb_arrayGetNI( pAddrItm, 1 ) )
         {
            case HB_SOCKET_AF_INET:
               fOK = hb_socketInetAddr( pSockAddr, puiLen,
                                        hb_arrayGetCPtr( pAddrItm, 2 ),
                                        hb_arrayGetNI( pAddrItm, 3 ) );
               break;
            case HB_SOCKET_AF_INET6:
               fOK = hb_socketInet6Addr( pSockAddr, puiLen,
                                         hb_arrayGetCPtr( pAddrItm, 2 ),
                                         hb_arrayGetNI( pAddrItm, 3 ) );
               break;
            case HB_SOCKET_AF_LOCAL:
               fOK = hb_socketLocalAddr( pSockAddr, puiLen,
                                         hb_arrayGetCPtr( pAddrItm, 2 ) );
               break;
            case HB_SOCKET_AF_PACKET:
            case HB_SOCKET_AF_IPX:
               break;
         }
      }
   }
   hb_socketSetRawError( fOK ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
   return fOK;
}

PHB_ITEM hb_socketAddrToItem( const void * pSockAddr, unsigned len )
{
   PHB_ITEM pAddrItm = NULL;

   switch( hb_socketGetAddrFamily( pSockAddr, len ) )
   {
#if defined( AF_INET )
      case AF_INET:
         if( len >= sizeof( struct sockaddr_in ) )
         {
            const struct sockaddr_in * sa = ( const struct sockaddr_in * ) pSockAddr;
            const char * szAddr;
#  if defined( HB_HAS_INET_NTOP )
            char buf[ INET_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET, &sa->sin_addr, buf, sizeof( buf ) );
#  elif defined( HB_IS_INET_NTOA_MT_SAFE )
            szAddr = inet_ntoa( sa->sin_addr );
#  else
            char buf[ INET_ADDRSTRLEN ];
            szAddr = hb_inet_ntoa( &sa->sin_addr, buf );
#  endif
            if( szAddr )
            {
               pAddrItm = hb_itemArrayNew( 3 );
               hb_arraySetNI( pAddrItm, 1, HB_SOCKET_AF_INET );
               hb_arraySetC( pAddrItm, 2, szAddr );
               hb_arraySetNI( pAddrItm, 3, ntohs( sa->sin_port ) );
            }
         }
         break;
#endif
#if defined( HB_HAS_INET6 )
      case AF_INET6:
         if( len >= sizeof( struct sockaddr_in6 ) )
         {
            const struct sockaddr_in6 * sa = ( const struct sockaddr_in6 * ) pSockAddr;
            const char * szAddr;
#  if defined( HB_HAS_INET_NTOP )
            char buf[ INET6_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET6, &sa->sin6_addr, buf, sizeof( buf ) );
#  else
            {
               int iTODO;
               szAddr = NULL;
            }
#  endif
            if( szAddr )
            {
               pAddrItm = hb_itemArrayNew( 3 );
               hb_arraySetNI( pAddrItm, 1, HB_SOCKET_AF_INET6 );
               hb_arraySetC( pAddrItm, 2, szAddr );
               hb_arraySetNI( pAddrItm, 3, ntohs( sa->sin6_port ) );
            }
         }
         break;
#endif
#if defined( HB_HAS_UNIX )
#  if defined( AF_UNIX )
      case AF_UNIX:
#  else
      case AF_LOCAL:
#  endif
         if( len >= sizeof( struct sockaddr_un ) )
         {
            const struct sockaddr_un * sa = ( const struct sockaddr_un * ) pSockAddr;
            pAddrItm = hb_itemArrayNew( 2 );
            hb_arraySetNI( pAddrItm, 1, HB_SOCKET_AF_LOCAL );
            hb_arraySetC( pAddrItm, 2, sa->sun_path );
         }
         break;
#endif
#if defined( AF_IPX )
      case AF_IPX:
         break;
#endif
#if defined( AF_PACKET )
      case AF_PACKET:
         break;
#endif
   }
   hb_socketSetRawError( pAddrItm ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
   return pAddrItm;
}

int hb_socketGetSockName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   HB_SOCKADDR_STORAGE st;
   socklen_t len = sizeof( st );
   int ret;

   ret = getsockname( sd, &st.sa, &len );
   hb_socketSetOsError( ret == 0 ? 0 : HB_SOCK_GETERROR() );
   if( ret == 0 )
   {
      *pSockAddr = memcpy( hb_xgrab( len + 1 ), &st.sa, len );
      *puiLen = ( unsigned ) len;
   }
   else
   {
      *pSockAddr = NULL;
      *puiLen = 0;
   }

   return ret;
}

int hb_socketGetPeerName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   int ret;

#if defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
   /* it's still not supported by Linux OpenWatcom port :-( */
   ret = -1;
   hb_socketSetRawError( HB_SOCKET_ERR_NOSUPPORT );
#else
   HB_SOCKADDR_STORAGE st;
   socklen_t len = sizeof( st );

   ret = getpeername( sd, &st.sa, &len );
   hb_socketSetOsError( ret == 0 ? 0 : HB_SOCK_GETERROR() );
   if( ret == 0 )
   {
      *pSockAddr = memcpy( hb_xgrab( len + 1 ), &st.sa, len );
      *puiLen = ( unsigned ) len;
   }
   else
#endif
   {
      *pSockAddr = NULL;
      *puiLen = 0;
   }

   return ret;
}

HB_SOCKET hb_socketOpen( int domain, int type, int protocol )
{
   HB_SOCKET sd = HB_NO_SOCKET;
   int err = 0;

#if defined( HB_SOCKET_TRANSLATE_DOMAIN )
   domain = hb_socketTransDomain( domain, &err );
#endif

#if defined( HB_SOCKET_TRANSLATE_TYPE )
   if( err == 0 )
      type = hb_socketTransType( type, &err );
#endif

   if( err == 0 )
   {
      sd = socket( domain, type, protocol );
      hb_socketSetOsError( sd != HB_NO_SOCKET ? 0 : HB_SOCK_GETERROR() );
   }
   else
      hb_socketSetRawError( err );

   return sd;
}

int hb_socketClose( HB_SOCKET sd )
{
   int ret;

   hb_vmUnlock();
#if defined( HB_OS_WIN )
   ret = closesocket( sd );
#elif defined( HB_OS_DOS )
   ret = close_s( sd );
#else
#  if defined( EINTR )
   {
      /* ignoring EINTR in close() it's quite common bug when sockets or
       * pipes are used. Without such protection it's not safe to use
       * signals in user code.
       */
      do
      {
         ret = close( sd );
      }
      while( ret == -1 && errno == EINTR );
   }
#  else
   ret = close( sd );
#  endif
#endif
   hb_socketSetOsError( ret == 0 ? 0 : HB_SOCK_GETERROR() );
   hb_vmLock();

   return ret;
}

int hb_socketShutdown( HB_SOCKET sd, int iMode )
{
   int ret;

#if defined( HB_OS_WIN )
   if( iMode == HB_SOCKET_SHUT_RD )
      iMode = SD_RECEIVE;
   else if( iMode == HB_SOCKET_SHUT_WR )
      iMode = SD_SEND;
   else if( iMode == HB_SOCKET_SHUT_RDWR )
      iMode = SD_BOTH;
#elif defined( HB_OS_OS2 )
   if( iMode == HB_SOCKET_SHUT_RD )
      iMode = SO_RCV_SHUTDOWN;
   else if( iMode == HB_SOCKET_SHUT_WR )
      iMode = SO_SND_SHUTDOWN;
   else if( iMode == HB_SOCKET_SHUT_RDWR )
      iMode = SO_RCV_SHUTDOWN | SO_SND_SHUTDOWN;
#elif defined( __WATCOMC__ )
   if( iMode == HB_SOCKET_SHUT_RD ||
       iMode == HB_SOCKET_SHUT_WR ||
       iMode == HB_SOCKET_SHUT_RDWR )
   { ; }
#else
   if( iMode == HB_SOCKET_SHUT_RD )
      iMode = SHUT_RD;
   else if( iMode == HB_SOCKET_SHUT_WR )
      iMode = SHUT_WR;
   else if( iMode == HB_SOCKET_SHUT_RDWR )
      iMode = SHUT_RDWR;
#endif
   else
   {
      hb_socketSetRawError( HB_SOCKET_ERR_PARAMVALUE );
      return -1;
   }

#if defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
{
   int iTODO;
   /* it's still not supported by Linux OpenWatcom port :-( */
   ret = -1;
   hb_socketSetRawError( HB_SOCKET_ERR_NOSUPPORT );
}
#else
   hb_vmUnlock();
   ret = shutdown( sd, iMode );
   hb_socketSetOsError( ret == 0 ? 0 : HB_SOCK_GETERROR() );
   hb_vmLock();
#endif
   return ret;
}

int hb_socketBind( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen )
{
   int ret;

   ret = bind( sd, ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiLen );
   hb_socketSetOsError( ret == 0 ? 0 : HB_SOCK_GETERROR() );

   return ret;
}

int hb_socketListen( HB_SOCKET sd, int iBacklog )
{
   int ret;

   ret = listen( sd, iBacklog );
   hb_socketSetOsError( ret == 0 ? 0 : HB_SOCK_GETERROR() );

   return ret;
}

HB_SOCKET hb_socketAccept( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen, HB_MAXINT timeout )
{
   HB_SOCKET newsd = HB_NO_SOCKET;
   HB_SOCKADDR_STORAGE st;
   socklen_t len = sizeof( st );
   int ret, err;

   hb_vmUnlock();
   ret = hb_socketSelectRD( sd, timeout );
   if( ret > 0 )
   {
      /* it's necessary to set non blocking IO to be sure that application
       * will not be frozen inside accept(). It may happen if some asynchronous
       * network error appear after above Select() or when other thread
       * accepts incoming connection (concurrent calls).
       */
      ret = timeout < 0 ? 0 : hb_socketSetBlockingIO( sd, HB_FALSE );
      newsd = accept( sd, &st.sa, &len );
      err = newsd != HB_NO_SOCKET ? 0 : HB_SOCK_GETERROR();

      if( ret > 0 )
         hb_socketSetBlockingIO( sd, HB_TRUE );
      if( pSockAddr && puiLen )
      {
         if( newsd == HB_NO_SOCKET )
         {
            *pSockAddr = NULL;
            *puiLen = 0;
         }
         else
         {
            *pSockAddr = memcpy( hb_xgrab( len + 1 ), &st.sa, len );
            *puiLen = ( unsigned ) len;
         }
      }
      /* it's not guarantied that socket returned by accept will use
       * blocking IO operations. On some systems it inherits blocking IO
       * from parent handler so we have to force blocking IO mode
       * explicitly.
       */
      if( newsd != HB_NO_SOCKET )
         hb_socketSetBlockingIO( newsd, HB_TRUE );

      hb_socketSetOsError( err );
   }
   else if( ret == 0 )
      hb_socketSetRawError( HB_SOCKET_ERR_TIMEOUT );
   hb_vmLock();
   return newsd;
}

int hb_socketConnect( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen, HB_MAXINT timeout )
{
   int ret, blk, err, rawerr;

   hb_vmUnlock();

   /* set not blocking IO to implement timeout in connect() operation in
    * portable way without using signals
    */
   blk = timeout < 0 ? 0 : hb_socketSetBlockingIO( sd, HB_FALSE );
   ret = connect( sd, ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiLen );
   err = ret == 0 ? 0 : HB_SOCK_GETERROR();
   hb_socketSetOsError( err );
   if( ret != 0 && timeout >= 0 && HB_SOCK_IS_EINPROGRES( err ) )
   {
      /* inside hb_socketSelectWRE() we have code which hides differences
       * between Windows and POSIX platforms in error detection.
       */
      ret = hb_socketSelectWRE( sd, timeout );
      if( ret > 0 )
         ret = 0;
      else if( ret == 0 )
      {
         hb_socketSetRawError( HB_SOCKET_ERR_TIMEOUT );
         ret = -1;
      }
   }

   if( blk > 0 )
   {
      err = hb_socketGetOsError();
      rawerr = err ? 0 : hb_socketGetError();

      hb_socketSetBlockingIO( sd, HB_TRUE );

      if( err )
         hb_socketSetOsError( err );
      else
         hb_socketSetRawError( rawerr );
   }

   hb_vmLock();
   return ret;
}

long hb_socketSend( HB_SOCKET sd, const void * data, long len, int flags, HB_MAXINT timeout )
{
   long lSent = 0;

   hb_vmUnlock();

   if( timeout >= 0 )
   {
      lSent = hb_socketSelectWR( sd, timeout );
      if( lSent == 0 )
      {
         hb_socketSetRawError( HB_SOCKET_ERR_TIMEOUT );
         lSent = -1;
      }
   }
   if( lSent >= 0 )
   {
      int iError;

      /* in POSIX systems writing data to broken connection stream causes
       * that system generates SIGPIPE which has to be caught by application
       * otherwise the default action for SIGPIPE is application termination.
       * we do not want to generate it so we are setting MSG_NOSIGNAL flag.
       */
#if defined( MSG_NOSIGNAL )
      flags |= MSG_NOSIGNAL;
#endif
      do
      {
         lSent = send( sd, ( const char * ) data, len, flags );
         iError = HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
      }
      while( lSent == -1 && HB_SOCK_IS_EINTR( iError ) &&
             hb_vmRequestQuery() == 0 );
   }
   hb_vmLock();

   return lSent;
}

long hb_socketSendTo( HB_SOCKET sd, const void * data, long len, int flags,
                      const void * pSockAddr, unsigned uiSockLen, HB_MAXINT timeout )
{
   long lSent = 0;

   hb_vmUnlock();

   if( timeout >= 0 )
   {
      lSent = hb_socketSelectWR( sd, timeout );
      if( lSent == 0 )
      {
         hb_socketSetRawError( HB_SOCKET_ERR_TIMEOUT );
         lSent = -1;
      }
   }
   if( lSent >= 0 )
   {
      int iError;

      /* see note above about SIGPIPE */
#if defined( MSG_NOSIGNAL )
      flags |= MSG_NOSIGNAL;
#endif
      do
      {
         lSent = sendto( sd, ( const char * ) data, len, flags,
                         ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiSockLen );
         iError = HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
      }
      while( lSent == -1 && HB_SOCK_IS_EINTR( iError ) &&
             hb_vmRequestQuery() == 0 );
   }
   hb_vmLock();

   return lSent;
}

long hb_socketRecv( HB_SOCKET sd, void * data, long len, int flags, HB_MAXINT timeout )
{
   long lReceived = 0;

   hb_vmUnlock();

   if( timeout >= 0 )
   {
      lReceived = hb_socketSelectRD( sd, timeout );
      if( lReceived == 0 )
      {
         hb_socketSetRawError( HB_SOCKET_ERR_TIMEOUT );
         lReceived = -1;
      }
   }
   if( lReceived >= 0 )
   {
      int iError;

      do
      {
         lReceived = recv( sd, ( char * ) data, len, flags );
         iError = HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
      }
      while( lReceived == -1 && HB_SOCK_IS_EINTR( iError ) &&
             hb_vmRequestQuery() == 0 );
   }
   hb_vmLock();

   return lReceived;
}

long hb_socketRecvFrom( HB_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, HB_MAXINT timeout )
{
   long lReceived = 0;

   hb_vmUnlock();

   if( timeout >= 0 )
   {
      lReceived = hb_socketSelectRD( sd, timeout );
      if( lReceived == 0 )
      {
         hb_socketSetRawError( HB_SOCKET_ERR_TIMEOUT );
         lReceived = -1;
      }
   }
   if( lReceived >= 0 )
   {
      HB_SOCKADDR_STORAGE st;
      socklen_t salen = sizeof( st );
      int iError;

      do
      {
         lReceived = recvfrom( sd, ( char * ) data, len, flags, &st.sa, &salen );
         iError = HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
      }
      while( lReceived == -1 && HB_SOCK_IS_EINTR( iError ) &&
             hb_vmRequestQuery() == 0 );

      if( pSockAddr && puiSockLen )
      {
         if( lReceived == -1 )
         {
            *pSockAddr = NULL;
            *puiSockLen = 0;
         }
         else
         {
            *pSockAddr = memcpy( hb_xgrab( salen + 1 ), &st.sa, salen );
            *puiSockLen = ( unsigned ) salen;
         }
      }
   }
   hb_vmLock();

   return lReceived;
}

int hb_socketSetBlockingIO( HB_SOCKET sd, HB_BOOL fBlocking )
{
   int ret;

#if defined( HB_OS_WIN )
   u_long mode = fBlocking ? 0 : 1;
   ret = ioctlsocket( sd, FIONBIO, &mode );
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   if( ret == 0 )
      ret = 1;
#elif defined( HB_OS_DOS )
   int mode = fBlocking ? 0 : 1;
   ret = ioctlsocket( sd, FIONBIO, ( char * ) &mode );
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   if( ret == 0 )
      ret = 1;
#elif defined( O_NONBLOCK )
   ret = fcntl( sd, F_GETFL, 0 );
   if( ret != -1 )
   {
      HB_BOOL fBlocked;
      long flags;
      fBlocked = ( ret & O_NONBLOCK ) == 0;
      if( fBlocking ? ! fBlocked : fBlocked )
      {
         if( fBlocking )
            flags = ret & ~O_NONBLOCK;
         else
            flags = ret | O_NONBLOCK;
         ret = fcntl( sd, F_SETFL, flags );
         if( ret == 0 )
            ret = 1;
      }
      else
         ret = 0;
   }
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
#elif defined( HB_OS_OS2 )
   unsigned long mode = fBlocking ? 0 : 1;
   ret = ioctl( sd, FIONBIO, ( char * ) &mode );
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   if( ret == 0 )
      ret = 1;
#else
   int iTODO;
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fBlocking );
   hb_socketSetRawError( HB_SOCKET_ERR_NOSUPPORT );
   ret = -1;
#endif
   return ret;
}

int hb_socketSetNoDelay( HB_SOCKET sd, HB_BOOL fNoDelay )
{
   int ret;

#if defined( TCP_NODELAY )
   /*
    * Turn off the nagle algorithm for the specified socket.
    * The nagle algorithm says that we should delay sending
    * partial packets in the hopes of getting more data.
    * There are bad interactions between persistent connections and
    * Nagle's algorithm that have severe performance penalties.
    */
   int val = fNoDelay ? 1 : 0;
   ret = setsockopt( sd, IPPROTO_TCP, TCP_NODELAY, ( const char * ) &val, sizeof( val ) );
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
#else
   int iTODO;
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fNoDelay );
   hb_socketSetRawError( HB_SOCKET_ERR_NOSUPPORT );
   ret = -1;
#endif
   return ret;
}

/* NOTE: For notes on Windows, see:
         http://paste.lisp.org/display/59751
         [vszakats] */
int hb_socketSetExclusiveAddr( HB_SOCKET sd, HB_BOOL fExclusive )
{
   int ret;

   #if defined( HB_OS_WIN )
      #if defined( SO_EXCLUSIVEADDRUSE )
         int val = fExclusive ? 1 : 0;
         ret = setsockopt( sd, SOL_SOCKET, SO_EXCLUSIVEADDRUSE, ( const char * ) &val, sizeof( val ) );
         hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
      #else
         HB_SYMBOL_UNUSED( sd );
         HB_SYMBOL_UNUSED( fExclusive );
         hb_socketSetRawError( HB_SOCKET_ERR_NOSUPPORT );
         ret = -1;
      #endif
   #else
      HB_SYMBOL_UNUSED( sd );
      HB_SYMBOL_UNUSED( fExclusive );
      hb_socketSetOsError( 0 );
      ret = 0;
   #endif
   return ret;
}

int hb_socketSetReuseAddr( HB_SOCKET sd, HB_BOOL fReuse )
{
   int ret;

   /* it allows to reuse port immediately without timeout used to
    * clean all pending connections addressed to previous port owner
    */
   #if defined( HB_OS_WIN )
      /* SO_REUSEADDR in MS-Windows makes sth completly different
       * then in other OS-es
       */
      HB_SYMBOL_UNUSED( sd );
      HB_SYMBOL_UNUSED( fReuse );
      hb_socketSetRawError( HB_SOCKET_ERR_NOSUPPORT );
      ret = -1;
   #else
   {
      int val = fReuse ? 1 : 0;
      ret = setsockopt( sd, SOL_SOCKET, SO_REUSEADDR, ( const char * ) &val, sizeof( val ) );
      hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   }
   #endif
   return ret;
}

int hb_socketSetKeepAlive( HB_SOCKET sd, HB_BOOL fKeepAlive )
{
   int val = fKeepAlive ? 1 : 0, ret;

   ret = setsockopt( sd, SOL_SOCKET, SO_KEEPALIVE, ( const char * ) &val, sizeof( val ) );
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   return ret;
}

int hb_socketSetBroadcast( HB_SOCKET sd, HB_BOOL fBroadcast )
{
#if defined( SO_BROADCAST )
   int val = fBroadcast ? 1 : 0, ret;
   ret = setsockopt( sd, SOL_SOCKET, SO_BROADCAST, ( const char * ) &val, sizeof( val ) );
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   return ret;
#else
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fBroadcast );

   return -1;
#endif
}

int hb_socketSetSndBufSize( HB_SOCKET sd, int iSize )
{
   int ret = setsockopt( sd, SOL_SOCKET, SO_SNDBUF, ( const char * ) &iSize, sizeof( iSize ) );

   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   return ret;
}

int hb_socketSetRcvBufSize( HB_SOCKET sd, int iSize )
{
   int ret = setsockopt( sd, SOL_SOCKET, SO_RCVBUF, ( const char * ) &iSize, sizeof( iSize ) );

   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   return ret;
}

int hb_socketGetSndBufSize( HB_SOCKET sd, int * piSize )
{
   socklen_t len = sizeof( * piSize );
   int ret = getsockopt( sd, SOL_SOCKET, SO_SNDBUF, ( char * ) piSize, &len );

   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   return ret;
}

int hb_socketGetRcvBufSize( HB_SOCKET sd, int * piSize )
{
   socklen_t len = sizeof( * piSize );
   int ret = getsockopt( sd, SOL_SOCKET, SO_RCVBUF, ( char * ) piSize, &len );

   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   return ret;
}

int hb_socketSetMulticast( HB_SOCKET sd, int af, const char * szAddr )
{
   if( af == HB_SOCKET_AF_INET )
   {
#if defined( IP_ADD_MEMBERSHIP ) /* && defined( IPPROTO_IP ) */
      struct ip_mreq mreq;
      int ret;

      mreq.imr_multiaddr.s_addr = inet_addr( szAddr );
      mreq.imr_interface.s_addr = htonl( INADDR_ANY );

      ret = setsockopt( sd, IPPROTO_IP, IP_ADD_MEMBERSHIP, ( const char * ) &mreq, sizeof( mreq ) );
      hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
      return ret;
#else
      int iTODO;
#endif
   }
#if defined( HB_HAS_INET6 )
   else if( af == HB_SOCKET_AF_INET6 )
   {
#if defined( HB_HAS_INET_PTON )
      struct ipv6_mreq mreq;
      int err = inet_pton( AF_INET6, szAddr, &mreq.ipv6mr_multiaddr ), ret;
      if( err > 0 )
      {
         mreq.ipv6mr_interface = 0;
#if ! defined( IPV6_JOIN_GROUP ) && defined( IPV6_ADD_MEMBERSHIP )
#  define IPV6_JOIN_GROUP  IPV6_ADD_MEMBERSHIP
#endif
         ret = setsockopt( sd, IPPROTO_IPV6, IPV6_JOIN_GROUP, ( const char * ) &mreq, sizeof( mreq ) );
         hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
         return ret;
      }
      else if( err == 0 )
         hb_socketSetRawError( HB_SOCKET_ERR_WRONGADDR );
      else
         hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
      return -1;
#else
      int iTODO;
#endif
   }
#endif

   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return -1;
}

int hb_socketSelectRead( HB_SOCKET sd, HB_MAXINT timeout )
{
   int ret;

   hb_vmUnlock();
   ret = hb_socketSelectRD( sd, timeout );
   hb_vmLock();

   return ret;
}

int hb_socketSelectWrite( HB_SOCKET sd, HB_MAXINT timeout )
{
   int ret;

   hb_vmUnlock();
   ret = hb_socketSelectWR( sd, timeout );
   hb_vmLock();

   return ret;
}

int hb_socketSelectWriteEx( HB_SOCKET sd, HB_MAXINT timeout )
{
   int ret;

   hb_vmUnlock();
   ret = hb_socketSelectWRE( sd, timeout );
   hb_vmLock();

   return ret;
}

static HB_SOCKET s_socketSelectCallback( PHB_ITEM pItem )
{
   HB_SOCKET sd = HB_NO_SOCKET;

   if( pItem )
   {
      if( HB_IS_NUMERIC( pItem ) )
         sd = ( HB_SOCKET ) hb_itemGetNInt( pItem );
      else if( HB_IS_POINTER( pItem ) )
         sd = ( HB_SOCKET ) ( HB_PTRDIFF ) hb_itemGetPtr( pItem );
   }
   return sd;
}

int hb_socketSelect( PHB_ITEM pArrayRD, HB_BOOL fSetRD,
                     PHB_ITEM pArrayWR, HB_BOOL fSetWR,
                     PHB_ITEM pArrayEX, HB_BOOL fSetEX,
                     HB_MAXINT timeout, HB_SOCKET_FUNC pFunc )
{
   HB_SOCKET maxsd, sd;
   int i, ret;
   HB_SIZE nLen, nPos, ul;
   PHB_ITEM pItemSets[ 3 ];
   HB_BOOL pSet[ 3 ];
   fd_set fds[ 3 ], * pfds[ 3 ];
   struct timeval tv, * ptv;

   if( pFunc == NULL )
      pFunc = s_socketSelectCallback;

   pItemSets[ 0 ] = pArrayRD;
   pItemSets[ 1 ] = pArrayWR;
   pItemSets[ 2 ] = pArrayEX;
   pSet[ 0 ] = fSetRD;
   pSet[ 1 ] = fSetWR;
   pSet[ 2 ] = fSetEX;

   maxsd = 0;
   for( i = 0; i < 3; i++ )
   {
      ret = 0;
      nLen = pItemSets[ i ] ? hb_arrayLen( pItemSets[ i ] ) : 0;
      if( nLen > 0 )
      {
         FD_ZERO( &fds[ i ] );
         for( ul = 1; ul <= nLen; ul++ )
         {
            sd = pFunc( hb_arrayGetItemPtr( pItemSets[ i ], ul ) );
            if( sd != HB_NO_SOCKET )
            {
               if( maxsd < sd )
                  maxsd = sd;
               FD_SET( ( HB_SOCKET_T ) sd, &fds[ i ] );
               ret = 1;
            }
         }
      }
      pfds[ i ] = ret ? &fds[ i ] : NULL;
   }

   if( timeout >= 0 )
   {
      tv.tv_sec = ( long ) ( timeout / 1000 );
      tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      ptv = &tv;
   }
   else
      ptv = NULL;

   ret = select( ( int ) ( maxsd + 1 ), pfds[ 0 ], pfds[ 1 ], pfds[ 2 ], ptv );
   hb_socketSetOsError( ret == -1 ? HB_SOCK_GETERROR() : 0 );

   for( i = 0; i < 3; i++ )
   {
      if( pfds[ i ] && pSet[ i ] )
      {
         nPos = 0;
         if( ret > 0 )
         {
            nLen = hb_arrayLen( pItemSets[ i ] );
            for( ul = 1; ul <= nLen; ul++ )
            {
               sd = pFunc( hb_arrayGetItemPtr( pItemSets[ i ], ul ) );
               if( sd != HB_NO_SOCKET && FD_ISSET( ( HB_SOCKET_T ) sd, pfds[ i ] ) )
               {
                  if( ++nPos != ul )
                  {
                     hb_itemCopy( hb_arrayGetItemPtr( pItemSets[ i ], nPos ),
                                  hb_arrayGetItemPtr( pItemSets[ i ], ul ) );
                  }
               }
            }
         }
         hb_arraySize( pItemSets[ i ], nPos );
      }
   }

   return ret;
}


/*
 * DNS functions
 */
HB_BOOL hb_socketResolveInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort )
{
#if defined( AF_INET )
   struct sockaddr_in sa;
   HB_BOOL fTrans;

   memset( &sa, 0, sizeof( sa ) );
   sa.sin_family = AF_INET;
   sa.sin_port = htons( ( HB_U16 ) iPort );
   if( ! szAddr || ! *szAddr )
   {
      sa.sin_addr.s_addr = htonl( INADDR_ANY );
      *pSockAddr = memcpy( hb_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return HB_TRUE;
   }

#if defined( HB_HAS_INET_PTON )
   fTrans = inet_pton( AF_INET, szAddr, &sa.sin_addr ) > 0;
#elif defined( HB_HAS_INET_ATON )
   fTrans = inet_aton( szAddr, &sa.sin_addr ) != 0;
#else
   sa.sin_addr.s_addr = inet_addr( szAddr );
   fTrans = sa.sin_addr.s_addr != INADDR_NONE ||
            strcmp( "255.255.255.255", szAddr ) == 0; /* dirty hack */
#endif

   if( ! fTrans )
   {
#if defined( HB_HAS_ADDRINFO )
      struct addrinfo hints, * res = NULL;

      hb_vmUnlock();
      memset( &hints, 0, sizeof( hints ) );
      hints.ai_family = AF_INET;
      if( getaddrinfo( szAddr, NULL, &hints, &res ) == 0 )
      {
         if( ( int ) res->ai_addrlen >= ( int ) sizeof( struct sockaddr_in ) &&
             hb_socketGetAddrFamily( res->ai_addr, res->ai_addrlen ) == AF_INET )
         {
            sa.sin_addr.s_addr = ( ( struct sockaddr_in * ) res->ai_addr )->sin_addr.s_addr;
            fTrans = HB_TRUE;
         }
         freeaddrinfo( res );
      }
      hb_vmLock();
#else
      struct hostent * he;

      hb_vmUnlock();
      he = gethostbyname( szAddr );
      if( he && he->h_addr_list[ 0 ] )
      {
         sa.sin_addr.s_addr = ( ( struct in_addr * ) he->h_addr_list[ 0 ] )->s_addr;
         fTrans = HB_TRUE;
      }
      hb_vmLock();
#endif
   }

   if( fTrans )
   {
      *pSockAddr = memcpy( hb_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return HB_TRUE;
   }
#else
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
#endif
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

char * hb_socketResolveAddr( const char * szAddr, int af )
{
   char * szResult = NULL;
   HB_BOOL fTrans = HB_FALSE;

   if( ! szAddr || ! *szAddr )
      return NULL;

   if( af == HB_SOCKET_AF_INET )
   {
      struct in_addr sin;
#if defined( HB_HAS_INET_PTON )
      fTrans = inet_pton( AF_INET, szAddr, &sin ) > 0;
#elif defined( HB_HAS_INET_ATON )
      fTrans = inet_aton( szAddr, &sin ) != 0;
#else
      sin.s_addr = inet_addr( szAddr );
      fTrans = sin.s_addr != INADDR_NONE ||
            strcmp( "255.255.255.255", szAddr ) == 0; /* dirty hack */
#endif

#if ! defined( HB_HAS_ADDRINFO )
      if( ! fTrans )
      {
         struct hostent * he;

         hb_vmUnlock();
         he = gethostbyname( szAddr );
         if( he && he->h_addr_list[ 0 ] )
         {
            sin.s_addr = ( ( struct in_addr * ) he->h_addr_list[ 0 ] )->s_addr;
            fTrans = HB_TRUE;
         }
         hb_vmLock();
      }
#endif

      if( fTrans )
      {
#  if defined( HB_HAS_INET_NTOP )
         char buf[ INET_ADDRSTRLEN ];
         szAddr = inet_ntop( AF_INET, &sin, buf, sizeof( buf ) );
#  elif defined( HB_IS_INET_NTOA_MT_SAFE )
         szAddr = inet_ntoa( sin );
#  else
         char buf[ INET_ADDRSTRLEN ];
         szAddr = hb_inet_ntoa( &sin, buf );
#  endif
         szResult = hb_strdup( szAddr );
      }
   }
#if defined( HB_HAS_INET6 )
   else if( af == HB_SOCKET_AF_INET6 )
   {
#if defined( HB_HAS_INET_PTON )
      struct in6_addr sin;
      fTrans = inet_pton( AF_INET6, szAddr, &sin ) > 0;
      if( fTrans )
      {
#  if defined( HB_HAS_INET_NTOP )
         char buf[ INET6_ADDRSTRLEN ];
         szAddr = inet_ntop( AF_INET6, &sin, buf, sizeof( buf ) );
#  else
         int iTODO;
#  endif
         szResult = hb_strdup( szAddr );
      }
#else
      int iTODO;
      fTrans = HB_FALSE;
#endif
   }
#endif

   if( ! fTrans )
   {
#if defined( HB_HAS_ADDRINFO )
      struct addrinfo hints, * res = NULL;

      hb_vmUnlock();
#  if defined( HB_SOCKET_TRANSLATE_DOMAIN )
      af = hb_socketTransDomain( af, NULL );
#  endif
      memset( &hints, 0, sizeof( hints ) );
      hints.ai_family = af;
      if( getaddrinfo( szAddr, NULL, &hints, &res ) == 0 )
      {
         szResult = hb_socketAddrGetName( res->ai_addr, res->ai_addrlen );
         freeaddrinfo( res );
      }
      hb_vmLock();
#endif
   }

   return szResult;
}

PHB_ITEM hb_socketGetHosts( const char * szAddr, int af )
{
   PHB_ITEM pItem = NULL;

#if defined( HB_HAS_ADDRINFO )
   struct addrinfo hints, * res = NULL, * ai;
   int iResult;

   hb_vmUnlock();
#if defined( HB_SOCKET_TRANSLATE_DOMAIN )
   af = hb_socketTransDomain( af, NULL );
#endif
   memset( &hints, 0, sizeof( hints ) );
   hints.ai_family = af;
   iResult = getaddrinfo( szAddr, NULL, &hints, &res );
   hb_vmLock();

   if( iResult == 0 )
   {
      int iCount = 0, i;
      ai = res;
      while( ai )
      {
         ++iCount;
         ai = ai->ai_next;
      }
      if( iCount )
      {
         pItem = hb_itemArrayNew( iCount );
         ai = res;
         iCount = 0;
         while( ai )
         {
            char * szResult = hb_socketAddrGetName( res->ai_addr, res->ai_addrlen );
            if( szResult )
            {
               for( i = 1; i <= iCount; ++i )
               {
                  if( strcmp( hb_arrayGetCPtr( pItem, i ), szResult ) == 0 )
                  {
                     hb_xfree( szResult );
                     szResult = NULL;
                     break;
                  }
               }
               if( szResult )
               {
                  ++iCount;
                  if( ! hb_arraySetCLPtr( pItem, iCount, szResult, strlen( szResult ) ) )
                     hb_xfree( szResult );
               }
            }
            ai = ai->ai_next;
         }
         hb_arraySize( pItem, iCount );
      }
      freeaddrinfo( res );
   }
#else

   if( af == HB_SOCKET_AF_INET )
   {
      struct hostent * he = NULL;
      int iCount = 0;

      hb_vmUnlock();

      /* gethostbyname() in Windows and OS2 does not accept direct IP
       * addresses
       */
#if ( defined( HB_OS_WIN ) || defined( HB_OS_OS2 ) ) && \
    defined( HB_HAS_GETHOSTBYADDR )
      {
         ULONG addr = inet_addr( szAddr );
         if( addr != INADDR_NONE || strcmp( "255.255.255.255", szAddr ) == 0 )
            he = gethostbyaddr( ( const char * ) &addr, sizeof( addr ), AF_INET );
      }
#endif
      if( he == NULL )
         he = gethostbyname( szAddr );

      hb_vmLock();

      if( he )
      {
         while( he->h_addr_list[ iCount ] )
            ++iCount;
      }
      if( iCount > 0 )
      {
         pItem = hb_itemArrayNew( iCount );
         do
         {
            struct in_addr * sin = ( struct in_addr * ) he->h_addr_list[ iCount - 1 ];
#  if defined( HB_HAS_INET_NTOP )
            char buf[ INET_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET, sin, buf, sizeof( buf ) );
#  elif defined( HB_IS_INET_NTOA_MT_SAFE )
            szAddr = inet_ntoa( *sin );
#  else
            char buf[ INET_ADDRSTRLEN ];
            szAddr = hb_inet_ntoa( sin, buf );
#  endif
            hb_arraySetC( pItem, iCount, szAddr );
         }
         while( --iCount );
      }
   }
#if defined( HB_HAS_INET6 )
   else if( af == HB_SOCKET_AF_INET6 )
   {
      int iTODO;
   }
#endif

#endif

   return pItem;
}

PHB_ITEM hb_socketGetAliases( const char * szAddr, int af )
{
   /* TODO: implement it */
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( af );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

char * hb_socketGetHostName( const void * pSockAddr, unsigned len )
{
   char * szResult = NULL;
   int af = hb_socketGetAddrFamily( pSockAddr, len );

   if( af != -1 )
   {
#if defined( HB_HAS_NAMEINFO )
      #if ! defined( NI_MAXHOST )
         #define NI_MAXHOST  1025
      #endif
      char szHost[ NI_MAXHOST ];
      int iResult;

      hb_vmUnlock();
      iResult = getnameinfo( ( const struct sockaddr * ) pSockAddr, len, szHost, NI_MAXHOST, NULL, 0, 0 );
      hb_vmLock();
      if( iResult == 0 )
         szResult = hb_strdup( szHost );
#elif defined( HB_HAS_ADDRINFO ) && ! defined( HB_HAS_GETHOSTBYADDR )
      char * szAddr = hb_socketAddrGetName( pSockAddr, len );
      if( szAddr )
      {
         struct addrinfo hints, * res = NULL;

         hb_vmUnlock();
         memset( &hints, 0, sizeof( hints ) );
         hints.ai_family = af;
         hints.ai_flags = AI_CANONNAME;
         if( getaddrinfo( szAddr, NULL, &hints, &res ) == 0 )
         {
            if( res->ai_canonname )
               szResult = hb_strdup( res->ai_canonname );
            freeaddrinfo( res );
         }
         hb_vmLock();
      }
#else
      struct hostent * he = NULL;

      if( af == AF_INET )
      {
#if defined( HB_HAS_GETHOSTBYADDR )
         const struct sockaddr_in * sa = ( const struct sockaddr_in * ) pSockAddr;
         hb_vmUnlock();
         he = gethostbyaddr( ( const char * ) &sa->sin_addr, sizeof( sa->sin_addr ), af );
         hb_vmLock();
#else
         char * szAddr = hb_socketAddrGetName( pSockAddr, len );
         if( szAddr )
         {
            hb_vmUnlock();
            he = gethostbyname( szAddr );
            hb_vmLock();
         }
#endif
      }
#if defined( HB_HAS_INET6 ) && defined( HB_HAS_GETHOSTBYADDR )
      else if( af == AF_INET6 )
      {
         const struct sockaddr_in6 * sa = ( const struct sockaddr_in6 * ) pSockAddr;
         hb_vmUnlock();
         he = gethostbyaddr( ( const char * ) &sa->sin6_addr, sizeof( sa->sin6_addr ), af );
         hb_vmLock();
      }
#endif
      if( he && he->h_name )
         szResult = hb_strdup( he->h_name );
#endif
   }
   return szResult;
}


/*
 * IFACEs
 */
#if defined( HB_OS_WIN ) || ( defined( SIOCGIFCONF ) && \
    !( ( defined( HB_OS_LINUX ) || defined( HB_OS_DOS ) ) && defined( __WATCOMC__ ) ) )
static void hb_socketArraySetInetAddr( PHB_ITEM pItem, HB_SIZE nPos,
                                       const void * pSockAddr, unsigned len )
{
   char * szAddr = hb_socketAddrGetName( pSockAddr, len );

   if( szAddr )
   {
      if( ! hb_arraySetCLPtr( pItem, nPos, szAddr, strlen( szAddr ) ) )
         hb_xfree( szAddr );
   }
}
#endif

PHB_ITEM hb_socketGetIFaces( int af, HB_BOOL fNoAliases )
{
   PHB_ITEM pArray = NULL;
   PHB_ITEM pItem = NULL;
   int iError = 0;

/*
 * TODO: add support for alternative long interface introduced in some
 *       new systems using 'struct lifreq' with SIOCGLIF* ioctls instead
 *       of 'struct ifreq' and SIOCGIF*
 */
#if defined( SIOCGIFCONF ) && \
    !( ( defined( HB_OS_LINUX ) || defined( HB_OS_DOS ) ) && defined( __WATCOMC__ ) )
   struct ifconf ifc;
   struct ifreq * pifr;
   char * buf, * ptr;
   const char * pLastName = NULL;
   int len = 0, size, iLastName = 0, iLastFamily = 0, flags, family;
   HB_SOCKET sd;

   sd = hb_socketOpen( af ? af : HB_SOCKET_AF_INET, HB_SOCKET_PT_DGRAM, 0 );
   if( sd != HB_NO_SOCKET )
   {
#  if defined( HB_SOCKET_TRANSLATE_DOMAIN )
      af = hb_socketTransDomain( af, NULL );
#  endif
#  ifdef SIOCGIFNUM
      if( ioctl( sd, SIOCGIFNUM, &len ) == -1 )
         len = 0;
#  endif
      if( len <= 0 )
         len = 0x8000;
      len *= sizeof( struct ifreq );
      buf = ( char * ) hb_xgrab( len );

      ifc.ifc_len = len;
      ifc.ifc_buf = buf;

      /* Warning: On some platforms this code can effectively work only with
       *          IP4 interfaces and IP6 will need different implementation.
       */

      if( ioctl( sd, SIOCGIFCONF, &ifc ) != -1 )
      {
         for( ptr = ifc.ifc_buf, size = ifc.ifc_len; size > 0; )
         {
            pifr = ( struct ifreq * ) ptr;
            family = pifr->ifr_addr.sa_family;
#  if defined( HB_HAS_SOCKADDR_SA_LEN )
            len = pifr->ifr_addr.sa_len;
            if( len < ( int ) sizeof( struct sockaddr ) )
               len = sizeof( struct sockaddr );
#  else
            switch( family )
            {
#     if defined( HB_HAS_INET6 )
               case AF_INET6:
                  len = sizeof( struct sockaddr_in6 );
                  break;
#     endif
#     if defined( AF_INET )
               case AF_INET:
#     endif
               default:
                  len = sizeof( struct sockaddr );
                  break;
            }
#  endif
            len += sizeof( pifr->ifr_name );
#  if ! defined( HB_OS_BEOS )
            if( len < ( int ) sizeof( struct ifreq ) )
               len = ( int ) sizeof( struct ifreq );
#  endif
            ptr += len;
            size -= len;

            if( af && family != af )
               continue;

            /* skip alias devices */
            if( fNoAliases )
            {
               const char * cptr = strchr( pifr->ifr_name, ':' );

               len = cptr ? ( int ) ( cptr - pifr->ifr_name ) :
                            ( int ) strlen( pifr->ifr_name );
               if( pLastName && len == iLastName && family == iLastFamily &&
                   memcmp( pLastName, pifr->ifr_name, len ) == 0 )
                  continue;
               pLastName = pifr->ifr_name;
               iLastName = len;
               iLastFamily = family;
            }

            {
               struct ifreq ifr = *pifr;
               if( ioctl(  sd, SIOCGIFFLAGS, &ifr ) == -1 )
                  continue;
               flags = ifr.ifr_flags;
            }

            if( ( flags & IFF_UP ) == 0 )
               continue;

            if( pItem == NULL )
               pItem = hb_itemNew( NULL );

            hb_arrayNew( pItem, HB_SOCKET_IFINFO_LEN );

            pifr->ifr_name[ sizeof( pifr->ifr_name ) - 1 ] = '\0';
            hb_arraySetC( pItem, HB_SOCKET_IFINFO_NAME, pifr->ifr_name );

            switch( family )
            {
#  if defined( HB_HAS_INET6 )
               case AF_INET6:
                  len = sizeof( struct sockaddr_in6 );
                  family = HB_SOCKET_AF_INET6;
                  break;
#  endif
#  if defined( AF_INET )
               case AF_INET:
                  len = sizeof( struct sockaddr_in );
                  family = HB_SOCKET_AF_INET;
                  break;
#  endif
               default:
                  len = 0;
                  break;
            }
            hb_arraySetNI( pItem, HB_SOCKET_IFINFO_FAMILY, family );

            if( len )
            {
               hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_ADDR,
                                          &pifr->ifr_addr, len );

#  if defined( SIOCGIFNETMASK )
#     ifndef ifr_netmask
#        define ifr_netmask   ifr_addr
#     endif
               if( ioctl( sd, SIOCGIFNETMASK, pifr ) != -1 )
                  hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_NETMASK,
                                             &pifr->ifr_netmask, len );
#  endif
#  if defined( SIOCGIFBRDADDR )
               if( flags & IFF_BROADCAST )
               {
                  if( ioctl( sd, SIOCGIFBRDADDR, pifr ) != -1 )
                     hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_BROADCAST,
                                                &pifr->ifr_broadaddr, len );
               }
#  endif
#  if defined( SIOCGIFDSTADDR )
               if( flags & IFF_POINTOPOINT )
               {
                  if( ioctl( sd, SIOCGIFDSTADDR, pifr ) != -1 )
                     hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_P2PADDR,
                                                &pifr->ifr_dstaddr, len );
               }
#  endif
#  if defined( SIOCGIFHWADDR )
#     ifndef ifr_hwaddr
#        define ifr_hwaddr    ifr_addr
#     endif
               if( ioctl( sd, SIOCGIFHWADDR, pifr ) != -1 )
               {
                  char hwaddr[ 24 ];
                  unsigned char * data;
                  data = ( unsigned char * ) &pifr->ifr_hwaddr.sa_data[0];
                  hb_snprintf( hwaddr, sizeof( hwaddr ),
                               "%02X:%02X:%02X:%02X:%02X:%02X",
                               data[ 0 ], data[ 1 ], data[ 2 ],
                               data[ 3 ], data[ 4 ], data[ 5 ] );
                  hb_arraySetC( pItem, HB_SOCKET_IFINFO_HWADDR, hwaddr );
               }
#  elif defined( SIOCGENADDR )
               if( ioctl( sd, SIOCGENADDR, pifr ) != -1 )
               {
                  char hwaddr[ 24 ];
                  unsigned char * data;
                  data = ( unsigned char * ) &pifr->ifr_enaddr[0];
                  hb_snprintf( hwaddr, sizeof( hwaddr ),
                               "%02X:%02X:%02X:%02X:%02X:%02X",
                               data[ 0 ], data[ 1 ], data[ 2 ],
                               data[ 3 ], data[ 4 ], data[ 5 ] );
                  hb_arraySetC( pItem, HB_SOCKET_IFINFO_HWADDR, hwaddr );
               }
#  endif
            }

            flags = ( ( flags & IFF_UP ) ?
                      HB_SOCKET_IFF_UP : 0 ) |
                    ( ( flags & IFF_BROADCAST ) ?
                      HB_SOCKET_IFF_BROADCAST : 0 ) |
                    ( ( flags & IFF_LOOPBACK ) ?
                      HB_SOCKET_IFF_LOOPBACK : 0 ) |
                    ( ( flags & IFF_POINTOPOINT ) ?
                      HB_SOCKET_IFF_POINTOPOINT : 0 ) |
                    ( ( flags & IFF_MULTICAST ) ?
                      HB_SOCKET_IFF_MULTICAST : 0 );
            hb_arraySetNI( pItem, HB_SOCKET_IFINFO_FLAGS, flags );

            if( pArray == NULL )
               pArray = hb_itemArrayNew( 0 );
            hb_arrayAddForward( pArray, pItem );
         }
      }
      else
         iError = HB_SOCK_GETERROR();
      hb_xfree( buf );
      hb_socketClose( sd );
   }
#elif defined( HB_OS_WIN ) && ! defined( __DMC__ )
   HB_SOCKET sd;

   /* TODO: add support for IP6 */

   /* TODO: implement it */
   HB_SYMBOL_UNUSED( fNoAliases );

   sd = hb_socketOpen( af ? af : HB_SOCKET_AF_INET, HB_SOCKET_PT_DGRAM, 0 );
   if( sd != HB_NO_SOCKET )
   {
      DWORD dwBuffer = 0x8000 * sizeof( INTERFACE_INFO );
      void * pBuffer = hb_xgrab( dwBuffer );
      LPINTERFACE_INFO pIfInfo = ( LPINTERFACE_INFO ) pBuffer;

      if( WSAIoctl( sd, SIO_GET_INTERFACE_LIST, NULL, 0, pIfInfo, dwBuffer,
                    &dwBuffer, 0, 0 ) != SOCKET_ERROR )
      {
         int iCount = dwBuffer / sizeof( INTERFACE_INFO );

         while( iCount-- )
         {
            u_long flags = pIfInfo->iiFlags;

            if( flags & IFF_UP )
            {
               if( pItem == NULL )
                  pItem = hb_itemNew( NULL );
               hb_arrayNew( pItem, HB_SOCKET_IFINFO_LEN );

               hb_arraySetNI( pItem, HB_SOCKET_IFINFO_FAMILY,
                              pIfInfo->iiAddress.Address.sa_family );

               hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_ADDR,
                                          &pIfInfo->iiAddress,
                                          sizeof( pIfInfo->iiAddress ) );
               hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_NETMASK,
                                          &pIfInfo->iiNetmask,
                                          sizeof( pIfInfo->iiNetmask ) );
               if( flags & IFF_BROADCAST )
                  hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_BROADCAST,
                                             &pIfInfo->iiBroadcastAddress,
                                             sizeof( pIfInfo->iiBroadcastAddress ) );
               if( flags & IFF_POINTTOPOINT )
                  hb_socketArraySetInetAddr( pItem, HB_SOCKET_IFINFO_P2PADDR,
                                             &pIfInfo->iiBroadcastAddress,
                                             sizeof( pIfInfo->iiBroadcastAddress ) );

               /* TODO:
                *       hb_arraySetC( pItem, HB_SOCKET_IFINFO_HWADDR, hwaddr );
                */

               flags = ( ( flags & IFF_UP ) ?
                         HB_SOCKET_IFF_UP : 0 ) |
                       ( ( flags & IFF_BROADCAST ) ?
                         HB_SOCKET_IFF_BROADCAST : 0 ) |
                       ( ( flags & IFF_LOOPBACK ) ?
                         HB_SOCKET_IFF_LOOPBACK : 0 ) |
                       ( ( flags & IFF_POINTTOPOINT ) ?
                         HB_SOCKET_IFF_POINTOPOINT : 0 ) |
                       ( ( flags & IFF_MULTICAST ) ?
                         HB_SOCKET_IFF_MULTICAST : 0 );
               hb_arraySetNI( pItem, HB_SOCKET_IFINFO_FLAGS, flags );

               /* Windows does not support interface names like other OS-es
                * use interface IP address instead
                */
               hb_arraySet( pItem, HB_SOCKET_IFINFO_NAME,
                            hb_arrayGetItemPtr( pItem, HB_SOCKET_IFINFO_ADDR ) );

               if( pArray == NULL )
                  pArray = hb_itemArrayNew( 0 );
               hb_arrayAddForward( pArray, pItem );
            }
            pIfInfo++;
         }
      }
      else
         iError = HB_SOCK_GETERROR();

      hb_xfree( pBuffer );
      hb_socketClose( sd );
   }
   else
      iError = HB_SOCK_GETERROR();
#else
   int iTODO;
   HB_SYMBOL_UNUSED( af );
   HB_SYMBOL_UNUSED( fNoAliases );
   hb_socketSetRawError( HB_SOCKET_ERR_AFNOSUPPORT );
#endif

   if( pItem )
      hb_itemRelease( pItem );

   if( iError != 0 )
      hb_socketSetOsError( iError );

   return pArray;
}
#endif /* ! HB_SOCKET_OFF */
