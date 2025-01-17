/*
 * Socket C API
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

   platform supports poll() function:
      #define HB_HAS_POLL

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
#     if ! defined( HB_HAS_POLL ) && ! defined( HB_NO_POLL ) && \
         defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
         /* use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
            file handle limit */
#        define HB_HAS_POLL
#     endif
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
#     if defined( HB_CPU_MIPS )
#        define HB_SOCKET_TRANSLATE_TYPE
#     endif
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
#     if ( NTDDI_VERSION >= 0x06000000 ) && ! defined( HB_WINSOCK_USE_OLDFUNC )
#        define HB_HAS_INET_PTON
#        define HB_HAS_INET_NTOP
#     endif
#     define HB_HAS_SOCKADDR_STORAGE
/* #     define HB_HAS_INET6 */
#  elif defined( __MINGW32__ )
#     define HB_HAS_SOCKADDR_STORAGE
#  elif defined( __POCC__ ) && ! defined( __XCC__ )
#     define HB_HAS_SOCKADDR_STORAGE
#  elif defined( _MSC_VER )
#     if _MSC_VER >= 1800 && ! defined( HB_WINSOCK_USE_OLDFUNC )
#        define HB_HAS_INET_PTON
#        define HB_HAS_INET_NTOP
#        define HB_HAS_ADDRINFO
#        define HB_HAS_NAMEINFO
#     else
#        define _WINSOCK_DEPRECATED_NO_WARNINGS
#     endif
#  endif
#  define HB_IS_INET_NTOA_MT_SAFE
#  define HB_HAS_GETHOSTBYADDR
#  define hb_socketSetResolveError( err ) hb_socketSetOsError( err )
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

#if defined( HB_HAS_NAMEINFO ) && ! defined( HB_HAS_ADDRINFO )
#  undef HB_HAS_NAMEINFO
#endif


#if defined( HB_OS_WIN )
#  include <winsock2.h>
#  include <ws2tcpip.h>
#  if ! defined( __DMC__ )
#     include <iphlpapi.h>
#  endif
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
#  if defined( HB_OS_BSD )
#     include <ifaddrs.h>
#     include <net/if_dl.h>
#  endif
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
#  if defined( HB_HAS_POLL )
#     include <poll.h>
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
   /* FIXME: Bad workaround for the '__WSAFDIsSet unresolved' problem
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

/* TODO change error description to something more user friendly */
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

void hb_socketSetError( int err )
{
   PHB_IOERRORS pError = hb_stackIOErrors();

   pError->uiSocketError = ( HB_ERRCODE ) err;
   pError->iSocketOsError = 0;
}

#if defined( HB_SOCKET_OFF )

int hb_socketInit( void ) { return -1; }

void hb_socketCleanup( void ) { ; }

HB_U16 hb_socketNToHS( HB_U16 netshort )
{
#if defined( HB_LITTLE_ENDIAN )
   return HB_SWAP_UINT16( netshort );
#else
   return netshort;
#endif
}

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
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

HB_BOOL hb_socketInetAddr( void ** pSockAddr, unsigned * puiLen,
                           const char * szAddr, int iPort )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

HB_BOOL hb_socketInet6Addr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr, int iPort )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

char * hb_socketAddrGetName( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

int hb_socketAddrGetPort( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return -1;
}

HB_BOOL hb_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PHB_ITEM pAddrItm )
{
   HB_SYMBOL_UNUSED( pAddrItm );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

PHB_ITEM hb_socketAddrToItem( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

int hb_socketGetSockName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   HB_SYMBOL_UNUSED( sd );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return -1;
}

int hb_socketGetPeerName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   HB_SYMBOL_UNUSED( sd );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return -1;
}

HB_SOCKET hb_socketOpen( int domain, int type, int protocol )
{
   HB_SYMBOL_UNUSED( domain );
   HB_SYMBOL_UNUSED( type );
   HB_SYMBOL_UNUSED( protocol );
   hb_socketSetError( HB_SOCKET_ERR_PFNOSUPPORT );
   return HB_NO_SOCKET;
}

int hb_socketClose( HB_SOCKET sd )
{
   HB_SYMBOL_UNUSED( sd );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketShutdown( HB_SOCKET sd, int iMode )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iMode );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketBind( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( uiLen );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketListen( HB_SOCKET sd, int iBacklog )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iBacklog );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

HB_SOCKET hb_socketAccept( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_NO_SOCKET;
}

int hb_socketConnect( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( uiLen );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long hb_socketSend( HB_SOCKET sd, const void * data, long len, int flags, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( flags );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
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
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long hb_socketRecv( HB_SOCKET sd, void * data, long len, int flags, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( flags );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long hb_socketRecvFrom( HB_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( flags );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiSockLen = 0;
   return -1;
}

int hb_socketSetBlockingIO( HB_SOCKET sd, HB_BOOL fBlocking )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fBlocking );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetNoDelay( HB_SOCKET sd, HB_BOOL fNoDelay )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fNoDelay );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetNoSigPipe( HB_SOCKET sd, HB_BOOL fNoSigPipe )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fNoSigPipe );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetExclusiveAddr( HB_SOCKET sd, HB_BOOL fExclusive )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fExclusive );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetReuseAddr( HB_SOCKET sd, HB_BOOL fReuse )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fReuse );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetKeepAlive( HB_SOCKET sd, HB_BOOL fKeepAlive )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fKeepAlive );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetBroadcast( HB_SOCKET sd, HB_BOOL fBroadcast )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fBroadcast );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetSndBufSize( HB_SOCKET sd, int iSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iSize );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetRcvBufSize( HB_SOCKET sd, int iSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( iSize );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketGetSndBufSize( HB_SOCKET sd, int * piSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( piSize );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketGetRcvBufSize( HB_SOCKET sd, int * piSize )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( piSize );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSetMulticast( HB_SOCKET sd, int af, const char * szAddr )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( af );
   HB_SYMBOL_UNUSED( szAddr );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSelectRead( HB_SOCKET sd, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int hb_socketSelectWrite( HB_SOCKET sd, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}


int hb_socketSelectWriteEx( HB_SOCKET sd, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
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
   hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
   return -1;
}

HB_BOOL hb_socketResolveInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

char * hb_socketResolveAddr( const char * szAddr, int af )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( af );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PHB_ITEM hb_socketGetHosts( const char * szAddr, int af )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( af );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PHB_ITEM hb_socketGetAliases( const char * szAddr, int af )
{
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( af );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

char * hb_socketGetHostName( const void * pSockAddr, unsigned len )
{
   HB_SYMBOL_UNUSED( pSockAddr );
   HB_SYMBOL_UNUSED( len );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PHB_ITEM hb_socketGetIFaces( int af, HB_BOOL fNoAliases )
{
   HB_SYMBOL_UNUSED( af );
   HB_SYMBOL_UNUSED( fNoAliases );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

#else

#define HB_SOCKADDR_MAX_LEN   256

#if defined( HB_OS_WIN )
#  define HB_SOCK_GETERROR()              WSAGetLastError()
#  define HB_SOCK_GETHERROR()             WSAGetLastError()
#  define HB_SOCK_IS_EINTR( err )         ( (err) == WSAEINTR )
#  define HB_SOCK_IS_EINPROGRES( err )    ( (err) == WSAEWOULDBLOCK )
#elif defined( HB_OS_OS2 ) && defined( __WATCOMC__ )
#  define HB_SOCK_GETERROR()              sock_errno()
#  define HB_SOCK_GETHERROR()             h_errno
#  define HB_SOCK_IS_EINTR( err )         ( (err) == EINTR )
#  define HB_SOCK_IS_EINPROGRES( err )    ( (err) == EINPROGRESS )
#elif defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
   /* h_errno is still not supported by Linux OpenWatcom port :-( */
#  define HB_SOCK_GETERROR()              errno
#  define HB_SOCK_GETHERROR()             errno
#  define HB_SOCK_IS_EINTR( err )         ( (err) == EINTR )
#  define HB_SOCK_IS_EINPROGRES( err )    ( (err) == EINPROGRESS )
#else
#  define HB_SOCK_GETERROR()              errno
#  define HB_SOCK_GETHERROR()             h_errno
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
      case WSA_NOT_ENOUGH_MEMORY:
         uiErr = HB_SOCKET_ERR_NOMEM;
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
      default:
         uiErr = HB_SOCKET_ERR_OTHER;
         break;
   }
#endif

   pError->uiSocketError = uiErr;
   pError->iSocketOsError = err;
}

#if ! defined( HB_OS_WIN )
static void hb_socketSetResolveError( int err )
{
   PHB_IOERRORS pError = hb_stackIOErrors();
   HB_ERRCODE uiErr;

   switch( err )
   {
      case 0:
         uiErr = 0;
         break;

#if defined( HB_HAS_ADDRINFO ) || defined( HB_HAS_NAMEINFO )

      /* getaddrinfo() / getnameinfo() */
#if defined( EAI_AGAIN )
      case EAI_AGAIN:
         uiErr = HB_SOCKET_ERR_TRYAGAIN;
         break;
#endif
#if defined( EAI_BADFLAGS )
      case EAI_BADFLAGS:
         uiErr = HB_SOCKET_ERR_INVAL;
         break;
#endif
#if defined( EAI_FAIL )
      case EAI_FAIL:
         uiErr = HB_SOCKET_ERR_NORECOVERY;
         break;
#endif
#if defined( EAI_ADDRFAMILY )
      case EAI_ADDRFAMILY:
#endif
#if defined( EAI_FAMILY )
      case EAI_FAMILY:
         uiErr = HB_SOCKET_ERR_AFNOSUPPORT;
         break;
#endif
#if defined( EAI_MEMORY )
      case EAI_MEMORY:
         uiErr = HB_SOCKET_ERR_NOMEM;
         break;
#endif
#if defined( EAI_NODATA )
      case EAI_NODATA:
         uiErr = HB_SOCKET_ERR_NODATA;
         break;
#endif
#if defined( EAI_NONAME )
      case EAI_NONAME:
         uiErr = HB_SOCKET_ERR_HOSTNOTFOUND;
         break;
#endif
#if defined( EAI_OVERFLOW )
      case EAI_OVERFLOW:
         uiErr = HB_SOCKET_ERR_NAMETOOLONG;
         break;
#endif
#if defined( EAI_SERVICE )
      case EAI_SERVICE:
         uiErr = HB_SOCKET_ERR_TYPENOTFOUND;
         break;
#endif
#if defined( EAI_SOCKTYPE )
      case EAI_SOCKTYPE:
         uiErr = HB_SOCKET_ERR_NOSUPPORT;
         break;
#endif
#if defined( EAI_SYSTEM )
      case EAI_SYSTEM:
         uiErr = HB_SOCKET_ERR_SYSCALLFAILURE;
         break;
#endif

#else /* ! HB_HAS_ADDRINFO && ! HB_HAS_NAMEINFO */

      /* gethostbyname() / gethostbyaddr() */
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

#endif /* ! HB_HAS_ADDRINFO && ! HB_HAS_NAMEINFO */
      default:
         uiErr = HB_SOCKET_ERR_WRONGADDR;
         break;
   }

   pError->uiSocketError = uiErr;
   pError->iSocketOsError = err;
}
#endif

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

static int hb_socketTransFlags( int flags )
{
   int iResult = 0;

   if( flags )
   {
#ifdef MSG_OOB
      if( flags & HB_SOCKET_MSG_OOB )
         iResult |= MSG_OOB;
#endif
#ifdef MSG_PEEK
      if( flags & HB_SOCKET_MSG_PEEK )
         iResult |= MSG_PEEK;
#endif
#ifdef MSG_DONTROUTE
      if( flags & HB_SOCKET_MSG_DONTROUTE )
         iResult |= MSG_DONTROUTE;
#endif
#ifdef MSG_WAITALL
      if( flags & HB_SOCKET_MSG_WAITALL )
         iResult |= MSG_WAITALL;
#endif
   }
   return iResult;
}

static int hb_socketSelectRD( HB_SOCKET sd, HB_MAXINT timeout )
{
#if defined( HB_HAS_POLL )
   HB_MAXUINT timer = hb_timerInit( timeout );
   struct pollfd fds;
   int iResult;

   fds.fd = sd;
   fds.events = POLLIN;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout, iError;
      iResult = poll( &fds, 1, tout );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      if( iResult > 0 && ( fds.revents & POLLIN ) == 0 )
      {
         if( fds.revents & POLLNVAL )
         {
            iError = EBADF;
            iResult = -1;
         }
         else
         {
            timeout = 0;
            iResult = fds.revents & ( POLLHUP | POLLERR ) ? 1 : 0;
         }
      }
      hb_socketSetOsError( iError );
      if( iResult == -1 && HB_SOCK_IS_EINTR( iError ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = hb_timerTest( timeout, &timer ) ) != 0 &&
          hb_vmRequestQuery() == 0 );

   return iResult;
#else /* ! HB_HAS_POLL */
   int iResult;
   struct timeval tv;
   fd_set rfds;
#  if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = hb_timerInit( timeout );
#  else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      int iError;

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( HB_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &rfds );
      FD_SET( ( HB_SOCKET_T ) sd, &rfds );
      iResult = select( ( int ) ( sd + 1 ), &rfds, NULL, NULL, &tv );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      hb_socketSetOsError( iError );

      if( iResult == -1 && HB_SOCK_IS_EINTR( iError ) )
         iResult = 0;

#  if defined( HB_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || hb_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( timeout = hb_timerTest( timeout, &timer ) ) == 0 ||
          hb_vmRequestQuery() != 0 )
         break;
#  endif
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, &rfds ) ? 1 : 0 );
#endif /* ! HB_HAS_POLL */
}

static int hb_socketSelectWR( HB_SOCKET sd, HB_MAXINT timeout )
{
#if defined( HB_HAS_POLL )
   HB_MAXUINT timer = hb_timerInit( timeout );
   struct pollfd fds;
   int iResult;

   fds.fd = sd;
   fds.events = POLLOUT;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout, iError;
      iResult = poll( &fds, 1, tout );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      if( iResult > 0 && ( fds.revents & POLLOUT ) == 0 )
      {
         if( fds.revents & POLLNVAL )
         {
            iError = EBADF;
            iResult = -1;
         }
         else if( fds.revents & ( POLLHUP | POLLERR ) )
         {
            iError = EPIPE;
            iResult = -1;
         }
         else
         {
            timeout = 0;
            iResult = 0;
         }
      }
      hb_socketSetOsError( iError );
      if( iResult == -1 && HB_SOCK_IS_EINTR( iError ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = hb_timerTest( timeout, &timer ) ) != 0 &&
          hb_vmRequestQuery() == 0 );

   return iResult;
#else /* ! HB_HAS_POLL */
   int iResult;
   struct timeval tv;
   fd_set wfds;
#  if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = hb_timerInit( timeout );
#  else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      int iError;

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( HB_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &wfds );
      FD_SET( ( HB_SOCKET_T ) sd, &wfds );
      iResult = select( ( int ) ( sd + 1 ), NULL, &wfds, NULL, &tv );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      hb_socketSetOsError( iError );

      if( iResult == -1 && HB_SOCK_IS_EINTR( iError ) )
         iResult = 0;

#  if defined( HB_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || hb_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( timeout = hb_timerTest( timeout, &timer ) ) == 0 ||
          hb_vmRequestQuery() != 0 )
         break;
#  endif
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, &wfds ) ? 1 : 0 );
#endif /* ! HB_HAS_POLL */
}

static int hb_socketSelectWRE( HB_SOCKET sd, HB_MAXINT timeout )
{
#if defined( HB_HAS_POLL )
   HB_MAXUINT timer = hb_timerInit( timeout );
   int iResult, iError, tout;
   struct pollfd fds;

   fds.fd = sd;
   fds.events = POLLOUT;
   fds.revents = 0;

   do
   {
      tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout;
      iResult = poll( &fds, 1, tout );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      if( iResult > 0 && ( fds.revents & POLLOUT ) == 0 )
      {
         if( fds.revents & POLLNVAL )
         {
            iError = EBADF;
            iResult = -1;
         }
         else if( fds.revents & ( POLLHUP | POLLERR ) )
         {
            iError = EPIPE;
            iResult = -1;
         }
         else
         {
            timeout = 0;
            iResult = 0;
         }
      }
      hb_socketSetOsError( iError );
      if( iResult == -1 && HB_SOCK_IS_EINTR( iError ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = hb_timerTest( timeout, &timer ) ) != 0 &&
          hb_vmRequestQuery() == 0 );

   if( iResult > 0 )
   {
      socklen_t len = sizeof( iError );
      if( getsockopt( sd, SOL_SOCKET, SO_ERROR, ( char * ) &iError, &len ) != 0 )
      {
         iResult = -1;
         iError = HB_SOCK_GETERROR();
      }
      else if( iError != 0 )
         iResult = -1;
      hb_socketSetOsError( iError );
   }
   return iResult;
#else /* ! HB_HAS_POLL */
   int iResult, iError;
   struct timeval tv;
   fd_set wfds;
#  if defined( HB_OS_WIN )
   fd_set efds;
#  endif
   socklen_t len;
#  if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = hb_timerInit( timeout );
#  else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      fd_set * pefds;

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( HB_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &wfds );
      FD_SET( ( HB_SOCKET_T ) sd, &wfds );
#  if defined( HB_OS_WIN )
      FD_ZERO( &efds );
      FD_SET( ( HB_SOCKET_T ) sd, &efds );
      pefds = &efds;
#  else
      pefds = NULL;
#  endif

      iResult = select( ( int ) ( sd + 1 ), NULL, &wfds, pefds, &tv );
      iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
      hb_socketSetOsError( iError );
#  if defined( HB_OS_WIN )
      if( iResult > 0 && FD_ISSET( ( HB_SOCKET_T ) sd, pefds ) )
      {
         iResult = -1;
         len = sizeof( iError );
         if( getsockopt( sd, SOL_SOCKET, SO_ERROR, ( char * ) &iError, &len ) != 0 )
            iError = HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
      }
      else
#  endif
      if( iResult == -1 && HB_SOCK_IS_EINTR( iError ) )
         iResult = 0;

#  if defined( HB_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || hb_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( timeout = hb_timerTest( timeout, &timer ) ) == 0 ||
          hb_vmRequestQuery() != 0 )
         break;
#  endif
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
#endif /* ! HB_HAS_POLL */
}

HB_U16 hb_socketNToHS( HB_U16 netshort )
{
   return ntohs( netshort );
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
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
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
         hb_socketSetError( HB_SOCKET_ERR_WRONGADDR );
   }
#else
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
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
         hb_socketSetError( HB_SOCKET_ERR_WRONGADDR );
      else
         hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
#else
      int iTODO;
#endif
   }
#else
   HB_SYMBOL_UNUSED( szAddr );
   HB_SYMBOL_UNUSED( iPort );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
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
            szAddr = inet_ntop( AF_INET, HB_UNCONST( &sa->sin_addr ), buf, sizeof( buf ) );
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
            szAddr = inet_ntop( AF_INET6, HB_UNCONST( &sa->sin6_addr ), buf, sizeof( buf ) );
#  else
            {
               int iTODO;
               szAddr = NULL;
               HB_SYMBOL_UNUSED( sa );
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
   hb_socketSetError( szName ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
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
   hb_socketSetError( iPort != -1 ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
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
   hb_socketSetError( fOK ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
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
            szAddr = inet_ntop( AF_INET, HB_UNCONST( &sa->sin_addr ), buf, sizeof( buf ) );
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
            szAddr = inet_ntop( AF_INET6, HB_UNCONST( &sa->sin6_addr ), buf, sizeof( buf ) );
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
   hb_socketSetError( pAddrItm ? 0 : HB_SOCKET_ERR_AFNOSUPPORT );
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
   hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
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
      hb_socketSetError( err );

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
      hb_socketSetError( HB_SOCKET_ERR_PARAMVALUE );
      return -1;
   }

#if defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
{
   int iTODO;
   /* it's still not supported by Linux OpenWatcom port :-( */
   ret = -1;
   hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
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

#if defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
   ret = bind( sd, ( struct sockaddr * ) pSockAddr, ( socklen_t ) uiLen );
#else
   ret = bind( sd, ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiLen );
#endif
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
   int ret;

   hb_vmUnlock();
   if( pSockAddr && puiLen )
   {
      *pSockAddr = NULL;
      *puiLen = 0;
   }
   ret = hb_socketSelectRD( sd, timeout );
   if( ret > 0 )
   {
      int err;

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
      if( newsd != HB_NO_SOCKET )
      {
         if( pSockAddr && puiLen )
         {
            *pSockAddr = memcpy( hb_xgrab( len + 1 ), &st.sa, len );
            *puiLen = ( unsigned ) len;
         }
         /* it's not guarantied that socket returned by accept will use
          * blocking IO operations. On some systems it inherits blocking
          * IO from parent handler so we have to force blocking IO mode
          * explicitly.
          */
         hb_socketSetBlockingIO( newsd, HB_TRUE );
      }

      hb_socketSetOsError( err );
   }
   else if( ret == 0 )
      hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
   hb_vmLock();
   return newsd;
}

int hb_socketConnect( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen, HB_MAXINT timeout )
{
   int ret, blk, err;

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
      {
         hb_socketSetError( 0 );
         ret = 0;
      }
      else if( ret == 0 )
      {
         hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
         ret = -1;
      }
   }

   if( blk > 0 )
   {
      int rawerr;
      err = hb_socketGetOsError();
      rawerr = err ? 0 : hb_socketGetError();

      hb_socketSetBlockingIO( sd, HB_TRUE );

      if( err )
         hb_socketSetOsError( err );
      else
         hb_socketSetError( rawerr );
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
         hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
         lSent = -1;
      }
   }
   if( lSent >= 0 )
   {
      int iError;

      flags = hb_socketTransFlags( flags );
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
         iError = lSent > 0 ? 0 : HB_SOCK_GETERROR();
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
         hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
         lSent = -1;
      }
   }
   if( lSent >= 0 )
   {
      int iError;

      flags = hb_socketTransFlags( flags );
      /* see note above about SIGPIPE */
#if defined( MSG_NOSIGNAL )
      flags |= MSG_NOSIGNAL;
#endif
      do
      {
         lSent = sendto( sd, ( const char * ) data, len, flags,
                         ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiSockLen );
         iError = lSent > 0 ? 0 : HB_SOCK_GETERROR();
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
         hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
         lReceived = -1;
      }
   }
   if( lReceived >= 0 )
   {
      int iError;

      flags = hb_socketTransFlags( flags );
      do
      {
         lReceived = recv( sd, ( char * ) data, len, flags );
         iError = lReceived > 0 ? 0 : HB_SOCK_GETERROR();
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

   if( pSockAddr && puiSockLen )
   {
      *pSockAddr = NULL;
      *puiSockLen = 0;
   }

   if( timeout >= 0 )
   {
      lReceived = hb_socketSelectRD( sd, timeout );
      if( lReceived == 0 )
      {
         hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
         lReceived = -1;
      }
   }
   if( lReceived >= 0 )
   {
      HB_SOCKADDR_STORAGE st;
      socklen_t salen = sizeof( st );
      int iError;

      flags = hb_socketTransFlags( flags );
      do
      {
         lReceived = recvfrom( sd, ( char * ) data, len, flags, &st.sa, &salen );
         iError = lReceived > 0 ? 0 : HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
      }
      while( lReceived == -1 && HB_SOCK_IS_EINTR( iError ) &&
             hb_vmRequestQuery() == 0 );

      if( lReceived != -1 && pSockAddr && puiSockLen )
      {
         *pSockAddr = memcpy( hb_xgrab( salen + 1 ), &st.sa, salen );
         *puiSockLen = ( unsigned ) salen;
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
      int flags;
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
   hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
   ret = -1;
#endif
   return ret;
}

int hb_socketSetNoDelay( HB_SOCKET sd, HB_BOOL fNoDelay )
{
   int ret;

#if defined( TCP_NODELAY )
   /*
    * Turn off the Nagle algorithm for the specified socket.
    * The Nagle algorithm says that we should delay sending
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
   hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
   ret = -1;
#endif
   return ret;
}

int hb_socketSetNoSigPipe( HB_SOCKET sd, HB_BOOL fNoSigPipe )
{
#if defined( SO_NOSIGPIPE )
   int val = fNoSigPipe ? 1 : 0, ret;
   ret = setsockopt( sd, SOL_SOCKET, SO_NOSIGPIPE, ( const char * ) &val, sizeof( val ) );
   hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
   return ret;
#else
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( fNoSigPipe );
   hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
   return -1;
#endif
}

/* NOTE: For notes on Windows, see:
         https://msdn.microsoft.com/library/ms740621
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
         hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
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
      /* SO_REUSEADDR in MS-Windows makes something completely different
       * then in other OS-es
       */
      HB_SYMBOL_UNUSED( sd );
      HB_SYMBOL_UNUSED( fReuse );
      hb_socketSetError( HB_SOCKET_ERR_NOSUPPORT );
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

#if defined( HB_HAS_INET_PTON )
      ret = inet_pton( AF_INET, szAddr, &mreq.imr_multiaddr ) > 0 ? 0 : -1;
#elif defined( HB_HAS_INET_ATON )
      ret = inet_aton( szAddr, &mreq.imr_multiaddr ) != 0 ? 0 : -1;
#else
      mreq.imr_multiaddr.s_addr = inet_addr( szAddr );
      ret = ( mreq.imr_multiaddr.s_addr != INADDR_NONE ||
              strcmp( "255.255.255.255", szAddr ) == 0 ) ? 0 : -1; /* dirty hack */
#endif
      mreq.imr_interface.s_addr = htonl( INADDR_ANY );

      if( ret == 0 )
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
      int err = inet_pton( AF_INET6, szAddr, &mreq.ipv6mr_multiaddr );
      if( err > 0 )
      {
         int ret;
         mreq.ipv6mr_interface = 0;
#if ! defined( IPV6_JOIN_GROUP ) && defined( IPV6_ADD_MEMBERSHIP )
#  define IPV6_JOIN_GROUP  IPV6_ADD_MEMBERSHIP
#endif
         ret = setsockopt( sd, IPPROTO_IPV6, IPV6_JOIN_GROUP, ( const char * ) &mreq, sizeof( mreq ) );
         hb_socketSetOsError( ret != -1 ? 0 : HB_SOCK_GETERROR() );
         return ret;
      }
      else if( err == 0 )
         hb_socketSetError( HB_SOCKET_ERR_WRONGADDR );
      else
         hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
      return -1;
#else
      int iTODO;
#endif
   }
#endif

   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
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
      {
         sd = hb_socketItemGet( pItem );
         if( sd == HB_NO_SOCKET )
            sd = ( HB_SOCKET ) ( HB_PTRUINT ) hb_itemGetPtr( pItem );
      }
   }
   return sd;
}

#if defined( HB_HAS_POLL )
static int s_socketPollCheck( HB_SOCKET sd, struct pollfd * pfds, nfds_t nfds )
{
   nfds_t npos;

   for( npos = 0; npos < nfds; ++npos )
   {
      if( pfds[ npos ].fd == sd )
         return ( int ) npos;
   }
   return -1;
}
#endif /* HB_HAS_POLL */

int hb_socketSelect( PHB_ITEM pArrayRD, HB_BOOL fSetRD,
                     PHB_ITEM pArrayWR, HB_BOOL fSetWR,
                     PHB_ITEM pArrayEX, HB_BOOL fSetEX,
                     HB_MAXINT timeout, HB_SOCKET_FUNC pFunc )
{
#if defined( HB_HAS_POLL )
   HB_SOCKET sd;
   HB_SIZE nLen, nPos, ul;
   int iResult, iError, tout, iPos, i;
   PHB_ITEM pItemSets[ 3 ];
   HB_BOOL pSet[ 3 ];
   int pEvents[ 3 ];
   struct pollfd * pfds = NULL;
   nfds_t nfds = 0, ncnt = 0;

   if( pFunc == NULL )
      pFunc = s_socketSelectCallback;

   pItemSets[ 0 ] = pArrayRD;
   pItemSets[ 1 ] = pArrayWR;
   pItemSets[ 2 ] = pArrayEX;
   pSet[ 0 ] = fSetRD;
   pSet[ 1 ] = fSetWR;
   pSet[ 2 ] = fSetEX;
   pEvents[ 0 ] = POLLIN;
   pEvents[ 1 ] = POLLOUT;
   pEvents[ 2 ] = POLLPRI;

   for( i = 0; i < 3; i++ )
   {
      if( pItemSets[ i ] )
         ncnt += ( nfds_t ) hb_arrayLen( pItemSets[ i ] );
   }

   if( ncnt > 0 )
      pfds = ( struct pollfd * ) hb_xgrab( ncnt * sizeof( struct pollfd ) );

   for( i = 0; i < 3; i++ )
   {
      nLen = pItemSets[ i ] ? hb_arrayLen( pItemSets[ i ] ) : 0;
      for( ul = 1; ul <= nLen && nfds < ncnt; ul++ )
      {
         sd = pFunc( hb_arrayGetItemPtr( pItemSets[ i ], ul ) );
         if( sd != HB_NO_SOCKET )
         {
            iPos = s_socketPollCheck( sd, pfds, nfds );
            if( iPos < 0 )
            {
               iPos = ( int ) nfds++;
               pfds[ iPos ].fd = sd;
               pfds[ iPos ].revents = pfds[ iPos ].events = 0;
            }
            pfds[ iPos ].events |= pEvents[ i ];
         }
      }
   }

   if( hb_vmRequestQuery() == 0 )
   {
      HB_MAXUINT timer = hb_timerInit( timeout );

      hb_vmUnlock();
      do
      {
         tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout;
         iResult = poll( pfds, nfds, tout );
         iError = iResult >= 0 ? 0 : HB_SOCK_GETERROR();
         hb_socketSetOsError( iError );
         if( iResult == -1 && HB_SOCK_IS_EINTR( iError ) )
            iResult = 0;
      }
      while( iResult == 0 && ( timeout = hb_timerTest( timeout, &timer ) ) != 0 &&
             hb_vmRequestQuery() == 0 );
      hb_vmLock();

      pEvents[ 0 ] |= POLLHUP | POLLPRI;
      pEvents[ 2 ] |= POLLHUP | POLLERR | POLLNVAL;
      for( i = 0; i < 3; i++ )
      {
         if( pItemSets[ i ] && pSet[ i ] )
         {
            nPos = 0;
            if( iResult > 0 )
            {
               nLen = hb_arrayLen( pItemSets[ i ] );
               for( ul = 1; ul <= nLen; ul++ )
               {
                  sd = pFunc( hb_arrayGetItemPtr( pItemSets[ i ], ul ) );
                  if( sd != HB_NO_SOCKET )
                  {
                     iPos = s_socketPollCheck( sd, pfds, nfds );
                     if( iPos >= 0 &&
                         ( pfds[ iPos ].revents & pEvents[ i ] ) != 0 )
                     {
                        if( ++nPos != ul )
                           hb_itemCopy( hb_arrayGetItemPtr( pItemSets[ i ], nPos ),
                                        hb_arrayGetItemPtr( pItemSets[ i ], ul ) );
                     }
                  }
               }
            }
            hb_arraySize( pItemSets[ i ], nPos );
         }
      }
   }
   else
   {
      hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
      iResult = -1;
   }

   if( pfds )
      hb_xfree( pfds );

   return iResult;
#else /* ! HB_HAS_POLL */
   HB_SOCKET maxsd, sd;
   int i, ret, iError;
   HB_SIZE nLen, nPos, ul;
   PHB_ITEM pItemSets[ 3 ];
   HB_BOOL pSet[ 3 ];
   fd_set fds[ 3 ], * pfds[ 3 ];
   struct timeval tv;
   HB_MAXUINT timer;

   if( pFunc == NULL )
      pFunc = s_socketSelectCallback;

   pItemSets[ 0 ] = pArrayRD;
   pItemSets[ 1 ] = pArrayWR;
   pItemSets[ 2 ] = pArrayEX;
   pSet[ 0 ] = fSetRD;
   pSet[ 1 ] = fSetWR;
   pSet[ 2 ] = fSetEX;

   timer = hb_timerInit( timeout );

   do
   {
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

      if( hb_vmRequestQuery() != 0 )
      {
         hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
         pSet[ 0 ] = pSet[ 1 ] = pSet[ 2 ] = HB_FALSE;
         ret = -1;
         break;
      }

      if( timeout < 0 || timeout >= 1000 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }

      hb_vmUnlock();

      ret = select( ( int ) ( maxsd + 1 ), pfds[ 0 ], pfds[ 1 ], pfds[ 2 ], &tv );
      iError = ret >= 0 ? 0 : HB_SOCK_GETERROR();
      hb_socketSetOsError( iError );
      if( ret == -1 && HB_SOCK_IS_EINTR( iError ) )
         ret = 0;

      hb_vmLock();
   }
   while( ret == 0 && ( timeout = hb_timerTest( timeout, &timer ) ) != 0 &&
          hb_vmRequestQuery() == 0 );

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
#endif /* ! HB_HAS_POLL */
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
      int iError;

      hb_vmUnlock();
      memset( &hints, 0, sizeof( hints ) );
      hints.ai_family = AF_INET;
      iError = getaddrinfo( szAddr, NULL, &hints, &res );
      hb_socketSetResolveError( iError );
      if( iError == 0 )
      {
         if( ( int ) res->ai_addrlen >= ( int ) sizeof( struct sockaddr_in ) &&
             hb_socketGetAddrFamily( res->ai_addr, ( unsigned ) res->ai_addrlen ) == AF_INET )
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
      hb_socketSetResolveError( he == NULL ? HB_SOCK_GETHERROR() : 0 );
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
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
#endif
   *pSockAddr = NULL;
   *puiLen = 0;
   return HB_FALSE;
}

char * hb_socketResolveAddr( const char * szAddr, int af )
{
   char * szResult = NULL;
   HB_BOOL fTrans = HB_FALSE;
   int iError = 0;

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
         else
            iError = HB_SOCK_GETHERROR();
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
      iError = getaddrinfo( szAddr, NULL, &hints, &res );
      if( iError == 0 )
      {
         szResult = hb_socketAddrGetName( res->ai_addr, ( unsigned ) res->ai_addrlen );
         freeaddrinfo( res );
      }
      hb_vmLock();
#endif
   }
   hb_socketSetResolveError( iError );

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
   hb_socketSetResolveError( iResult );
   hb_vmLock();

   if( iResult == 0 )
   {
      int iCount = 0;
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
            char * szResult = hb_socketAddrGetName( res->ai_addr, ( unsigned ) res->ai_addrlen );
            if( szResult )
            {
               int i;
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
#else /* ! HB_HAS_ADDRINFO */

   if( af == HB_SOCKET_AF_INET )
   {
      struct hostent * he = NULL;
      int iCount = 0;

      hb_vmUnlock();

      /* gethostbyname() in Windows and OS/2 does not accept direct IP
       * addresses
       */
#if ( defined( HB_OS_WIN ) || defined( HB_OS_OS2 ) ) && \
    defined( HB_HAS_GETHOSTBYADDR )
      {
         struct in_addr sia;

#if defined( HB_HAS_INET_PTON )
         if( inet_pton( AF_INET, szAddr, &sia ) > 0 )
#elif defined( HB_HAS_INET_ATON )
         if( inet_aton( szAddr, &sia ) != 0 )
#else
         sia.s_addr = inet_addr( szAddr );
         if( sia.s_addr != INADDR_NONE ||
             strcmp( "255.255.255.255", szAddr ) == 0 )  /* dirty hack */
#endif
         {
            he = gethostbyaddr( ( const char * ) &sia, sizeof( sia ), AF_INET );
         }
      }
#endif
      if( he == NULL )
         he = gethostbyname( szAddr );

      hb_socketSetResolveError( he == NULL ? HB_SOCK_GETHERROR() : 0 );

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
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
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
      hb_socketSetResolveError( iResult );
      hb_vmLock();
      if( iResult == 0 )
         szResult = hb_strdup( szHost );
#elif defined( HB_HAS_ADDRINFO )
      char * szAddr = hb_socketAddrGetName( pSockAddr, len );
      if( szAddr )
      {
         struct addrinfo hints, * res = NULL;
         int iError;

         hb_vmUnlock();
         memset( &hints, 0, sizeof( hints ) );
         hints.ai_family = af;
         hints.ai_flags = AI_CANONNAME;
         iError = getaddrinfo( szAddr, NULL, &hints, &res );
         hb_socketSetResolveError( iError );
         if( iError == 0 )
         {
            if( res->ai_canonname )
               szResult = hb_strdup( res->ai_canonname );
            freeaddrinfo( res );
         }
         hb_vmLock();
      }
#else /* ! HB_HAS_ADDRINFO */
      struct hostent * he = NULL;

      if( af == AF_INET )
      {
#if defined( HB_HAS_GETHOSTBYADDR )
         const struct sockaddr_in * sa = ( const struct sockaddr_in * ) pSockAddr;
         hb_vmUnlock();
         he = gethostbyaddr( ( const char * ) &sa->sin_addr, sizeof( sa->sin_addr ), af );
         hb_socketSetResolveError( he == NULL ? HB_SOCK_GETHERROR() : 0 );
         hb_vmLock();
#else
         char * szAddr = hb_socketAddrGetName( pSockAddr, len );
         if( szAddr )
         {
            hb_vmUnlock();
            he = gethostbyname( szAddr );
            hb_socketSetResolveError( he == NULL ? HB_SOCK_GETHERROR() : 0 );
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
         hb_socketSetResolveError( he == NULL ? HB_SOCK_GETHERROR() : 0 );
         hb_vmLock();
      }
#endif
      if( he && he->h_name )
         szResult = hb_strdup( he->h_name );
#endif /* ! HB_HAS_ADDRINFO */
   }
   return szResult;
}


/*
 * IFACEs
 */
#if defined( HB_OS_WIN ) || ( defined( SIOCGIFCONF ) && \
    !( defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) ) )
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
#if defined( HB_OS_WIN ) && ! defined( SIOCGIFCONF )
static HB_SIZE hb_socketArrayFindInetAddr( const char * szAddr,
                                           PHB_ITEM pArray, HB_SIZE nPos )
{
   HB_SIZE nLen = hb_arrayLen( pArray );

   for( ; nPos <= nLen; ++nPos )
   {
      PHB_ITEM pIfItem = hb_arrayGetItemPtr( pArray, nPos );

      if( strcmp( hb_arrayGetCPtr( pIfItem, HB_SOCKET_IFINFO_ADDR ), szAddr ) == 0 )
         return nPos;
   }
   return 0;
}
#endif
#if defined( SIOCGIFCONF ) && defined( HB_OS_BSD ) && \
    ! defined( SIOCGIFHWADDR ) && ! defined( SIOCGENADDR )
static char * hb_getMAC( const char * pszIfName )
{
   struct ifaddrs * ifap = NULL;
   char * pszMAC = NULL;

   if( getifaddrs( &ifap ) == 0 && ifap )
   {
      struct ifaddrs * ifa = ifap;

      while( ifa != NULL )
      {
         if( ifa->ifa_addr != NULL && ifa->ifa_addr->sa_family == AF_LINK &&
             ifa->ifa_name && strcmp( ifa->ifa_name, pszIfName ) == 0 )
         {
            struct sockaddr_dl * sdl = ( struct sockaddr_dl * ) ifa->ifa_addr;
            unsigned char * data = ( unsigned char * ) LLADDR( sdl );
            char hwaddr[ 24 ];

            hb_snprintf( hwaddr, sizeof( hwaddr ),
                         "%02X:%02X:%02X:%02X:%02X:%02X",
                         data[ 0 ], data[ 1 ], data[ 2 ],
                         data[ 3 ], data[ 4 ], data[ 5 ] );
            pszMAC = hb_strdup( hwaddr );
            break;
         }
         ifa = ifa->ifa_next;
      }
      freeifaddrs( ifap );
   }
   return pszMAC;
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
    !( defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) )
   HB_SOCKET sd;

   sd = hb_socketOpen( af ? af : HB_SOCKET_AF_INET, HB_SOCKET_PT_DGRAM, 0 );
   if( sd != HB_NO_SOCKET )
   {
      struct ifconf ifc;
      struct ifreq * pifr;
      char * buf, * ptr;
      const char * pLastName = NULL;
      int len = 0, size, iLastName = 0, iLastFamily = 0, flags, family;

#  if defined( HB_OS_DOS )
#     undef ioctl
#     define ioctl( s, cmd, argp )  ioctlsocket( ( s ), ( cmd ), ( char * ) ( argp ) )
#  endif

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
      ifc.ifc_buf = ( caddr_t ) buf;

      /* Warning: On some platforms this code can effectively work only with
       *          IP4 interfaces and IP6 will need different implementation.
       */

      if( ioctl( sd, SIOCGIFCONF, &ifc ) != -1 )
      {
         for( ptr = ( char * ) ifc.ifc_buf, size = ifc.ifc_len; size > 0; )
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
                  data = ( unsigned char * ) &pifr->ifr_hwaddr.sa_data[ 0 ];
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
                  data = ( unsigned char * ) &pifr->ifr_enaddr[ 0 ];
                  hb_snprintf( hwaddr, sizeof( hwaddr ),
                               "%02X:%02X:%02X:%02X:%02X:%02X",
                               data[ 0 ], data[ 1 ], data[ 2 ],
                               data[ 3 ], data[ 4 ], data[ 5 ] );
                  hb_arraySetC( pItem, HB_SOCKET_IFINFO_HWADDR, hwaddr );
               }
#  elif defined( HB_OS_BSD )
               {
                  char * hwaddr = hb_getMAC( pifr->ifr_name );
                  if( hwaddr != NULL )
                     hb_arraySetCPtr( pItem, HB_SOCKET_IFINFO_HWADDR, hwaddr );
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

         if( pArray && hb_arrayLen( pArray ) > 0 )
         {
            PIP_ADAPTER_INFO pAdapterInfo;
            ULONG ulBufLen = sizeof( IP_ADAPTER_INFO );
            DWORD dwResult;

            pAdapterInfo = ( PIP_ADAPTER_INFO ) hb_xgrab( ulBufLen );
            dwResult = GetAdaptersInfo( pAdapterInfo, &ulBufLen );
            if( dwResult == ERROR_BUFFER_OVERFLOW )
            {
               hb_xfree( pAdapterInfo );
               pAdapterInfo = ( PIP_ADAPTER_INFO ) hb_xgrab( ulBufLen );
               dwResult = GetAdaptersInfo( pAdapterInfo, &ulBufLen );
            }
            if( dwResult == NO_ERROR )
            {
               PIP_ADAPTER_INFO pAdapter = pAdapterInfo;

               do
               {
                  PIP_ADDR_STRING pIpAddress = &pAdapter->IpAddressList;

                  do
                  {
                     HB_SIZE nPos = 0;

                     while( ( nPos = hb_socketArrayFindInetAddr( pIpAddress->IpAddress.String,
                                                                 pArray, nPos + 1 ) ) != 0 )
                     {
                        PHB_ITEM pIfItem = hb_arrayGetItemPtr( pArray, nPos );
                        if( ! hb_arrayGetCPtr( pIfItem, HB_SOCKET_IFINFO_HWADDR )[ 0 ] )
                        {
                           char hwaddr[ 3 * MAX_ADAPTER_ADDRESS_LENGTH ];
                           UINT count, size = 0;

                           for( count = 0; count < pAdapter->AddressLength; ++count )
                           {
                              if( count )
                                 hwaddr[ size++ ] = ':';
                              size += hb_snprintf( hwaddr + size, sizeof( hwaddr ) - size,
                                                   "%02X", ( int ) pAdapter->Address[ count ] );
                           }
                           hb_arraySetCL( pIfItem, HB_SOCKET_IFINFO_HWADDR, hwaddr, size );
                        }
                     }
                     pIpAddress = pIpAddress->Next;
                  }
                  while( pIpAddress );

                  pAdapter = pAdapter->Next;
               }
               while( pAdapter );
            }
         }
      }
      else
         iError = HB_SOCK_GETERROR();

      hb_xfree( pBuffer );
      hb_socketClose( sd );
   }
#else
   int iTODO;
   HB_SYMBOL_UNUSED( af );
   HB_SYMBOL_UNUSED( fNoAliases );
   hb_socketSetError( HB_SOCKET_ERR_AFNOSUPPORT );
#endif

   if( pItem )
      hb_itemRelease( pItem );

   if( iError != 0 )
      hb_socketSetOsError( iError );

   return pArray;
}
#endif /* ! HB_SOCKET_OFF */
