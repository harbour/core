/*
 * $Id$
 */

// hbwhat
// AJ Wos

// Note: functions: SELECT and ACCEPT may collide with Clipper/xHarbour names


#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#define _WINSOCKAPI_  // Prevents inclusion of Winsock.h in Windows.h

#include "hbwhat.h"

#if !(defined(__POCC__) && __POCC__ < 500)
   #include <winsock2.h>
#endif
#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>
#include <time.h>

#if defined(__POCC__) && __POCC__ < 500
   #include <winsock2.h>
#endif

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

//-----------------------------------------------------------------------------
//  SOCKET  accept( IN SOCKET s, OUT struct sockaddr * addr, IN OUT int * addrlen );

// syntax: accept(s [,@cAddr][,@nAddrLen]) -> s

HB_FUNC( VWN_ACCEPT )
{
   char *addr  ;
   int addrlen ;

   if ( ISNIL(2) )
      hb_retnl( (LONG) accept( (SOCKET) hb_parnl(1), NULL, NULL ) );
   else
   {
      addr = hb_parcx(2);
      addrlen = ISNIL(3) ? hb_parni(3) : ( int ) hb_parclen(2);
      hb_retnl( (LONG) accept( (SOCKET) hb_parnl(1), ( struct sockaddr *) addr, &addrlen ) );
      hb_storclen( addr, addrlen, 2 );
      hb_storni( addrlen, 3);
   }
}


//-----------------------------------------------------------------------------
//  int  bind( IN SOCKET s, IN const struct sockaddr * name, IN int namelen );

// syntax: bind(s, sa:value, sa:sizeof ) -> nInt

HB_FUNC( VWN_BIND )
{
   char *name = (char *) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING )->item.asString.value ;

   hb_retni(( int ) bind( (SOCKET) hb_parnl(1), ( struct sockaddr *) name, hb_parni( 3 ) ) );
}


//-----------------------------------------------------------------------------
//  int  closesocket( IN SOCKET s );

HB_FUNC( VWN_CLOSESOCKET )
{
   hb_retni( closesocket( (SOCKET) hb_parnl(1) ) );
}


//-----------------------------------------------------------------------------
//  int  connect( IN SOCKET s, IN const struct sockaddr * name, IN int namelen );

// syntax connect( s, san:value, san:sizeof ) -> nInt

HB_FUNC( VWN_CONNECT )
{
   char *name = (char *) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING )->item.asString.value ;

   hb_retni( (int ) connect((SOCKET) hb_parnl( 1 ), ( struct sockaddr *) name, hb_parni( 3 ) ) );
}


//-----------------------------------------------------------------------------
//  int  ioctlsocket( IN SOCKET s, IN long cmd, IN OUT u_long * argp );

//syntax: ioctlsocket( s, nCmd, @nArg) -> nErr

HB_FUNC( VWN_IOCTLSOCKET )
{
   ULONG arg = hb_parnl( 3 );

   hb_retni( (int ) ioctlsocket((SOCKET) hb_parnl( 1 ), hb_parnl( 2 ), &arg ) );
   hb_stornl( arg, 3 );
}


//-----------------------------------------------------------------------------
//  int  getpeername( IN SOCKET s, OUT struct sockaddr * name, IN OUT int * namelen );

// syntax: getpeername( s, @san, @nLen ) -> int

HB_FUNC( VWN_GETPEERNAME )
{
   char *name  = (char *) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING )->item.asString.value ;
   int addrlen = ISNIL(3) ? hb_parni(3) : ( int ) hb_parclen(2);

   hb_retni( (int ) getpeername((SOCKET) hb_parnl( 1 ), ( struct sockaddr *) name, &addrlen ) );
   hb_storclen( name, addrlen, 2 );
   hb_storni( addrlen, 3);
}


//-----------------------------------------------------------------------------
//  int  getsockname( IN SOCKET s, OUT struct sockaddr * name, IN OUT int * namelen );

// syntax: getstockname( s, @san, @nLen )

HB_FUNC( VWN_GETSOCKNAME )
{
   char *name  = (char *) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING )->item.asString.value ;
   int addrlen = ISNIL(3) ? hb_parni(3) : ( int ) hb_parclen(2);

   hb_retni( (int ) getsockname((SOCKET) hb_parnl( 1 ), ( struct sockaddr *) name, &addrlen ) );
   hb_storclen( name, addrlen, 2 );
   hb_storni( addrlen, 3);
}


//-----------------------------------------------------------------------------
//  int  getsockopt( IN SOCKET s, IN int level, IN int optname, OUT char * optval, IN OUT int * optlen );

// syntax: getsockopt( s, nLevel, nOptName, @cOptVal, @nOptName) -> nErr

HB_FUNC( VWN_GETSOCKOPT )
{
   char *optval = (char *) hb_parc( 4 ); //hb_param( 4, HB_IT_STRING )->item.asString.value ;
   int  optlen  = hb_parni( 5 );

   hb_retni( (int ) getsockopt( hb_parnl( 1 ) ,
                                hb_parni( 2 ),
                                hb_parni( 3 ),
                                optval      ,
                                &optlen
                              ) );

   hb_storclen( optval, optlen, 4 );
   hb_storni( optlen, 5 );
}


//-----------------------------------------------------------------------------
//  u_long  htonl( IN u_long hostlong );

HB_FUNC( VWN_HTONL )
{
   hb_retnl( (ULONG) htonl( hb_parnl( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  u_short  htons( IN u_short hostshort );

HB_FUNC( VWN_HTONS )
{
   hb_retni( (USHORT) htons( (USHORT) hb_parni( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  unsigned long  inet_addr( IN const char * cp );

HB_FUNC( VWN_INET_ADDR )
{
   hb_retnl( (ULONG) inet_addr( hb_parcx( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  char *  inet_ntoa( IN struct in_addr in );

HB_FUNC( VWN_INET_NTOA )
{
   struct in_addr *in = (struct in_addr *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value ;

   hb_retc( inet_ntoa( *in ) );
}


//-----------------------------------------------------------------------------
//  int  listen( IN SOCKET s, IN int backlog );

HB_FUNC( VWN_LISTEN )
{
 hb_retni( (int) listen((SOCKET) hb_parnl( 1 ), hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
//  u_long  ntohl( IN u_long netlong );

HB_FUNC( VWN_NTOHL )
{
   hb_retnl( (ULONG) ntohl( hb_parnl(1 ) ) );
}


//-----------------------------------------------------------------------------
//  u_short  ntohs( IN u_short netshort );

HB_FUNC( VWN_NTOHS )
{
   hb_retni( ( USHORT ) ntohs( (USHORT) hb_parni(1) ) );
}


//-----------------------------------------------------------------------------
//  int  recv( IN SOCKET s, OUT char * buf, IN int len, IN int flags );

// syntax: recv( s, @cBuff, [nLen] , nFlags)-> nRecv or nErr

HB_FUNC( VWN_RECV )
{
   int  iBuffLen = (ISNIL(3) ? (ISNIL(2) ? 0 : ( int ) hb_parclen(2) ) : hb_parni(3));
   char   *buf  = ( char *) hb_xgrab(iBuffLen);
   int iRet;

   iRet = recv((SOCKET) hb_parnl( 1 ), buf, iBuffLen, hb_parni( 4 ) );
   if ( iRet && ISBYREF( 2 ) )
      hb_storclen( buf, iRet, 2 );

   hb_retni( iRet );
   hb_xfree( buf );
}


//-----------------------------------------------------------------------------
//  int  recvfrom( IN SOCKET s, OUT char * buf, IN int len, IN int flags, OUT ( struct sockaddr *) from, IN OUT int * fromlen );

// syntax: recvfrom( s, @cBuff, nLen, nFlags [, @cSockAddr] [, @nSockAddrLen] )-> nRecv or nErr

HB_FUNC( VWN_RECVFROM )
{
   int  iBuffLen = (ISNIL(3) ? (ISNIL(2) ? 0 : ( int ) hb_parclen(2) ) : hb_parni(3));
   char *buf     = ( char *) hb_xgrab(iBuffLen);
   char *from    = (ISNIL(5) ? NULL : (char *) hb_parc( 5 )); //hb_param( 5, HB_IT_STRING )->item.asString.value );
   int  iAddrLen = (ISNIL(6) ? (ISNIL(5) ? 0 : ( int ) hb_parclen(5) ) : hb_parni(6));
   int  iRet;

   iRet = ( int ) recvfrom( (SOCKET) hb_parnl( 1 )  ,
                              buf         ,
                              iBuffLen,
                              hb_parni( 4 ),
                              ( struct sockaddr *)from  ,
                              &iAddrLen
                              );

   if ( iRet && ISBYREF( 2 ) )
      hb_storclen( buf, iRet, 2 );

   if ( iAddrLen && ISBYREF(5) )
      hb_storclen(from, iAddrLen, 5 );

   hb_retni( iRet );
   hb_xfree( buf );

}


//-----------------------------------------------------------------------------
//  int  select( IN int nfds, IN OUT fd_set * readfds, IN OUT fd_set * writefds, IN OUT fd_set *exceptfds, IN const struct timeval * timeout );

// syntax: select

HB_FUNC( VWN_SOCKSELECT )
{
   fd_set   *readfds   = NULL;
   fd_set   *writefds  = NULL;
   fd_set   *exceptfds = NULL;
   struct timeval  *timeout = NULL;

   if ( ISCHAR( 2 ) )
      readfds = (fd_set *) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING )->item.asString.value ;

   if ( ISCHAR( 3 ) )
      writefds = (fd_set *) hb_parc( 3 ); //hb_param( 3, HB_IT_STRING )->item.asString.value ;

   if ( ISCHAR( 4 ) )
      exceptfds = (fd_set *) hb_parc( 4 ); //hb_param( 4, HB_IT_STRING )->item.asString.value ;

   if ( ISCHAR( 5 ) )
      timeout = (struct timeval *) hb_parc( 4 ); //hb_param( 4, HB_IT_STRING )->item.asString.value ;

   hb_retni(( int ) select( hb_parni( 1 ),
                            readfds      ,
                            writefds     ,
                            exceptfds    ,
                            timeout
                           ) );

    if ( ISCHAR( 2 ) && ISBYREF( 2 ) )
       hb_storclen( ( char *) readfds, sizeof(fd_set), 2 );
    if ( ISCHAR( 3 ) && ISBYREF( 3 ) )
       hb_storclen( ( char *) writefds, sizeof(fd_set), 3 );
    if ( ISCHAR( 4 ) && ISBYREF( 4 ) )
       hb_storclen( ( char *) exceptfds, sizeof(fd_set), 4 );

 }


//-----------------------------------------------------------------------------
//  int  send( IN SOCKET s, IN const char * buf, IN int len, IN int flags );

HB_FUNC( VWN_SEND )
{
   int  iBuffLen = (ISNIL(3) ? (ISNIL(2) ? 0 : ( int ) hb_parclen(2) ) : hb_parni(3));
   hb_retni( ( int ) send((SOCKET) hb_parnl( 1 ), hb_parcx(2), iBuffLen, hb_parni( 4 ) ) );
}


//-----------------------------------------------------------------------------
//  int  sendto( IN SOCKET s, IN const char * buf, IN int len, IN int flags, IN const struct sockaddr * to, IN int tolen );

// syntax: sendto( s, cBuff, [nBuffLen], nFlags, [sockaddr:value], [sockaddr:sizeof])-> nSent or nErr

HB_FUNC( VWN_SENDTO )
{

   int  iBuffLen = (ISNIL(3) ? ( ISNIL(2) ? 0 : ( int ) hb_parclen(2) ) : hb_parni(3));
   struct sockaddr *to = NULL;
   int iToLen = 0 ;

   if ( ISCHAR( 5 ) )
   {
     to = (struct sockaddr *) hb_parc( 5 ); //hb_param( 5, HB_IT_STRING )->item.asString.value ;
     iToLen = (ISNIL(6) ? (ISNIL(5) ? 0 : ( int ) hb_parclen(5) ) : hb_parni(6));
   }

   hb_retni( (int ) sendto( (SOCKET) hb_parnl( 1 )       ,
                                            hb_parcx( 2 ) ,
                                            iBuffLen     ,
                                            hb_parni( 4 ),
                                            to           ,
                                            iToLen
                                          ) );
}


//-----------------------------------------------------------------------------
//  int  setsockopt( IN SOCKET s, IN int level, IN int optname, IN const char * optval, IN int optlen );

HB_FUNC( VWN_SETSOCKOPT )
{
//   SOCKET s       ;
   INT    optval  = hb_parni(5);

   hb_retni( (int ) setsockopt( (SOCKET) hb_parnl( 1 ) ,
                                 hb_parni( 2 ),
                                 hb_parni( 3 ),
                                 (const char *) &optval      ,
                                 sizeof( optval) //hb_parni( 5 )
                               ) );
}


//-----------------------------------------------------------------------------
//  int  shutdown( IN SOCKET s, IN int how );

HB_FUNC( VWN_SHUTDOWN )
{
   hb_retni( (int ) shutdown((SOCKET) hb_parnl( 1 ), hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
//  SOCKET  socket( IN int af, IN int type, IN int protocol );

HB_FUNC( VWN_SOCKET )
{
   hb_retnl( ( ULONG ) socket( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}


//-----------------------------------------------------------------------------
//  struct hostent *  gethostbyaddr( IN const char * addr, IN int len, IN int type );

HB_FUNC( VWN_GETHOSTBYADDR )
{
   HOSTENT *he ;
   he = gethostbyaddr( hb_parcx( 1 ) ,
                       hb_parni( 2 ),
                       hb_parni( 3 )
                     );

   hb_retclen( ( char *)he, sizeof(HOSTENT) );
}


//-----------------------------------------------------------------------------
//  struct hostent *  gethostbyname( IN const char * name );

HB_FUNC( VWN_GETHOSTBYNAME )
{
   HOSTENT *he ;

   he = gethostbyname( hb_parcx( 1 ) );

   hb_retclen( ( char *)he, sizeof(HOSTENT) );

}


//-----------------------------------------------------------------------------
//  int  gethostname( OUT char * name, IN int namelen );

// syntax: gethostbyname( @cBuff ) -> nErr

HB_FUNC( VWN_GETHOSTNAME )
{
   char *name  = ( char*) hb_parcx( 1 )  ;
   int iLen    =  hb_parclen( 1 );

   hb_retni( (int ) gethostname( name, iLen ) )  ;
   hb_storc( name, 1 );
}


//-----------------------------------------------------------------------------
//  struct servent *  getservbyport( IN int port, IN const char * proto );

HB_FUNC( VWN_GETSERVBYPORT )
{
   hb_retclen( ( char * ) getservbyport( hb_parni( 1 ),hb_parcx( 2 ) ), sizeof(SERVENT) );
}


//-----------------------------------------------------------------------------
//  struct servent *  getservbyname( IN const char * name, IN const char * proto );

HB_FUNC( VWN_GETSERVBYNAME )
{
   hb_retclen( ( char *) getservbyname( hb_parcx( 1 ), hb_parcx( 2 ) ), sizeof(SERVENT) );
}


//-----------------------------------------------------------------------------
//  struct protoent *  getprotobynumber( IN int number );

HB_FUNC( VWN_GETPROTOBYNUMBER )
{
   hb_retclen( ( char * ) getprotobynumber( hb_parni( 1 ) ), sizeof(PROTOENT) );
}


//-----------------------------------------------------------------------------
//  struct protoent *  getprotobyname( IN const char * name );

HB_FUNC( VWN_GETPROTOBYNAME )
{
   hb_retclen( ( char * ) getprotobyname( hb_parcx( 1 ) ), sizeof(PROTOENT) );
}


//-----------------------------------------------------------------------------
//  int  WSAStartup( IN WORD wVersionRequested, OUT LPWSADATA lpWSAData );

HB_FUNC( VWN_WSASTARTUP )
{
   WSADATA WSAData  ;

   hb_retni( (int ) WSAStartup( ( WORD ) hb_parni( 1 ), &WSAData ) );

   if ( ISBYREF( 2 ) )
     hb_storclen( ( char * ) &WSAData, sizeof( WSADATA ), 2 );
}


//-----------------------------------------------------------------------------
//  int  WSACleanup( void );


HB_FUNC( VWN_WSACLEANUP )
{
   hb_retni( (int ) WSACleanup( ) );
}


//-----------------------------------------------------------------------------
//  void  WSASetLastError( IN int iError );

HB_FUNC( VWN_WSASETLASTERROR )
{
   WSASetLastError( hb_parni( 1 ) );
}


//-----------------------------------------------------------------------------
//  int  WSAGetLastError( void );

HB_FUNC( VWN_WSAGETLASTERROR )
{
   hb_retni( (int ) WSAGetLastError( ) );
}


//-----------------------------------------------------------------------------
//  BOOL  WSAIsBlocking( void );

HB_FUNC( VWN_WSAISBLOCKING )
{
   hb_retl( WSAIsBlocking( ) );
}


//-----------------------------------------------------------------------------
//  int  WSAUnhookBlockingHook( void );

HB_FUNC( VWN_WSAUNHOOKBLOCKINGHOOK )
{
   hb_retni( (int ) WSAUnhookBlockingHook() );
}


//-----------------------------------------------------------------------------
//  FARPROC  WSASetBlockingHook( IN FARPROC lpBlockFunc );

// OBSOLETE !!!

/*

HB_FUNC( VWN_WSASETBLOCKINGHOOK )
{
   FARPROC lpBlockFunc ;

   // Your code goes here

// ( FARPROC ) WSASetBlockingHook( lpBlockFunc ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSACancelBlockingCall( void );

HB_FUNC( VWN_WSACANCELBLOCKINGCALL )
{
   hb_retni( (int ) WSACancelBlockingCall() );
}


//-----------------------------------------------------------------------------
//  HANDLE  WSAAsyncGetServByName( IN HWND hWnd, IN u_int wMsg, IN const char * name,
//                                 IN const char * proto, OUT char * buf, IN int buflen );



// syntax : se IS SERVENT
//          if (h:=WSAAsyncGetServByName( hWnd, wMsg, name, [proto], @c )) <> 0
//             se:Buffer(c)
//          endif

HB_FUNC( VWN_WSAASYNCGETSERVBYNAME )
{
   char * buf = ( char *) hb_xgrab( MAXGETHOSTSTRUCT );
   HANDLE hRet ;

   if( ( hRet = WSAAsyncGetServByName( (HWND)         HB_PARWH( 1 ),
                                     (unsigned int) hb_parni( 2 ),
                                     ( char *)      hb_parcx( 3 ) ,
                                     ISNIL( 4 ) ? NULL : ( char *) hb_parcx( 4 ) ,
                                     ( char *)      buf          ,
                                     ( int)         MAXGETHOSTSTRUCT ) ) != 0 )

      hb_storclen( buf, sizeof(SERVENT), 5 );

   HB_RETWH( hRet );
   hb_xfree( buf );
}


//-----------------------------------------------------------------------------
//  HANDLE  WSAAsyncGetServByPort( IN HWND hWnd, IN u_int wMsg, IN int port,
//                                 IN const char * proto, OUT char * buf, IN int buflen );

// syntax : se IS SERVENT
//          if (h:=WSAAsyncGetServByPort( hWnd, wMsg, port, [proto], @c )) <> 0
//             se:Buffer(c)
//          endif

HB_FUNC( VWN_WSAASYNCGETSERVBYPORT )
{
   char * buf = (char *) hb_xgrab( MAXGETHOSTSTRUCT );
   HANDLE hRet ;

   if( ( hRet = WSAAsyncGetServByPort( (HWND) HB_PARWH( 1 )        ,
                                      (unsigned int) hb_parni( 2 ),
                                       hb_parni( 3 )              ,
                                       ISNIL( 4 ) ? NULL : ( char *) hb_parcx( 4 ) ,
                                       ( char *)      buf         ,
                                       ( int)         MAXGETHOSTSTRUCT  ) ) != 0 )

        hb_storclen( buf, sizeof(SERVENT), 5 );

   HB_RETWH( hRet );
   hb_xfree( buf );
}


//-----------------------------------------------------------------------------
//  HANDLE  WSAAsyncGetProtoByName( IN HWND hWnd, IN u_int wMsg, IN const char * name,
//                                  OUT char * buf, IN int buflen );

// syntax : se IS PROTOENT
//          if (h:=WSAAsyncGetProtByName( hWnd, wMsg, name, @c )) <> 0
//             se:Buffer(c)
//          endif


HB_FUNC( VWN_WSAASYNCGETPROTOBYNAME )
{
   char * buf = ( char * ) hb_xgrab( MAXGETHOSTSTRUCT );
   HANDLE hRet ;

   if( ( hRet = WSAAsyncGetProtoByName( (HWND)         HB_PARWH( 1 ),
                                       (unsigned int) hb_parni( 2 ),
                                       ( char *)      hb_parcx( 3 ) ,
                                       ( char *)      buf          ,
                                       ( int)         MAXGETHOSTSTRUCT  ) ) != 0 )

        hb_storclen( buf, sizeof(PROTOENT), 4);

   HB_RETWH( hRet );
   hb_xfree( buf );
}


//-----------------------------------------------------------------------------
//  HANDLE  WSAAsyncGetProtoByNumber( IN HWND hWnd, IN u_int wMsg, IN int number,
//                                    OUT char * buf, IN int buflen );

// syntax : se IS PROTOENT
//          if (h:=WSAAsyncGetProtByNumber( hWnd, wMsg, number, @c )) <> 0
//             se:Buffer(c)
//          endif


HB_FUNC( VWN_WSAASYNCGETPROTOBYNUMBER )
{
   char * buf = ( char *) hb_xgrab( MAXGETHOSTSTRUCT );
   HANDLE hRet ;

   if( ( hRet = WSAAsyncGetProtoByNumber( (HWND)         HB_PARWH( 1 ),
                                         (unsigned int) hb_parni( 2 ),
                                         (int)          hb_parni( 3 ),
                                         ( char *)      buf          ,
                                         ( int)         MAXGETHOSTSTRUCT  ) ) != 0 )

        hb_storclen( buf, sizeof(PROTOENT), 4);

   HB_RETWH( hRet );
   hb_xfree( buf );
}

//-----------------------------------------------------------------------------
//  HANDLE  WSAAsyncGetHostByName( IN HWND hWnd, IN u_int wMsg, IN const char * name,
//                                 OUT char * buf, IN int buflen );

// syntax : se IS HOSTENT
//          if (h:=WSAAsyncGetHostByName( hWnd, wMsg, name, @c )) <> 0
//             se:Buffer(c)
//          endif

HB_FUNC( VWN_WSAASYNCGETHOSTBYNAME )
{
   char * buf = ( char *) hb_xgrab( MAXGETHOSTSTRUCT );
   HANDLE hRet ;

   if( ( hRet = WSAAsyncGetHostByName( (HWND) HB_PARWH( 1 ),
                                       (unsigned int) hb_parni( 2 ),
                                       ( char *)      hb_parcx( 3 ) ,
                                       ( char *)      buf          ,
                                       ( int)         MAXGETHOSTSTRUCT ) ) != 0 )

      hb_storclen( buf, sizeof(HOSTENT), 4);

   HB_RETWH( hRet );
   hb_xfree( buf );
}


//-----------------------------------------------------------------------------
//  HANDLE  WSAAsyncGetHostByAddr( IN HWND hWnd, IN u_int wMsg, IN const char * addr,
//                                 IN int len, IN int type, OUT char * buf, IN int buflen );

// syntax : se IS HOSTENT
//          if (h:=WSAAsyncGetHostByName( hWnd, wMsg, cAddr, type, @c )) <> 0
//             se:Buffer(c)
//          endif


HB_FUNC( VWN_WSAASYNCGETHOSTBYADDR )
{

   char * buf = ( char *) hb_xgrab( MAXGETHOSTSTRUCT );
   HANDLE hRet ;

   if( ( hRet = WSAAsyncGetHostByAddr( (HWND)         HB_PARWH( 1 ) ,
                                      (unsigned int) hb_parni( 2 ) ,
                                      ( char *)      hb_parcx( 3 )  ,
                                      ( int )        hb_parclen( 3),
                                      ( int )        hb_parni( 4 ) ,
                                      ( char *)      buf           ,
                                      ( int)         MAXGETHOSTSTRUCT  ) ) != 0 )

      hb_storclen( buf, sizeof(HOSTENT), 5);

   HB_RETWH( hRet );
   hb_xfree( buf );


}


//-----------------------------------------------------------------------------
//  int  WSACancelAsyncRequest( IN HANDLE hAsyncTaskHandle );

HB_FUNC( VWN_WSACANCELASYNCREQUEST )
{
   hb_retni( (int) WSACancelAsyncRequest( (HANDLE) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  int  WSAAsyncSelect( IN SOCKET s, IN HWND hWnd, IN u_int wMsg, IN long lEvent );

HB_FUNC( VWN_WSAASYNCSELECT )
{
   hb_retni( (int ) WSAAsyncSelect( (SOCKET) hb_parnl( 1 ) ,
                                    (HWND) HB_PARWH( 2 )   ,
                                    (UINT) hb_parni( 3 )   ,
                                     hb_parnl( 4 )
                                  ) );
}

//-----------------------------------------------------------------------------
// int CALLBACK ConditionFunc(  IN LPWSABUF lpCallerId,  IN LPWSABUF lpCallerData,
//                              IN OUT LPQOS lpSQOS,  IN OUT LPQOS lpGQOS,
//                              IN LPWSABUF lpCalleeId, OUT LPWSABUF lpCalleeData,
//                              OUT GROUP * g, IN DWORD dwCallbackData );


// dwCallbackData should contain the Harbour function pointer or NULL/0


int _stdcall _WSACondFunc( LPWSABUF lpCallerId,  LPWSABUF lpCallerData, LPQOS lpSQOS,
                 LPQOS lpGQOS, LPWSABUF lpCalleeId, LPWSABUF lpCalleeData,
                 GROUP * g, DWORD_PTR dwCallbackData )
{

   int res = CF_ACCEPT ;

   if ( dwCallbackData != 0 )
   {
      hb_vmPushSymbol( (HB_SYMB *) dwCallbackData ); // Harbour function pointer
      hb_vmPushNil();

      hb_vmPushNumInt( ( HB_PTRDIFF ) lpCallerId );
      hb_vmPushNumInt( ( HB_PTRDIFF ) lpCallerId );
      hb_vmPushNumInt( ( HB_PTRDIFF ) lpCallerData );
      hb_vmPushNumInt( ( HB_PTRDIFF ) lpSQOS );
      hb_vmPushNumInt( ( HB_PTRDIFF ) lpGQOS );
      hb_vmPushNumInt( ( HB_PTRDIFF ) lpCalleeId );
      hb_vmPushNumInt( ( HB_PTRDIFF ) lpCalleeData );
      hb_vmPushNumInt( ( HB_PTRDIFF ) g );

      hb_vmDo(7);
      res = hb_itemGetNI( (PHB_ITEM) hb_param( -1, HB_IT_ANY ) );

   }
   return res;
}


//-----------------------------------------------------------------------------
//  SOCKET  WSAAccept( IN SOCKET s, OUT struct sockaddr * addr, IN OUT LPINT addrlen,
//                     IN LPCONDITIONPROC lpfnCondition, IN DWORD_PTR dwCallbackData );


// syntax: if (skt := WSAAccept( s, [@addr], pHrbFunc )) <> INVALID_SOCKET
//

// Callback function pointer pHrbFunc should be obtained using HB_FuncPtr(),
//      HB_ObjMsgPtr(), or @MyFunc().

// If you require to pass extra data to the callback function
// you must use the extra parameter, when establishing the Harbour
// function pointer using HB_FuncPtr or HB_ObjMsgPtr.


HB_FUNC( VWN_WSAACCEPT )
{
//   SOCKET          s              ;
   struct sockaddr addr           ;
   INT           addrlen  = ISBYREF( 2 ) ? 0 : sizeof(addr)  ;
   SOCKET sRet ;

   sRet = WSAAccept( (SOCKET) hb_parnl( 1 )   ,
                     &addr                    ,
                     &addrlen                 ,
                     _WSACondFunc              ,
                     ISNIL( 3 ) ? 0 : (DWORD_PTR) hb_parnl( 3 ) );

   if( ( sRet != INVALID_SOCKET ) && ISBYREF( 2 ) )
        hb_storclen( ( char * ) &addr, addrlen, 2 );

    hb_retnl( ( ULONG ) sRet );

}



//-----------------------------------------------------------------------------
//  BOOL  WSACloseEvent( IN WSAEVENT hEvent );

HB_FUNC( VWN_WSACLOSEEVENT )
{
   hb_retl( ( BOOL ) WSACloseEvent( (WSAEVENT) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  int  WSAConnect( IN SOCKET s, IN const struct sockaddr * name, IN int namelen,
//                   IN LPWSABUF lpCallerData, OUT LPWSABUF lpCalleeData, IN LPQOS lpSQOS, IN LPQOS lpGQOS );

// syntax: WSAConnect( s, cSockAddr, [cCallerData], [@cCalleeData]

/*
HB_FUNC( VWN_WSACONNECT )
{
   sockaddr struct *name  ( sockaddr struct *) hb_param( 2, HB_IT_STRING )->item.asString.value ;
   WSABUF          *CallerData ;
   WSABUF          *CalleeData ;
   LPQOS           lpSQOS       ;
   LPQOS           lpGQOS       ;
   int             iRet         ;


   if( !ISNIL( 3 ) )
   {
     CallerData.len =
     CallerData.buf =

   }

    iRet = WSAConnect( (SOCKET) hb_parnl( 1 )            ,
                        name           ,
                        hb_parclen( 2 ),
                        ISNIL( 3 ) ? NULL : CallerData ,
                        lpCalleeData ,
                        lpSQOS       ,
                        lpGQOS
                       )  ;


<<<<<<< _winsock.c



   hb_retni( (int ) iRet );

=======
// hb_retni( (int ) WSAConnect( (SOCKET) hb_parnl( 1 )            ,
                                                &name        ,
                                                hb_parni( 3 ),
                                                lpCallerData ,
                                                lpCalleeData ,
                                                lpSQOS       ,
                                                lpGQOS
                                              ) );
>>>>>>> 1.4
}
*/


//-----------------------------------------------------------------------------
//  WSAEVENT  WSACreateEvent( void );

HB_FUNC( VWN_WSACREATEEVENT )
{
   HB_RETWH( WSACreateEvent( ) );
}


//-----------------------------------------------------------------------------
//  int  WSADuplicateSocketA( IN SOCKET s, IN DWORD dwProcessId,
//                            OUT LPWSAPROTOCOL_INFOA lpProtocolInfo );

/*

HB_FUNC( VWN_WSADUPLICATESOCKET )
{
   SOCKET              s              ;
   LPWSAPROTOCOL_INFOA lpProtocolInfo ;

   // Your code goes here

// hb_retni( (int ) WSADuplicateSocket( (SOCKET) hb_parnl( 1 )               ,
                                                        (DWORD) hb_parnl( 2 ),
                                                        lpProtocolInfo
                                                      ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSAEnumNetworkEvents( IN SOCKET s, IN WSAEVENT hEventObject, OUT LPWSANETWORKEVENTS lpNetworkEvents );

/*

HB_FUNC( VWN_WSAENUMNETWORKEVENTS )
{
   SOCKET             s               ;
   WSAEVENT           hEventObject    ;
   LPWSANETWORKEVENTS lpNetworkEvents ;

   // Your code goes here

// hb_retni( (int ) WSAEnumNetworkEvents( (SOCKET) hb_parnl( 1 )s              ,
                                                          hEventObject   ,
                                                          lpNetworkEvents
                                                        ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSAEnumProtocolsA( IN LPINT lpiProtocols, OUT LPWSAPROTOCOL_INFOA lpProtocolBuffer, IN OUT LPDWORD lpdwBufferLength );

/*

HB_FUNC( VWN_WSAENUMPROTOCOLS )
{
   LPINT               lpiProtocols     ;
   LPWSAPROTOCOL_INFOA lpProtocolBuffer ;
   LPDWORD             lpdwBufferLength ;

   // Your code goes here

// hb_retni( (int ) WSAEnumProtocols( lpiProtocols    ,
                                      lpProtocolBuffer,
                                      lpdwBufferLength
                                     ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSAEventSelect( IN SOCKET s, IN WSAEVENT hEventObject, IN long lNetworkEvents );

HB_FUNC( VWN_WSAEVENTSELECT )
{
   hb_retni( (int ) WSAEventSelect( (SOCKET)   hb_parnl( 1 ),
                                    (WSAEVENT) HB_PARWH( 2 ),
                                    (long)     hb_parnl( 3 )
                                   ) );
}


//-----------------------------------------------------------------------------
//  BOOL  WSAGetOverlappedResult( IN SOCKET s, IN LPWSAOVERLAPPED lpOverlapped, OUT LPDWORD lpcbTransfer, IN BOOL fWait, OUT LPDWORD lpdwFlags );

/*

HB_FUNC( VWN_WSAGETOVERLAPPEDRESULT )
{
   SOCKET          s            ;
   LPWSAOVERLAPPED lpOverlapped ;
   LPDWORD         lpcbTransfer ;
   LPDWORD         lpdwFlags    ;

   // Your code goes here

// hb_retl( WSAGetOverlappedResult( (SOCKET) hb_parnl( 1 )           ,
                                    lpOverlapped,
                                    lpcbTransfer,
                                    hb_parl( 4 ),
                                    lpdwFlags
                                   ) );
}

*/


//-----------------------------------------------------------------------------
//  BOOL  WSAGetQOSByName( IN SOCKET s, IN LPWSABUF lpQOSName, OUT LPQOS lpQOS );

/*

HB_FUNC( VWN_WSAGETQOSBYNAME )
{
   SOCKET   s         ;
   LPWSABUF lpQOSName ;
   LPQOS    lpQOS     ;

   // Your code goes here

// hb_retl( WSAGetQOSByName((SOCKET) hb_parnl( 1 ), lpQOSName, lpQOS ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSAHtonl( IN SOCKET s, IN u_long hostlong, OUT u_long * lpnetlong );

/*

HB_FUNC( VWN_WSAHTONL )
{
   SOCKET s         ;
   u_long hostlong  ;
   u_long lpnetlong ;

   // Your code goes here

// hb_retni( (int ) WSAHtonl((SOCKET) hb_parnl( 1 ), hostlong, &lpnetlong ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSAHtons( IN SOCKET s, IN u_short hostshort, OUT u_short * lpnetshort );

/*

HB_FUNC( VWN_WSAHTONS )
{
   SOCKET  s          ;
   u_short hostshort  ;
   u_short lpnetshort ;

   // Your code goes here

// hb_retni( (int ) WSAHtons((SOCKET) hb_parnl( 1 ), hostshort, &lpnetshort ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSAIoctl( IN SOCKET s, IN DWORD dwIoControlCode, IN LPVOID lpvInBuffer, IN DWORD cbInBuffer, OUT LPVOID lpvOutBuffer, IN DWORD cbOutBuffer, OUT LPDWORD lpcbBytesReturned, IN LPWSAOVERLAPPED lpOverlapped, IN LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine );

/*

HB_FUNC( VWN_WSAIOCTL )
{
   SOCKET                             s                   ;
   LPVOID                             lpvInBuffer         ;
   LPVOID                             lpvOutBuffer        ;
   LPDWORD                            lpcbBytesReturned   ;
   LPWSAOVERLAPPED                    lpOverlapped        ;
   LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine ;

   // Your code goes here

// hb_retni( (int ) WSAIoctl( (SOCKET) hb_parnl( 1 )s                    ,
                                              (DWORD) hb_parnl( 2 ),
                                              lpvInBuffer          ,
                                              (DWORD) hb_parnl( 4 ),
                                              lpvOutBuffer         ,
                                              (DWORD) hb_parnl( 6 ),
                                              lpcbBytesReturned    ,
                                              lpOverlapped         ,
                                              lpCompletionRoutine
                                            ) );
}

*/


//-----------------------------------------------------------------------------
//  SOCKET  WSAJoinLeaf( IN SOCKET s, IN const struct sockaddr * name, IN int namelen, IN LPWSABUF lpCallerData, OUT LPWSABUF lpCalleeData, IN LPQOS lpSQOS, IN LPQOS lpGQOS, IN DWORD dwFlags );

/*

HB_FUNC( VWN_WSAJOINLEAF )
{
   SOCKET          s            ;
   sockaddr struct name         ;
   LPWSABUF        lpCallerData ;
   LPWSABUF        lpCalleeData ;
   LPQOS           lpSQOS       ;
   LPQOS           lpGQOS       ;

   // Your code goes here

// hb_retnl( WSAJoinLeaf( (SOCKET) hb_parnl( 1 )s                    ,
                                                    &name                ,
                                                    hb_parni( 3 )        ,
                                                    lpCallerData         ,
                                                    lpCalleeData         ,
                                                    lpSQOS               ,
                                                    lpGQOS               ,
                                                    (DWORD) hb_parnl( 8 )
                                                  ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSANtohl( IN SOCKET s, IN u_long netlong, OUT u_long * lphostlong );

/*

HB_FUNC( VWN_WSANTOHL )
{
   SOCKET s          ;
   u_long netlong    ;
   u_long lphostlong ;

   // Your code goes here

   hb_retni( (int ) WSANtohl((SOCKET) hb_parnl( 1 ), netlong, &lphostlong ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSANtohs( IN SOCKET s, IN u_short netshort, OUT u_short * lphostshort );

/*

HB_FUNC( VWN_WSANTOHS )
{
   SOCKET  s           ;
   u_short netshort    ;
   u_short lphostshort ;

   // Your code goes here

   hb_retni( (int ) WSANtohs((SOCKET) hb_parnl( 1 ), netshort, &lphostshort ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSARecv( IN SOCKET s, IN OUT LPWSABUF lpBuffers, IN DWORD dwBufferCount, OUT LPDWORD lpNumberOfBytesRecvd, IN OUT LPDWORD lpFlags, IN LPWSAOVERLAPPED lpOverlapped, IN LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine );

/*

HB_FUNC( VWN_WSARECV )
{
   SOCKET                             s                    ;
   LPWSABUF                           lpBuffers            ;
   LPDWORD                            lpNumberOfBytesRecvd ;
   LPDWORD                            lpFlags              ;
   LPWSAOVERLAPPED                    lpOverlapped         ;
   LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine  ;

   // Your code goes here

   hb_retni( (int ) WSARecv( (SOCKET) hb_parnl( 1 )               ,
                                             lpBuffers            ,
                                             (DWORD) hb_parnl( 3 ),
                                             lpNumberOfBytesRecvd ,
                                             lpFlags              ,
                                             lpOverlapped         ,
                                             lpCompletionRoutine
                                           ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSARecvDisconnect( IN SOCKET s, OUT LPWSABUF lpInboundDisconnectData );

/*

HB_FUNC( VWN_WSARECVDISCONNECT )
{
    LPWSABUF lpInboundDisconnectData ;


// hb_retni( (int ) WSARecvDisconnect( (SOCKET) hb_parnl( 1 )                  ,
                                                       lpInboundDisconnectData
                                                     ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSARecvFrom( IN SOCKET s, IN OUT LPWSABUF lpBuffers, IN DWORD dwBufferCount, OUT LPDWORD lpNumberOfBytesRecvd, IN OUT LPDWORD lpFlags, OUT struct sockaddr * lpFrom, IN OUT LPINT lpFromlen, IN LPWSAOVERLAPPED lpOverlapped, IN LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine );

/*

HB_FUNC( VWN_WSARECVFROM )
{
   LPWSABUF                           lpBuffers            ;
   LPDWORD                            lpNumberOfBytesRecvd ;
   LPDWORD                            lpFlags              ;
   sockaddr struct                    lpFrom               ;
   LPINT                              lpFromlen            ;
   LPWSAOVERLAPPED                    lpOverlapped         ;
   LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine  ;


// hb_retni( (int ) WSARecvFrom( (SOCKET) hb_parnl( 1 )               ,
                                                 lpBuffers            ,
                                                 (DWORD) hb_parnl( 3 ),
                                                 lpNumberOfBytesRecvd ,
                                                 lpFlags              ,
                                                 &lpFrom              ,
                                                 lpFromlen            ,
                                                 lpOverlapped         ,
                                                 lpCompletionRoutine
                                               ) );
}

*/


//-----------------------------------------------------------------------------
//  BOOL  WSAResetEvent( IN WSAEVENT hEvent );

HB_FUNC( VWN_WSARESETEVENT )
{
   hb_retl( WSAResetEvent( (WSAEVENT) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  int  WSASend( IN SOCKET s, IN LPWSABUF lpBuffers, IN DWORD dwBufferCount, OUT LPDWORD lpNumberOfBytesSent, IN DWORD dwFlags, IN LPWSAOVERLAPPED lpOverlapped, IN LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine );

/*

HB_FUNC( VWN_WSASEND )
{
   SOCKET                             s                   ;
   LPWSABUF                           lpBuffers           ;
   LPDWORD                            lpNumberOfBytesSent ;
   LPWSAOVERLAPPED                    lpOverlapped        ;
   LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine ;


   hb_retni( (int ) WSASend( (SOCKET) hb_parnl( 1 )               ,
                                             lpBuffers            ,
                                             (DWORD) hb_parnl( 3 ),
                                             lpNumberOfBytesSent  ,
                                             (DWORD) hb_parnl( 5 ),
                                             lpOverlapped         ,
                                             lpCompletionRoutine
                                           ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSASendDisconnect( IN SOCKET s, IN LPWSABUF lpOutboundDisconnectData );

/*

HB_FUNC( VWN_WSASENDDISCONNECT )
{
   SOCKET   s                        ;
   LPWSABUF lpOutboundDisconnectData ;

   // Your code goes here

// hb_retni( (int ) WSASendDisconnect( (SOCKET) hb_parnl( 1 )s                       ,
                                                       lpOutboundDisconnectData
                                                     ) );
}

*/


//-----------------------------------------------------------------------------
//  int  WSASendTo( IN SOCKET s, IN LPWSABUF lpBuffers, IN DWORD dwBufferCount, OUT LPDWORD lpNumberOfBytesSent, IN DWORD dwFlags, IN const struct sockaddr * lpTo, IN int iTolen, IN LPWSAOVERLAPPED lpOverlapped, IN LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine );

/*

HB_FUNC( VWN_WSASENDTO )
{
   SOCKET                             s                   ;
   LPWSABUF                           lpBuffers           ;
   LPDWORD                            lpNumberOfBytesSent ;
   sockaddr struct                    lpTo                ;
   LPWSAOVERLAPPED                    lpOverlapped        ;
   LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine ;

   // Your code goes here

// hb_retni( (int ) WSASendTo( (SOCKET) hb_parnl( 1 )s                    ,
                                               lpBuffers            ,
                                               (DWORD) hb_parnl( 3 ),
                                               lpNumberOfBytesSent  ,
                                               (DWORD) hb_parnl( 5 ),
                                               &lpTo                ,
                                               hb_parni( 7 )        ,
                                               lpOverlapped         ,
                                               lpCompletionRoutine
                                             ) );
}

*/


//-----------------------------------------------------------------------------
//  BOOL  WSASetEvent( IN WSAEVENT hEvent );

HB_FUNC( VWN_WSASETEVENT )
{
   hb_retl( WSASetEvent( (WSAEVENT) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  SOCKET  WSASocketA( IN int af, IN int type, IN int protocol, IN LPWSAPROTOCOL_INFOA lpProtocolInfo, IN GROUP g, IN DWORD dwFlags );

/*

HB_FUNC( VWN_WSASOCKET )
{
   LPWSAPROTOCOL_INFOA lpProtocolInfo ;
   GROUP               g              ;

   // Your code goes here

   hb_retnl( ( ULONG ) WSASocket( hb_parni( 1 )        ,
                         hb_parni( 2 )        ,
                                                  hb_parni( 3 )        ,
                                                  lpProtocolInfo       ,
                                                  g                    ,
                                                  (DWORD) hb_parnl( 6 )
                                                ) );
}

*/


//-----------------------------------------------------------------------------
//  DWORD  WSAWaitForMultipleEvents( IN DWORD cEvents, IN const WSAEVENT * lphEvents, IN BOOL fWaitAll, IN DWORD dwTimeout, IN BOOL fAlertable );

/*

HB_FUNC( VWN_WSAWAITFORMULTIPLEEVENTS )
{
   WSAEVENT lphEvents  ;

   // Your code goes here

   hb_retnl( ( DWORD ) WSAWaitForMultipleEvents( (DWORD) hb_parnl( 1 ),
                                                                &lphEvents           ,
                                                                hb_parl( 3 )         ,
                                                                (DWORD) hb_parnl( 4 ),
                                                                hb_parl( 5 )
                                                              ) );
}

*/


//-----------------------------------------------------------------------------
//  INT  WSAAddressToStringA( IN LPSOCKADDR lpsaAddress, IN DWORD dwAddressLength, IN LPWSAPROTOCOL_INFOA lpProtocolInfo, IN OUT LPSTR lpszAddressString, IN OUT LPDWORD lpdwAddressStringLength );

/*

HB_FUNC( VWN_WSAADDRESSTOSTRING )
{
   LPSOCKADDR          lpsaAddress             ;
   LPWSAPROTOCOL_INFOA lpProtocolInfo          ;
   LPDWORD             lpdwAddressStringLength ;

   // Your code goes here

// hb_retni( (int ) WSAAddressToString( lpsaAddress            ,
                                                        (DWORD) hb_parnl( 2 )  ,
                                                        lpProtocolInfo         ,
                                                        (LPSTR) hb_parcx( 4 )   ,
                                                        lpdwAddressStringLength
                                                      ) );
}

*/



//-----------------------------------------------------------------------------
//  INT  WSAStringToAddressA( IN LPSTR AddressString, IN INT AddressFamily, IN LPWSAPROTOCOL_INFOA lpProtocolInfo, OUT LPSOCKADDR lpAddress, IN OUT LPINT lpAddressLength );

/*

HB_FUNC( VWN_WSASTRINGTOADDRESS )
{
   LPWSAPROTOCOL_INFOA lpProtocolInfo  ;
   LPSOCKADDR          lpAddress       ;
   LPINT               lpAddressLength ;

   // Your code goes here

// hb_retni( (int ) WSAStringToAddress( (LPSTR) hb_parcx( 1 ),
                                                        hb_parni( 2 )       ,
                                                        lpProtocolInfo      ,
                                                        lpAddress           ,
                                                        lpAddressLength
                                                      ) );
}

*/



//-----------------------------------------------------------------------------
//  INT  WSALookupServiceBeginA( IN LPWSAQUERYSETA lpqsRestrictions, IN DWORD dwControlFlags, OUT LPHANDLE lphLookup );

/*

HB_FUNC( VWN_WSALOOKUPSERVICEBEGIN )
{
   LPWSAQUERYSETA lpqsRestrictions ;
   LPHANDLE       lphLookup        ;

   // Your code goes here

// hb_retni( (int ) WSALookupServiceBegin( lpqsRestrictions     ,
                                                           (DWORD) hb_parnl( 2 ),
                                                           lphLookup
                                                         ) );
}

*/



//-----------------------------------------------------------------------------
//  INT  WSALookupServiceNextA( IN HANDLE hLookup, IN DWORD dwControlFlags, IN OUT LPDWORD lpdwBufferLength, OUT LPWSAQUERYSETA lpqsResults );

/*

HB_FUNC( VWN_WSALOOKUPSERVICENEXT )
{
   LPDWORD        lpdwBufferLength ;
   LPWSAQUERYSETA lpqsResults      ;

   // Your code goes here

// hb_retni( (int ) WSALookupServiceNext( (HANDLE) HB_PARWH( 1 ),
                                                          (DWORD) hb_parnl( 2 ) ,
                                                          lpdwBufferLength      ,
                                                          lpqsResults
                                                        ) );
}

*/



//-----------------------------------------------------------------------------
//  INT  WSALookupServiceEnd( IN HANDLE hLookup );

HB_FUNC( VWN_WSALOOKUPSERVICEEND )
{
   hb_retni( (int ) WSALookupServiceEnd( (HANDLE) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
//  INT  WSAInstallServiceClassA( IN LPWSASERVICECLASSINFOA lpServiceClassInfo );

/*

HB_FUNC( VWN_WSAINSTALLSERVICECLASS )
{
   LPWSASERVICECLASSINFOA lpServiceClassInfo ;

   // Your code goes here

// hb_retni( (int ) WSAInstallServiceClass( lpServiceClassInfo ) );
}

*/



//-----------------------------------------------------------------------------
//  INT  WSARemoveServiceClass( IN LPGUID lpServiceClassId );

/*

HB_FUNC( VWN_WSAREMOVESERVICECLASS )
{
   LPGUID lpServiceClassId ;

   // Your code goes here

// hb_retni( (int ) WSARemoveServiceClass( lpServiceClassId ) );
}

*/


//-----------------------------------------------------------------------------
//  INT  WSAGetServiceClassInfoA( IN LPGUID lpProviderId, IN LPGUID lpServiceClassId, IN OUT LPDWORD lpdwBufSize, OUT LPWSASERVICECLASSINFOA lpServiceClassInfo );

/*

HB_FUNC( VWN_WSAGETSERVICECLASSINFO )
{
   LPGUID                 lpProviderId       ;
   LPGUID                 lpServiceClassId   ;
   LPDWORD                lpdwBufSize        ;
   LPWSASERVICECLASSINFOA lpServiceClassInfo ;

   // Your code goes here

// hb_retni( (int ) WSAGetServiceClassInfo( lpProviderId      ,
                                                            lpServiceClassId  ,
                                                            lpdwBufSize       ,
                                                            lpServiceClassInfo
                                                          ) );
}

*/


//-----------------------------------------------------------------------------
//  INT  WSAEnumNameSpaceProvidersA( IN OUT LPDWORD lpdwBufferLength, OUT LPWSANAMESPACE_INFOA lpnspBuffer );

/*

HB_FUNC( VWN_WSAENUMNAMESPACEPROVIDERS )
{
   LPDWORD              lpdwBufferLength ;
   LPWSANAMESPACE_INFOA lpnspBuffer      ;

   // Your code goes here

// hb_retni( (int ) WSAEnumNameSpaceProviders( lpdwBufferLength,
                                                               lpnspBuffer
                                                             ) );
}

*/


//-----------------------------------------------------------------------------
//  INT  WSAGetServiceClassNameByClassIdA( IN LPGUID lpServiceClassId, OUT LPSTR lpszServiceClassName, IN OUT LPDWORD lpdwBufferLength );

/*

HB_FUNC( VWN_WSAGETSERVICECLASSNAMEBYCLASSID )
{
   LPGUID  lpServiceClassId     ;
   LPDWORD lpdwBufferLength     ;

   // Your code goes here

// hb_retni( (int ) WSAGetServiceClassNameByClassId( lpServiceClassId    ,
                                                                     (LPSTR) hb_parcx( 2 ),
                                                                     lpdwBufferLength
                                                                   ) );
}

*/


//-----------------------------------------------------------------------------
//  INT  WSASetServiceA( IN LPWSAQUERYSETA lpqsRegInfo, IN WSAESETSERVICEOP essoperation, IN DWORD dwControlFlags );

/*

HB_FUNC( VWN_WSASETSERVICE )
{
   LPWSAQUERYSETA   lpqsRegInfo    ;
   WSAESETSERVICEOP essoperation   ;

   // Your code goes here

// hb_retni( (int ) WSASetService( lpqsRegInfo          ,
                                                   essoperation         ,
                                                   (DWORD) hb_parnl( 3 )
                                                 ) );
}

*/


//-----------------------------------------------------------------------------
//  INT  WSAProviderConfigChange( IN OUT LPHANDLE lpNotificationHandle, IN LPWSAOVERLAPPED lpOverlapped, IN LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine );

/*

HB_FUNC( VWN_WSAPROVIDERCONFIGCHANGE )
{
   LPHANDLE                           lpNotificationHandle ;
   LPWSAOVERLAPPED                    lpOverlapped         ;
   LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine  ;

   // Your code goes here

// hb_retni( (int ) WSAProviderConfigChange( lpNotificationHandle,
                                                             lpOverlapped        ,
                                                             lpCompletionRoutine
                                                           ) );
}

*/

//-----------------------------------------------------------------------------
// End.
