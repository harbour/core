/*
 * $Id$
 */

/*

  Function naming:
    The intention of this library is to be as close as possible to the original
    socket implementation. This supposed to be valid for function names also,
    but some of the names are very platform dependent, ex., WSA*() functions.
    select() function name is reserved for standard Harbour's function, so,
    socket_*() prefix was used:
      socket_init()    - WSAStartup()
      socket_exit()    - WSACleanup()
      socket_error()   - WSALastError()
      socket_select()  - select()
    Finally I renamed all functions to have socket_*() prefix to be more "prefix
    compatible" and not to occupy a general function names like send(), bind(),
    accept(), listen(), etc.:
      socket_create()       - socket()
      socket_close()        - closesocket()
      socket_shutdown()     - shutdown()
      socket_bind()         - bind()
      socket_listen()       - listen()
      socket_accept()       - accept()
      socket_send()         - send()
      socket_recv()         - recv()
      socket_recv()         - recv()
      socket_getsockname()  - getsockname()
      socket_getpeername()  - getpeername()


  Types mapping:
    SOCKET
      UINT_PTR in Windows, let's map it to pointer type, and INVALID_SOCKET value to NIL

    struct sockaddr
      It is not only IP addresses, also can be IPX, etc. All network-host byte order
      conversion should be hidden from Harbour API. So, let's map to:
        { adress_familly, ... }
          AF_INET: { AF_INET, cAddr, nPort }
          other:   { AF_?, cAddressDump }
*/

#include "hbsocket.h"
#include "hbapiitm.h"

static int hb_parnidef( int iParam, int iValue )
{
   return HB_ISNUM( iParam ) ? hb_parni( iParam ) : iValue;
}

static HB_SOCKET hb_parsocket( int iParam )
{
   return HB_ISPOINTER( iParam ) ? ( HB_SOCKET ) ( HB_PTRDIFF )
                                   hb_parptr( iParam ) : HB_NO_SOCKET;
}

static void hb_retsocket( HB_SOCKET hSocket )
{
   if( hSocket != HB_NO_SOCKET )
      hb_retptr( ( void * ) ( HB_PTRDIFF ) hSocket );
}

static HB_SOCKET hb_itemGetSocket( PHB_ITEM pSocket )
{
   if( pSocket && HB_IS_POINTER( pSocket ) )
      return ( HB_SOCKET ) ( HB_PTRDIFF ) hb_itemGetPtr( pSocket );
   else
      return HB_NO_SOCKET;
}

HB_FUNC( SOCKET_INIT )
{
   hb_retni( hb_socketInit() );
}

HB_FUNC( SOCKET_EXIT )
{
   hb_socketCleanup();
}

HB_FUNC( SOCKET_ERROR )
{
   hb_retni( hb_socketGetError() );
}

HB_FUNC( SOCKET_CREATE )
{
   hb_retsocket( hb_socketOpen( hb_parnidef( 1, HB_SOCKET_PF_INET ),
                                hb_parnidef( 2, HB_SOCKET_PT_STREAM ),
                                hb_parnidef( 3, HB_SOCKET_IPPROTO_TCP ) ) );
}

HB_FUNC( SOCKET_CLOSE )
{
   hb_retni( hb_socketClose( hb_parsocket( 1 ) ) );
}

HB_FUNC( SOCKET_BIND )
{
   void * sa;
   unsigned len;

   if( hb_socketAddrFromItem( &sa, &len, hb_param( 2, HB_IT_ANY ) ) )
   {
      hb_retni( hb_socketBind( hb_parsocket( 1 ), sa, len ) );
      hb_xfree( sa );
   }
}

HB_FUNC( SOCKET_LISTEN )
{
   hb_retni( hb_socketListen( hb_parsocket( 1 ), hb_parnidef( 2, 10 ) ) );
}

HB_FUNC( SOCKET_ACCEPT )
{
   if( HB_ISBYREF( 2 ) )
   {
      void * sa;
      unsigned len;
      PHB_ITEM pItem;

      hb_retsocket( hb_socketAccept( hb_parsocket( 1 ), &sa, &len,
                                     HB_ISNUM( 3 ) ? hb_parnint( 3 ) : -1 ) );
      pItem = hb_socketAddrToItem( sa, len );
      if( pItem )
      {
         hb_itemParamStoreForward( 2, pItem );
         hb_itemRelease( pItem );
      }
      else
         hb_stor( 2 );

      if( sa )
         hb_xfree( sa );
   }
   else
      hb_retsocket( hb_socketAccept( hb_parsocket( 1 ), NULL, 0,
                                     HB_ISNUM( 3 ) ? hb_parnint( 3 ) : -1 ) );
}

HB_FUNC( SOCKET_SHUTDOWN )
{
   hb_retni( hb_socketShutdown( hb_parsocket( 1 ),
                                hb_parnidef( 2, HB_SOCKET_SHUT_RDWR ) ) );
}

HB_FUNC( SOCKET_RECV )
{
   char *   pBuf;
   long     len;

   len = hb_parni( 3 );
   if( len <= 0 )
      len = 4096;
   pBuf = ( char * ) hb_xgrab( len + 1 );
   len = hb_socketRecv( hb_parsocket( 1 ), pBuf, len, hb_parni( 4 ),
                        HB_ISNUM( 5 ) ? hb_parnint( 5 ) : -1 );
   hb_retni( len );
   hb_storclen( pBuf, len > 0 ? len : 0, 2 );
   hb_xfree( pBuf );
}

HB_FUNC( SOCKET_SEND )
{
   hb_retni( hb_socketSend( hb_parsocket( 1 ), hb_parc( 2 ), hb_parclen( 2 ),
                            hb_parni( 4 ), HB_ISNUM( 5 ) ? hb_parnint( 5 ) : -1 ) );
}

HB_FUNC( SOCKET_GETSOCKNAME )
{
   void * sa;
   unsigned len;
   int iRet;

   iRet = hb_socketGetSockName( hb_parsocket( 1 ), &sa, &len );
   hb_retni( iRet );
   if( HB_ISBYREF( 2 ) )
   {
      PHB_ITEM pItem = hb_socketAddrToItem( sa, len );
      if( pItem )
      {
         hb_itemParamStoreForward( 2, pItem );
         hb_itemRelease( pItem );
      }
      else
         hb_stor( 2 );
   }
   if( sa )
      hb_xfree( sa );
}

HB_FUNC( SOCKET_GETPEERNAME )
{
   void * sa;
   unsigned len;
   int iRet;

   iRet = hb_socketGetPeerName( hb_parsocket( 1 ), &sa, &len );
   hb_retni( iRet );
   if( HB_ISBYREF( 2 ) )
   {
      PHB_ITEM pItem = hb_socketAddrToItem( sa, len );
      if( pItem )
      {
         hb_itemParamStoreForward( 2, pItem );
         hb_itemRelease( pItem );
      }
      else
         hb_stor( 2 );
   }
   if( sa )
      hb_xfree( sa );
}

HB_FUNC( SOCKET_CONNECT )
{
   void * sa;
   unsigned len;

   if( hb_socketAddrFromItem( &sa, &len, hb_param( 2, HB_IT_ANY ) ) )
   {
      hb_retni( hb_socketConnect( hb_parsocket( 1 ), sa, len,
                                  HB_ISNUM( 3 ) ? hb_parnint( 3 ) : -1 ) );
      hb_xfree( sa );
   }
}

HB_FUNC( SOCKET_SELECT )
{
   hb_retni( hb_socketSelect( hb_param( 1, HB_IT_ARRAY ), HB_ISBYREF( 1 ),
                              hb_param( 2, HB_IT_ARRAY ), HB_ISBYREF( 2 ),
                              hb_param( 3, HB_IT_ARRAY ), HB_ISBYREF( 3 ),
                              HB_ISNUM( 4 ) ? hb_parnint( 4 ) : -1,
                              hb_itemGetSocket ) );
}
