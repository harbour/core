/*
 * $Id$
 */

#include "hbapi.h"
#include "hbapiitm.h"

#if defined( HB_OS_WIN )
   #define _WINSOCKAPI_  /* Prevents inclusion of winsock.h in windows.h */
   #define HB_SOCKET_T SOCKET
   #include <winsock2.h>
   #include <windows.h>

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

#ifdef hb_parnidef
#undef hb_parnidef
#endif


static int hb_parnidef( int iParam, int iValue )
{
   return ISNUM( iParam ) ? hb_parni( iParam ) : iValue;
}


static SOCKET hb_parsocket( int iParam )
{
   return ISPOINTER( iParam ) ? ( SOCKET ) hb_parptr( 1 ) : INVALID_SOCKET;
}


static void hb_retsocket( SOCKET hSocket )
{
   if( hSocket == INVALID_SOCKET )
      hb_ret();
   else
      hb_retptr( ( void* ) hSocket );
}


static SOCKET hb_itemGetSocket( PHB_ITEM pItem )
{
   return HB_IS_POINTER( pItem ) ? ( SOCKET ) hb_itemGetPtr( pItem ) : INVALID_SOCKET;
}


static PHB_ITEM hb_itemPutSocket( PHB_ITEM pItem, SOCKET hSocket )
{
   if( ! pItem )
      pItem = hb_itemNew( NULL );

   if( hSocket == INVALID_SOCKET )
      hb_itemClear( pItem );
   else
      hb_itemPutPtr( pItem, ( void* ) hSocket );

   return pItem;
}


static void hb_itemGetSockaddr( PHB_ITEM pItem, struct sockaddr* sa )
{
   memset( sa, 0, sizeof( struct sockaddr ) );

   if( HB_IS_ARRAY( pItem ) )
   {
      sa->sa_family = hb_arrayGetNI( pItem, 1 );

      if( sa->sa_family == AF_INET )
      {
         ( ( struct sockaddr_in* ) sa)->sin_addr.S_un.S_addr = inet_addr( hb_arrayGetCPtr( pItem, 2 ) );
         ( ( struct sockaddr_in* ) sa)->sin_port = htons( hb_arrayGetNI( pItem, 3 ) );
      }
      else
      {
         ULONG  ulLen = hb_arrayGetCLen( pItem, 2 );

         if( ulLen > sizeof( sa->sa_data ) )
            ulLen = sizeof( sa->sa_data );
         memcpy( sa->sa_data, hb_arrayGetCPtr( pItem, 2 ), ulLen );
      }
   }
}


static PHB_ITEM hb_itemPutSockaddr( PHB_ITEM pItem, const struct sockaddr* saddr )
{
   pItem = hb_itemNew( pItem );

   if( saddr->sa_family == AF_INET )
   {
      hb_arrayNew( pItem, 3 );
      hb_arraySetNI( pItem, 1, saddr->sa_family );
      hb_arraySetC( pItem, 2, inet_ntoa( ( ( struct sockaddr_in* ) saddr )->sin_addr ) );
      hb_arraySetNI( pItem, 3, ntohs( ( ( struct sockaddr_in* ) saddr )->sin_port ) );
   }
   else
   {
      hb_arrayNew( pItem, 2 );
      hb_arraySetNI( pItem, 1, saddr->sa_family );
      hb_arraySetCL( pItem, 2, saddr->sa_data, sizeof( saddr->sa_data ) );
   }
   return pItem;
}


HB_FUNC ( SOCKET_INIT )
{
   WSADATA  wsad;

   hb_retni( WSAStartup( hb_parnidef( 1, 257 ), &wsad ) );
   hb_storclen( (char*) &wsad, sizeof( WSADATA ), 2 );
}


HB_FUNC ( SOCKET_EXIT )
{
   hb_retni( WSACleanup() );
}


HB_FUNC ( SOCKET_ERROR )
{
   hb_retni( WSAGetLastError() );
}


HB_FUNC ( SOCKET_CREATE )
{
   hb_retsocket( socket( hb_parnidef( 1, PF_INET ),
                         hb_parnidef( 2, SOCK_STREAM ),
                         hb_parnidef( 3, IPPROTO_TCP ) ) );
}


HB_FUNC ( SOCKET_CLOSE )
{
   hb_retni( closesocket( hb_parsocket( 1 ) ) );
}


HB_FUNC ( SOCKET_BIND )
{
   struct sockaddr   sa;

   hb_itemGetSockaddr( hb_param( 2, HB_IT_ANY ), &sa );
   hb_retni( bind( hb_parsocket( 1 ), &sa, sizeof( struct sockaddr ) ) );
}


HB_FUNC ( SOCKET_LISTEN )
{
   hb_retni( listen( hb_parsocket( 1 ), hb_parnidef( 2, 10 ) ) );
}


HB_FUNC ( SOCKET_ACCEPT )
{
   struct sockaddr   saddr;
   int               iSize = sizeof( struct sockaddr );

   hb_retsocket( accept( hb_parsocket( 1 ), &saddr, &iSize ) );

   if( ISBYREF( 2 ) )
   {
      PHB_ITEM pItem = hb_itemPutSockaddr( NULL, &saddr );
      hb_itemParamStoreForward( 2, pItem );
      hb_itemRelease( pItem );
   }
}


HB_FUNC ( SOCKET_SHUTDOWN )
{
   hb_retni( shutdown( hb_parsocket( 1 ), hb_parnidef( 2, SD_BOTH ) ) );
}


HB_FUNC ( SOCKET_RECV )
{
   int     iLen, iRet;
   char*   pBuf;

   iLen = hb_parni( 3 );

   if( iLen > 65536 || iLen <= 0 )
      iLen = 4096;

   pBuf = ( char* ) hb_xgrab( ( ULONG ) iLen );
   iRet = recv( hb_parsocket( 1 ), pBuf, iLen, hb_parnidef( 4, 0 ) );
   hb_retni( iRet );
   hb_storclen( pBuf, iRet > 0 ? iRet : 0, 2 );
   hb_xfree( pBuf );
}


HB_FUNC ( SOCKET_SEND )
{
   hb_retni( send( hb_parsocket( 1 ), hb_parc( 2 ), hb_parclen( 2 ), hb_parni( 3, 0 ) ) );
}


HB_FUNC ( SOCKET_SELECT )
{
   fd_set             setread, setwrite, seterror;
   BOOL               bRead = 0, bWrite = 0, bError = 0;
   struct timeval     tv;
   SOCKET             socket, maxsocket;
   PHB_ITEM           pArray, pItem;
   ULONG              ulLen, ulIndex, ulCount;
   LONG               lTimeout;
   int                iRet;


   FD_ZERO( &setread );
   FD_ZERO( &setwrite );
   FD_ZERO( &seterror );

   maxsocket = (SOCKET) 0;

   pArray = hb_param( 1, HB_IT_ARRAY );
   if( pArray )
   {
      ulLen = hb_arrayLen( pArray );
      for( ulIndex = 1; ulIndex <= ulLen; ulIndex++ )
      {
         socket = hb_itemGetSocket( hb_arrayGetItemPtr( pArray, ulIndex ) );
         if( socket != INVALID_SOCKET )
         {
            bRead = 1;
            FD_SET( socket, &setread );
            if( socket > maxsocket )
               maxsocket = socket;
         }
      }
   }

   pArray = hb_param( 2, HB_IT_ARRAY );
   if( pArray )
   {
      ulLen = hb_arrayLen( pArray );
      for( ulIndex = 1; ulIndex <= ulLen; ulIndex++ )
      {
         socket = hb_itemGetSocket( hb_arrayGetItemPtr( pArray, ulIndex ) );
         if( socket != INVALID_SOCKET )
         {
            bWrite = 1;
            FD_SET( socket, &setwrite );
            if( socket > maxsocket )
               maxsocket = socket;
         }
      }
   }

   pArray = hb_param( 3, HB_IT_ARRAY );
   if( pArray )
   {
      ulLen = hb_arrayLen( pArray );
      for( ulIndex = 1; ulIndex <= ulLen; ulIndex++ )
      {
         socket = hb_itemGetSocket( hb_arrayGetItemPtr( pArray, ulIndex ) );
         if( socket != INVALID_SOCKET )
         {
            bError = 1;
            FD_SET( socket, &seterror );
            if( socket > maxsocket )
               maxsocket = socket;
         }
      }
   }

   /* Default forever */
   lTimeout = ISNUM( 4 ) ? hb_parnl( 4 ) : -1;

   if( lTimeout == -1 )
   {
      iRet = select( maxsocket + 1, bRead ? &setread : NULL, bWrite ? &setwrite: NULL,
                     bError ? &seterror : NULL, NULL );
   }
   else
   {
      tv.tv_sec = lTimeout / 1000;
      tv.tv_usec = ( lTimeout % 1000 ) * 1000;
      iRet = select( maxsocket + 1, bRead ? &setread : NULL, bWrite ? &setwrite: NULL,
                     bError ? &seterror : NULL, &tv );
   }

   pArray = hb_param( 1, HB_IT_ARRAY );
   if( pArray && ISBYREF( 1 ) )
   {
      ulLen = hb_arrayLen( pArray );
      pItem = hb_itemNew( NULL );
      hb_arrayNew( pItem, ulLen );
      ulCount = 0;
      for( ulIndex = 1; ulIndex <= ulLen; ulIndex++ )
      {
         socket = hb_itemGetSocket( hb_arrayGetItemPtr( pArray, ulIndex ) );
         if( socket != INVALID_SOCKET )
         {
            if( FD_ISSET( socket, &setread ) )
            {
               hb_arraySetForward( pItem, ++ulCount, hb_itemPutSocket( NULL, socket ) );
            }
         }
      }
      hb_itemParamStoreForward( 1, pItem );
   }

   pArray = hb_param( 2, HB_IT_ARRAY );
   if( pArray && ISBYREF( 2 ) )
   {
      ulLen = hb_arrayLen( pArray );
      pItem = hb_itemNew( NULL );
      hb_arrayNew( pItem, ulLen );
      ulCount = 0;
      for( ulIndex = 1; ulIndex <= ulLen; ulIndex++ )
      {
         socket = hb_itemGetSocket( hb_arrayGetItemPtr( pArray, ulIndex ) );
         if( socket != INVALID_SOCKET )
         {
            if( FD_ISSET( socket, &setwrite ) )
            {
               hb_arraySetForward( pItem, ++ulCount, hb_itemPutSocket( NULL, socket ) );
            }
         }
      }
      hb_itemParamStoreForward( 2, pItem );
   }

   pArray = hb_param( 3, HB_IT_ARRAY );
   if( pArray && ISBYREF( 3 ) )
   {
      ulLen = hb_arrayLen( pArray );
      pItem = hb_itemNew( NULL );
      hb_arrayNew( pItem, ulLen );
      ulCount = 0;
      for( ulIndex = 1; ulIndex <= ulLen; ulIndex++ )
      {
         socket = hb_itemGetSocket( hb_arrayGetItemPtr( pArray, ulIndex ) );
         if( socket != INVALID_SOCKET )
         {
            if( FD_ISSET( socket, &seterror ) )
            {
               hb_arraySetForward( pItem, ++ulCount, hb_itemPutSocket( NULL, socket ) );
            }
         }
      }
      hb_itemParamStoreForward( 3, pItem );
   }

   hb_retni( iRet );
}


HB_FUNC ( SOCKET_GETSOCKNAME )
{
   struct sockaddr   saddr;
   int               iSize = sizeof( struct sockaddr );

   hb_retni( getsockname( hb_parsocket( 1 ), &saddr, &iSize ) );
   if( ISBYREF( 2 ) )
   {
      PHB_ITEM pItem = hb_itemPutSockaddr( NULL, &saddr );
      hb_itemParamStoreForward( 2, pItem );
      hb_itemRelease( pItem );
   }
}


HB_FUNC ( SOCKET_GETPEERNAME )
{
   struct sockaddr   saddr;
   int               iSize = sizeof( struct sockaddr );

   hb_retni( getpeername( hb_parsocket( 1 ), &saddr, &iSize ) );
   if( ISBYREF( 2 ) )
   {
      PHB_ITEM pItem = hb_itemPutSockaddr( NULL, &saddr );
      hb_itemParamStoreForward( 2, pItem );
      hb_itemRelease( pItem );
   }
}


HB_FUNC ( CONNECT )
{
   struct sockaddr   sa;

   hb_itemGetSockaddr( hb_param( 2, HB_IT_ANY ), &sa );
   hb_retni( connect( hb_parsocket( 1 ), &sa, sizeof( struct sockaddr ) ) );
}

#endif
