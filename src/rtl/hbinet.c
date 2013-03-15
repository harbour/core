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
 * www - http://harbour-project.org
 *
 * Copyright 2008 Miguel Angel marchuet <miguelangel@marchuet.net>
 *    added dinamic system buffer
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbsocket.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbthread.h"
#include "hbznet.h"

#define HB_INET_ERR_OK            0
#define HB_INET_ERR_TIMEOUT       ( -1 )
#define HB_INET_ERR_CLOSEDCONN    ( -2 )
#define HB_INET_ERR_BUFFOVERRUN   ( -3 )
#define HB_INET_ERR_CLOSEDSOCKET  ( -4 )

typedef struct
{
   HB_SOCKET      sd;
   void *         remote;
   unsigned       remotelen;
   char *         buffer;
   long           inbuffer;
   long           posbuffer;
   long           readahead;
   int            iError;
   int            iCount;
   int            iTimeout;
   int            iTimeLimit;
   PHB_ITEM       pPeriodicBlock;
   PHB_ZNETSTREAM stream;
   HB_INET_RFUNC  recvFunc;
   HB_INET_SFUNC  sendFunc;
   HB_INET_FFUNC  flushFunc;
   HB_INET_CFUNC  cleanFunc;
} HB_SOCKET_STRUCT, * PHB_SOCKET_STRUCT;

#define HB_INET_BUFFER_LEN  256

#define HB_INET_INITIALIZE()  if( s_initialize ) hb_inetAutoInit()

#define HB_PARSOCKET( n )     ( ( PHB_SOCKET_STRUCT ) hb_parptrGC( &s_gcInetFuncs, n ) )

#define HB_SOCKET_INIT( s, p ) \
   do \
   { \
      HB_INET_INITIALIZE(); \
      s = ( PHB_SOCKET_STRUCT ) hb_gcAllocate( sizeof( HB_SOCKET_STRUCT ), &s_gcInetFuncs ); \
      memset( s, 0, sizeof( HB_SOCKET_STRUCT ) ); \
      s->sd         = HB_NO_SOCKET; \
      s->readahead  = HB_INET_BUFFER_LEN; \
      s->iTimeout   = -1; \
      s->iTimeLimit = -1; \
      s->iError     = HB_INET_ERR_OK; \
      p = hb_itemPutPtrGC( p, s ); \
   } while( 0 )

static const char * const s_inetCRLF = "\r\n";

static HB_COUNTER s_initialize = 1;

#if defined( HB_OS_LINUX )
/* #define HB_INET_LINUX_INTERRUPT     SIGUSR1 + 90 */
#  ifdef HB_INET_LINUX_INTERRUPT
#     include <signal.h>

static void hb_inetLinuxSigusrHandle( int sig )
{
   /* nothing to do */
   HB_SYMBOL_UNUSED( sig );
}
#  endif
#endif

static void hb_inetErrRT( void )
{
   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static HB_BOOL hb_inetIsOpen( PHB_SOCKET_STRUCT socket )
{
   if( socket->sd == HB_NO_SOCKET )
   {
      socket->iError = HB_INET_ERR_CLOSEDSOCKET;
      return HB_FALSE;
   }
   return HB_TRUE;
}

static int hb_inetCloseSocket( PHB_SOCKET_STRUCT socket )
{
   int ret = hb_socketClose( socket->sd );

   socket->sd       = HB_NO_SOCKET;
   socket->inbuffer = 0;
   return ret;
}

static void hb_inetGetError( PHB_SOCKET_STRUCT socket )
{
   socket->iError = hb_socketGetError();
   if( socket->iError == HB_SOCKET_ERR_TIMEOUT )
      socket->iError = HB_INET_ERR_TIMEOUT;
}

static void hb_inetCloseStream( PHB_SOCKET_STRUCT socket )
{
   if( socket->cleanFunc )
      socket->cleanFunc( socket->stream );

   socket->recvFunc = NULL;
   socket->sendFunc = NULL;
   socket->flushFunc = NULL;
   socket->cleanFunc = NULL;
   socket->stream = NULL;
}

static HB_GARBAGE_FUNC( hb_inetSocketFinalize )
{
   PHB_SOCKET_STRUCT socket = ( PHB_SOCKET_STRUCT ) Cargo;

   if( socket->sd != HB_NO_SOCKET )
   {
      hb_socketShutdown( socket->sd, HB_SOCKET_SHUT_RDWR );
      hb_inetCloseSocket( socket );
   }

   if( socket->pPeriodicBlock )
   {
      hb_itemRelease( socket->pPeriodicBlock );
      socket->pPeriodicBlock = NULL;
   }
   if( socket->remote )
   {
      hb_xfree( socket->remote );
      socket->remote = NULL;
   }
   if( socket->buffer )
   {
      hb_xfree( socket->buffer );
      socket->buffer = NULL;
   }
   hb_inetCloseStream( socket );
}

static HB_GARBAGE_FUNC( hb_inetSocketMark )
{
   PHB_SOCKET_STRUCT socket = ( PHB_SOCKET_STRUCT ) Cargo;

   if( socket->pPeriodicBlock )
      hb_gcMark( socket->pPeriodicBlock );
}

static const HB_GC_FUNCS s_gcInetFuncs =
{
   hb_inetSocketFinalize,
   hb_inetSocketMark
};

/*****************************************************
 * Socket Initialization
 ***/

static void hb_inetAutoInit( void )
{
   if( s_initialize )
   {
      if( hb_atomic_dec( &s_initialize ) )
      {
         hb_socketInit();
#if defined( HB_INET_LINUX_INTERRUPT )
         signal( HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle );
#endif
      }
   }
}

HB_BOOL hb_znetInetInitialize( PHB_ITEM pItem, PHB_ZNETSTREAM pStream,
                               HB_INET_RFUNC recvFunc,
                               HB_INET_SFUNC sendFunc,
                               HB_INET_FFUNC flushFunc,
                               HB_INET_CFUNC cleanFunc )
{
   PHB_SOCKET_STRUCT socket = ( PHB_SOCKET_STRUCT ) hb_itemGetPtrGC( pItem, &s_gcInetFuncs );

   if( socket )
   {
      hb_inetCloseStream( socket );

      socket->recvFunc = recvFunc;
      socket->sendFunc = sendFunc;
      socket->flushFunc = flushFunc;
      socket->cleanFunc = cleanFunc;
      socket->stream = pStream;
      return HB_TRUE;
   }

   hb_inetErrRT();
   return HB_FALSE;
}

HB_FUNC( HB_INETINIT )
{
   int ret;

   hb_atomic_set( &s_initialize, 0 );
   ret = hb_socketInit();
   if( ret == 0 )
   {
#if defined( HB_INET_LINUX_INTERRUPT )
      signal( HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle );
#endif
   }
   hb_retl( ret == 0 );
}

HB_FUNC( HB_INETCLEANUP )
{
   hb_socketCleanup();
}

/*****************************************************
 * Socket Creation and destruction
 ***/

HB_FUNC( HB_INETCREATE )
{
   PHB_ITEM pSocket = NULL;
   PHB_SOCKET_STRUCT socket;

   HB_SOCKET_INIT( socket, pSocket );

   if( HB_ISNUM( 1 ) )
      socket->iTimeout = hb_parni( 1 );

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETCLOSE )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      if( socket->sd != HB_NO_SOCKET )
      {
         hb_socketShutdown( socket->sd, HB_SOCKET_SHUT_RDWR );
         hb_retni( hb_inetCloseSocket( socket ) );
#ifdef HB_INET_LINUX_INTERRUPT
         kill( 0, HB_INET_LINUX_INTERRUPT );
#endif
      }
      else
         hb_retni( -1 );
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETFD )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      hb_retnint( socket->sd );

      if( hb_parl( 2 ) )
         socket->sd = HB_NO_SOCKET;
   }
   else
      hb_inetErrRT();
}

/************************************************
 * Socket data access & management
 ***/

HB_FUNC( HB_INETSTATUS )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
      hb_retni( socket->sd == HB_NO_SOCKET ? -1 : 1 );  /* TODO: hb_retni( socket->status ); */
   else
      hb_inetErrRT();
}

/* Prepared, but still not used; being in wait for comments */
#if 0
HB_FUNC( HB_INETSTATUSDESC )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      switch( socket->status )
      {
         case 0:  hb_retc_const( "Connection not opened" ); return;
         case 1:  hb_retc_const( "Connection alive" ); return;
         case 2:  hb_retc_const( "Last operation error" ); return;
         case 3:  hb_retc_const( "Last operation timeout" ); return;
         default: hb_retc_const( "unknown" );
      }
   }
   else
      hb_inetErrRT();
}
#endif

HB_FUNC( HB_INETERRORCODE )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
      hb_retni( socket->iError );
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETERRORDESC )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      switch( socket->iError )
      {
         case HB_INET_ERR_OK           : hb_retc_null(); return;
         case HB_INET_ERR_TIMEOUT      : hb_retc_const( "Timeout" ); return;
         case HB_INET_ERR_CLOSEDCONN   : hb_retc_const( "Connection closed" ); return;
         case HB_INET_ERR_CLOSEDSOCKET : hb_retc_const( "Closed socket" ); return;
         case HB_INET_ERR_BUFFOVERRUN  : hb_retc_const( "Buffer overrun" ); return;
         default:
            hb_retc( hb_socketErrorStr( socket->iError ) );
      }
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETCLEARERROR )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
      socket->iError = HB_INET_ERR_OK;
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETCOUNT )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
      hb_retni( socket->iCount );
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETADDRESS )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      char * szAddr = socket->remote ?
            hb_socketAddrGetName( socket->remote, socket->remotelen ) : NULL;
      if( szAddr )
         hb_retc_buffer( szAddr );
      else
         hb_retc_null();
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETPORT )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
      hb_retni( socket->remote ?
                hb_socketAddrGetPort( socket->remote, socket->remotelen ) : 0 );
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETTIMEOUT )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      hb_retni( socket->iTimeout );

      if( HB_ISNUM( 2 ) )
         socket->iTimeout = hb_parni( 2 );
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETCLEARTIMEOUT )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
      socket->iTimeout = -1;
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETTIMELIMIT )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      hb_retni( socket->iTimeLimit );

      if( HB_ISNUM( 2 ) )
         socket->iTimeLimit = hb_parni( 2 );
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETCLEARTIMELIMIT )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
      socket->iTimeLimit = -1;
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETPERIODCALLBACK )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      PHB_ITEM pExec = hb_param( 2, HB_IT_ARRAY | HB_IT_BLOCK | HB_IT_SYMBOL );

      if( socket->pPeriodicBlock )
         hb_itemReturn( socket->pPeriodicBlock );

      if( pExec )
      {
         if( socket->pPeriodicBlock )
            hb_itemRelease( socket->pPeriodicBlock );
         socket->pPeriodicBlock = hb_itemClone( pExec );
         hb_gcUnlock( socket->pPeriodicBlock );
      }
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETCLEARPERIODCALLBACK )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      if( socket->pPeriodicBlock )
      {
         hb_itemRelease( socket->pPeriodicBlock );
         socket->pPeriodicBlock = NULL;
      }
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETGETSNDBUFSIZE )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( socket ) )
      {
         if( hb_socketGetSndBufSize( socket->sd, &iSize ) != 0 )
            iSize = -1;
      }
      hb_retni( iSize );
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETGETRCVBUFSIZE )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( socket ) )
      {
         if( hb_socketGetRcvBufSize( socket->sd, &iSize ) != 0 )
            iSize = -1;
      }
      hb_retni( iSize );
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETSETSNDBUFSIZE )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( socket ) )
      {
         iSize = hb_parni( 2 );
         hb_socketSetSndBufSize( socket->sd, iSize );
      }
      hb_retni( iSize );
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETSETRCVBUFSIZE )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( socket ) )
      {
         iSize = hb_parni( 2 );
         hb_socketSetRcvBufSize( socket->sd, iSize );
      }
      hb_retni( iSize );
   }
   else
      hb_inetErrRT();
}



/********************************************************************
 * TCP receive and send functions
 ***/

static long s_inetRecv( PHB_SOCKET_STRUCT socket, char * buffer, long size, HB_BOOL readahead )
{
   long rec = 0;

   if( readahead && socket->inbuffer == 0 && socket->readahead > size )
   {
      if( socket->buffer == NULL )
         socket->buffer = ( char * ) hb_xgrab( socket->readahead );
      socket->posbuffer = 0;
      if( socket->recvFunc )
         rec = socket->recvFunc( socket->stream, socket->sd,
                                 socket->buffer, socket->readahead,
                                 socket->iTimeout );
      else
         rec = hb_socketRecv( socket->sd, socket->buffer, socket->readahead,
                              0, socket->iTimeout );
      socket->inbuffer = HB_MAX( 0, rec );
   }
   else
      readahead = HB_FALSE;

   if( socket->inbuffer > 0 )
   {
      rec = HB_MIN( size, socket->inbuffer );
      memcpy( buffer, socket->buffer + socket->posbuffer, rec );
      socket->posbuffer += rec;
      socket->inbuffer -= rec;
      if( size > rec && ! readahead )
      {
         if( socket->recvFunc )
            rec = socket->recvFunc( socket->stream, socket->sd,
                                    buffer + rec, size - rec,
                                    socket->iTimeout );
         else
            size = hb_socketRecv( socket->sd, buffer + rec, size - rec, 0, 0 );

         if( size > 0 )
            rec += size;
      }
   }
   else if( ! readahead )
   {
      if( socket->recvFunc )
         rec = socket->recvFunc( socket->stream, socket->sd,
                                 buffer, size, socket->iTimeout );
      else
         rec = hb_socketRecv( socket->sd, buffer, size, 0, socket->iTimeout );
   }

   return rec;
}

static void s_inetRecvInternal( int iMode )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   char * buffer;
   HB_SIZE nLen;
   int iLen, iMaxLen, iReceived;
   int iTimeElapsed;

   if( socket == NULL || pBuffer == NULL || ! HB_ISBYREF( 2 ) )
      hb_inetErrRT();
   else if( ! hb_inetIsOpen( socket ) )
      hb_retni( -1 );
   else
   {
      if( hb_itemGetWriteCL( pBuffer, &buffer, &nLen ) )
         iLen = ( int ) nLen;
      else
      {
         iLen = 0;
         buffer = NULL;
      }

      if( HB_ISNUM( 3 ) )
      {
         iMaxLen = hb_parni( 3 );
         if( iMaxLen < 0 )
            iMaxLen = 0;
         else if( iLen < iMaxLen )
            iMaxLen = iLen;
      }
      else
         iMaxLen = iLen;

      iReceived = 0;
      iTimeElapsed = 0;
      socket->iError = HB_INET_ERR_OK;
      do
      {
         iLen = s_inetRecv( socket, buffer + iReceived, iMaxLen - iReceived, HB_FALSE );
         if( iLen >= 0 )
         {
            iReceived += iLen;
            if( iMode == 0 ) /* Called from InetRecv()? */
               break;
         }
         else if( iLen == -1 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
         {
            /* timed out; let's see if we have to run a cb routine */
            iTimeElapsed += socket->iTimeout;
            /* if we have a pPeriodicBlock, timeLimit is our REAL timeout */
            if( socket->pPeriodicBlock )
            {
               hb_execFromArray( socket->pPeriodicBlock );
               /* do we continue? */
               if( hb_parl( -1 ) && hb_vmRequestQuery() == 0 &&
                   ( socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit ) )
                  iLen = 1;   /* Declare success to continue loop */
            }
         }
      }
      while( iReceived < iMaxLen && iLen > 0 );

      socket->iCount = iReceived;

      if( iLen == 0 )
         socket->iError = HB_INET_ERR_CLOSEDCONN;
      else if( iLen < 0 )
         hb_inetGetError( socket );

      hb_retni( iReceived > 0 ? iReceived : iLen );
   }
}

HB_FUNC( HB_INETRECV )
{
   s_inetRecvInternal( 0 );
}


HB_FUNC( HB_INETRECVALL )
{
   s_inetRecvInternal( 1 );
}


static void s_inetRecvPattern( const char * const * patterns, int * patternsizes,
                               int iPatternsCount, int iParam )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );
   PHB_ITEM pResult         = hb_param( iParam, HB_IT_BYREF );
   PHB_ITEM pMaxSize        = hb_param( iParam + 1, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize     = hb_param( iParam + 2, HB_IT_NUMERIC );

   char cChar = '\0';
   char * buffer;
   int iPaternFound = 0;
   int iTimeElapsed = 0;
   int iPos = 0;
   int iLen;
   int iAllocated, iBufferSize, iMax;
   int i;

   if( socket == NULL )
   {
      hb_inetErrRT();
      return;
   }
   else if( ! hb_inetIsOpen( socket ) )
   {
      if( pResult )
         hb_itemPutNI( pResult, -1 );
      hb_retc_null();
      return;
   }


   iBufferSize = pBufferSize ? hb_itemGetNI( pBufferSize ) : 80;
   iMax = pMaxSize ? hb_itemGetNI( pMaxSize ) : 0;

   socket->iError = HB_INET_ERR_OK;

   buffer = ( char * ) hb_xgrab( iBufferSize );
   iAllocated = iBufferSize;

   do
   {
      if( iPos == iAllocated - 1 )
      {
         iAllocated += iBufferSize;
         buffer = ( char * ) hb_xrealloc( buffer, iAllocated );
      }

      iLen = s_inetRecv( socket, &cChar, 1, HB_TRUE );
      if( iLen == -1 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
      {
         iLen = -2;     /* this signals timeout */
         iTimeElapsed += socket->iTimeout;
         if( socket->pPeriodicBlock )
         {
            HB_BOOL fResult;

            hb_execFromArray( socket->pPeriodicBlock );
            fResult = hb_parl( -1 ) && hb_vmRequestQuery() == 0;

            if( fResult &&
                ( socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit ) )
               iLen = 1;
         }
      }
      else if( iLen > 0 )
      {
         buffer[ iPos++ ] = cChar;
         for( i = 0; i < iPatternsCount; ++i )
         {
            if( patternsizes[ i ] <= iPos &&
                cChar == patterns[ i ][ patternsizes[ i ] - 1 ] )
            {
               if( memcmp( buffer + iPos - patternsizes[ i ],
                           patterns[ i ], patternsizes[ i ] ) == 0 )
               {
                  iPaternFound = i + 1;
                  break;
               }
            }
         }
      }
   }
   while( iLen > 0 && iPaternFound == 0 && ( iMax == 0 || iPos < iMax ) );

   if( iPaternFound )
   {
      socket->iCount = iPos;
      if( pResult )
         hb_itemPutNI( pResult, iPos );
      hb_retclen_buffer( buffer, iPos - patternsizes[ iPaternFound - 1 ] );
   }
   else
   {
      if( iLen == 0 )
         socket->iError = HB_INET_ERR_CLOSEDCONN;
      else if( iLen < 0 )
         hb_inetGetError( socket );
      else
      {
         socket->iError = HB_INET_ERR_BUFFOVERRUN;
         iLen = -1;
      }
      if( pResult )
         hb_itemPutNI( pResult, iLen );
      hb_xfree( buffer );
      hb_retc_null();
   }
}

HB_FUNC( HB_INETRECVLINE )
{
   int iEolLen = ( int ) strlen( s_inetCRLF );

   s_inetRecvPattern( &s_inetCRLF, &iEolLen, 1, 2 );
}

#define HB_PATERN_BUF_SIZE  16

HB_FUNC( HB_INETRECVENDBLOCK )
{
   PHB_ITEM pProto = hb_param( 2, HB_IT_ARRAY | HB_IT_STRING );
   const char * patterns_buf[ HB_PATERN_BUF_SIZE ];
   const char ** patterns = patterns_buf;
   int patternsizes_buf[ HB_PATERN_BUF_SIZE ];
   int * patternsizes = patternsizes_buf;
   int iPatternsCount = 0;
   int iLen;

   if( pProto && HB_IS_ARRAY( pProto ) )
   {
      int iPatternsMax = ( int ) hb_arrayLen( pProto ), i;

      for( i = 1; i <= iPatternsMax; i++ )
      {
         iLen = ( int ) hb_arrayGetCLen( pProto, i );
         if( iLen > 0 )
            ++iPatternsCount;
      }
      if( iPatternsCount > 0 )
      {
         if( iPatternsCount > HB_PATERN_BUF_SIZE )
         {
            patterns = ( const char ** ) hb_xgrab( sizeof( char * ) * iPatternsCount );
            patternsizes = ( int * ) hb_xgrab( sizeof( int ) * iPatternsCount );
         }
         iPatternsCount = 0;
         for( i = 1; i <= iPatternsMax; i++ )
         {
            iLen = ( int ) hb_arrayGetCLen( pProto, i );
            if( iLen > 0 )
            {
               patterns[ iPatternsCount ]     = hb_arrayGetCPtr( pProto, i );
               patternsizes[ iPatternsCount ] = iLen;
               ++iPatternsCount;
            }
         }
      }
   }

   if( iPatternsCount == 0 )
   {
      iLen = ( int ) hb_itemGetCLen( pProto );
      if( iLen > 0 )
      {
         patterns[ 0 ]     = hb_itemGetCPtr( pProto );
         patternsizes[ 0 ] = iLen;
      }
      else
      {
         patterns[ 0 ]     = s_inetCRLF;
         patternsizes[ 0 ] = ( int ) strlen( s_inetCRLF );
      }
      iPatternsCount = 1;
   }

   s_inetRecvPattern( patterns, patternsizes, iPatternsCount, 3 );

   if( iPatternsCount > HB_PATERN_BUF_SIZE )
   {
      hb_xfree( ( void * ) patterns );
      hb_xfree( patternsizes );
   }
}


HB_FUNC( HB_INETDATAREADY )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );
   int iVal;

   if( socket == NULL || ( hb_pcount() >= 2 && ! HB_ISNUM( 2 ) ) )
      hb_inetErrRT();
   else if( ! hb_inetIsOpen( socket ) )
      hb_retni( -1 );
   else
   {
      socket->iError = HB_INET_ERR_OK;
      if( socket->inbuffer > 0 )
         iVal = 1;
      else
      {
         iVal = hb_socketSelectRead( socket->sd, hb_parnint( 2 ) /* default to 0 */ );
         if( iVal < 0 )
            hb_inetGetError( socket );
      }
      hb_retni( iVal );
   }
}


static void s_inetSendInternal( HB_BOOL lAll )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   const char * buffer;
   int iLen, iSent, iSend;
   long lLastSnd = 1;

   if( socket == NULL || pBuffer == NULL )
      hb_inetErrRT();
   else if( ! hb_inetIsOpen( socket ) )
      hb_retni( -1 );
   else
   {
      buffer = hb_itemGetCPtr( pBuffer );
      iSend = ( int ) hb_itemGetCLen( pBuffer );
      if( HB_ISNUM( 3 ) )
      {
         iLen = hb_parni( 3 );
         if( iLen < iSend )
            iSend = iLen;
      }

      socket->iError = HB_INET_ERR_OK;

      iSent = iLen = 0;
      while( iSent < iSend )
      {
         if( socket->sendFunc )
         {
            iLen = socket->sendFunc( socket->stream, socket->sd,
                                     buffer + iSent, iSend - iSent,
                                     socket->iTimeout, &lLastSnd );
            if( lLastSnd <= 0 && iLen > 0 )
            {
               iSent += iLen;
               iLen = ( int ) lLastSnd;
            }
         }
         else
            iLen = hb_socketSend( socket->sd, buffer + iSent, iSend - iSent,
                                  0, socket->iTimeout );
         if( iLen > 0 )
         {
            iSent += iLen;
            if( ! lAll )
               break;
         }
         else
         {
            hb_inetGetError( socket );
            break;
         }
      }
      socket->iCount = iSent;

      if( socket->flushFunc && ( lLastSnd > 0 || ( lLastSnd == -1 &&
             socket->iTimeout >= 0 && socket->iTimeout < 10000 &&
             hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT ) ) )
      {
         /* TODO: safe information about unflushed data and try to call
                  flush before entering receive wait sate */
         socket->flushFunc( socket->stream, socket->sd, socket->iTimeout < 0 ?
                            socket->iTimeout : HB_MAX( socket->iTimeout, 10000 ) );
      }

      hb_retni( iSent > 0 ? iSent : iLen );
   }
}

HB_FUNC( HB_INETSEND )
{
   s_inetSendInternal( HB_FALSE );
}

HB_FUNC( HB_INETSENDALL )
{
   s_inetSendInternal( HB_TRUE );
}


/*******************************************
 * Name resolution interface functions
 ***/

HB_FUNC( HB_INETGETHOSTS )
{
   const char * szHost = hb_parc( 1 );

   if( szHost )
   {
      PHB_ITEM pHosts;

      HB_INET_INITIALIZE();
      pHosts = hb_socketGetHosts( szHost, HB_SOCKET_PF_INET );
      if( pHosts )
         hb_itemReturnRelease( pHosts );
      else
         hb_reta( 0 );
   }
   else
      hb_inetErrRT();
}

HB_FUNC( HB_INETGETALIAS )
{
   const char * szHost = hb_parc( 1 );

   if( szHost )
   {
      PHB_ITEM pHosts;

      HB_INET_INITIALIZE();
      pHosts = hb_socketGetAliases( szHost, HB_SOCKET_PF_INET );
      if( pHosts )
         hb_itemReturnRelease( pHosts );
      else
         hb_reta( 0 );
   }
   else
      hb_inetErrRT();
}


/**********************************************
 * Interface information function
 ****/
HB_FUNC( HB_INETIFINFO )
{
   PHB_ITEM pInfo;

   HB_INET_INITIALIZE();
   pInfo = hb_socketGetIFaces( hb_parnidef( 2, HB_SOCKET_PF_INET ),
                               hb_parl( 1 ) );
   if( pInfo )
      hb_itemReturnRelease( pInfo );
   else
      hb_reta( 0 );
}

/**********************************************
 * Server Specific functions
 ****/

static int s_inetBind( PHB_SOCKET_STRUCT socket, const void * pSockAddr, unsigned uiLen )
{
#if ! defined( HB_OS_WIN )
   hb_socketSetReuseAddr( socket->sd, HB_TRUE );
#endif
   return hb_socketBind( socket->sd, pSockAddr, uiLen );
}

HB_FUNC( HB_INETSERVER )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 2 );
   PHB_ITEM pSocket = NULL;

   if( ! HB_ISNUM( 1 ) || ( socket == NULL && ! HB_ISNIL( 2 ) ) )
   {
      hb_inetErrRT();
      return;
   }

   if( ! socket )
      HB_SOCKET_INIT( socket, pSocket );
   else if( socket->sd != HB_NO_SOCKET )
      hb_inetCloseSocket( socket );
   socket->sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0 );
   if( socket->sd == HB_NO_SOCKET )
      hb_inetGetError( socket );
   else
   {
      int iPort = hb_parni( 1 );
      const char * szAddress = hb_parc( 3 );
      int iListen = hb_parnidef( 4, 10 );

      if( socket->remote )
         hb_xfree( socket->remote );
      if( ! hb_socketInetAddr( &socket->remote, &socket->remotelen, szAddress, iPort ) ||
          s_inetBind( socket, socket->remote, socket->remotelen ) != 0 ||
          hb_socketListen( socket->sd, iListen ) != 0 )
      {
         hb_inetGetError( socket );
         hb_inetCloseSocket( socket );
      }
      else
         socket->iError = HB_INET_ERR_OK;
   }
   if( pSocket )
      hb_itemReturnRelease( pSocket );
   else
      hb_itemReturn( hb_param( 2, HB_IT_ANY ) );
}

HB_FUNC( HB_INETACCEPT )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );

   if( socket == NULL )
      hb_inetErrRT();
   else if( hb_inetIsOpen( socket ) )
   {
      do
      {
         void * sa;
         unsigned len;
         HB_SOCKET incoming = hb_socketAccept( socket->sd, &sa, &len, socket->iTimeout );

         if( incoming == HB_NO_SOCKET )
            hb_inetGetError( socket );
         else
         {
            PHB_SOCKET_STRUCT new_socket;
            PHB_ITEM pSocket = NULL;
            HB_SOCKET_INIT( new_socket, pSocket );
            new_socket->remote = sa;
            new_socket->remotelen = len;
            new_socket->sd = incoming;
            hb_itemReturnRelease( pSocket );
            socket->iError = HB_INET_ERR_OK;
            break;
         }
      }
      while( socket->iError == HB_SOCKET_ERR_AGAIN &&
             hb_vmRequestQuery() == 0 );
   }
}

/**********************************************
 * Client specific (connection functions)
 ****/

static void hb_inetConnectInternal( HB_BOOL fResolve )
{
   const char * szHost = hb_parc( 1 );
   char * szAddr = NULL;
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 3 );
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 2 );

   if( szHost == NULL || iPort == 0 || ( socket == NULL && ! HB_ISNIL( 3 ) ) )
      hb_inetErrRT();
   else
   {
      if( ! socket )
         HB_SOCKET_INIT( socket, pSocket );
      else if( socket->sd != HB_NO_SOCKET )
         hb_inetCloseSocket( socket );

      if( fResolve )
         szHost = szAddr = hb_socketResolveAddr( szHost, HB_SOCKET_AF_INET );

      if( fResolve && ! szAddr )
         hb_inetGetError( socket );
      else
      {
         /* Creates comm socket */
         socket->sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0 );
         if( socket->sd == HB_NO_SOCKET )
            hb_inetGetError( socket );
         else
         {
            if( socket->remote )
               hb_xfree( socket->remote );
            if( hb_socketInetAddr( &socket->remote, &socket->remotelen,
                                   szHost, iPort ) )
            {
               hb_socketSetKeepAlive( socket->sd, HB_TRUE );
               if( hb_socketConnect( socket->sd, socket->remote, socket->remotelen,
                                     socket->iTimeout ) != 0 )
                  hb_inetGetError( socket );
               else
                  socket->iError = HB_INET_ERR_OK;
            }
            else
               hb_inetGetError( socket );
         }
         if( szAddr )
            hb_xfree( szAddr );
      }
      if( pSocket )
         hb_itemReturnRelease( pSocket );
      else
         hb_itemReturn( hb_param( 3, HB_IT_ANY ) );
   }
}

HB_FUNC( HB_INETCONNECT )
{
   hb_inetConnectInternal( HB_TRUE );
}

HB_FUNC( HB_INETCONNECTIP )
{
   hb_inetConnectInternal( HB_FALSE );
}

/***********************************************************
 * Datagram functions
 ************************************************************/

HB_FUNC( HB_INETDGRAMBIND )
{
   PHB_SOCKET_STRUCT socket;
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 1 );
   const char * szAddress;

   /* Parameter error checking */
   if( iPort == 0 || ( hb_pcount() >= 4 && ! HB_ISCHAR( 4 ) ) )
   {
      hb_inetErrRT();
      return;
   }

   HB_SOCKET_INIT( socket, pSocket );

   /* Creates comm socket */
   socket->sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_DGRAM, HB_SOCKET_IPPROTO_UDP );
   if( socket->sd == HB_NO_SOCKET )
   {
      hb_inetGetError( socket );
      hb_itemReturnRelease( pSocket );
      return;
   }

   /* Setting broadcast if needed. */
   if( hb_parl( 3 ) )
      hb_socketSetBroadcast( socket->sd, HB_TRUE );

   szAddress = hb_parc( 2 );
   if( socket->remote )
      hb_xfree( socket->remote );
   if( ! hb_socketInetAddr( &socket->remote, &socket->remotelen,
                            szAddress, iPort ) ||
       s_inetBind( socket, socket->remote, socket->remotelen ) != 0 )
   {
      hb_inetGetError( socket );
      hb_inetCloseSocket( socket );
   }
   else if( hb_pcount() >= 4 )
   {
      if( hb_socketSetMulticast( socket->sd, HB_SOCKET_PF_INET, hb_parc( 4 ) ) != 0 )
         hb_inetGetError( socket );
   }

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETDGRAM )
{
   PHB_SOCKET_STRUCT socket;
   PHB_ITEM pSocket = NULL;

   HB_SOCKET_INIT( socket, pSocket );

   /* Creates comm socket */
   socket->sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_DGRAM, HB_SOCKET_IPPROTO_UDP );
   if( socket->sd == HB_NO_SOCKET )
   {
      hb_inetGetError( socket );
      hb_itemReturnRelease( pSocket );
      return;
   }

   /* Setting broadcast if needed. */
   if( hb_parl( 1 ) )
      hb_socketSetBroadcast( socket->sd, HB_TRUE );

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETDGRAMSEND )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );
   const char * szAddress = hb_parc( 2 );
   int iPort = hb_parni( 3 );
   PHB_ITEM pBuffer = hb_param( 4, HB_IT_STRING );
   int iLen;
   const char * szBuffer;

   if( socket == NULL || szAddress == NULL || iPort == 0 || pBuffer == NULL )
      hb_inetErrRT();
   else if( ! hb_inetIsOpen( socket ) )
   {
      socket->iCount = 0;
      hb_retni( -1 );
   }
   else
   {
      socket->iCount = 0;
      if( socket->remote )
         hb_xfree( socket->remote );
      if( ! hb_socketInetAddr( &socket->remote, &socket->remotelen, szAddress, iPort ) )
      {
         hb_inetGetError( socket );
         iLen = -1;
      }
      else
      {
         szBuffer = hb_itemGetCPtr( pBuffer );
         iLen = ( int ) hb_itemGetCLen( pBuffer );
         if( HB_ISNUM( 5 ) )
         {
            int iMaxLen = hb_parni( 5 );
            if( iMaxLen < iLen )
               iLen = HB_MAX( iMaxLen, 0 );
         }
         iLen = hb_socketSendTo( socket->sd, szBuffer, iLen, 0,
                                 socket->remote, socket->remotelen,
                                 socket->iTimeout );
         if( iLen == -1 )
            hb_inetGetError( socket );
         else
         {
            socket->iError = HB_INET_ERR_OK;
            socket->iCount = iLen;
         }
      }
      hb_retni( iLen );
   }
}

HB_FUNC( HB_INETDGRAMRECV )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   int iTimeElapsed = 0;
   int iLen = 0, iMax;
   char * buffer = NULL;
   HB_SIZE nLen;
   HB_BOOL fRepeat;

   if( socket == NULL || pBuffer == NULL || ! HB_ISBYREF( 2 ) )
      hb_inetErrRT();
   else if( ! hb_inetIsOpen( socket ) )
   {
      socket->iCount = 0;
      hb_retni( -1 );
   }
   else
   {
      socket->iCount = 0;
      if( hb_itemGetWriteCL( pBuffer, &buffer, &nLen ) )
         iLen = ( int ) nLen;
      if( HB_ISNUM( 3 ) )
      {
         iMax = hb_parni( 3 );
         if( iMax < iLen )
            iLen = HB_MAX( iMax, 0 );
      }

      do
      {
         fRepeat = HB_FALSE;
         if( socket->remote )
            hb_xfree( socket->remote );
         iMax = hb_socketRecvFrom( socket->sd, buffer, iLen, 0,
                                   &socket->remote, &socket->remotelen,
                                   socket->iTimeout );
         iTimeElapsed += socket->iTimeout;
         if( socket->pPeriodicBlock )
         {
            hb_execFromArray( socket->pPeriodicBlock );
            /* do we continue? */
            fRepeat = hb_parl( -1 ) && hb_vmRequestQuery() == 0 &&
                      ( socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit );
         }
      }
      while( fRepeat );

      if( iMax < 0 )
         hb_inetGetError( socket );
      else
         socket->iError = iMax == 0 ? HB_INET_ERR_CLOSEDCONN : HB_INET_ERR_OK;

      hb_retni( iMax );
   }
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
