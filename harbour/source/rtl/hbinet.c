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
#include "hbsocket.h"
#include "hbapierr.h"
#include "hbvm.h"

typedef struct _HB_SOCKET
{
   HB_SOCKET_T        com;
   void *             remote;
   unsigned           remotelen;
   char               szErrorText[ 128 ];
   const char *       pszErrorText;
   int                iErrorCode;
   int                iCount;
   int                iTimeout;
   int                iTimeLimit;
   PHB_ITEM           pPeriodicBlock;
} HB_SOCKET, * PHB_SOCKET;

#define HB_PARSOCKET( n )     ( ( PHB_SOCKET ) hb_parptrGC( hb_inetSocketFinalize, n ) )

#define HB_SOCKET_ZERO_ERROR( s )   \
         do { \
            s->iErrorCode = 0; \
            s->pszErrorText = ""; \
         } while( 0 )

#define HB_SOCKET_SET_ERROR( s )      \
         do { \
            s->iErrorCode = hb_socketGetError(); \
            s->pszErrorText = hb_socketErrorStr( s->iErrorCode ); \
         } while( 0 )

#define HB_SOCKET_SET_ERROR1( s, code )   \
         do { \
            s->iErrorCode = code; \
            s->pszErrorText = hb_socketErrorStr( code ); \
         } while( 0 )

#define HB_SOCKET_SET_ERROR2( s, code, desc )   \
         do { \
            s->iErrorCode = code; \
            s->pszErrorText = desc; \
         } while( 0 )

#define HB_SOCKET_INIT( s, p ) \
      do { \
         s = ( PHB_SOCKET ) hb_gcAlloc( sizeof( HB_SOCKET ), hb_inetSocketFinalize ); \
         memset( s, '\0', sizeof( HB_SOCKET ) ); \
         s->com = HB_NO_SOCKET; \
         s->iTimeout = -1; \
         s->iTimeLimit = -1; \
         s->pszErrorText = ""; \
         p = hb_itemPutPtrGC( p, s ); \
      } while( 0 )

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

static const char * s_inetCRLF = "\r\n";

static BOOL hb_inetIsOpen( PHB_SOCKET Socket )
{
   if( Socket->com == HB_NO_SOCKET )
   {
      HB_SOCKET_SET_ERROR2( Socket, -4, "Closed socket" );
      return FALSE;
   }
   return TRUE;
}

static int hb_inetConnect( PHB_SOCKET Socket )
{
   int iErr;

   hb_socketSetKeepAlive( Socket->com, TRUE );

   iErr = hb_socketConnect( Socket->com, Socket->remote, Socket->remotelen,
                            Socket->iTimeout );
   if( iErr != 0 )
      HB_SOCKET_SET_ERROR( Socket );

   return iErr == 0;
}

static HB_GARBAGE_FUNC( hb_inetSocketFinalize )
{
   PHB_SOCKET Socket = ( PHB_SOCKET ) Cargo;

   if( Socket->com != HB_NO_SOCKET )
   {
      hb_socketShutdown( Socket->com, HB_SOCK_SHUT_RDWR );
      hb_socketClose( Socket->com );
      Socket->com = HB_NO_SOCKET;
   }

   if( Socket->pPeriodicBlock )
   {
      hb_itemRelease( Socket->pPeriodicBlock );
      Socket->pPeriodicBlock = NULL;
   }
   if( Socket->remote )
   {
      hb_xfree( Socket->remote );
      Socket->remote = NULL;
   }
}

/*****************************************************
* Socket Initialization
***/

HB_FUNC( HB_INETINIT )
{
   hb_socketInit();
#if defined( HB_INET_LINUX_INTERRUPT )
   signal( HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle );
#endif
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
   PHB_SOCKET Socket;
   HB_SOCKET_INIT( Socket, pSocket );

   if( HB_ISNUM( 1 ) )
      Socket->iTimeout = hb_parni( 1 );

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETCLOSE )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      if( Socket->com != HB_NO_SOCKET )
      {
         hb_socketShutdown( Socket->com, HB_SOCK_SHUT_RDWR );
         hb_retni( hb_socketClose( Socket->com ) );
         Socket->com = HB_NO_SOCKET;

#ifdef HB_INET_LINUX_INTERRUPT
         kill( 0, HB_INET_LINUX_INTERRUPT );
#endif
      }
      else
         hb_retni( -1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETFD )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      hb_retnint( Socket->com );

      if( hb_parl( 2 ) )
         Socket->com = HB_NO_SOCKET;
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/************************************************
* Socket data access & management
***/

HB_FUNC( HB_INETSTATUS )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( Socket->com == HB_NO_SOCKET ? -1 : 1 ); /* TODO: hb_retni( Socket->status ); */
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Prepared, but still not used; being in wait for comments
HB_FUNC( HB_INETSTATUSDESC )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( Socket->iErrorCode );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETERRORDESC )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retc( Socket->pszErrorText );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCLEARERROR )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      HB_SOCKET_ZERO_ERROR( Socket );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETCOUNT )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( Socket->iCount );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETADDRESS )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      char * szAddr = Socket->remote ?
            hb_socketAddrGetName( Socket->remote, Socket->remotelen ) : NULL;
      if( szAddr )
         hb_retc_buffer( szAddr );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETPORT )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      hb_retni( Socket->remote ?
                hb_socketAddrGetPort( Socket->remote, Socket->remotelen ) : 0 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETTIMEOUT )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      Socket->iTimeout = -1;
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETTIMELIMIT )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
      Socket->iTimeLimit = -1;
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETPERIODCALLBACK )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( Socket ) )
      {
         if( hb_socketGetSndBufSize( Socket->com, &iSize ) != 0 )
            iSize = -1;
      }
      hb_retni( iSize );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETGETRCVBUFSIZE )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( Socket ) )
      {
         if( hb_socketGetRcvBufSize( Socket->com, &iSize ) != 0 )
            iSize = -1;
      }
      hb_retni( iSize );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETSETSNDBUFSIZE )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( Socket ) )
      {
         iSize = hb_parni( 2 );
         hb_socketSetSndBufSize( Socket->com, iSize );
      }
      hb_retni( iSize );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_INETSETRCVBUFSIZE )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );

   if( Socket )
   {
      int iSize = -1;
      if( hb_inetIsOpen( Socket ) )
      {
         iSize = hb_parni( 2 );
         hb_socketSetRcvBufSize( Socket->com, iSize );
      }
      hb_retni( iSize );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}



/********************************************************************
* TCP receive and send functions
***/

static void s_inetRecvInternal( int iMode )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
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

   iReceived = 0;
   iTimeElapsed = 0;
   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      iLen = hb_socketRecv( Socket->com, buffer + iReceived, iMaxLen - iReceived,
                            0, Socket->iTimeout );
      if( iLen >= 0 )
      {
         iReceived += iLen;

         /* Called from InetRecv()? */
         if( iMode == 0 )
            break;
      }
      else if( iLen == -1 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
      {
         /* timed out; let's see if we have to run a cb routine */
         iTimeElapsed += Socket->iTimeout;

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
      }
   }
   while( iReceived < iMaxLen && iLen > 0 );

   Socket->iCount = iReceived;

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pResult     = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 4, HB_IT_NUMERIC );

   char cChar = '\0';
   char * Buffer;
   int iAllocated, iBufferSize, iMax;
   int iLen, iPatLen;
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

      iLen = hb_socketRecv( Socket->com, &cChar, 1, 0, Socket->iTimeout );
      if( iLen == -1 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
      {
         iTimeElapsed += Socket->iTimeout;

         if( Socket->pPeriodicBlock )
         {
            BOOL fResult;

            hb_execFromArray( Socket->pPeriodicBlock );
            fResult = hb_parl( -1 ) && hb_vmRequestQuery() == 0;

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
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

      iLen = hb_socketRecv( Socket->com, &cChar, 1, 0, Socket->iTimeout );
      if( iLen == -1 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
      {
         iTimeElapsed += Socket->iTimeout;
         if( Socket->pPeriodicBlock )
         {
            BOOL fResult;

            hb_execFromArray( Socket->pPeriodicBlock );
            fResult = hb_parl( -1 ) && hb_vmRequestQuery() == 0;

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
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
   int iVal;

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

   HB_SOCKET_ZERO_ERROR( Socket );
   iVal = hb_socketSelectRead( Socket->com, HB_ISNUM( 2 ) ? hb_parnint( 2 ) : 0 );
   if( iVal < 0 )
      HB_SOCKET_SET_ERROR( Socket );
   hb_retni( iVal );
}


static void s_inetSendInternal( int iMode )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
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

   HB_SOCKET_ZERO_ERROR( Socket );

   iSent = 0;
   iLen = 0;
   while( iSent < iSend )
   {
      iLen = hb_socketSend( Socket->com, Buffer + iSent, iSend - iSent, 0,
                            Socket->iTimeout );
      if( iLen > 0 )
      {
         iSent += iLen;
      }
      else if( iLen == -1 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
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
      PHB_ITEM pHosts = hb_socketGetHosts( szHost, HB_SOCK_PF_INET );

      if( pHosts )
         hb_itemReturnRelease( pHosts );
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
      PHB_ITEM pHosts = hb_socketGetAliases( szHost, HB_SOCK_PF_INET );

      if( pHosts )
         hb_itemReturnRelease( pHosts );
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
   PHB_SOCKET Socket = HB_PARSOCKET( 2 );
   PHB_ITEM pSocket = NULL;
   const char * szAddress;
   int iPort, iListen;

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
   Socket->com = hb_socketOpen( HB_SOCK_PF_INET, HB_SOCK_STREAM, 0 );
   if( Socket->com == HB_NO_SOCKET )
   {
      HB_SOCKET_SET_ERROR( Socket );
      if( pSocket )
         hb_itemReturnRelease( pSocket );
      else
         hb_itemReturn( hb_param( 2, HB_IT_ANY ) );
      return;
   }

   /* we'll be using only nonblocking sockets */
   /* hb_socketSetBlockingIO( Socket->com, FALSE ); */

   hb_socketSetReuseAddr( Socket->com, TRUE );

   iPort = hb_parni( 1 );
   szAddress = hb_parc( 2 );
   iListen = HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10;
   if( Socket->remote )
      hb_xfree( Socket->remote );
   if( hb_socketInetAddr( &Socket->remote, &Socket->remotelen,
                          szAddress ? szAddress : "255.255.255.255", iPort ) )
   {
      if( hb_socketBind( Socket->com, Socket->remote, Socket->remotelen ) == -1 )
      {
         HB_SOCKET_SET_ERROR( Socket );
         hb_socketClose( Socket->com );
         Socket->com = HB_NO_SOCKET;
      }
      else if( hb_socketListen( Socket->com, iListen ) == -1 )
      {
         HB_SOCKET_SET_ERROR( Socket );
         hb_socketClose( Socket->com );
         Socket->com = HB_NO_SOCKET;
      }
   }
   else
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_socketClose( Socket->com );
      Socket->com = HB_NO_SOCKET;
   }

   if( pSocket )
      hb_itemReturnRelease( pSocket );
   else
      hb_itemReturn( hb_param( 2, HB_IT_ANY ) );
}

HB_FUNC( HB_INETACCEPT )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
   PHB_SOCKET NewSocket;
   HB_SOCKET_T incoming;
   void * sa;
   unsigned len;

   if( Socket == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   else if( ! hb_inetIsOpen( Socket ) )
      return;

   /*
    * Accept can (and should) be asynchronously stopped by closing the
    * accepting socket. this will make the wait to terminate, and the
    * calling program will be notivfied through the status of the
    * returned socket.
    */

   HB_SOCKET_ZERO_ERROR( Socket );

   incoming = hb_socketAccept( Socket->com, &sa, &len, Socket->iTimeout );

   if( incoming == HB_NO_SOCKET )
   {
      if( hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      else
         HB_SOCKET_SET_ERROR( Socket );
   }
   else
   {
      PHB_ITEM pSocket = NULL;
      HB_SOCKET_INIT( NewSocket, pSocket );
      NewSocket->remote = sa;
      NewSocket->remotelen = len;
      NewSocket->com = incoming;
      /* we'll be using only nonblocking sockets */
      /* hb_socketSetBlockingIO( Socket->com, FALSE ); */
      hb_itemReturnRelease( pSocket );
   }
}

/**********************************************
* Client specific (connection functions)
****/

HB_FUNC( HB_INETCONNECT )
{
   const char * szHost = hb_parc( 1 );
   char * szAddr;
   PHB_SOCKET Socket = HB_PARSOCKET( 3 );
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 2 );

   if( szHost == NULL || iPort == 0 || ( Socket == NULL && ! HB_ISNIL( 3 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket )
   {
      if( Socket->com != HB_NO_SOCKET )
      {
         hb_socketClose( Socket->com );
         Socket->com = HB_NO_SOCKET;
      }
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
      HB_SOCKET_INIT( Socket, pSocket );

   szAddr = hb_socketResolveAddr( szHost, HB_SOCK_AF_INET );
   if( !szAddr )
      HB_SOCKET_SET_ERROR( Socket );
   else
   {
      /* Creates comm socket */
      Socket->com = hb_socketOpen( HB_SOCK_PF_INET, HB_SOCK_STREAM, 0 );
      if( Socket->com == HB_NO_SOCKET )
         HB_SOCKET_SET_ERROR( Socket );
      else
      {
         if( Socket->remote )
            hb_xfree( Socket->remote );
         if( hb_socketInetAddr( &Socket->remote, &Socket->remotelen,
                                szAddr, iPort ) )
            hb_inetConnect( Socket );
         else
            HB_SOCKET_SET_ERROR( Socket );
      }
      hb_xfree( szAddr );
   }

   if( pSocket )
      hb_itemReturnRelease( pSocket );
   else
      hb_itemReturn( hb_param( 3, HB_IT_ANY ) );
}

HB_FUNC( HB_INETCONNECTIP )
{
   const char * szHost = hb_parc( 1 );
   PHB_SOCKET Socket = HB_PARSOCKET( 3 );
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 2 );

   if( szHost == NULL || iPort == 0 || ( Socket == NULL && ! HB_ISNIL( 3 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( Socket )
   {
      if( Socket->com != HB_NO_SOCKET )
      {
         hb_socketClose( Socket->com );
         Socket->com = HB_NO_SOCKET;
      }
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
      HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
   Socket->com = hb_socketOpen( HB_SOCK_PF_INET, HB_SOCK_STREAM, 0 );
   if( Socket->com == HB_NO_SOCKET )
      HB_SOCKET_SET_ERROR( Socket );
   else
   {
      if( Socket->remote )
         hb_xfree( Socket->remote );
      if( hb_socketInetAddr( &Socket->remote, &Socket->remotelen,
                             szHost, iPort ) )
         hb_inetConnect( Socket );
      else
         HB_SOCKET_SET_ERROR( Socket );
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
   PHB_SOCKET Socket;
   PHB_ITEM pSocket = NULL;
   int iPort = hb_parni( 1 );
   const char * szAddress;

   /* Parameter error checking */
   if( iPort == 0 || ( hb_pcount() >= 4 && ! HB_ISCHAR( 4 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
   Socket->com = hb_socketOpen( HB_SOCK_PF_INET, HB_SOCK_DGRAM, HB_SOCK_IPPROTO_UDP );
   if( Socket->com == HB_NO_SOCKET )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_itemReturnRelease( pSocket );
      return;
   }

   hb_socketSetReuseAddr( Socket->com, TRUE );

   /* Setting broadcast if needed. */
   if( hb_parl( 3 ) )
      hb_socketSetBroadcast( Socket->com, TRUE );

   szAddress = hb_parc( 2 );
   if( Socket->remote )
      hb_xfree( Socket->remote );
   if( !hb_socketInetAddr( &Socket->remote, &Socket->remotelen,
                           szAddress ? szAddress : "255.255.255.255", iPort ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_socketClose( Socket->com );
      Socket->com = HB_NO_SOCKET;
   }
   else if( hb_socketBind( Socket->com, Socket->remote, Socket->remotelen ) == -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_socketClose( Socket->com );
      Socket->com = HB_NO_SOCKET;
   }
   else if( hb_pcount() >= 4 )
   {
      if( hb_socketSetMulticast( Socket->com, HB_SOCK_PF_INET, hb_parc( 4 ) ) != 0 )
         HB_SOCKET_SET_ERROR( Socket );
   }

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETDGRAM )
{
   PHB_SOCKET Socket;
   PHB_ITEM pSocket = NULL;

   HB_SOCKET_INIT( Socket, pSocket );

   /* Creates comm socket */
   Socket->com = hb_socketOpen( HB_SOCK_PF_INET, HB_SOCK_DGRAM, HB_SOCK_IPPROTO_UDP );
   if( Socket->com == HB_NO_SOCKET )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_itemReturnRelease( pSocket );
      return;
   }

   /* Setting broadcast if needed. */
   if( hb_parl( 1 ) )
      hb_socketSetBroadcast( Socket->com, TRUE );

   /* we'll be using non blocking sockets in all functions */
   /* hb_socketSetBlockingIO( Socket->com, FALSE ); */

   hb_itemReturnRelease( pSocket );
}

HB_FUNC( HB_INETDGRAMSEND )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
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

   if( Socket->remote )
      hb_xfree( Socket->remote );
   if( !hb_socketInetAddr( &Socket->remote, &Socket->remotelen, szAddress, iPort ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->iCount = 0;
      hb_retni( 0 );
      return;
   }

   szBuffer = hb_itemGetCPtr( pBuffer );
   iLen = ( int ) hb_itemGetCLen( pBuffer );
   if( HB_ISNUM( 5 ) )
   {
      int iMaxLen = hb_parni( 5 );
      if( iMaxLen < iLen )
         iLen = iMaxLen;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   Socket->iCount = hb_socketSendTo( Socket->com, szBuffer, iLen, 0,
                                     Socket->remote, Socket->remotelen,
                                     Socket->iTimeout );
   hb_retni( Socket->iCount );

   if( Socket->iCount == -1 )
   {
      Socket->iCount = 0;
      if( hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      else
         HB_SOCKET_SET_ERROR( Socket );
   }
}

HB_FUNC( HB_INETDGRAMRECV )
{
   PHB_SOCKET Socket = HB_PARSOCKET( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   int iTimeElapsed = 0;
   int iLen, iMaxLen;
   char * Buffer;
   ULONG ulLen;
   BOOL fRepeat;

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

   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      fRepeat = FALSE;
      if( Socket->remote )
         hb_xfree( Socket->remote );
      iLen = hb_socketRecvFrom( Socket->com, Buffer, iMaxLen, 0,
                                &Socket->remote, &Socket->remotelen,
                                Socket->iTimeout );
      iTimeElapsed += Socket->iTimeout;
      if( Socket->pPeriodicBlock )
      {
         hb_execFromArray( Socket->pPeriodicBlock );
         /* do we continue? */
         fRepeat = hb_parl( -1 ) && hb_vmRequestQuery() == 0 &&
                   ( Socket->iTimeLimit == -1 || iTimeElapsed < Socket->iTimeLimit );
      }
   }
   while( fRepeat );

   if( iLen == -1 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT )
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
