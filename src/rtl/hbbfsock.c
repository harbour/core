/*
 * Harbour extended socket filter with BlowFish encryption
 *
 * Copyright 2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* this has to be declared before hbsocket.h is included */
#define _HB_SOCKEX_IMPLEMENTATION_

#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbsocket.h"
#include "hbbfish.h"
#include "hbinit.h"

#define HB_BFSOCK_READAHEAD   0x40
#define HB_BFSOCK_WRBUFSIZE   4096

#define HB_BFSOCK_GET( p )    ( ( PHB_SOCKEX_BF ) p->cargo )

typedef struct
{
   PHB_SOCKEX     sock;

   HB_BLOWFISH    bf;
   HB_BYTE        encryptkey[ HB_BF_CIPHERBLOCK ];
   HB_BYTE        decryptkey[ HB_BF_CIPHERBLOCK ];
   HB_BYTE        encounter[ HB_BF_CIPHERBLOCK ];
   HB_BYTE        decounter[ HB_BF_CIPHERBLOCK ];
   HB_BYTE        buffer[ HB_BFSOCK_WRBUFSIZE ];
   long           inbuffer;
   int            encoded;
   int            decoded;
}
HB_SOCKEX_BF, * PHB_SOCKEX_BF;

static void s_bf_hash( const HB_BLOWFISH * bf,
                       HB_BYTE * vect, HB_BYTE * counter )
{
   HB_U32 xl, xr, cl, cr;

   cl = xl = HB_GET_BE_UINT32( &counter[ 0 ] );
   cr = xr = HB_GET_BE_UINT32( &counter[ 4 ] );
   ++cr;
   HB_PUT_BE_UINT32( &counter[ 4 ], cr );
   if( cr == 0 )
   {
      ++cl;
      HB_PUT_BE_UINT32( &counter[ 0 ], cl );
   }
   hb_blowfishEncrypt( bf, &xl, &xr );
   HB_PUT_BE_UINT32( &vect[ 0 ], xl );
   HB_PUT_BE_UINT32( &vect[ 4 ], xr );
}

static long s_bf_send( PHB_SOCKEX_BF pBF, HB_MAXINT timeout )
{
   long lSent = 0, len = pBF->inbuffer;

   while( lSent < len )
   {
      long l = hb_sockexWrite( pBF->sock, pBF->buffer + lSent, len - lSent, timeout );
      if( l <= 0 )
      {
         switch( hb_socketGetError() )
         {
            case HB_SOCKET_ERR_TIMEOUT:
            case HB_SOCKET_ERR_AGAIN:
            case HB_SOCKET_ERR_TRYAGAIN:
               break;
            default:
               lSent = -1;
               break;
         }
         break;
      }
      lSent += l;
      if( timeout > 0 )
         timeout = 0;
   }

   if( lSent > 0 )
   {
      if( lSent < len )
         memmove( pBF->buffer, pBF->buffer + lSent, len - lSent );
      pBF->inbuffer -= lSent;
   }

   return lSent;
}

/* socket filter */

static long s_sockexRead( PHB_SOCKEX pSock, void * data, long len, HB_MAXINT timeout )
{
   PHB_SOCKEX_BF pBF = HB_BFSOCK_GET( pSock );
   long lRecv;

   if( pSock->inbuffer > 0 && len > 0 )
   {
      lRecv = HB_MIN( pSock->inbuffer, len );
      memcpy( data, pSock->buffer + pSock->posbuffer, lRecv );
      if( ( pSock->inbuffer -= lRecv ) > 0 )
         pSock->posbuffer += lRecv;
      else
         pSock->posbuffer = 0;
   }
   else
   {
      lRecv = hb_sockexRead( pBF->sock, data, len, timeout );
      if( lRecv > 0 )
      {
         HB_BYTE * pData = ( HB_BYTE * ) data;
         long l;

         for( l = 0; l < lRecv; ++l )
         {
            if( ( pBF->decoded & ( HB_BF_CIPHERBLOCK - 1 ) ) == 0 )
            {
               s_bf_hash( &pBF->bf, pBF->decryptkey, pBF->decounter );
               pBF->decoded = 0;
            }
            pData[ l ] ^= pBF->decryptkey[ pBF->decoded++ ];
         }
      }
   }
   return lRecv;
}

static long s_sockexWrite( PHB_SOCKEX pSock, const void * data, long len, HB_MAXINT timeout )
{
   PHB_SOCKEX_BF pBF = HB_BFSOCK_GET( pSock );
   const HB_BYTE * pData = ( const HB_BYTE * ) data;
   long lWritten = 0, lDone;

   for( lDone = 0; lDone < len; ++lDone )
   {
      if( pBF->inbuffer == HB_BFSOCK_WRBUFSIZE )
      {
         lWritten = s_bf_send( pBF, timeout );
         if( lWritten <= 0 )
            break;
         timeout = 0;
      }
      if( ( pBF->encoded & ( HB_BF_CIPHERBLOCK - 1 ) ) == 0 )
      {
         s_bf_hash( &pBF->bf, pBF->encryptkey, pBF->encounter );
         pBF->encoded = 0;
      }
      pBF->buffer[ pBF->inbuffer++ ] = pData[ lDone ] ^ pBF->encryptkey[ pBF->encoded++ ];
   }

   return lWritten >= 0 ? lDone : lWritten;
}

static long s_sockexFlush( PHB_SOCKEX pSock, HB_MAXINT timeout, HB_BOOL fSync )
{
   PHB_SOCKEX_BF pBF = HB_BFSOCK_GET( pSock );

   while( pBF->inbuffer > 0 )
   {
      if( s_bf_send( pBF, timeout ) <= 0 )
         break;
   }
   return pBF->inbuffer + hb_sockexFlush( pBF->sock, timeout, fSync );
}

static int s_sockexCanRead( PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout )
{
   return pSock->inbuffer > 0 ? 1 :
          hb_sockexCanRead( HB_BFSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static int s_sockexCanWrite( PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout )
{
   return hb_sockexCanWrite( HB_BFSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static char * s_sockexName( PHB_SOCKEX pSock )
{
   char * pszName = hb_sockexIsRaw( HB_BFSOCK_GET( pSock )->sock ) ? NULL :
                    hb_sockexName( HB_BFSOCK_GET( pSock )->sock );
   if( pszName )
   {
      char * pszFree = pszName;
      pszName = hb_xstrcpy( NULL, pSock->pFilter->pszName, "|", pszName, NULL );
      hb_xfree( pszFree );
   }
   else
      pszName = hb_strdup( pSock->pFilter->pszName );

   return pszName;
}

static const char * s_sockexErrorStr( PHB_SOCKEX pSock, int iError )
{
   return hb_sockexErrorStr( HB_BFSOCK_GET( pSock )->sock, iError );
}

static int s_sockexClose( PHB_SOCKEX pSock, HB_BOOL fClose )
{
   PHB_SOCKEX_BF pBF = HB_BFSOCK_GET( pSock );
   int iResult = 0;

   if( pBF )
   {
      if( pBF->sock )
         s_sockexFlush( pSock, HB_MAX( 15000, pSock->iAutoFlush ), HB_TRUE );

      if( pBF->sock )
      {
         if( pSock->fShutDown )
            pBF->sock->fShutDown = HB_TRUE;
         if( pSock->iAutoFlush != 0 && pBF->sock->iAutoFlush == 0 )
            pBF->sock->iAutoFlush = pSock->iAutoFlush;
         iResult = hb_sockexClose( pBF->sock, fClose );
      }
      memset( pBF, 0, sizeof( *pBF ) );
      hb_xfree( pBF );
   }
   /* call hb_sockexRawClear() with fClose = HB_FALSE because
      hb_sockexClose() already closed real socket */
   hb_sockexRawClear( pSock, HB_FALSE );
   hb_xfree( pSock );

   return iResult;
}

static PHB_SOCKEX s_sockexNext( PHB_SOCKEX pSock, PHB_ITEM pParams );

static PHB_SOCKEX s_sockexNew( HB_SOCKET sd, PHB_ITEM pParams )
{
   PHB_SOCKEX pSock, pSockNew = NULL;

   pSock = hb_sockexNew( sd, NULL, pParams );
   if( pSock )
   {
      pSockNew = s_sockexNext( pSock, pParams );
      if( pSockNew == NULL )
         hb_sockexClose( pSock, HB_FALSE );
   }

   return pSockNew;
}

static const HB_SOCKET_FILTER s_sockFilter =
{
   "BFSOCK",
   s_sockexNew,
   s_sockexNext,
   s_sockexClose,
   s_sockexRead,
   s_sockexWrite,
   s_sockexFlush,
   s_sockexCanRead,
   s_sockexCanWrite,
   s_sockexName,
   s_sockexErrorStr
};

static PHB_SOCKEX s_sockexNext( PHB_SOCKEX pSock, PHB_ITEM pParams )
{
   PHB_SOCKEX pSockNew = NULL;

   if( pSock )
   {
      const void * keydata = NULL, * iv = NULL;
      int keylen = 0, ivlen = 0;

      hb_socekxParamsGetStd( pParams, &keydata, &keylen, &iv, &ivlen, NULL, NULL );
      if( keylen > 0 )
      {
         PHB_SOCKEX_BF pBF = ( PHB_SOCKEX_BF ) hb_xgrabz( sizeof( HB_SOCKEX_BF ) );
         const HB_BYTE * pVect = ( const HB_BYTE * ) ( ivlen > 0 ? iv : NULL );
         int i;

         hb_blowfishInit( &pBF->bf, keydata, keylen );
         for( i = 0; i < HB_BF_CIPHERBLOCK; ++i )
         {
            if( pVect && ivlen > 0 )
               pBF->encounter[ i ] =
               pBF->decounter[ i ] = pVect[ i % ivlen ];
            else
               pBF->encounter[ i ] =
               pBF->decounter[ i ] = ( HB_BYTE ) i;
         }

         pSockNew = ( PHB_SOCKEX ) hb_xgrabz( sizeof( HB_SOCKEX ) );
         pSockNew->sd = pSock->sd;
         pSockNew->fRedirAll = HB_TRUE;
         pSockNew->fShutDown = pSock->fShutDown;
         pSockNew->iAutoFlush = pSock->iAutoFlush;
         pSockNew->pFilter = &s_sockFilter;
         pSockNew->cargo = ( void * ) pBF;
         pBF->sock = pSock;
         hb_socekxParamsInit( pSockNew, pParams );
      }
   }

   return pSockNew;
}

/* hb_socketNewBFSock( <pSocket>, [<hParams>] ) -> <pSocket> */
HB_FUNC( HB_SOCKETNEWBFSOCK )
{
   PHB_SOCKEX pSock = hb_sockexParam( 1 );

   if( pSock )
   {
      pSock = s_sockexNext( pSock, hb_param( 2, HB_IT_HASH ) );
      if( pSock )
      {
         hb_sockexItemClear( hb_param( 1, HB_IT_POINTER ) );
         hb_sockexItemPut( hb_param( -1, HB_IT_ANY ), pSock );
      }
   }
}

HB_CALL_ON_STARTUP_BEGIN( _hb_bfsock_init_ )
   hb_sockexRegister( &s_sockFilter );
HB_CALL_ON_STARTUP_END( _hb_bfsock_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_bfsock_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_bfsock_init_ )
   #include "hbiniseg.h"
#endif
