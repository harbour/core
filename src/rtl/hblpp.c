/*
 * Length Prefix Protocol
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hblpp.h"

PHB_LPP hb_lppCreate( HB_SOCKET sd )
{
   PHB_LPP pSocket;

   pSocket         = ( PHB_LPP ) hb_xgrabz( sizeof( HB_LPP ) );
   pSocket->sd     = sd;
   pSocket->nLimit = 1024;
   return pSocket;
}

void hb_lppDestroy( PHB_LPP pSocket )
{
   if( pSocket->pSendBuffer )
      hb_xfree( pSocket->pSendBuffer );

   if( pSocket->pRecvBuffer )
      hb_xfree( pSocket->pRecvBuffer );

   hb_xfree( pSocket );
}

void hb_lppSetLimit( PHB_LPP pSocket, HB_SIZE nLimit )
{
   pSocket->nLimit = nLimit;
}

int hb_lppError( PHB_LPP pSocket )
{
   return pSocket->iError;
}

HB_BOOL hb_lppSend( PHB_LPP pSocket, const void * data, HB_SIZE len, HB_MAXINT timeout )
{
   HB_MAXUINT timer;
   long       lSend;

   if( ! pSocket->pSendBuffer )
   {
      pSocket->pSendBuffer = ( char * ) hb_xgrab( len + 4 );
      HB_PUT_LE_UINT32( pSocket->pSendBuffer, len );
      hb_xmemcpy( pSocket->pSendBuffer + 4, data, len );
      pSocket->nSendLen = len + 4;
      pSocket->nSendPos = 0;
   }

   timer = hb_timerInit( timeout );

   for( ;; )
   {
      if( pSocket->nSendLen - pSocket->nSendPos < ( HB_SIZE ) LONG_MAX )
         lSend = ( long ) ( pSocket->nSendLen - pSocket->nSendPos );
      else
         lSend = LONG_MAX;

      lSend = hb_socketSend( pSocket->sd, pSocket->pSendBuffer + pSocket->nSendPos, lSend, 0, timeout );
      if( lSend == -1 )
      {
         pSocket->iError = hb_socketGetError();
         return HB_FALSE;
      }
      pSocket->nSendPos += lSend;
      if( pSocket->nSendPos == pSocket->nSendLen )
      {
         hb_xfree( pSocket->pSendBuffer );
         pSocket->pSendBuffer = NULL;
         pSocket->iError      = 0;
         return HB_TRUE;
      }
      if( ( timeout = hb_timerTest( timeout, &timer ) ) == 0 )
      {
         pSocket->iError = HB_SOCKET_ERR_TIMEOUT;
         return HB_FALSE;
      }
   }
}

HB_BOOL hb_lppRecv( PHB_LPP pSocket, void ** data, HB_SIZE * len, HB_MAXINT timeout )
{
   HB_MAXUINT timer;
   long       lRecv;

   if( ! pSocket->pRecvBuffer )
   {
      pSocket->pRecvBuffer  = ( char * ) hb_xgrab( 4 );
      pSocket->nRecvLen     = 0;
      pSocket->fRecvHasSize = HB_FALSE;
   }

   timer = hb_timerInit( timeout );

   for( ;; )
   {
      if( ! pSocket->fRecvHasSize )
      {
         lRecv = ( long ) ( 4 - pSocket->nRecvLen );
         lRecv = hb_socketRecv( pSocket->sd, pSocket->pRecvBuffer + pSocket->nRecvLen, lRecv, 0, timeout );
         if( lRecv == -1 )
         {
            pSocket->iError = hb_socketGetError();
            return HB_FALSE;
         }
         else if( lRecv == 0 )
         {
            /* peer closed connection */
            pSocket->iError = 0;
            return HB_FALSE;
         }

         pSocket->nRecvLen += lRecv;
         if( pSocket->nRecvLen < 4 )
         {
            pSocket->iError = HB_SOCKET_ERR_TIMEOUT;
            return HB_FALSE;
         }

         pSocket->nRecvSize = HB_GET_UINT32( pSocket->pRecvBuffer );

         if( pSocket->nLimit && pSocket->nRecvSize > pSocket->nLimit )
         {
            /* protection against remote memory exhaust attack */
            pSocket->iError = HB_LPP_ERR_TOOLARGE;
            hb_xfree( pSocket->pRecvBuffer );
            pSocket->pRecvBuffer = NULL;
            return HB_FALSE;
         }

         pSocket->nRecvLen     = 0;
         pSocket->fRecvHasSize = HB_TRUE;
         if( pSocket->nRecvSize != 4 )
            pSocket->pRecvBuffer = ( char * ) hb_xrealloc( pSocket->pRecvBuffer, pSocket->nRecvSize );
      }

      if( pSocket->nRecvSize - pSocket->nRecvLen < ( HB_SIZE ) LONG_MAX )
         lRecv = ( long ) ( pSocket->nRecvSize - pSocket->nRecvLen );
      else
         lRecv = LONG_MAX;

      lRecv = hb_socketRecv( pSocket->sd, pSocket->pRecvBuffer + pSocket->nRecvLen, lRecv, 0, timeout );
      if( lRecv == -1 )
      {
         pSocket->iError = hb_socketGetError();
         return HB_FALSE;
      }
      else if( lRecv == 0 )
      {
         /* peer closed connection */
         pSocket->iError = 0;
         return HB_FALSE;
      }

      pSocket->nRecvLen += lRecv;
      if( pSocket->nRecvSize == pSocket->nRecvLen )
      {
         *data = pSocket->pRecvBuffer;
         *len  = pSocket->nRecvLen;
         pSocket->pRecvBuffer = NULL;
         pSocket->iError      = 0;
         return HB_TRUE;
      }
      if( ( timeout = hb_timerTest( timeout, &timer ) ) == 0 )
      {
         pSocket->iError = HB_SOCKET_ERR_TIMEOUT;
         return HB_FALSE;
      }
   }
}

HB_SIZE hb_lppSendLen( PHB_LPP pSocket )
{
   return pSocket->nSendLen - pSocket->nSendPos;
}

HB_SIZE hb_lppRecvLen( PHB_LPP pSocket )
{
   return pSocket->nRecvLen;
}
