/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Not SCTP (Stream Control Transmission Protocol)
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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


#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbnsctp.h"


PHB_NSCTP hb_nsctpCreate( HB_SOCKET sd )
{
   PHB_NSCTP pSocket;

   pSocket = ( PHB_NSCTP ) hb_xgrab( sizeof( HB_NSCTP ) );
   memset( pSocket, 0, sizeof( HB_NSCTP ) );
   pSocket->sd = sd;
   pSocket->nLimit = 1024;
   return pSocket;
}


void hb_nsctpDestroy( PHB_NSCTP pSocket )
{
   if( pSocket->pSendBuffer )
      hb_xfree( pSocket->pSendBuffer );

   if( pSocket->pRecvBuffer )
      hb_xfree( pSocket->pRecvBuffer );

   hb_xfree( pSocket );
}


void hb_nsctpSetLimit( PHB_NSCTP pSocket, HB_SIZE nLimit )
{
   pSocket->nLimit = nLimit;
}


int hb_nsctpError( PHB_NSCTP pSocket )
{
   return pSocket->iError;
}


HB_BOOL hb_nsctpSend( PHB_NSCTP pSocket, const void * data, HB_SIZE len, HB_MAXINT timeout )
{
   HB_MAXINT  nTime = 0;
   long       lSend;

   if( ! pSocket->pSendBuffer )
   {
      pSocket->pSendBuffer = ( char * ) hb_xgrab( len + 4 );
      HB_PUT_LE_UINT32( pSocket->pSendBuffer, len );
      hb_xmemcpy( pSocket->pSendBuffer + 4, data, len );
      pSocket->nSendLen = len + 4;
      pSocket->nSendPos = 0;
   }

   if( timeout > 0 )
      nTime = ( HB_MAXINT ) hb_dateMilliSeconds() + timeout;
   
   for( ; ; ) 
   {
      if( pSocket->nSendLen - pSocket->nSendPos < ( HB_SIZE ) LONG_MAX )
         lSend = pSocket->nSendLen - pSocket->nSendPos;
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
         pSocket->iError = 0;
         return HB_TRUE;
      }
      if( timeout == 0 || 
          ( timeout > 0 && ( timeout = nTime - ( HB_MAXINT ) hb_dateMilliSeconds() ) <= 0 ) )
      {
         pSocket->iError = HB_SOCKET_ERR_TIMEOUT;
         return HB_FALSE;
      }
   }
}


HB_BOOL hb_nsctpRecv( PHB_NSCTP pSocket, void ** data, HB_SIZE * len, HB_MAXINT timeout )
{
   HB_MAXINT  nTime = 0;
   long       lRecv;

   if( ! pSocket->pRecvBuffer )
   {
      pSocket->pRecvBuffer = ( char * ) hb_xgrab( 4 );
      pSocket->nRecvLen = 0;
      pSocket->fRecvHasSize = HB_FALSE;
   }

   if( timeout > 0 )
      nTime = ( HB_MAXINT ) hb_dateMilliSeconds() + timeout;

   for( ; ; )
   {
      if( ! pSocket->fRecvHasSize )
      {
         lRecv = 4 - pSocket->nRecvLen;
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
            pSocket->iError = HB_NSCTP_ERR_TOOLARGE;
            hb_xfree( pSocket->pRecvBuffer );
            pSocket->pRecvBuffer = NULL;
            return HB_FALSE;
         }
      
         pSocket->nRecvLen = 0;
         pSocket->fRecvHasSize = HB_TRUE;
         if( pSocket->nRecvSize != 4 )
            pSocket->pRecvBuffer = ( char * ) hb_xrealloc( pSocket->pRecvBuffer, pSocket->nRecvSize );
      }
      
      if( pSocket->nRecvSize - pSocket->nRecvLen < ( HB_SIZE ) LONG_MAX )
         lRecv = pSocket->nRecvSize - pSocket->nRecvLen;
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
         * data = pSocket->pRecvBuffer;
         * len = pSocket->nRecvLen;
         pSocket->pRecvBuffer = NULL;
         pSocket->iError = 0;
         return HB_TRUE;
      }
      if( timeout == 0 || 
          ( timeout > 0 && ( timeout = nTime - ( HB_MAXINT ) hb_dateMilliSeconds() ) <= 0 ) )
      {
         pSocket->iError = HB_SOCKET_ERR_TIMEOUT;
         return HB_FALSE;
      }
   }
}


HB_SIZE hb_nsctpSendLen( PHB_NSCTP pSocket )
{
   return pSocket->nSendLen - pSocket->nSendPos;
}


HB_SIZE hb_nsctpRecvLen( PHB_NSCTP pSocket )
{
   return pSocket->nRecvLen;
}
