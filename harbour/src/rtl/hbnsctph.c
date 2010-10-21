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


 /*

  Idea and protocol
  =================
  Very often it is required to accept the whole data structure/packet
  from TCP connection. Because of stream nature of TCP, this requires 
  additional steps from application like start/end marker, or sending 
  length of structure before the structure. The latter simple approach 
  was used in NSCTP. Protocol can easily be described by simple Clipper 
  expression:
     BIN2L(LEN(cData)) + cData

  Future extensions: Protocol is limitted to 4GB size for a single NSCTP 
  "packet". This can be extended in future to use highest bit of length 
  (or some highest length values 2^32-1, etc) as a special marker for 
  64-bit or similar length encoding. 

  Name
  ====
  Name is abbreviature for Not SCTP. SCTP is abbreviature for Stream 
  Contol Transfer Protocol. Wikipedia writes:
     Stream Control Transmission Protocol (SCTP) is a Transport Layer 
  protocol, serving in a similar role to the popular protocols 
  Transmission Control Protocol (TCP) and User Datagram Protocol (UDP). 
  It provides some of the same service features of both: it is 
  message-oriented like UDP and ensures reliable, in-sequence transport 
  of messages with congestion control like TCP.
     All above is true for NSCTP, it helps to communicate in messages
  (up to 4GB size), and ensures reliability, in-sequence transport, etc
  (because it is based on TCP). But NSCTP is Not SCTP, so, it's NSCTP :)
     I find NSCTP name a good for API like this. It does not occupy 
  some more general names like hb_psocket*, it is shorter. I can write
  hbnsctph.c file without additional need to delete some letters to fit 
  into 8.3 format. Yes, NSCTP looks a little like a random set of 
  letters after you see it for the first time, but it should for such 
  not widely accepted protocol. After you write a first app using NSCTP, 
  you'll be able to type and pronounce it just easily as TCP, UDP, or 
  HTTP.

  Public functions and procedures
  ===============================
  hb_nsctpCreate( hSocket ) --> hNSCTP
  hb_nsctpDestroy( hNSTP )
     Destroys only NSCTP related structures. Socket remains open and
     it is possible to continue data transfers using hb_socket*()
     functions.
  hb_nsctpError( hNSCTP ) --> nError
     nError value is compatible with Harbour socket error API,
     the only new error code (until now) is hb_nsctp_ERROR_TOOLARGE
  hb_nsctpSetLimit( hNSCTP, nLimit )
     Sets size limit for receiving data. Sending 4 bytes containing 
     large 32-bit value makes receiving application to alllocate a 
     large memory block for storage of data to be received. It is very 
     easy to crash  application (or system) using such protocol and 
     logic. hb_nsctpSetLimit() helps to protect against such attacks.
     On hb_nsctpCreate() limit is set to 1024 bytes. This is enough 
     for server/client authentification. After successful 
     authentification server can increase size limit and large NSCTP 
     packets can be used.
  hb_nsctpSend( hNSCTP, cBuf [, nTimeout = FOREVER ] ) --> lSuccess
  hb_nsctpRecv( hNSCTP, @cBuf [, nTimeout = FOREVER ] ) --> lSuccess
  hb_nsctpSendLen( hNSCTP ) --> nBytesSent
     Useful for drawing progress bars, etc.
  hb_nsctpRecvLen( hNSCTP ) --> nBytesReceived
     Useful for drawing progress bars, etc.

  Sample code
  ===========
  // send sample
  hNSCTP := hb_nsctpCreate( hSocket )
  DO WHILE ! ( lI := hb_nsctpSend( hNSCTP, cData, nTimeout ) ) .AND. ;
           hb_nsctpError( hNSCTP ) == HB_SOCKET_ERR_TIMEOUT )
    // draw progressbar using hb_nsctpSendLen( hNSCTP )
  ENDDO
  IF lI   // or hb_nsctpError( hNSCTP ) == 0
    // Sent OK
  ELSE
    // error
  ENDIF
  hb_hsctpDestroy( hNSCTP )
  

  // recv sample
  DO WHILE ! ( lI := hb_nsctpRecv( hNSCTP, @cData, nTimeout ) ) .AND. ;
           hb_nsctpError( hNSCTP ) == HB_SOCKET_ERR_TIMEOUT )
    // draw progressbar using hb_nsctpRecvLen( hNSCTP )
  ENDDO
  IF lI
    // Rcvd OK, data in cData
  ELSE
    IF hb_nsctpError( hNSCTP ) == 0
      // remote side shutdown connection
    ELSE
      // error
    ENDIF
  ENDIF

*/


#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbnsctp.h"

typedef struct
{
   PHB_NSCTP    pSocket;
   PHB_ITEM     pItemSocket;
} HB_NSCTP_GC, * PHB_NSCTP_GC;


static HB_GARBAGE_FUNC( hb_nsctp_destructor )
{
   PHB_NSCTP_GC pGC = ( PHB_NSCTP_GC ) Cargo;

   if( pGC->pSocket )
   {
      hb_nsctpDestroy( pGC->pSocket );
      pGC->pSocket = NULL;
   }
   if( pGC->pItemSocket )
   {
      hb_itemRelease( pGC->pItemSocket );
      pGC->pItemSocket = NULL;
   }
}


static HB_GARBAGE_FUNC( hb_nsctp_mark )
{
   PHB_NSCTP_GC pGC = ( PHB_NSCTP_GC ) Cargo;

   if( pGC->pItemSocket )
      hb_gcMark( pGC->pItemSocket );
}


static const HB_GC_FUNCS s_gcPSocketFuncs =
{
   hb_nsctp_destructor,
   hb_nsctp_mark
};


HB_FUNC( HB_NSCTPCREATE )
{
   HB_SOCKET sd;
   PHB_NSCTP_GC pGC;
   PHB_ITEM pItem;

   pItem = hb_param( 1, HB_IT_POINTER );
   if( ! pItem || ( sd = hb_socketItemGet( pItem ) ) == HB_NO_SOCKET )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   pGC = ( PHB_NSCTP_GC ) hb_gcAllocate( sizeof( HB_NSCTP_GC ), &s_gcPSocketFuncs );
   pGC->pSocket = hb_nsctpCreate( sd );
   pGC->pItemSocket = hb_itemNew( pItem );
   hb_gcUnlock( pGC->pItemSocket );
   hb_retptrGC( pGC );
}


HB_FUNC( HB_NSCTPDESTROY )
{
   PHB_NSCTP_GC pGC;

   pGC = ( PHB_NSCTP_GC ) hb_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   hb_nsctpDestroy( pGC->pSocket );
   pGC->pSocket = NULL;
   hb_itemRelease( pGC->pItemSocket );
   pGC->pItemSocket = NULL;
}


HB_FUNC( HB_NSCTPERROR )
{
   PHB_NSCTP_GC pGC;

   pGC = ( PHB_NSCTP_GC ) hb_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   hb_retni( hb_nsctpError( pGC->pSocket ) );
}


HB_FUNC( HB_NSCTPSETLIMIT )
{
   PHB_NSCTP_GC pGC;

   pGC = ( PHB_NSCTP_GC ) hb_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   hb_nsctpSetLimit( pGC->pSocket, hb_parns( 2 ) );
}


HB_FUNC( HB_NSCTPSEND )
{
   PHB_NSCTP_GC pGC;
   PHB_ITEM pData;

   pGC = ( PHB_NSCTP_GC ) hb_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket || hb_socketItemGet( pGC->pItemSocket ) == HB_NO_SOCKET )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   pData = hb_param( 2, HB_IT_STRING );
   hb_retl( hb_nsctpSend( pGC->pSocket, pData ? hb_itemGetCPtr( pData ) : "", 
                          hb_itemGetCLen( pData ), hb_parnintdef( 3, -1 ) ) );
}


HB_FUNC( HB_NSCTPRECV )
{
   PHB_NSCTP_GC pGC;
   HB_BOOL bRet;
   void * data;
   HB_SIZE len;

   pGC = ( PHB_NSCTP_GC ) hb_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket || hb_socketItemGet( pGC->pItemSocket ) == HB_NO_SOCKET )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   bRet = hb_nsctpRecv( pGC->pSocket, &data, &len, hb_parnintdef( 3, -1 ) );
   if( bRet )
   {
      if( HB_ISBYREF( 2 ) )
         hb_storclen( ( char * ) data, len, 2 );
      hb_xfree( data );
   }
   hb_retl( bRet );
}


HB_FUNC( HB_NSCTPSENDLEN )
{
   PHB_NSCTP_GC pGC;

   pGC = ( PHB_NSCTP_GC ) hb_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   hb_retns( hb_nsctpSendLen( pGC->pSocket ) );
}



HB_FUNC( HB_NSCTPRECVLEN )
{
   PHB_NSCTP_GC pGC;

   pGC = ( PHB_NSCTP_GC ) hb_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   hb_retns( hb_nsctpRecvLen( pGC->pSocket ) );
}
