/*
 * Harbour Project source code:
 *    socket C API
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_SOCKET_H_
#define HB_SOCKET_H_

#include "hbapi.h"
#include "hbsocket.ch"

HB_EXTERN_BEGIN

#if defined( HB_OS_WIN ) && ! defined( HB_OS_UNIX )
   typedef HB_PTRUINT   HB_SOCKET;
#else
   typedef int          HB_SOCKET;
#endif

typedef HB_SOCKET ( * HB_SOCKET_FUNC )( PHB_ITEM );

#define HB_NO_SOCKET          ( ( HB_SOCKET ) -1 )

extern HB_EXPORT int          hb_socketInit( void );
extern HB_EXPORT void         hb_socketCleanup( void );
extern HB_EXPORT int          hb_socketGetError( void );
extern HB_EXPORT int          hb_socketGetOsError( void );
extern HB_EXPORT const char * hb_socketErrorStr( int iError );
extern HB_EXPORT int          hb_socketGetAddrFamily( const void * pSockAddr, unsigned len );
extern HB_EXPORT HB_BOOL      hb_socketLocalAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr );
extern HB_EXPORT HB_BOOL      hb_socketInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
extern HB_EXPORT HB_BOOL      hb_socketInet6Addr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
extern HB_EXPORT char *       hb_socketAddrGetName( const void * pSockAddr, unsigned len );
extern HB_EXPORT HB_BOOL      hb_socketResolveInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
extern HB_EXPORT char *       hb_socketResolveAddr( const char * szAddr, int af );
extern HB_EXPORT PHB_ITEM     hb_socketGetHosts( const char * szAddr, int af );
extern HB_EXPORT PHB_ITEM     hb_socketGetAliases( const char * szAddr, int af );
extern HB_EXPORT char *       hb_socketGetHostName( const void * pSockAddr, unsigned len );
extern HB_EXPORT PHB_ITEM     hb_socketGetIFaces( int af, HB_BOOL fNoAliases );
extern HB_EXPORT int          hb_socketAddrGetPort( const void * pSockAddr, unsigned len );
extern HB_EXPORT HB_BOOL      hb_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PHB_ITEM pAddrItm );
extern HB_EXPORT PHB_ITEM     hb_socketAddrToItem( const void * pSockAddr, unsigned len );
extern HB_EXPORT int          hb_socketGetSockName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen );
extern HB_EXPORT int          hb_socketGetPeerName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen );
extern HB_EXPORT HB_SOCKET    hb_socketOpen( int domain, int type, int protocol );
extern HB_EXPORT int          hb_socketClose( HB_SOCKET sd );
extern HB_EXPORT int          hb_socketShutdown( HB_SOCKET sd, int iMode );
extern HB_EXPORT int          hb_socketBind( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen );
extern HB_EXPORT int          hb_socketListen( HB_SOCKET sd, int iBacklog );
extern HB_EXPORT HB_SOCKET    hb_socketAccept( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen, HB_MAXINT timeout );
extern HB_EXPORT int          hb_socketConnect( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen, HB_MAXINT timeout );
extern HB_EXPORT long         hb_socketSend( HB_SOCKET sd, const void * data, long len, int flags, HB_MAXINT timeout );
extern HB_EXPORT long         hb_socketSendTo( HB_SOCKET sd, const void * data, long len, int flags, const void * pSockAddr, unsigned uiSockLen, HB_MAXINT timeout );
extern HB_EXPORT long         hb_socketRecv( HB_SOCKET sd, void * data, long len, int flags, HB_MAXINT timeout );
extern HB_EXPORT long         hb_socketRecvFrom( HB_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, HB_MAXINT timeout );
extern HB_EXPORT int          hb_socketSetBlockingIO( HB_SOCKET sd, HB_BOOL fBlocking );
extern HB_EXPORT int          hb_socketSetNoDelay( HB_SOCKET sd, HB_BOOL fNoDelay );
extern HB_EXPORT int          hb_socketSetExclusiveAddr( HB_SOCKET sd, HB_BOOL fExclusive );
extern HB_EXPORT int          hb_socketSetReuseAddr( HB_SOCKET sd, HB_BOOL fReuse );
extern HB_EXPORT int          hb_socketSetKeepAlive( HB_SOCKET sd, HB_BOOL fKeepAlive );
extern HB_EXPORT int          hb_socketSetBroadcast( HB_SOCKET sd, HB_BOOL fBroadcast );
extern HB_EXPORT int          hb_socketSetSndBufSize( HB_SOCKET sd, int iSize );
extern HB_EXPORT int          hb_socketSetRcvBufSize( HB_SOCKET sd, int iSize );
extern HB_EXPORT int          hb_socketGetRcvBufSize( HB_SOCKET sd, int * piSize );
extern HB_EXPORT int          hb_socketGetSndBufSize( HB_SOCKET sd, int * piSize );
extern HB_EXPORT int          hb_socketSetMulticast( HB_SOCKET sd, int af, const char * szAddr );
extern HB_EXPORT int          hb_socketSelectRead( HB_SOCKET sd, HB_MAXINT timeout );
extern HB_EXPORT int          hb_socketSelectWrite( HB_SOCKET sd, HB_MAXINT timeout );
extern HB_EXPORT int          hb_socketSelectWriteEx( HB_SOCKET sd, HB_MAXINT timeout );
extern HB_EXPORT int          hb_socketSelect( PHB_ITEM pArrayRD, HB_BOOL fSetRD,
                                               PHB_ITEM pArrayWR, HB_BOOL fSetWR,
                                               PHB_ITEM pArrayEX, HB_BOOL fSetEX,
                                               HB_MAXINT timeout, HB_SOCKET_FUNC pFunc );

/* Harbour level socket item API functions */
extern HB_EXPORT HB_SOCKET hb_socketParam( int iParam );
extern HB_EXPORT HB_SOCKET hb_socketItemGet( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM  hb_socketItemPut( PHB_ITEM pItem, HB_SOCKET sd );
extern HB_EXPORT void      hb_socketItemClear( PHB_ITEM pItem );

HB_EXTERN_END

#endif /* HB_SOCKET_H_ */
