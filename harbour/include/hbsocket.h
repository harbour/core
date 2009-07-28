/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    socket C API
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_SOCKET_H_
#define HB_SOCKET_H_

#include "hbapi.h"
#include "hbsocket.ch"

HB_EXTERN_BEGIN

#if defined( HB_OS_WIN ) && ! defined( HB_OS_UNIX_COMPATIBLE )
   typedef HB_PTRUINT   HB_SOCKET;
#else
   typedef int          HB_SOCKET;
#endif

typedef HB_SOCKET ( * HB_SOCKET_FUNC )( PHB_ITEM );

#define HB_NO_SOCKET          ( ( HB_SOCKET ) -1 )

HB_EXPORT extern int          hb_socketInit( void );
HB_EXPORT extern void         hb_socketCleanup( void );
HB_EXPORT extern int          hb_socketGetError( void );
HB_EXPORT extern int          hb_socketGetOsError( void );
HB_EXPORT extern const char * hb_socketErrorStr( int iError );
HB_EXPORT extern int          hb_socketGetAddrFamilly( const void * pSockAddr, unsigned len );
HB_EXPORT extern BOOL         hb_socketLocalAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr );
HB_EXPORT extern BOOL         hb_socketInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
HB_EXPORT extern BOOL         hb_socketInet6Addr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
HB_EXPORT extern char *       hb_socketAddrGetName( const void * pSockAddr, unsigned len );
HB_EXPORT extern char *       hb_socketResolveAddr( const char * szAddr, int af );
HB_EXPORT extern PHB_ITEM     hb_socketGetHosts( const char * szAddr, int af );
HB_EXPORT extern PHB_ITEM     hb_socketGetAliases( const char * szAddr, int af );
HB_EXPORT extern int          hb_socketAddrGetPort( const void * pSockAddr, unsigned len );
HB_EXPORT extern BOOL         hb_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PHB_ITEM pAddrItm );
HB_EXPORT extern PHB_ITEM     hb_socketAddrToItem( const void * pSockAddr, unsigned len );
HB_EXPORT extern int          hb_socketGetSockName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen );
HB_EXPORT extern int          hb_socketGetPeerName( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen );
HB_EXPORT extern HB_SOCKET    hb_socketOpen( int domain, int type, int protocol );
HB_EXPORT extern int          hb_socketClose( HB_SOCKET sd );
HB_EXPORT extern int          hb_socketShutdown( HB_SOCKET sd, int iMode );
HB_EXPORT extern int          hb_socketBind( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen );
HB_EXPORT extern int          hb_socketListen( HB_SOCKET sd, int iBacklog );
HB_EXPORT extern HB_SOCKET    hb_socketAccept( HB_SOCKET sd, void ** pSockAddr, unsigned * puiLen, HB_LONG timeout );
HB_EXPORT extern int          hb_socketConnect( HB_SOCKET sd, const void * pSockAddr, unsigned uiLen, HB_LONG timeout );
HB_EXPORT extern long         hb_socketSend( HB_SOCKET sd, const void * data, long len, int flags, HB_LONG timeout );
HB_EXPORT extern long         hb_socketSendTo( HB_SOCKET sd, const void * data, long len, int flags, const void * pSockAddr, unsigned uiSockLen, HB_LONG timeout );
HB_EXPORT extern long         hb_socketRecv( HB_SOCKET sd, void * data, long len, int flags, HB_LONG timeout );
HB_EXPORT extern long         hb_socketRecvFrom( HB_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, HB_LONG timeout );
HB_EXPORT extern int          hb_socketSetBlockingIO( HB_SOCKET sd, BOOL fBlocking );
HB_EXPORT extern int          hb_socketSetReuseAddr( HB_SOCKET sd, BOOL fReuse );
HB_EXPORT extern int          hb_socketSetKeepAlive( HB_SOCKET sd, BOOL fKeepAlive );
HB_EXPORT extern int          hb_socketSetBroadcast( HB_SOCKET sd, BOOL fBroadcast );
HB_EXPORT extern int          hb_socketSetSndBufSize( HB_SOCKET sd, int iSize );
HB_EXPORT extern int          hb_socketSetRcvBufSize( HB_SOCKET sd, int iSize );
HB_EXPORT extern int          hb_socketGetRcvBufSize( HB_SOCKET sd, int * piSize );
HB_EXPORT extern int          hb_socketGetSndBufSize( HB_SOCKET sd, int * piSize );
HB_EXPORT extern int          hb_socketSetMulticast( HB_SOCKET sd, int af, const char * szAddr );
HB_EXPORT extern int          hb_socketSelectRead( HB_SOCKET sd, HB_LONG timeout );
HB_EXPORT extern int          hb_socketSelectWrite( HB_SOCKET sd, HB_LONG timeout );
HB_EXPORT extern int          hb_socketSelectWriteEx( HB_SOCKET sd, HB_LONG timeout );
HB_EXPORT extern int          hb_socketSelect( PHB_ITEM pArrayRD, BOOL fSetRD,
                                               PHB_ITEM pArrayWR, BOOL fSetWR,
                                               PHB_ITEM pArrayEX, BOOL fSetEX,
                                               HB_LONG timeout, HB_SOCKET_FUNC pFunc );

HB_EXTERN_END

#endif /* HB_SOCKET_H_ */
