/*
 * ZLIB compression for Harbour stream sockets
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_ZNET_H_
#define HB_ZNET_H_

#include "hbapi.h"
#include "hbsocket.h"

HB_EXTERN_BEGIN

#define HB_INET_ERR_OK            0
#define HB_INET_ERR_TIMEOUT       ( -1 )
#define HB_INET_ERR_CLOSEDCONN    ( -2 )
#define HB_INET_ERR_BUFFOVERRUN   ( -3 )
#define HB_INET_ERR_CLOSEDSOCKET  ( -4 )

#define HB_ZNET_SOCK_ERROR_BASE   100

#if defined( _HB_ZNET_INTERNAL_ )
   struct _HB_ZNETSTREAM;
   typedef struct _HB_ZNETSTREAM * PHB_ZNETSTREAM;
#else
   typedef void * PHB_ZNETSTREAM;
#endif

typedef long ( * HB_INET_RDFUNC ) ( PHB_ZNETSTREAM, HB_SOCKET, void *, long, HB_MAXINT );
typedef long ( * HB_INET_WRFUNC ) ( PHB_ZNETSTREAM, HB_SOCKET, const void *, long, HB_MAXINT, long * );
typedef long ( * HB_INET_FLFUNC ) ( PHB_ZNETSTREAM, HB_SOCKET, HB_MAXINT, HB_BOOL );
typedef void ( * HB_INET_CLFUNC ) ( PHB_ZNETSTREAM );
typedef int  ( * HB_INET_ERFUNC ) ( PHB_ZNETSTREAM );
typedef const char * ( * HB_INET_ESFUNC ) ( PHB_ZNETSTREAM, int );

extern HB_EXPORT PHB_ZNETSTREAM hb_znetOpen( int level, int strategy );
extern HB_EXPORT void    hb_znetEncryptKey( PHB_ZNETSTREAM pStream, const void * keydata, int keylen );
extern HB_EXPORT void    hb_znetClose( PHB_ZNETSTREAM pStream );
extern HB_EXPORT int     hb_znetError( PHB_ZNETSTREAM pStream );
extern HB_EXPORT long    hb_znetRead( PHB_ZNETSTREAM pStream, HB_SOCKET sd, void * buffer, long len, HB_MAXINT timeout );
extern HB_EXPORT long    hb_znetFlush( PHB_ZNETSTREAM pStream, HB_SOCKET sd, HB_MAXINT timeout, HB_BOOL fSync );
extern HB_EXPORT long    hb_znetWrite( PHB_ZNETSTREAM pStream, HB_SOCKET sd, const void * buffer, long len, HB_MAXINT timeout, long * plast );

extern HB_EXPORT HB_SOCKET hb_znetInetFD( PHB_ITEM pItem, HB_BOOL fError );
extern HB_EXPORT HB_MAXINT hb_znetInetTimeout( PHB_ITEM pItem, HB_BOOL fError );
extern HB_EXPORT HB_BOOL   hb_znetInetInitialize( PHB_ITEM, PHB_ZNETSTREAM,
                                                  HB_INET_RDFUNC,
                                                  HB_INET_WRFUNC,
                                                  HB_INET_FLFUNC,
                                                  HB_INET_CLFUNC,
                                                  HB_INET_ERFUNC,
                                                  HB_INET_ESFUNC );

extern HB_EXPORT PHB_SOCKEX hb_sockexNewZNet( HB_SOCKET sd, const void * keydata, int keylen,
                                              int level, int strategy );

HB_EXTERN_END

#endif /* HB_ZNET_H_ */
