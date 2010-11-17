/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ZLIB compression for Harbour stream sockets
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_ZNET_H_
#define HB_ZNET_H_

#include "hbapi.h"

HB_EXTERN_BEGIN

#if defined( _HB_ZNET_INTERNAL_ )
   struct _HB_ZNETSTREAM;
   typedef struct _HB_ZNETSTREAM * PHB_ZNETSTREAM;
#else
   typedef void * PHB_ZNETSTREAM;
#endif

typedef long ( * HB_INET_SFUNC ) ( PHB_ZNETSTREAM, HB_SOCKET, const void *, long, HB_MAXINT, long * );
typedef long ( * HB_INET_RFUNC ) ( PHB_ZNETSTREAM, HB_SOCKET, void *, long, HB_MAXINT );
typedef long ( * HB_INET_FFUNC ) ( PHB_ZNETSTREAM, HB_SOCKET, HB_MAXINT );
typedef void ( * HB_INET_CFUNC ) ( PHB_ZNETSTREAM );

extern HB_EXPORT PHB_ZNETSTREAM hb_znetOpen( int level, int strategy );
extern HB_EXPORT void    hb_znetEncryptKey( PHB_ZNETSTREAM pStream, const void * keydata, int keylen );
extern HB_EXPORT void    hb_znetClose( PHB_ZNETSTREAM pStream );
extern HB_EXPORT int     hb_znetError( PHB_ZNETSTREAM pStream );
extern HB_EXPORT long    hb_znetRead( PHB_ZNETSTREAM pStream, HB_SOCKET sd, void * buffer, long len, HB_MAXINT timeout );
extern HB_EXPORT long    hb_znetFlush( PHB_ZNETSTREAM pStream, HB_SOCKET sd, HB_MAXINT timeout );
extern HB_EXPORT long    hb_znetWrite( PHB_ZNETSTREAM pStream, HB_SOCKET sd, const void * buffer, long len, HB_MAXINT timeout, long * plast );

extern HB_EXPORT HB_BOOL hb_znetInetInitialize( PHB_ITEM, PHB_ZNETSTREAM,
                                                HB_INET_RFUNC,
                                                HB_INET_SFUNC,
                                                HB_INET_FFUNC,
                                                HB_INET_CFUNC );

HB_EXTERN_END

#endif /* HB_ZNET_H_ */
