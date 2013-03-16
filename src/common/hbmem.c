/*
 * Harbour Project source code:
 * hb_xmemcpy(), hb_xmemset()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/* hb_xmemcpy() and hb_xmemset() are only needed when
   unsigned int and unsigned long differ in length */

/* unfortunately it's not true - on 64bit platforms int is 32 bit
   and long is 64.
   we need these functions only when max(size_t) < max(long)
   what could be detected and set in header files. Here check
   only for hb_xmem* macro definition

   #if UINT_MAX != ULONG_MAX
 */
#ifndef hb_xmemcpy
void * hb_xmemcpy( void * pDestArg, void * pSourceArg, HB_SIZE nLen )
{
   HB_BYTE * pDest;
   HB_BYTE * pSource;
   HB_SIZE   nRemaining;
   int       iCopySize;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xmemcpy(%p, %p, %" HB_PFS "u)", pDestArg, pSourceArg, nLen ) );

   pDest = ( HB_BYTE * ) pDestArg;
   pSource = ( HB_BYTE * ) pSourceArg;
   nRemaining = nLen;

   while( nRemaining )
   {
      /* Overcome the memcpy() size_t limitation */
      if( nRemaining > UINT_MAX )
      {
         iCopySize = UINT_MAX;
         nRemaining -= ( HB_SIZE ) iCopySize;
      }
      else
      {
         iCopySize = ( int ) nRemaining;
         nRemaining = 0;
      }
      memcpy( pDest, pSource, iCopySize );
      pDest += iCopySize;
      pSource += iCopySize;
   }

   return pDestArg;
}
#endif

#ifndef hb_xmemset
void * hb_xmemset( void * pDestArg, int iFill, HB_SIZE nLen )
{
   HB_BYTE * pDest;
   HB_SIZE   nRemaining;
   int       iSetSize;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xmemset(%p, %d, %" HB_PFS "u)", pDestArg, iFill, nLen ) );

   pDest = ( HB_BYTE * ) pDestArg;
   nRemaining = nLen;

   while( nRemaining )
   {
      /* Overcome the memset() size_t limitation */
      if( nRemaining > UINT_MAX )
      {
         iSetSize = UINT_MAX;
         nRemaining -= ( HB_SIZE ) iSetSize;
      }
      else
      {
         iSetSize = ( int ) nRemaining;
         nRemaining = 0;
      }
      memset( pDest, iFill, iSetSize );
      pDest += iSetSize;
   }

   return pDestArg;
}
#endif
