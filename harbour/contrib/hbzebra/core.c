/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include "hbzebra.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbstack.h"


/* ================ Bit buffer ================ */

PHB_BITBUFFER hb_bitbuffer_create( void )
{
   PHB_BITBUFFER  pBitBuffer = ( PHB_BITBUFFER ) hb_xgrab( sizeof( HB_BITBUFFER ) );
   hb_xmemset( pBitBuffer, 0, sizeof( HB_BITBUFFER ) );
   return pBitBuffer;
}

void hb_bitbuffer_destroy( PHB_BITBUFFER pBitBuffer )
{
   if( pBitBuffer->pBuffer )
      hb_xfree( pBitBuffer->pBuffer );
   hb_xfree( pBitBuffer );
}

void hb_bitbuffer_set( PHB_BITBUFFER pBitBuffer, HB_SIZE nPos, HB_BOOL fValue )
{
   if( pBitBuffer->nAlloc * 8 <= nPos )
   {
      HB_SIZE  nNewAlloc = ( ( pBitBuffer->nAlloc >> 1 ) + nPos + 8 ) / 8;
      pBitBuffer->pBuffer = ( HB_BYTE * ) hb_xrealloc( pBitBuffer->pBuffer, nNewAlloc );
      hb_xmemset( pBitBuffer->pBuffer + pBitBuffer->nAlloc, 0, nNewAlloc - pBitBuffer->nAlloc );
      pBitBuffer->nAlloc = nNewAlloc;
   }

   if( fValue )
      * ( pBitBuffer->pBuffer + ( nPos >> 3 ) ) |= 1 << ( nPos & 0x7 );
   else
      * ( pBitBuffer->pBuffer + ( nPos >> 3 ) ) &= ~ ( 1 << ( nPos & 0x7 ) );
}

void hb_bitbuffer_cat_int( PHB_BITBUFFER pBitBuffer, int iValue, int iLen )
{
   int i;

   if( ( pBitBuffer->nLen + iLen ) >= pBitBuffer->nAlloc * 8 )
   {
      int  nNewAlloc = pBitBuffer->nAlloc + ( ( pBitBuffer->nAlloc >> 1 ) + iLen + 7 ) / 8;
      pBitBuffer->pBuffer = ( HB_BYTE * ) hb_xrealloc( pBitBuffer->pBuffer, nNewAlloc );
      hb_xmemset( pBitBuffer->pBuffer + pBitBuffer->nAlloc, 0, nNewAlloc - pBitBuffer->nAlloc );
      pBitBuffer->nAlloc = nNewAlloc;
   }

   if( ( unsigned int ) iLen > sizeof( int ) * 8 )
       iLen = sizeof( int ) * 8;

   /* TODO: optimize */
   for( i = 0; i < iLen; i++ )
   {
      hb_bitbuffer_set( pBitBuffer, pBitBuffer->nLen, iValue & ( 1 << i ) );
      pBitBuffer->nLen++;
   }
}

HB_SIZE hb_bitbuffer_len( PHB_BITBUFFER pBitBuffer )
{
   return pBitBuffer->nLen;
}

HB_BOOL hb_bitbuffer_get( PHB_BITBUFFER pBitBuffer, HB_SIZE nPos )
{
   return nPos > pBitBuffer->nLen ? HB_FALSE :
          ( ( pBitBuffer->pBuffer[ nPos >> 3 ] >> ( nPos & 7 ) ) & 1 );
}

/* ================ GC pointer ================ */

static HB_GARBAGE_FUNC( hb_zebra_destructor )
{
   PHB_ZEBRA * ppZebra = ( PHB_ZEBRA * ) Cargo;

   if( * ppZebra )
   {
      hb_zebra_destroy( *ppZebra );
      *ppZebra = NULL;
   }
}

static const HB_GC_FUNCS s_gcZebraFuncs =
{
   hb_zebra_destructor,
   hb_gcDummyMark
};


PHB_ZEBRA hb_zebraItemGet( PHB_ITEM pItem )
{
   PHB_ZEBRA * ppZebra = ( PHB_ZEBRA * ) hb_itemGetPtrGC( pItem, &s_gcZebraFuncs );
   return ppZebra ? *ppZebra : NULL;
}

PHB_ITEM hb_zebraItemPut( PHB_ITEM pItem, PHB_ZEBRA pZebra )
{
   PHB_ZEBRA * ppZebra = ( PHB_ZEBRA * ) hb_gcAllocate( sizeof( PHB_ZEBRA ), &s_gcZebraFuncs );

   *ppZebra = pZebra;
   return hb_itemPutPtrGC( pItem, ppZebra );
}

void hb_zebraItemClear( PHB_ITEM pItem )
{
   PHB_ZEBRA * ppZebra = ( PHB_ZEBRA * ) hb_itemGetPtrGC( pItem, &s_gcZebraFuncs );

   if( ppZebra )
      * ppZebra = NULL;
}

PHB_ZEBRA hb_zebra_param( int iParam )
{
   PHB_ZEBRA * ppZebra = ( PHB_ZEBRA * ) hb_parptrGC( &s_gcZebraFuncs, iParam );

   if( ppZebra && *ppZebra )
      return *ppZebra;

   hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

void hb_zebra_ret( PHB_ZEBRA pZebra )
{
   hb_zebraItemPut( hb_stackReturnItem(), pZebra );
}


/* ================ Zebra ================ */

PHB_ZEBRA hb_zebra_create( void )
{
   PHB_ZEBRA  pZebra = ( PHB_ZEBRA ) hb_xgrab( sizeof( HB_ZEBRA ) );
   hb_xmemset( pZebra, 0, sizeof( HB_ZEBRA ) );
   return pZebra;
}

void hb_zebra_destroy( PHB_ZEBRA pZebra )
{
   if( pZebra->szCode )
      hb_xfree( pZebra->szCode );
   if( pZebra->pBits )
      hb_bitbuffer_destroy( pZebra->pBits );
   hb_xfree( pZebra );
}

HB_FUNC( HB_ZEBRA_DESTROY )
{
   PHB_ZEBRA pZebra = hb_zebra_param( 1 );
   if( pZebra )
   {
      hb_zebraItemClear( hb_param( 1, HB_IT_POINTER ) );
      hb_zebra_destroy( pZebra );
   }
}

HB_FUNC( HB_ZEBRA_GETERROR )
{
   PHB_ZEBRA pZebra = hb_zebra_param( 1 );
   if( pZebra )
   {
      hb_retni( pZebra->iError );
   }
}

HB_FUNC( HB_ZEBRA_GETCODE )
{
   PHB_ZEBRA pZebra = hb_zebra_param( 1 );
   if( pZebra )
   {
      hb_retc( pZebra->szCode );
   }
}
