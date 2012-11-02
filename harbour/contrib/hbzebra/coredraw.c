/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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
#include "hbapierr.h"
#include "hbvm.h"

typedef void ( *HB_ZEBRA_CALLBACK )( void * cargo, double dX, double dY, double dWidth, double dHeight );

int hb_zebra_draw( PHB_ZEBRA pZebra, HB_ZEBRA_CALLBACK pCallback, void * cargo, double dX, double dY, double dWidth, double dHeight, int iFlags )
{
   double  dLast;
   HB_SIZE n, nLen, nCount;
   HB_BOOL fBit, fLastBit;
   int     i, iCol = pZebra->iCol;

   HB_SYMBOL_UNUSED( iFlags );

   if( pZebra->iError != 0 )
      return HB_ZEBRA_ERROR_INVALIDZEBRA;

   nLen = hb_bitbuffer_len( pZebra->pBits );
   fLastBit = hb_bitbuffer_get( pZebra->pBits, 0 );
   dLast = dX;
   nCount = 0;
   i = 0;
   for( n = 0; n < nLen; n++ )
   {
      fBit = hb_bitbuffer_get( pZebra->pBits, n );
      if( fBit != fLastBit )
      {
         if( fLastBit && pCallback )
            pCallback( cargo, dLast, dY, dWidth * nCount, dHeight );

         dLast += dWidth * nCount;
         nCount = 0;
         fLastBit = fBit;
      }
      nCount++;
      if( ++i == iCol )
      {
         if( nCount )
         {
            if( fBit && pCallback )
               pCallback( cargo, dLast, dY, dWidth * nCount, dHeight );
            nCount = 0;
         }
         i = 0;
         dY += dHeight;
         dLast = dX;
         if( n + 1 < nLen )
            fLastBit = hb_bitbuffer_get( pZebra->pBits, n + 1 );
      }
   }
   if( fLastBit && nCount && pCallback )
      pCallback( cargo, dLast, dY, dWidth * nCount, dHeight );

   return 0;
}

static void hb_zebra_draw_codeblock_callback( void * pDrawBlock, double dX, double dY, double dWidth, double dHeight )
{
   if( pDrawBlock && HB_IS_BLOCK( pDrawBlock ) && hb_vmRequestReenter() )
   {
      hb_vmPushEvalSym();
      hb_vmPush( ( PHB_ITEM ) pDrawBlock );
      hb_vmPushDouble( dX, HB_DEFAULT_DECIMALS );
      hb_vmPushDouble( dY, HB_DEFAULT_DECIMALS );
      hb_vmPushDouble( dWidth, HB_DEFAULT_DECIMALS );
      hb_vmPushDouble( dHeight, HB_DEFAULT_DECIMALS );
      hb_vmSend( 4 );
      hb_vmRequestRestore();
   }
}

int hb_zebra_draw_codeblock( PHB_ZEBRA pZebra, PHB_ITEM pDrawBlock, double dX, double dY, double dWidth, double dHeight, int iFlags )
{
   return hb_zebra_draw( pZebra, hb_zebra_draw_codeblock_callback, pDrawBlock, dX, dY, dWidth, dHeight, iFlags );
}

HB_FUNC( HB_ZEBRA_DRAW )
{
   PHB_ZEBRA pZebra = hb_zebra_param( 1 );

   if( pZebra )
   {
      PHB_ITEM pDrawBlock = hb_param( 2, HB_IT_BLOCK );
      if( pDrawBlock )
         hb_retni( hb_zebra_draw_codeblock( pZebra, pDrawBlock, hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parni( 7 ) ) );
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}
