/*
 * xHarbour compatible functions for bit operators: |, &, ^^
 *
 * Copyright 2016 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#define XHB_AND   0
#define XHB_OR    1
#define XHB_XOR   2

static void s_xhb_bitOper( int iOper )
{
   PHB_ITEM pItem1 = hb_param( 1, HB_IT_ANY ),
            pItem2 = hb_param( 2, HB_IT_ANY );
   HB_SIZE nLen1 = hb_itemGetCLen( pItem1 ),
           nLen2 = hb_itemGetCLen( pItem2 );

   if( pItem1 && pItem2 )
   {
      if( HB_IS_NUMERIC( pItem1 ) && ( HB_IS_NUMERIC( pItem2 ) || nLen2 == 1 ) )
      {
         HB_MAXINT nVal1 = hb_itemGetNInt( pItem1 ),
                   nVal2 = nLen2 == 1 ? ( HB_BYTE ) hb_itemGetCPtr( pItem1 )[ 0 ] :
                                        hb_itemGetNInt( pItem2 );
         switch( iOper )
         {
            case XHB_AND:
               nVal1 &= nVal2;
               break;
            case XHB_OR:
               nVal1 |= nVal2;
               break;
            default: /* XHB_XOR */
               nVal1 ^= nVal2;
               break;
         }
         hb_retnint( nVal1 );
         return;
      }

      if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
      {
         if( ( nLen1 | nLen2 ) != 0 )
         {
            const char * pStr1 = hb_itemGetCPtr( pItem1 ),
                       * pStr2 = hb_itemGetCPtr( pItem2 );
            char * pRet = ( char * ) hb_xmemdup( pStr1, nLen1 + 1 );
            HB_SIZE n1, n2;

            switch( iOper )
            {
               case XHB_AND:
                  for( n1 = n2 = 0; n1 < nLen1; n1++ )
                  {
                     pRet[ n1 ] &= pStr2[ n2 ];
                     if( ++n2 == nLen2 )
                        n2 = 0;
                  }
                  break;
               case XHB_OR:
                  for( n1 = n2 = 0; n1 < nLen1; n1++ )
                  {
                     pRet[ n1 ] |= pStr2[ n2 ];
                     if( ++n2 == nLen2 )
                        n2 = 0;
                  }
                  break;
               default: /* XHB_XOR */
                  for( n1 = n2 = 0; n1 < nLen1; n1++ )
                  {
                     pRet[ n1 ] ^= pStr2[ n2 ];
                     if( ++n2 == nLen2 )
                        n2 = 0;
                  }
                  break;
            }
            hb_retclen_buffer( pRet, nLen1 );
         }
         else
            hb_itemReturn( pItem1 );
         return;
      }

      if( HB_IS_STRING( pItem1 ) && ( HB_IS_NUMERIC( pItem2 ) || nLen2 == 1 ) )
      {
         if( nLen1 )
         {
            const char * pStr = hb_itemGetCPtr( pItem1 );
            char * pRet = ( char * ) hb_xmemdup( pStr, nLen1 + 1 );
            char cVal = nLen2 == 1 ? hb_itemGetCPtr( pItem2 )[ 0 ] :
                                     ( char ) hb_itemGetNI( pItem2 );

            nLen2 = nLen1;
            switch( iOper )
            {
               case XHB_AND:
                  while( nLen2-- )
                     pRet[ nLen2 ] &= cVal;
                  break;
               case XHB_OR:
                  while( nLen2-- )
                     pRet[ nLen2 ] |= cVal;
                  break;
               default: /* XHB_XOR */
                  while( nLen2-- )
                     pRet[ nLen2 ] ^= cVal;
                  break;
            }
            hb_retclen_buffer( pRet, nLen1 );
         }
         else
            hb_itemReturn( pItem1 );
         return;
      }

      if( ( HB_IS_NUMERIC( pItem1 ) || nLen1 == 1 ) && HB_IS_STRING( pItem2 ) )
      {
         const char * pStr = hb_itemGetCPtr( pItem2 );
         int iVal = nLen1 == 1 ? hb_itemGetCPtr( pItem1 )[ 0 ] :
                                 hb_itemGetNI( pItem1 );

         switch( iOper )
         {
            case XHB_AND:
               while( nLen2 )
                  iVal &= ( HB_UCHAR ) pStr[ --nLen2 ];
               break;
            case XHB_OR:
               while( nLen2 )
                  iVal |= ( HB_UCHAR ) pStr[ --nLen2 ];
               break;
            default: /* XHB_XOR */
               while( nLen2 )
                  iVal ^= ( HB_UCHAR ) pStr[ --nLen2 ];
               break;
         }
         hb_retni( iVal );
         return;
      }
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1088, NULL,
                         iOper == XHB_AND ? "&" : ( iOper == XHB_OR ? "|" : "^^" ),
                         2, pItem1, pItem2 );
}

HB_FUNC( XHB_BITAND )
{
   s_xhb_bitOper( XHB_AND );
}

HB_FUNC( XHB_BITOR )
{
   s_xhb_bitOper( XHB_OR );
}

HB_FUNC( XHB_BITXOR )
{
   s_xhb_bitOper( XHB_XOR );
}
