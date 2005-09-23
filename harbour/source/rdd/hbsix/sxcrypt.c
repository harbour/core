/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *    SIX compatible functions:
 *          hb_sxEnCrypt()
 *          hb_sxDeCrypt()
 *
 *          SX_ENCRYPT()
 *          SX_DECRYPT()
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
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

#include "hbsxfunc.h"

#define rnd_mul1  0x0de6d
#define rnd_mul2  0x0278D

static UINT32 hb_sxInitSeed( BYTE * pKeyVal, UINT16 * puiKey )
{
   UINT32 ulSeed = 0;
   int i;

   for( i = 0; i < 7 ; i++ )
   {
      ulSeed = ( ( ( ulSeed >> 16 ) + ( ulSeed << 16 ) ) * 17 ) +
               HB_GET_LE_UINT16( &pKeyVal[i] );
   }
   ulSeed |= 1;
   *puiKey = ( UINT16 ) ulSeed;
   return ( ulSeed << 16 ) + ( ulSeed >> 16 );
}

static UINT32 hb_sxNextSeed( UINT32 ulSeed, BYTE * pKeyVal, UINT16 * puiKey )
{
   UINT32 ulTemp1, ulTemp2;
   UINT16 uiSeedLo, uiSeedHi;

   uiSeedLo = ( UINT16 ) ulSeed;
   ulTemp1  = ( UINT32 ) rnd_mul1 * ( UINT32 ) uiSeedLo;
   ulTemp2  = ( UINT32 ) rnd_mul2 * ( UINT32 ) uiSeedLo + ( ulTemp1 >> 16 );
   uiSeedLo = ( UINT16 ) ulTemp1;
   ulTemp1  = ( UINT32 ) rnd_mul1 * ( ulSeed >> 16 );
   uiSeedHi = ( UINT16 ) ( ulTemp1 + ulTemp2 );
   ulSeed   = ( ( UINT32 ) uiSeedHi << 16 ) + ( UINT32 ) uiSeedLo;
   uiSeedHi |= 1;
   *puiKey  = uiSeedHi + HB_GET_LE_UINT16( pKeyVal );
   return ulSeed;
}

void hb_sxEnCrypt( BYTE * pSrc, BYTE * pDst, BYTE * pKeyVal, ULONG ulLen )
{
   UINT32 ulSeed;
   UINT16 uiKey;
   BYTE uChar, uShft;
   ULONG ul;
   int i;

   ulSeed = hb_sxInitSeed( pKeyVal, &uiKey );
   for( ul = 0, i = 0 ; ul < ulLen ; ul++ )
   {
      uChar = pSrc[ul];
      uShft = uiKey & 0x07;
      pDst[ul] = ( uChar >> uShft ) + ( uChar << ( 8 - uShft ) ) +
                 ( uiKey & 0xFF );
      ulSeed = hb_sxNextSeed( ulSeed, &pKeyVal[i], &uiKey );
      if( ++i == 7 )
         i = 0;
   }
}

void hb_sxDeCrypt( BYTE * pSrc, BYTE * pDst, BYTE * pKeyVal, ULONG ulLen )
{
   UINT32 ulSeed;
   UINT16 uiKey;
   BYTE uChar, uShft;
   ULONG ul;
   int i;

   ulSeed = hb_sxInitSeed( pKeyVal, &uiKey );
   for( ul = 0, i = 0 ; ul < ulLen ; ul++ )
   {
      uChar = pSrc[ul] - ( uiKey & 0xFF );
      uShft = uiKey & 0x07;
      pDst[ul] = ( uChar << uShft ) + ( uChar >> ( 8 - uShft ) );
      ulSeed = hb_sxNextSeed( ulSeed, &pKeyVal[i], &uiKey );
      if( ++i == 7 )
         i = 0;
   }
}

static BOOL _hb_sxGetKey( PHB_ITEM pKeyItem, BYTE * pKeyVal )
{
   BOOL fResult = FALSE;
   PHB_ITEM pItem = NULL;
   ULONG ulKey;

   if( ! ( hb_itemType( pKeyItem ) & HB_IT_STRING ) )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pArea )
      {
         pItem = hb_itemNew( NULL );
         if( SELF_INFO( pArea, DBI_PASSWORD, pItem ) == SUCCESS )
            pKeyItem = pItem;
      }
   }
   if( hb_itemType( pKeyItem ) & HB_IT_STRING )
   {
      ulKey = hb_itemGetCLen( pKeyItem );
      if( ulKey )
         memcpy( pKeyVal, hb_itemGetCPtr( pKeyItem ), HB_MIN( ulKey, 8 ) );
      if( ulKey < 8 )
         memset( pKeyVal + ulKey, 0, 8 - ulKey );
      fResult = TRUE;
   }
   if( pItem )
      hb_itemRelease( pItem );
   return fResult;
}

HB_FUNC( SX_ENCRYPT )
{
   if( hb_pcount() > 0 )
   {
      BYTE keyBuf[ 8 ];
      ULONG ulLen = hb_parclen( 1 );

      if( ulLen > 0 && _hb_sxGetKey( hb_param( 2, HB_IT_ANY ), keyBuf ) )
      {
         BYTE * pDst = ( BYTE * ) hb_xgrab( ulLen + 1 );
         hb_sxEnCrypt( ( BYTE * ) hb_parc( 1 ), pDst, keyBuf, ulLen );
         pDst[ ulLen ] = 0;
         hb_retclen_buffer( ( char * ) pDst, ulLen );
      }
      else
      {
         hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
      }
   }
   else
      hb_ret();
}

HB_FUNC( SX_DECRYPT )
{
   if( hb_pcount() > 0 )
   {
      BYTE keyBuf[ 8 ];
      ULONG ulLen = hb_parclen( 1 );

      if( ulLen > 0 && _hb_sxGetKey( hb_param( 2, HB_IT_ANY ), keyBuf ) )
      {
         BYTE * pDst = ( BYTE * ) hb_xgrab( ulLen + 1 );
         hb_sxDeCrypt( ( BYTE * ) hb_parc( 1 ), pDst, keyBuf, ulLen );
         pDst[ ulLen ] = 0;
         hb_retclen_buffer( ( char * ) pDst, ulLen );
      }
      else
      {
         hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
      }
   }
   else
      hb_ret();
}
