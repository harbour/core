/*
 * Harbour Project source code:
 *    SIX compatible functions:
 *          hb_sxEnCrypt()
 *          hb_sxDeCrypt()
 *
 *          sx_Encrypt()
 *          sx_Decrypt()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbsxfunc.h"

#define rnd_mul1  0x0de6d
#define rnd_mul2  0x0278d

static HB_U32 hb_sxInitSeed( const char * pKeyVal, HB_U16 * puiKey )
{
   HB_U32 ulSeed = 0;
   int i;

   for( i = 0; i < 7; i++ )
   {
      ulSeed = ( ( ( ulSeed >> 16 ) + ( ulSeed << 16 ) ) * 17 ) +
               HB_GET_LE_UINT16( &pKeyVal[i] );
   }
   ulSeed |= 1;
   *puiKey = ( HB_U16 ) ulSeed;
   return ( ulSeed << 16 ) + ( ulSeed >> 16 );
}

static HB_U32 hb_sxNextSeed( HB_U32 ulSeed, const char * pKeyVal, HB_U16 * puiKey )
{
   HB_U32 ulTemp1, ulTemp2;
   HB_U16 uiSeedLo, uiSeedHi;

   uiSeedLo = ( HB_U16 ) ulSeed;
   ulTemp1  = ( HB_U32 ) rnd_mul1 * ( HB_U32 ) uiSeedLo;
   ulTemp2  = ( HB_U32 ) rnd_mul2 * ( HB_U32 ) uiSeedLo + ( ulTemp1 >> 16 );
   uiSeedLo = ( HB_U16 ) ulTemp1;
   ulTemp1  = ( HB_U32 ) rnd_mul1 * ( ulSeed >> 16 );
   uiSeedHi = ( HB_U16 ) ( ulTemp1 + ulTemp2 );
   ulSeed   = ( ( HB_U32 ) uiSeedHi << 16 ) + ( HB_U32 ) uiSeedLo;
   uiSeedHi |= 1;
   *puiKey  = uiSeedHi + HB_GET_LE_UINT16( pKeyVal );
   return ulSeed;
}

void hb_sxEnCrypt( const char * pSrc, char * pDst, const char * pKeyVal, HB_SIZE nLen )
{
   HB_U32 ulSeed;
   HB_U16 uiKey;
   HB_UCHAR ucChar, ucShft;
   HB_SIZE ul;
   int i;

   ulSeed = hb_sxInitSeed( pKeyVal, &uiKey );
   for( ul = 0, i = 0; ul < nLen; ul++ )
   {
      ucChar = ( HB_UCHAR ) pSrc[ ul ];
      ucShft = ( HB_UCHAR ) ( uiKey & 0x07 );
      pDst[ ul ] = ( ( ucChar >> ucShft ) + ( ucChar << ( 8 - ucShft ) ) +
                     ( uiKey & 0xFF ) );
      ulSeed = hb_sxNextSeed( ulSeed, &pKeyVal[ i ], &uiKey );
      if( ++i == 7 )
         i = 0;
   }
}

void hb_sxDeCrypt( const char * pSrc, char * pDst, const char * pKeyVal, HB_SIZE nLen )
{
   HB_U32 ulSeed;
   HB_U16 uiKey;
   HB_UCHAR ucChar, ucShft;
   HB_SIZE ul;
   int i;

   ulSeed = hb_sxInitSeed( pKeyVal, &uiKey );
   for( ul = 0, i = 0; ul < nLen; ul++ )
   {
      ucChar = ( HB_UCHAR ) pSrc[ ul ] - ( uiKey & 0xFF );
      ucShft = ( HB_UCHAR ) ( uiKey & 0x07 );
      pDst[ ul ] = ( ( ucChar << ucShft ) + ( ucChar >> ( 8 - ucShft ) ) );
      ulSeed = hb_sxNextSeed( ulSeed, &pKeyVal[ i ], &uiKey );
      if( ++i == 7 )
         i = 0;
   }
}

static HB_BOOL _hb_sxGetKey( PHB_ITEM pKeyItem, char * pKeyVal )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_ITEM pItem = NULL;
   HB_SIZE nKey;

   if( ! ( hb_itemType( pKeyItem ) & HB_IT_STRING ) )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pArea )
      {
         pItem = hb_itemNew( NULL );
         if( SELF_INFO( pArea, DBI_PASSWORD, pItem ) == HB_SUCCESS )
            pKeyItem = pItem;
      }
   }
   if( hb_itemType( pKeyItem ) & HB_IT_STRING )
   {
      nKey = hb_itemGetCLen( pKeyItem );
      if( nKey )
         memcpy( pKeyVal, hb_itemGetCPtr( pKeyItem ), HB_MIN( nKey, 8 ) );
      if( nKey < 8 )
         memset( pKeyVal + nKey, 0, 8 - nKey );
      fResult = HB_TRUE;
   }
   if( pItem )
      hb_itemRelease( pItem );
   return fResult;
}

HB_FUNC( SX_ENCRYPT )
{
   if( hb_pcount() > 0 )
   {
      char keyBuf[ 8 ];
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen > 0 && _hb_sxGetKey( hb_param( 2, HB_IT_ANY ), keyBuf ) )
      {
         char * pDst = ( char * ) hb_xgrab( nLen + 1 );
         hb_sxEnCrypt( hb_parc( 1 ), pDst, keyBuf, nLen );
         pDst[ nLen ] = 0;
         hb_retclen_buffer( pDst, nLen );
      }
      else
         hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
   }
}

HB_FUNC( SX_DECRYPT )
{
   if( hb_pcount() > 0 )
   {
      char keyBuf[ 8 ];
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen > 0 && _hb_sxGetKey( hb_param( 2, HB_IT_ANY ), keyBuf ) )
      {
         char * pDst = ( char * ) hb_xgrab( nLen + 1 );
         hb_sxDeCrypt( hb_parc( 1 ), pDst, keyBuf, nLen );
         pDst[ nLen ] = 0;
         hb_retclen_buffer( pDst, nLen );
      }
      else
         hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
   }
}
