/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    PRG functions for BlowFish encryption
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


#include "hbapi.h"
#include "hbapiitm.h"
#include "hbbfish.h"

HB_FUNC( HB_BLOWFISHKEY )
{
   int iLen = ( int ) hb_parclen( 1 );

   if( iLen )
   {
      HB_BLOWFISH bf;

      hb_blowfishInit( &bf, hb_parc( 1 ), iLen );
      hb_retclen( ( const char * ) &bf, sizeof( HB_BLOWFISH ) );
   }
}

HB_FUNC( HB_BLOWFISHENCRYPT )
{
   if( hb_parclen( 1 ) == sizeof( HB_BLOWFISH ) )
   {
      PHB_ITEM pData = hb_param( 2, HB_IT_STRING );

      if( pData )
      {
         HB_SIZE nLen = hb_itemGetCLen( pData ), nSize;

         if( nLen )
         {
            char * pszData;
            const HB_BLOWFISH * bf = ( const HB_BLOWFISH * ) hb_parc( 1 );
            HB_BOOL fRaw = hb_parl( 3 );

            /* In raw mode passed string is padded to 8 bytes with '\0'
             * otherwise ANSI X.923 padding is using
             */
            nSize = ( fRaw ? ( ( nLen + 7 ) >> 3 ) :
                             ( ( nLen >> 3 ) + 1 ) ) << 3;
            pszData = ( char * ) hb_xgrab( nSize + 1 );
            memcpy( pszData, hb_itemGetCPtr( pData ), nLen );
            memset( pszData + nLen, '\0', nSize - nLen );
            if( !fRaw )
               pszData[ nSize - 1 ] = ( char ) ( nSize - nLen );
            for( nLen = 0; nLen < nSize; nLen += 8 )
            {
               HB_U32 xl, xr;
               xl = HB_GET_BE_UINT32( &pszData[ nLen ] );
               xr = HB_GET_BE_UINT32( &pszData[ nLen + 4 ] );
               hb_blowfishEncrypt( bf, &xl, &xr );
               HB_PUT_BE_UINT32( &pszData[ nLen ], xl );
               HB_PUT_BE_UINT32( &pszData[ nLen + 4 ], xr );
            }
            hb_retclen_buffer( pszData, nSize );
         }
         else
            hb_retc_null();
      }
   }
}

HB_FUNC( HB_BLOWFISHDECRYPT )
{
   if( hb_parclen( 1 ) == sizeof( HB_BLOWFISH ) )
   {
      PHB_ITEM pData = hb_param( 2, HB_IT_STRING );

      if( pData )
      {
         HB_SIZE nSize = hb_itemGetCLen( pData ), nLen;

         if( nSize >= 8 && ( nSize & 0x07 ) == 0 )
         {
            const char * pszSource;
            char * pszData;
            const HB_BLOWFISH * bf = ( const HB_BLOWFISH * ) hb_parc( 1 );
            HB_BOOL fRaw = hb_parl( 3 );

            pszData = ( char * ) hb_xgrab( nSize + ( fRaw ? 1 : 0 ) );
            pszSource = hb_itemGetCPtr( pData );
            for( nLen = 0; nLen < nSize; nLen += 8 )
            {
               HB_U32 xl, xr;
               xl = HB_GET_BE_UINT32( &pszSource[ nLen ] );
               xr = HB_GET_BE_UINT32( &pszSource[ nLen + 4 ] );
               hb_blowfishDecrypt( bf, &xl, &xr );
               HB_PUT_BE_UINT32( &pszData[ nLen ], xl );
               HB_PUT_BE_UINT32( &pszData[ nLen + 4 ], xr );
            }
            if( !fRaw )
            {
               nSize = ( unsigned char ) pszData[ nSize - 1 ];
               nLen -= ( ( nSize - 1 ) & ~0x07 ) == 0 ? nSize : nLen;
            }
            if( nLen )
               hb_retclen_buffer( pszData, nLen );
            else
               hb_xfree( pszData );
         }
         else if( nSize == 0 )
            hb_retc_null();
      }
   }
}
