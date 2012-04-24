/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Binary and unicode string functions:
 *    HB_UCHAR(), HB_UCODE(), HB_ULEN(), HB_UPEEK(), HB_UPOKE()
 *    HB_BCHAR(), HB_BCODE(), HB_BLEN(), HB_BPEEK(), HB_BPOKE()
 *
 * Copyright 2012 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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
#include "hbapicdp.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* HB_UCHAR( <nCode> ) -> <cText>
 * return string with U+nCode character in HVM CP encoding
 */
HB_FUNC( HB_UCHAR )
{
   if( HB_ISNUM( 1 ) )
   {
      char szChar[ HB_MAX_CHAR_LEN ];
      HB_SIZE nLen;

      nLen = hb_cdpTextPutU16( hb_vmCDP(), szChar, sizeof( szChar ),
                                           ( HB_WCHAR ) hb_parni( 1 ) );
      hb_retclen( szChar, nLen );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_BCHAR( <nCode> ) -> <cText>
 * return 1 byte string with <nCode> value
 */
HB_FUNC( HB_BCHAR )
{
   if( HB_ISNUM( 1 ) )
   {
      char c = ( char ) hb_parni( 1 );

      hb_retclen( &c, 1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_UCODE( <cText> ) -> <nCode>
 * return unicode value of 1-st character (not byte) in given string
 */
HB_FUNC( HB_UCODE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
      hb_retni( hb_cdpTextGetU16( hb_vmCDP(), hb_itemGetCPtr( pText ),
                                              hb_itemGetCLen( pText ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_BCODE( <cText> ) -> <nCode>
 * return value of 1-st byte in given string
 */
HB_FUNC( HB_BCODE )
{
   const char * szText = hb_parc( 1 );

   if( szText )
      hb_retni( ( HB_UCHAR ) szText[ 0 ] );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_ULEN( <cText> ) -> <nChars>
 * return string length in characters
 */
HB_FUNC( HB_ULEN )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
      hb_retns( hb_cdpTextLen( hb_vmCDP(), hb_itemGetCPtr( pText ),
                                           hb_itemGetCLen( pText ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_BLEN( <cText> ) -> <nBytes>
 * return string length in bytes
 */
HB_FUNC( HB_BLEN )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
      hb_retns( hb_itemGetCLen( pText ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_UPEEK( <cText>, <n> ) -> <nCode>
 * return unicode value of <n>-th character in given string
 */
HB_FUNC( HB_UPEEK )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && HB_ISNUM( 2 ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      const char * szText = hb_itemGetCPtr( pText );
      HB_SIZE nLen = hb_itemGetCLen( pText );
      HB_SIZE nPos = hb_parns( 2 );
      HB_WCHAR wc = 0;

      if( nPos > 0 && nPos <= nLen )
      {
         nPos = hb_cdpTextPos( cdp, szText, nLen, nPos - 1 );
         nLen -= nPos;
         if( nLen > 0 )
            wc = hb_cdpTextGetU16( cdp, szText + nPos, nLen );
      }

      hb_retni( wc );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_BPEEK( <cText>, <n> ) -> <nCode>
 * return value of <n>-th byte in given string
 */
HB_FUNC( HB_BPEEK )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && HB_ISNUM( 2 ) )
   {
      HB_SIZE nPos = hb_parns( 2 );

      hb_retni( ( nPos > 0 && nPos <= hb_itemGetCLen( pText ) ) ?
                ( HB_UCHAR ) hb_itemGetCPtr( pText )[ nPos - 1 ] : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_UPOKE( [@]<cText>, <n>, <nVal> ) -> <cText>
 * change <n>-th character in given string to unicode <nVal> one and return modified text
 */
HB_FUNC( HB_UPOKE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      const char * szText = hb_itemGetCPtr( pText );
      HB_SIZE nLen = hb_itemGetCLen( pText );
      HB_SIZE nPos = hb_parns( 2 );

      if( nPos > 0 && nPos <= nLen )
      {
         nPos = hb_cdpTextPos( cdp, szText, nLen, nPos - 1 );
         if( nPos < nLen )
         {
            char szChar[ HB_MAX_CHAR_LEN ], * pszText;
            HB_SIZE nChar, nOldChar;

            nChar = hb_cdpTextPutU16( cdp, szChar, sizeof( szChar ),
                                      ( HB_WCHAR ) hb_parni( 3 ) );
            nOldChar = hb_cdpTextPos( cdp, szText + nPos, nLen - nPos, 1 );
            if( nChar == nOldChar )
            {
               if( hb_itemGetWriteCL( pText, &pszText, &nLen ) &&
                   nPos + nChar <= nLen )
                  memcpy( pszText + nPos, szChar, nChar );
            }
            else
            {
               pszText = ( char * ) hb_xgrab( nLen - nOldChar + nChar + 1 );

               memcpy( pszText, szText, nPos );
               memcpy( pszText + nPos, szChar, nChar );
               memcpy( pszText + nPos + nChar, szText + nPos + nOldChar,
                       nLen - nPos - nOldChar );
               if( HB_ISBYREF( 1 ) )
                  hb_storclen( pszText, nLen - nOldChar + nChar, 1 );
               hb_retclen_buffer( pszText, nLen - nOldChar + nChar );
               return;
            }
         }
      }
      hb_itemReturn( pText );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_BPOKE( [@]<cText>, <n>, <nVal> ) -> <cText>
 * change <n>-th byte in given string to <nVal> and return modified text
 */
HB_FUNC( HB_BPOKE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      HB_SIZE nPos = hb_parns( 2 ), nLen;
      char * pszText;

      if( nPos > 0 && hb_itemGetWriteCL( pText, &pszText, &nLen ) &&
          nPos <= nLen )
      {
         pszText[ nPos - 1 ] = ( char ) ( hb_parni( 3 ) & 0xff );
      }
      hb_itemReturn( pText );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* HB_BSUBSTR( <cString>, <nStart>, <nCount> ) -> <cSubstring>
 */
HB_FUNC( HB_BSUBSTR )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   int iPCount = hb_pcount();

   if( pText && HB_ISNUM( 2 ) && ( iPCount < 3 || HB_ISNUM( 3 ) ) )
   {
      const char * pszText = hb_itemGetCPtr( pText );
      HB_ISIZ nSize = hb_itemGetCLen( pText );
      HB_ISIZ nFrom = hb_parns( 2 );
      HB_ISIZ nCount = iPCount < 3 ? nSize : hb_parns( 3 );

      if( nFrom > 0 )
      {
         if( --nFrom > nSize )
            nCount = 0;
      }
      if( nCount > 0 )
      {
         if( nFrom < 0 )
            nFrom += nSize;
         if( nFrom > 0 )
         {
            pszText += nFrom;
            nSize -= nFrom;
         }
         if( nCount > nSize )
            nCount = nSize;
      }

      if( nCount > 0 )
      {
         if( nFrom <= 0 && nCount == nSize )
            hb_itemReturn( pText );
         else
            hb_retclen( pszText, nCount );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1110, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

}

/* HB_USUBSTR( <cString>, <nStart>, <nCount> ) -> <cSubstring>
 */
HB_FUNC( HB_USUBSTR )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   int iPCount = hb_pcount();

   if( pText && HB_ISNUM( 2 ) && ( iPCount < 3 || HB_ISNUM( 3 ) ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      const char * pszText = hb_itemGetCPtr( pText );
      HB_ISIZ nSize = hb_itemGetCLen( pText );
      HB_ISIZ nFrom = hb_parns( 2 );
      HB_ISIZ nCount = iPCount < 3 ? nSize : hb_parns( 3 );

      if( nFrom > 0 )
      {
         if( --nFrom > nSize )
            nCount = 0;
      }

      if( nCount > 0 )
      {
         if( nFrom < 0 )
            nFrom += hb_cdpTextLen( cdp, pszText, nSize );
         if( nFrom > 0 )
         {
            nFrom = hb_cdpTextPos( cdp, pszText, nSize, nFrom );
            pszText += nFrom;
            nSize -= nFrom;
         }
         nCount = hb_cdpTextPos( cdp, pszText, nSize, nCount );
      }

      if( nCount > 0 )
      {
         if( nFrom <= 0 && nCount == nSize )
            hb_itemReturn( pText );
         else
            hb_retclen( pszText, nCount );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1110, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
