/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009-2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapierr.h"
#include "hbapicdp.h"

static HB_SIZE utf8pos( const char * szUTF8, HB_SIZE nLen, HB_SIZE nUTF8Pos )
{
   if( nUTF8Pos > 0 && nUTF8Pos <= nLen )
   {
      HB_SIZE n1, n2;
      HB_WCHAR uc;
      int n = 0;

      for( n1 = n2 = 0; n1 < nLen; )
      {
         if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) szUTF8[ n1 ], &n, &uc ) )
            ++n1;

         if( n == 0 )
         {
            if( --nUTF8Pos == 0 )
               return n2 + 1;
            n2 = n1;
         }
      }
   }
   return 0;
}

HB_FUNC( HB_CDPSELECT )
{
   const char * id = hb_parc( 1 );

   hb_retc( hb_cdpID() );

   if( id )
      hb_cdpSelectID( id );
}

HB_FUNC( HB_CDPUNIID )
{
   const char * id = hb_parc( 1 );
   PHB_CODEPAGE cdp = id ? hb_cdpFind( id ) : hb_vmCDP();

   hb_retc( cdp ? cdp->uniTable->uniID : NULL );
}

HB_FUNC( HB_CDPINFO )
{
   const char * id = hb_parc( 1 );
   PHB_CODEPAGE cdp = id ? hb_cdpFindExt( id ) : hb_vmCDP();

   hb_retc( cdp ? cdp->info : NULL );
}

HB_FUNC( HB_CDPISUTF8 )
{
   hb_retl( hb_cdpIsUTF8( hb_cdpFindExt( hb_parcx( 1 ) ) ) );
}

HB_FUNC( HB_CDPLIST )
{
   const char ** list = hb_cdpList();
   HB_ISIZ nPos;

   nPos = 0;
   while( list[ nPos ] )
      ++nPos;

   hb_reta( nPos );

   nPos = 0;
   while( list[ nPos ] )
   {
      hb_storvc( list[ nPos ], -1, nPos + 1 );
      ++nPos;
   }

   hb_xfree( ( void * ) list );
}

HB_FUNC( __NATSORTVER )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATSORT v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATSORT v1.3i x19 06/Mar/95" */

   HB_FUNC_EXEC( HB_CDPINFO );
}

/*
 * extended CP PRG functions
 */
HB_FUNC( HB_TRANSLATE )
{
   HB_SIZE nLen = hb_parclen( 1 );
   const char * szIdIn = hb_parc( 2 );
   const char * szIdOut = hb_parc( 3 );

   if( nLen && ( szIdIn || szIdOut ) )
   {
      PHB_CODEPAGE cdpIn = szIdIn ? hb_cdpFindExt( szIdIn ) : hb_vmCDP();
      PHB_CODEPAGE cdpOut = szIdOut ? hb_cdpFindExt( szIdOut ) : hb_vmCDP();

      if( cdpIn && cdpOut && cdpIn != cdpOut &&
          ( cdpIn->uniTable != cdpOut->uniTable ||
            HB_CDP_ISCUSTOM( cdpIn ) ||
            HB_CDP_ISCUSTOM( cdpOut ) ) )
      {
         char * szResult = hb_cdpnDup( hb_parc( 1 ), &nLen, cdpIn, cdpOut );
         hb_retclen_buffer( szResult, nLen );
      }
      else
         hb_itemReturn( hb_param( 1, HB_IT_STRING ) );
   }
   else
      hb_retc_null();
}

HB_FUNC( HB_UTF8CHR )
{
   if( HB_ISNUM( 1 ) )
   {
      char utf8Char[ HB_MAX_CHAR_LEN ];
      int iLen;

      iLen = hb_cdpU16CharToUTF8( utf8Char, ( HB_WCHAR ) hb_parni( 1 ) );
      hb_retclen( utf8Char, iLen );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8ASC )
{
   const char * pszString = hb_parc( 1 );

   if( pszString )
   {
      HB_SIZE nLen = hb_parclen( 1 );
      HB_WCHAR wc = 0;
      int n = 0;

      while( nLen )
      {
         if( !hb_cdpUTF8ToU16NextChar( ( unsigned char ) *pszString, &n, &wc ) )
            break;
         if( n == 0 )
            break;
         pszString++;
         nLen--;
      }
      hb_retnint( wc );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_STRTOUTF8 )
{
   HB_SIZE nLen = hb_parclen( 1 ), nDest = 0;
   const char * szString;
   char * szDest = NULL;

   if( nLen )
   {
      PHB_CODEPAGE cdp = HB_ISCHAR( 2 ) ? hb_cdpFindExt( hb_parc( 2 ) ) : hb_vmCDP();

      if( cdp )
      {
         szString = hb_parc( 1 );
         nDest = hb_cdpStrAsUTF8Len( cdp, szString, nLen, 0 );
         szDest = ( char * ) hb_xgrab( nDest + 1 );
         hb_cdpStrToUTF8( cdp, szString, nLen, szDest, nDest + 1 );
      }
   }
   if( szDest )
      hb_retclen_buffer( szDest, nDest );
   else
      hb_retc_null();
}

HB_FUNC( HB_UTF8TOSTR )
{
   const char * szString = hb_parc( 1 );

   if( szString )
   {
      HB_SIZE nLen = hb_parclen( 1 ), nDest = 0;
      char * szDest = NULL;

      if( nLen )
      {
         PHB_CODEPAGE cdp = HB_ISCHAR( 2 ) ? hb_cdpFindExt( hb_parc( 2 ) ) : hb_vmCDP();

         if( cdp )
         {
            szString = hb_parc( 1 );
            nDest = hb_cdpUTF8AsStrLen( cdp, szString, nLen, 0 );
            szDest = ( char * ) hb_xgrab( nDest + 1 );
            hb_cdpUTF8ToStr( cdp, szString, nLen, szDest, nDest + 1 );
         }
      }

      if( szDest )
         hb_retclen_buffer( szDest, nDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8AT )
{
   PHB_ITEM pSub = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pText = hb_param( 2, HB_IT_STRING );

   if( pText && pSub )
   {
      HB_SIZE nTextLength = hb_itemGetCLen( pText );
      HB_SIZE nStart = hb_parnsdef( 3, 1 );
      HB_SIZE nEnd = hb_parnsdef( 4, nTextLength ); /* nTextLength can be > UTF8 len. No problem.*/

      if( nEnd < nStart )
         hb_retns( 0 );
      else
         hb_retns( hb_cdpUTF8StringAt( hb_itemGetCPtr( pSub ), hb_itemGetCLen( pSub ),
                                       hb_itemGetCPtr( pText ), nTextLength, nStart, nEnd, HB_FALSE ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * NOTE: In HB_UTF8RAT we are still traversing from
 *       left to right, as it would be required anyway to
 *       determine the real string length. [bacco]
 */

HB_FUNC( HB_UTF8RAT )
{
   PHB_ITEM pSub = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pText = hb_param( 2, HB_IT_STRING );

   if( pText && pSub )
   {
      HB_SIZE nTextLength = hb_itemGetCLen( pText );
      HB_SIZE nStart = hb_parnsdef( 3, 1 );
      HB_SIZE nEnd = hb_parnsdef( 4, nTextLength ); /* nTextLength can be > UTF8 len. No problem.*/

      if( nEnd < nStart )
         hb_retns( 0 );
      else
         hb_retns( hb_cdpUTF8StringAt( hb_itemGetCPtr( pSub ), hb_itemGetCLen( pSub ),
                                       hb_itemGetCPtr( pText ), nTextLength, nStart, nEnd, HB_TRUE ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8SUBSTR )
{
   const char * szString = hb_parc( 1 );
   int iPCount = hb_pcount();

   if( szString && ( iPCount < 2 || ( HB_ISNUM( 2 ) && ( iPCount < 3 || HB_ISNUM( 3 ) ) ) ) )
   {
      char * szDest = NULL;
      HB_SIZE nLen = hb_parclen( 1 ), nDest = 0;
      HB_ISIZ nFrom = hb_parns( 2 );
      HB_ISIZ nCount = iPCount < 3 ? ( HB_ISIZ ) nLen : hb_parns( 3 );

      if( nFrom < 0 )
      {
         nFrom += hb_cdpUTF8StringLength( szString, nLen );
         if( nFrom < 0 )
            nFrom = 0;
      }
      else if( nFrom )
         --nFrom;

      if( nLen > ( HB_SIZE ) nFrom && nCount > 0 )
         szDest = hb_cdpUTF8StringSubstr( szString, nLen,
                                          nFrom, nCount, &nDest );
      if( szDest )
         hb_retclen_buffer( szDest, nDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8LEFT )
{
   const char * szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) )
   {
      HB_ISIZ nLenReq = hb_parns( 2 );
      HB_SIZE nDest = 0;
      char * szDest = NULL;

      if( nLenReq > 0 )
         szDest = hb_cdpUTF8StringSubstr( szString, hb_parclen( 1 ),
                                          0, nLenReq, &nDest );

      if( szDest )
         hb_retclen_buffer( szDest, nDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8RIGHT )
{
   const char * szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) )
   {
      HB_ISIZ nLenReq = hb_parns( 2 ), nFrom;
      HB_SIZE nLen = hb_parclen( 1 ), nDest = 0;
      char * szDest = NULL;

      if( nLen && nLenReq > 0 )
      {
         nFrom = hb_cdpUTF8StringLength( szString, nLen ) - nLenReq;
         if( nFrom < 0 )
            nFrom = 0;
         szDest = hb_cdpUTF8StringSubstr( szString, nLen,
                                          nFrom, nLenReq, &nDest );
      }

      if( szDest )
         hb_retclen_buffer( szDest, nDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8PEEK )
{
   const char * szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) )
   {
      HB_SIZE nPos = hb_parns( 2 );
      HB_SIZE nLen = hb_parclen( 1 );

      if( nPos > 0 && nPos <= nLen )
         hb_retnint( hb_cdpUTF8StringPeek( szString, nLen, nPos - 1 ) );
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8POKE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      const char * szString = hb_itemGetCPtr( pText );
      HB_SIZE nLen = hb_itemGetCLen( pText ), nPos;

      nPos = utf8pos( szString, nLen, hb_parns( 2 ) );
      if( nPos )
      {
         HB_WCHAR uc, uc2;
         int n, n2;

         --nPos;
         uc = ( HB_WCHAR ) hb_parni( 3 );
         n = hb_cdpUTF8CharSize( uc );
         n2 = 0;
         hb_cdpUTF8ToU16NextChar( szString[nPos], &n2, &uc2 );
         ++n2;
         if( n == n2 )
         {
            char * szText;
            if( hb_itemGetWriteCL( pText, &szText, &nLen ) &&
                nPos + n <= nLen )
            {
               hb_cdpU16CharToUTF8( &szText[nPos], uc );
            }
            hb_itemReturn( pText );
         }
         else
         {
            char * szResult = ( char * ) hb_xgrab( nLen - n2 + n + 1 );

            memcpy( szResult, szString, nPos );
            hb_cdpU16CharToUTF8( &szResult[nPos], uc );
            memcpy( szResult + nPos + n, szString + nPos + n2, nLen - nPos - n2 );
            if( HB_ISBYREF( 1 ) )
               hb_storclen( szResult, nLen - n2 + n, 1 );
            hb_retclen_buffer( szResult, nLen - n2 + n );
         }
      }
      else
         hb_itemReturn( pText );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8STUFF )
{
   const char * szText = hb_parc( 1 );
   const char * szIns = hb_parc( 4 );

   if( szText && szIns && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      HB_SIZE nLen = hb_parclen( 1 );
      HB_SIZE nPos = hb_parns( 2 );
      HB_SIZE nDel = hb_parns( 3 );
      HB_SIZE nIns = hb_parclen( 4 );
      HB_SIZE nTot;

      if( nPos )
      {
         nPos = utf8pos( szText, nLen, nPos );
         if( nPos == 0 )
            nPos = nLen;
         else
            nPos--;
      }
      if( nDel )
      {
         if( nPos < nLen )
         {
            nDel = utf8pos( szText + nPos, nLen - nPos, nDel + 1 );
            if( nDel == 0 )
               nDel = nLen - nPos;
            else
               nDel--;
         }
         else
            nDel = 0;
      }

      if( ( nTot = nLen + nIns - nDel ) > 0 )
      {
         char * szResult = ( char * ) hb_xgrab( nTot + 1 );

         hb_xmemcpy( szResult, szText, nPos );
         hb_xmemcpy( szResult + nPos, szIns, nIns );
         hb_xmemcpy( szResult + nPos + nIns, szText + nPos + nDel,
                     nLen - ( nPos + nDel ) );
         hb_retclen_buffer( szResult, nTot );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8LEN )
{
   const char * szText = hb_parc( 1 );

   if( szText )
      hb_retnint( hb_cdpUTF8StringLength( szText, hb_parclen( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* none of numeric parameters in STRTRAN() (4-th and 5-th) refers to
 * character position in string so we do not need to create new
 * HB_UTF8STRTRAN() but we can safely use normal STRTRAN() function
 */
HB_FUNC_EXTERN( STRTRAN );

HB_FUNC( HB_UTF8STRTRAN )
{
   HB_FUNC_EXEC( STRTRAN )
}
