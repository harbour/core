/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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

/* Now we are using only 16bit Unicode values so the maximum size
 * of single character encoded in UTF8 is 3 though ISO 10646 Universal
 * Character Set (UCS) occupies even a 31-bit code space and to encode
 * all UCS values we will need 6 bytes. Now in practice no one uses
 * Unicode character over 0xFFFF but it may change in the future so
 * it's safer to use macro for maximum UTF8 character size. [druzus]
 */
#define HB_MAX_UTF8        3


static HB_SIZE utf8pos( const char * szUTF8, HB_SIZE ulLen, HB_SIZE ulUTF8Pos )
{
   if( ulUTF8Pos )
   {
      HB_SIZE ul, ul2;
      HB_WCHAR uc;
      int n = 0;

      for( ul = ul2 = 0; ul < ulLen; ++ul )
      {
         if( hb_cdpUTF8ToU16NextChar( ( UCHAR ) szUTF8[ ul ], &n, &uc ) )
         {
            if( n == 0 )
            {
               if( --ulUTF8Pos == 0 )
                  return ul2 + 1;
               ul2 = ul + 1;
            }
         }
      }
   }
   return 0;
}

HB_FUNC( HB_CDPSELECT )
{
   hb_retc( hb_cdpID() );

   if( HB_ISCHAR( 1 ) )
      hb_cdpSelectID( hb_parc( 1 ) );
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
   PHB_CODEPAGE cdp = id ? hb_cdpFind( id ) : hb_vmCDP();

   hb_retc( cdp ? cdp->info : NULL );
}

HB_FUNC( __NATSORTVER )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATSORT v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATSORT v1.3i x19 06/Mar/95" */

   HB_FUNC_EXEC( HB_CDPINFO );
}

#ifdef HB_LEGACY_LEVEL2

HB_FUNC( HB_SETCODEPAGE )
{
   HB_FUNC_EXEC( HB_CDPSELECT );
}

#endif

/*
 * extended CP PRG functions
 */
HB_FUNC( HB_TRANSLATE )
{
   HB_SIZE ulLen = hb_parclen( 1 );
   const char * szIdIn = hb_parc( 2 );
   const char * szIdOut = hb_parc( 3 );

   if( ulLen && ( szIdIn || szIdOut ) )
   {
      PHB_CODEPAGE cdpIn = szIdIn ? hb_cdpFindExt( szIdIn ) : hb_vmCDP();
      PHB_CODEPAGE cdpOut = szIdOut ? hb_cdpFindExt( szIdOut ) : hb_vmCDP();

      if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable )
      {
         char * szResult = hb_cdpnDup( hb_parc( 1 ), &ulLen, cdpIn, cdpOut );
         hb_retclen_buffer( szResult, ulLen );
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
      char utf8Char[ HB_MAX_UTF8 ];
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
      HB_SIZE ulLen = hb_parclen( 1 );
      HB_WCHAR wc = 0;
      int n = 0;

      while( ulLen-- )
      {
         if( hb_cdpUTF8ToU16NextChar( ( unsigned char ) *pszString++, &n, &wc ) )
         {
            if( n == 0 )
               break;
         }
      }
      hb_retnint( wc );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_STRTOUTF8 )
{
   HB_SIZE ulLen = hb_parclen( 1 ), ulDest = 0;
   const char * szString;
   char * szDest = NULL;

   if( ulLen )
   {
      PHB_CODEPAGE cdp = HB_ISCHAR( 2 ) ? hb_cdpFind( hb_parc( 2 ) ) : hb_vmCDP();

      if( cdp )
      {
         szString = hb_parc( 1 );
         ulDest = hb_cdpStrAsUTF8Len( cdp, HB_FALSE, szString, ulLen, 0 );
         szDest = ( char * ) hb_xgrab( ulDest + 1 );
         hb_cdpStrToUTF8( cdp, HB_FALSE, szString, ulLen, szDest, ulDest + 1 );
      }
   }
   if( szDest )
      hb_retclen_buffer( szDest, ulDest );
   else
      hb_retc_null();
}

HB_FUNC( HB_UTF8TOSTR )
{
   const char * szString = hb_parc( 1 );

   if( szString )
   {
      HB_SIZE ulLen = hb_parclen( 1 ), ulDest = 0;
      char * szDest = NULL;

      if( ulLen )
      {
         PHB_CODEPAGE cdp = HB_ISCHAR( 2 ) ? hb_cdpFind( hb_parc( 2 ) ) : hb_vmCDP();

         if( cdp )
         {
            szString = hb_parc( 1 );
            ulDest = hb_cdpUTF8AsStrLen( cdp, HB_FALSE, szString, ulLen, 0 );
            szDest = ( char * ) hb_xgrab( ulDest + 1 );
            hb_cdpUTF8ToStr( cdp, HB_FALSE, szString, ulLen, szDest, ulDest + 1 );
         }
      }

      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
      else
         hb_retc_null();
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
      HB_SIZE ulLen = hb_parclen( 1 ), ulDest = 0;
      HB_ISIZ lFrom = hb_parnl( 2 );
      HB_ISIZ lCount = iPCount < 3 ? ( HB_ISIZ ) ulLen : hb_parnl( 3 );

      if( lFrom < 0 )
      {
         lFrom += hb_cdpUTF8StringLength( szString, ulLen );
         if( lFrom < 0 )
            lFrom = 0;
      }
      else if( lFrom )
         --lFrom;

      if( ulLen && lCount > 0 )
         szDest = hb_cdpUTF8StringSubstr( szString, ulLen,
                                          lFrom, lCount, &ulDest );
      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
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
      HB_ISIZ lLen = hb_parnl( 2 );
      HB_SIZE ulDest = 0;
      char * szDest = NULL;

      if( lLen > 0 )
         szDest = hb_cdpUTF8StringSubstr( szString, hb_parclen( 1 ),
                                          0, lLen, &ulDest );

      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
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
      HB_ISIZ lLen = hb_parnl( 2 ), lFrom;
      HB_SIZE ulLen = hb_parclen( 1 ), ulDest = 0;
      char * szDest = NULL;

      if( ulLen && lLen > 0 )
      {
         lFrom = hb_cdpUTF8StringLength( szString, ulLen ) - lLen;
         if( lFrom < 0 )
            lFrom = 0;
         szDest = hb_cdpUTF8StringSubstr( szString, ulLen,
                                          lFrom, lLen, &ulDest );
      }

      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
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
      HB_SIZE ulPos = hb_parnl( 2 );
      HB_SIZE ulLen = hb_parclen( 1 );

      if( ulPos > 0 && ulPos <= ulLen )
         hb_retnint( hb_cdpUTF8StringPeek( szString, ulLen, ulPos - 1 ) );
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
      HB_SIZE ulLen = hb_parclen( 1 ), ulPos;

      ulPos = utf8pos( szString, ulLen, hb_parnl( 2 ) );
      if( ulPos )
      {
         HB_WCHAR uc, uc2;
         int n, n2;

         --ulPos;
         uc = ( HB_WCHAR ) hb_parni( 3 );
         n = hb_cdpUTF8CharSize( uc );
         n2 = 0;
         hb_cdpUTF8ToU16NextChar( szString[ulPos], &n2, &uc2 );
         ++n2;
         if( n == n2 )
         {
            char * szText;
            if( hb_itemGetWriteCL( pText, &szText, &ulLen ) &&
                ulPos + n <= ulLen )
            {
               hb_cdpU16CharToUTF8( &szText[ulPos], uc );
            }
            hb_itemReturn( pText );
         }
         else
         {
            char * szResult = ( char * ) hb_xgrab( ulLen - n2 + n + 1 );

            memcpy( szResult, szString, ulPos );
            hb_cdpU16CharToUTF8( &szResult[ulPos], uc );
            memcpy( szResult + ulPos + n, szString + ulPos + n2, ulLen - ulPos - n2 );
            if( HB_ISBYREF( 1 ) )
               hb_storclen( szResult, ulLen - n2 + n, 1 );
            hb_retclen_buffer( szResult, ulLen - n2 + n );
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
   const char * szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISCHAR( 4 ) )
   {
      HB_SIZE ulLen = hb_parclen( 1 );
      HB_SIZE ulPos = hb_parnl( 2 );
      HB_SIZE ulDel = hb_parnl( 3 );
      HB_SIZE ulIns = hb_parclen( 4 );
      HB_SIZE ulTot;

      if( ulPos )
      {
         ulPos = utf8pos( szString, ulLen, ulPos );
         if( ulPos == 0 )
            ulPos = ulLen;
         else
            ulPos--;
      }
      if( ulDel )
      {
         if( ulPos < ulLen )
         {
            ulDel = utf8pos( szString + ulPos, ulLen - ulPos, ulDel + 1 );
            if( ulDel == 0 )
               ulDel = ulLen - ulPos;
            else
               ulDel--;
         }
         else
            ulDel = 0;
      }

      if( ( ulTot = ulLen + ulIns - ulDel ) > 0 )
      {
         char * szResult = ( char * ) hb_xgrab( ulTot + 1 );

         hb_xmemcpy( szResult, szString, ulPos );
         hb_xmemcpy( szResult + ulPos, hb_parc( 4 ), ulIns );
         hb_xmemcpy( szResult + ulPos + ulIns, szString + ulPos + ulDel,
                     ulLen - ( ulPos + ulDel ) );
         hb_retclen_buffer( szResult, ulTot );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8LEN )
{
   const char * szString = hb_parc( 1 );

   if( szString )
      hb_retnint( hb_cdpUTF8StringLength( szString, hb_parclen( 1 ) ) );
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
