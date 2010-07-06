/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * memo line functions: MEMOLINE(), MLCOUNT(), MLPOS(), MLCTOPOS(), MPOSTOLC()
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
#include "hbset.h"

typedef struct
{
   const char *   szEOL;
   HB_SIZE        nLen;
} HB_EOL_INFO, * PHB_EOL_INFO;

static int hb_mlEol( const char * pszString, HB_SIZE nLen,
                     PHB_EOL_INFO pEOLs, int iEOLs )
{
   int i;
   for( i = 0; i < iEOLs; ++i )
   {
      if( nLen >= pEOLs[ i ].nLen &&
          memcmp( pszString, pEOLs[ i ].szEOL, pEOLs[ i ].nLen ) == 0 )
         return i;
   }
   return -1;
}

static HB_SIZE hb_mlGetLine( const char * pszString, HB_SIZE nLen, HB_SIZE nOffset,
                             HB_SIZE nLineLength, HB_SIZE nTabSize, HB_SIZE nMaxPos,
                             HB_BOOL fWordWrap, PHB_EOL_INFO pEOLs, int iEOLs,
                             HB_SIZE * pnLen, HB_SIZE * pnEOL )
{
   HB_SIZE nCol = 0, nBlankCol = 0, nBlankPos = 0;
   int i;

   if( pnEOL )
      * pnEOL = 0;

   while( nOffset < nLen && ( nMaxPos == 0 || nOffset < nMaxPos ) )
   {
      if( pszString[ nOffset ] == HB_CHAR_SOFT1 &&
          pszString[ nOffset + 1 ] == HB_CHAR_SOFT2 )
      {
         nOffset += 2;
         if( !fWordWrap )
            break;
         continue;
      }

      i = hb_mlEol( pszString + nOffset, nLen - nOffset, pEOLs, iEOLs );
      if( i >= 0 )
      {
         if( nMaxPos )
            nCol += pEOLs[ i ].nLen;
         else
            nOffset += pEOLs[ i ].nLen;
         if( pnEOL )
            * pnEOL = pEOLs[ i ].nLen;
         break;
      }

      if( pszString[ nOffset ] == ' ' || pszString[ nOffset ] == HB_CHAR_HT )
      {
         nBlankCol = nCol;
         nBlankPos = nOffset;
      }

      if( nCol >= nLineLength )
      {
         if( fWordWrap )
         {
            if( nBlankCol == 0 || pszString[ nOffset ] == ' ' ||
                                  pszString[ nOffset ] == HB_CHAR_HT )
            {
               nCol = nLineLength;
               if( pszString[ nOffset ] == ' ' )
                  ++nOffset;
               if( pszString[ nOffset ] == HB_CHAR_SOFT1 &&
                   pszString[ nOffset + 1 ] == HB_CHAR_SOFT2 )
                  nOffset += 2;
            }
            else
            {
               nCol = nBlankCol;
               nOffset = nBlankPos + 1;
            }
         }
         else
         {
            if( nCol > nLineLength )
               --nOffset;
            nCol = nLineLength;
         }
         break;
      }

      nCol += pszString[ nOffset ] == HB_CHAR_HT ?
              nTabSize - ( nCol % nTabSize ) : 1;
      nOffset++;
   }
   * pnLen = nCol;

   return nOffset;
}

static PHB_EOL_INFO hb_mlGetEOLs( int iParam, int * piEOLs )
{
   PHB_EOL_INFO pEOLs = NULL;
   int iEOLs = 0;

/* NOTE: This is a parameter extension (HB_EXTENSION) which breaks
         our effort to keep strict parameter compatibility with
         Clipper 5.x. In this case we've resorted to a compromise
         because there was no other idea which seemed natural enough.
         Clipper will ignore these parameters and use CRLF EOL hard
         coded. [vszakats] */
#ifndef HB_CLP_STRICT /* HB_EXTENSION */
   HB_SIZE nLen = hb_parclen( iParam );
   if( nLen )
   {
      pEOLs = ( PHB_EOL_INFO ) hb_xgrab( sizeof( HB_EOL_INFO ) );
      pEOLs->szEOL = hb_parc( iParam );
      pEOLs->nLen = nLen;
      iEOLs = 1;
   }
   else if( HB_ISARRAY( iParam ) )
   {
      PHB_ITEM pArray = hb_param( iParam, HB_IT_ARRAY );
      HB_SIZE nSize = hb_arrayLen( pArray ), n;

      for( n = 1; n <= nSize; ++n )
      {
         if( hb_arrayGetCLen( pArray, n ) > 0 )
            ++iEOLs;
      }
      if( iEOLs )
      {
         pEOLs = ( PHB_EOL_INFO ) hb_xgrab( sizeof( HB_EOL_INFO ) * iEOLs );
         iEOLs = 0;
         for( n = 1; n <= nSize; ++n )
         {
            nLen = hb_arrayGetCLen( pArray, n );
            if( nLen > 0 )
            {
               pEOLs[ iEOLs ].szEOL = hb_arrayGetCPtr( pArray, n );
               pEOLs[ iEOLs ].nLen = nLen;
               ++iEOLs;
            }
         }
      }
   }
#else
   HB_SYMBOL_UNUSED( iParam );
#endif

   if( iEOLs == 0 )
   {
      pEOLs = ( PHB_EOL_INFO ) hb_xgrab( sizeof( HB_EOL_INFO ) );
      pEOLs->szEOL = hb_setGetEOL();
      if( !pEOLs->szEOL || !pEOLs->szEOL[ 0 ] )
         pEOLs->szEOL = hb_conNewLine();
      pEOLs->nLen = strlen( pEOLs->szEOL );
      iEOLs = pEOLs->nLen ? 1 : 0;
   }

   * piEOLs = iEOLs;
   return pEOLs;
}

static const char * hb_mlGetParams( int iParAdd, HB_SIZE * pnLen,
                                    HB_SIZE * pnLineLength,
                                    HB_SIZE * pnTabSize, HB_BOOL * pfWordWrap,
                                    PHB_EOL_INFO * pEOLs, int * piEOLs )
{
   const char * pszString = hb_parc( 1 );
   if( pszString )
   {
      if( HB_ISNUM( 2 ) )
      {
         if( hb_parnd( 2 ) <= 0 )
            return NULL;
         * pnLineLength = hb_parns( 2 );
      }
      else
         * pnLineLength = 79;
      * pnLen = hb_parclen( 1 );
      * pnTabSize = hb_parnldef( 3 + iParAdd, 4 );
      * pfWordWrap = hb_parldef( 4 + iParAdd, 1 );
      * pEOLs = hb_mlGetEOLs( 5 + iParAdd, piEOLs );
#ifdef HB_CLP_STRICT
      if( * pnLineLength > 254 )
         * pnLineLength = 79;
#endif
      if( * pnTabSize >= * pnLineLength )
         * pnTabSize = * pnLineLength - 1;
      else if( * pnTabSize == 0 )
         * pnTabSize = 1;
   }
   return pszString;
}

HB_FUNC( MEMOLINE )
{
   HB_SIZE nLen, nLineLength, nTabSize;
   HB_BOOL fWordWrap;
   PHB_EOL_INFO pEOLs;
   int     iEOLs;
   const char * pszString = hb_mlGetParams( 1, &nLen, &nLineLength,
                                            &nTabSize, &fWordWrap,
                                            &pEOLs, &iEOLs );
   char * szLine;
   HB_SIZE nLine   = hb_parns( 3 );
   HB_SIZE nOffset = 0;
   HB_SIZE nCols   = 0;

   if( !pszString )
   {
      hb_retc_null();
      return;
   }

   if( nLine == 0 )
      nLine = 1;

   while( --nLine && nOffset < nLen )
   {
      nOffset = hb_mlGetLine( pszString, nLen, nOffset,
                              nLineLength, nTabSize, 0, fWordWrap,
                              pEOLs, iEOLs, &nCols, NULL );
   }
   if( nOffset < nLen )
   {
      HB_SIZE nCol = 0;
      hb_mlGetLine( pszString, nLen, nOffset,
                    nLineLength, nTabSize, 0, fWordWrap,
                    pEOLs, iEOLs, &nCols, NULL );
      szLine = ( char * ) hb_xgrab( nLineLength + 1 );
      while( nCol < nCols )
      {
         if( pszString[ nOffset ] == HB_CHAR_HT )
         {
            HB_SIZE n = nTabSize - ( nCol % nTabSize );
            do
               szLine[ nCol++ ] = ' ';
            while( --n && nCol < nCols );
         }
         else if( pszString[ nOffset ] == HB_CHAR_SOFT1 &&
                  pszString[ nOffset + 1 ] == HB_CHAR_SOFT2 )
            nOffset++;
         else
            szLine[ nCol++ ] = pszString[ nOffset ];
         nOffset++;
      }
      if( nCols < nLineLength )
         memset( szLine + nCols, ' ', nLineLength - nCols );
      szLine[ nLineLength ] = 0;
      hb_retclen_buffer( szLine, nLineLength );
   }
   else
      hb_retc_null();
   hb_xfree( pEOLs );
}

HB_FUNC( MLCOUNT )
{
   HB_SIZE nLen, nLineLength, nTabSize;
   HB_BOOL fWordWrap;
   PHB_EOL_INFO pEOLs;
   int     iEOLs;
   const char * pszString = hb_mlGetParams( 0, &nLen, &nLineLength,
                                            &nTabSize, &fWordWrap,
                                            &pEOLs, &iEOLs );
   HB_SIZE nLines  = 0;
   HB_SIZE nOffset = 0;
   HB_SIZE nCols   = 0;

   if( pszString )
   {
      while( nOffset < nLen )
      {
         ++nLines;
         nOffset = hb_mlGetLine( pszString, nLen, nOffset,
                                 nLineLength, nTabSize, 0, fWordWrap,
                                 pEOLs, iEOLs, &nCols, NULL );
      }
      hb_xfree( pEOLs );
   }
   hb_retns( nLines );
}

HB_FUNC( MLPOS )
{
   HB_SIZE nLen, nLineLength, nTabSize;
   HB_BOOL fWordWrap;
   PHB_EOL_INFO pEOLs;
   int     iEOLs;
   const char * pszString = hb_mlGetParams( 1, &nLen, &nLineLength,
                                            &nTabSize, &fWordWrap,
                                            &pEOLs, &iEOLs );
   HB_SIZE nLine   = hb_parns( 3 );
   HB_SIZE nOffset = 0;
   HB_SIZE nCols   = 0;

   if( pszString )
   {
      if( nLine == 0 )
         nLine = 1;
      while( --nLine && nOffset < nLen )
         nOffset = hb_mlGetLine( pszString, nLen, nOffset,
                                 nLineLength, nTabSize, 0, fWordWrap,
                                 pEOLs, iEOLs, &nCols, NULL );
      if( nOffset < nLen )
         ++nOffset;
      hb_xfree( pEOLs );
   }
   hb_retns( nOffset );
}

HB_FUNC( MLCTOPOS )
{
   HB_SIZE nLen, nLineLength, nTabSize;
   HB_BOOL fWordWrap;
   PHB_EOL_INFO pEOLs;
   int     iEOLs;
   const char * pszString = hb_mlGetParams( 2, &nLen, &nLineLength,
                                            &nTabSize, &fWordWrap,
                                            &pEOLs, &iEOLs );
   HB_SIZE nLine   = hb_parns( 3 );
   HB_SIZE nCol    = hb_parns( 4 );
   HB_SIZE nOffset = 0;
   HB_SIZE nCols   = 0;

   if( pszString )
   {
      if( nLineLength > 4 && nLine && HB_ISNUM( 4 ) )
      {
         while( --nLine && nOffset < nLen )
            nOffset = hb_mlGetLine( pszString, nLen, nOffset,
                                    nLineLength, nTabSize, 0, fWordWrap,
                                    pEOLs, iEOLs, &nCols, NULL );
         if( nOffset < nLen && nCol )
            nOffset = hb_mlGetLine( pszString, nLen, nOffset,
                                    nCol, nTabSize, nLen, HB_FALSE,
                                    pEOLs, iEOLs, &nCols, NULL );
      }
      hb_xfree( pEOLs );
   }
   ++nOffset;
   hb_retns( nOffset );
}

HB_FUNC( MPOSTOLC )
{
   HB_SIZE nLen, nLineLength, nTabSize;
   HB_BOOL fWordWrap;
   PHB_EOL_INFO pEOLs;
   int     iEOLs;
   const char * pszString = hb_mlGetParams( 1, &nLen, &nLineLength,
                                            &nTabSize, &fWordWrap,
                                            &pEOLs, &iEOLs );
   HB_SIZE nPos    = hb_parns( 3 );
   HB_SIZE nOffset = 0;
   HB_SIZE nLine   = 0;
   HB_SIZE nCol    = 0;
   HB_SIZE nEOL    = 0;

   if( pszString )
   {
      if( nPos && nLen )
      {
         if( --nPos )
         {
            do
            {
               ++nLine;
               nOffset = hb_mlGetLine( pszString, nLen, nOffset,
                                       nLineLength, nTabSize, nPos, fWordWrap,
                                       pEOLs, iEOLs, &nCol, &nEOL );
               if( nEOL )
               {
                  if( nOffset + nEOL == nPos )
                  {
                     nCol = 0;
                     ++nLine;
                     break;
                  }
                  nOffset += nEOL;
               }
            }
            while( nOffset < nLen && nOffset < nPos );

            if( nLine && nCol == nLineLength && nPos <= nLen &&
                ( hb_mlEol( pszString + nPos, nLen - nPos, pEOLs, iEOLs ) >= 0 ||
                  ( pszString[ nPos ] == HB_CHAR_SOFT1 &&
                    pszString[ nPos + 1 ] == HB_CHAR_SOFT2 ) ||
                  ( nPos > 0 && pszString[ nPos - 1 ] == HB_CHAR_SOFT1 &&
                                pszString[ nPos ] == HB_CHAR_SOFT2 ) ||
                  ( nPos > 1 && pszString[ nPos - 2 ] == HB_CHAR_SOFT1 &&
                                pszString[ nPos - 1 ] == HB_CHAR_SOFT2 ) ) )
            {
               nCol = 0;
               ++nLine;
            }
         }
         else
            ++nLine;
      }
      hb_xfree( pEOLs );
   }
   hb_reta( 2 );
   hb_storvns( nLine, -1, 1 );
   hb_storvns( nCol, -1, 2 );
}
