/*
 * Harbour Project source code:
 * memo line functions: MemoLine(), MLCount(), MLPos(), MLCToPos(), MPosToLC()
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
#include "hbapicdp.h"
#include "hbset.h"

#define HB_EOL_BUFFER_SIZE    4

typedef struct
{
   const char *   szEOL;
   HB_SIZE        nLen;
} HB_EOL_INFO, * PHB_EOL_INFO;

typedef struct
{
   const char *   pszString;
   HB_SIZE        nLen;
   HB_SIZE        nLineLength;
   HB_SIZE        nTabSize;
   HB_BOOL        fWordWrap;
   int            iEOLs;
   PHB_EOL_INFO   pEOLs;
   PHB_CODEPAGE   cdp;

   HB_SIZE        nOffset;
   HB_SIZE        nMaxCol;
   HB_SIZE        nMaxPos;
   HB_SIZE        nLine;
   HB_SIZE        nCol;
   HB_SIZE        nEOL;

   HB_EOL_INFO    EOL_buffer[ HB_EOL_BUFFER_SIZE ];
}
HB_MLC_INFO, * PHB_MLC_INFO;


static void hb_mlGetEOLs( PHB_MLC_INFO pMLC, int iParam )
{
   int iEOLs = 0;
   HB_SIZE nLen;

   pMLC->pEOLs = pMLC->EOL_buffer;

/* NOTE: This is a parameter extension (HB_EXTENSION) which breaks
         our effort to keep strict parameter compatibility with
         Clipper 5.x. In this case we've resorted to a compromise
         because there was no other idea which seemed natural enough.
         Clipper will ignore these parameters and use CRLF EOL hard
         coded. [vszakats] */
#ifndef HB_CLP_STRICT /* HB_EXTENSION */
   nLen = hb_parclen( iParam );
   if( nLen )
   {
      pMLC->pEOLs[ 0 ].szEOL = hb_parc( iParam );
      pMLC->pEOLs[ 0 ].nLen = nLen;
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
         if( iEOLs > HB_EOL_BUFFER_SIZE )
            pMLC->pEOLs = ( PHB_EOL_INFO ) hb_xgrab( sizeof( HB_EOL_INFO ) * iEOLs );
         iEOLs = 0;
         for( n = 1; n <= nSize; ++n )
         {
            nLen = hb_arrayGetCLen( pArray, n );
            if( nLen > 0 )
            {
               pMLC->pEOLs[ iEOLs ].szEOL = hb_arrayGetCPtr( pArray, n );
               pMLC->pEOLs[ iEOLs ].nLen = nLen;
               ++iEOLs;
            }
         }
      }
   }
#else
   HB_SYMBOL_UNUSED( iParam );
   HB_SYMBOL_UNUSED( nLen );
#endif

   if( iEOLs == 0 )
   {
      pMLC->pEOLs[ 0 ].szEOL = hb_setGetEOL();
      if( ! pMLC->pEOLs[ 0 ].szEOL || ! pMLC->pEOLs[ 0 ].szEOL[ 0 ] )
         pMLC->pEOLs[ 0 ].szEOL = hb_conNewLine();
      pMLC->pEOLs[ 0 ].nLen = strlen( pMLC->pEOLs[ 0 ].szEOL );
      iEOLs = pMLC->pEOLs[ 0 ].nLen ? 1 : 0;
   }

   pMLC->iEOLs = iEOLs;
}

static HB_BOOL hb_mlInit( PHB_MLC_INFO pMLC, int iParAdd )
{
   HB_ISIZ nSize = hb_parnsdef( 2, 79 );

   pMLC->pszString = hb_parc( 1 );
   if( pMLC->pszString && nSize > 0 )
   {
      pMLC->nOffset = pMLC->nMaxCol = pMLC->nMaxPos = pMLC->nLine =
      pMLC->nCol = pMLC->nEOL = 0;

      pMLC->nLineLength = nSize;
      pMLC->nLen = hb_parclen( 1 );

      pMLC->nTabSize = hb_parnldef( 3 + iParAdd, 4 );
      pMLC->fWordWrap = hb_parldef( 4 + iParAdd, 1 );

#ifdef HB_CLP_STRICT
      if( pMLC->nLineLength > 254 )
         pMLC->nLineLength = 79;
#endif
      if( pMLC->nTabSize >= pMLC->nLineLength )
         pMLC->nTabSize = pMLC->nLineLength - 1;
      if( pMLC->nTabSize == 0 )
         pMLC->nTabSize = 1;

      pMLC->cdp = hb_vmCDP();
      if( ! HB_CDP_ISCHARIDX( pMLC->cdp ) )
         pMLC->cdp = NULL;

      hb_mlGetEOLs( pMLC, 5 + iParAdd );
      return HB_TRUE;
   }

   return HB_FALSE;
}

static void hb_mlExit( PHB_MLC_INFO pMLC )
{
   if( pMLC->iEOLs > HB_EOL_BUFFER_SIZE )
      hb_xfree( pMLC->pEOLs );
}

static int hb_mlEol( PHB_MLC_INFO pMLC )
{
   const char * pszString = pMLC->pszString + pMLC->nOffset;
   HB_SIZE nLen = pMLC->nLen - pMLC->nOffset;
   PHB_EOL_INFO pEOLs = pMLC->pEOLs;
   int i;

   for( i = 0; i < pMLC->iEOLs; ++i )
   {
      if( pszString[ 0 ] == pEOLs[ i ].szEOL[ 0 ] &&
          ( pEOLs[ i ].nLen == 1 ||
            ( nLen >= pEOLs[ i ].nLen &&
              memcmp( pszString, pEOLs[ i ].szEOL, pEOLs[ i ].nLen ) == 0 ) ) )
         return i;
   }
   return -1;
}

static HB_SIZE hb_mlGetLine( PHB_MLC_INFO pMLC )
{
   HB_SIZE nBlankCol = 0, nBlankPos = 0, nLastPos;
   int i;

   pMLC->nCol = pMLC->nEOL = 0;

   if( pMLC->nOffset >= pMLC->nLen ||
       ( pMLC->nMaxPos > 0 && pMLC->nOffset >= pMLC->nMaxPos ) )
      return HB_FALSE;

   while( pMLC->nOffset < pMLC->nLen &&
          ( pMLC->nMaxPos == 0 || pMLC->nOffset < pMLC->nMaxPos ) )
   {
      HB_WCHAR ch;

      if( pMLC->pszString[ pMLC->nOffset ] == HB_CHAR_SOFT1 &&
          pMLC->pszString[ pMLC->nOffset + 1 ] == HB_CHAR_SOFT2 )
      {
         if( pMLC->nMaxCol && pMLC->nCol )
            break;
         pMLC->nOffset += 2;
         if( ! pMLC->fWordWrap )
            break;
         else if( nBlankPos + 2 == pMLC->nOffset )
            nBlankPos += 2;
         continue;
      }

      i = hb_mlEol( pMLC );
      if( i >= 0 )
      {
         pMLC->nEOL = pMLC->pEOLs[ i ].nLen;
         if( pMLC->nMaxCol == 0 )
            pMLC->nOffset += pMLC->nEOL;
         break;
      }
      else if( ! pMLC->fWordWrap && pMLC->nCol >= pMLC->nLineLength )
         break;

      nLastPos = pMLC->nOffset;
      if( pMLC->cdp )
      {
         if( ! HB_CDPCHAR_GET( pMLC->cdp, pMLC->pszString, pMLC->nLen, &pMLC->nOffset, &ch ) )
            break;
      }
      else
         ch = pMLC->pszString[ pMLC->nOffset++ ];

      if( ch == ' ' || ch == HB_CHAR_HT )
      {
         nBlankCol = pMLC->nCol;
         nBlankPos = pMLC->nOffset;
      }

      pMLC->nCol += ch == HB_CHAR_HT ?
                    pMLC->nTabSize - ( pMLC->nCol % pMLC->nTabSize ) : 1;

      if( pMLC->nMaxCol && pMLC->nCol >= pMLC->nMaxCol )
      {
         if( pMLC->nCol > pMLC->nMaxCol )
            pMLC->nOffset = nLastPos;
         break;
      }
      else if( pMLC->nCol > pMLC->nLineLength )
      {
         if( pMLC->fWordWrap )
         {
            if( nBlankCol != 0 )
            {
               if( pMLC->nMaxPos )
                  pMLC->nCol = nBlankCol + 1;
               else
                  pMLC->nCol = nBlankCol;
               pMLC->nOffset = nBlankPos;
            }
            else
               pMLC->nOffset = nLastPos;
         }
         else if( pMLC->nCol > pMLC->nLineLength )
            pMLC->nOffset = nLastPos;
         break;
      }
   }

   if( pMLC->nCol > pMLC->nLineLength )
      pMLC->nCol = pMLC->nLineLength;
   else if( pMLC->nMaxPos && pMLC->nCol )
      pMLC->nCol--;

   return HB_TRUE;
}


/* MemoLine( <cString>, [ <nLineLength>=79 ],
 *           [ <nLineNumber>=1 ],
 *           [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *           [ <cEOL>|<acEOLs> ] ) -> <cLine>
 */
HB_FUNC( MEMOLINE )
{
   HB_MLC_INFO MLC;
   HB_ISIZ nLine = hb_parnsdef( 3, 1 );
   char * szLine = NULL;
   HB_SIZE nIndex, nLen = 0, nSize, nCol;

   if( nLine >= 1 )
   {
      if( hb_mlInit( &MLC, 1 ) )
      {
         while( --nLine )
         {
            if( ! hb_mlGetLine( &MLC ) )
               break;
         }
         if( nLine == 0 )
         {
            nIndex = MLC.nOffset;

            /* CA-Cl*pper also does not check if line exists and always
             * fill one line more, i.e.:
             *    for i := 0 to 4
             *       ? "[" + MemoLine( " ", 20, i ) + "]"
             *    next
             * [druzus]
             */
            hb_mlGetLine( &MLC );

            if( MLC.cdp )
               nSize = ( MLC.nOffset - nIndex ) + MLC.nLineLength;
            else
               nSize = MLC.nLineLength;
            szLine = ( char * ) hb_xgrab( nSize + 1 );
            nCol = 0;
            while( nIndex < MLC.nLen && nCol < MLC.nCol )
            {
               if( MLC.pszString[ nIndex ] == HB_CHAR_SOFT1 &&
                   MLC.pszString[ nIndex + 1 ] == HB_CHAR_SOFT2 )
                  nIndex += 2 ;
               else
               {
                  HB_WCHAR wc;

                  if( MLC.cdp )
                  {
                     if( ! HB_CDPCHAR_GET( MLC.cdp, MLC.pszString, MLC.nLen, &nIndex, &wc ) )
                        break;
                  }
                  else
                     wc = MLC.pszString[ nIndex++ ];

                  if( wc == HB_CHAR_HT )
                  {
                     HB_SIZE n = MLC.nTabSize - ( nLen % MLC.nTabSize );
                     do
                     {
                        szLine[ nLen++ ] = ' ';
                     }
                     while( ++nCol < MLC.nCol && --n );
                  }
                  else
                  {
                     if( MLC.cdp )
                     {
                        if( ! HB_CDPCHAR_PUT( MLC.cdp, szLine, nSize, &nLen, wc ) )
                           break;
                     }
                     else
                        szLine[ nLen++ ] = ( char ) wc;
                     ++nCol;
                  }
               }
            }
            if( nCol < MLC.nLineLength )
            {
               nCol = MLC.nLineLength - nCol;
               if( nCol > nSize - nLen )
                  nCol = nSize - nLen;
               memset( szLine + nLen, ' ', nCol );
               nLen += nCol;
            }
         }
         hb_mlExit( &MLC );
      }
   }
   if( szLine == NULL )
      hb_retc_null();
   else
      hb_retclen_buffer( szLine, nLen );
}

/* MLCount( <cString>, [ <nLineLength>=79 ],
 *          [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *          [ <cEOL>|<acEOLs> ] ) -> <nLines>
 */
HB_FUNC( MLCOUNT )
{
   HB_MLC_INFO MLC;
   HB_SIZE nLines  = 0;

   if( hb_mlInit( &MLC, 0 ) )
   {
      while( hb_mlGetLine( &MLC ) )
         ++nLines;
      hb_mlExit( &MLC );
   }
   hb_retns( nLines );
}

/* MLPos( <cString>, [ <nLineLength>=79 ],
 *        [ <nLineNumber>=1 ],
 *        [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *        [ <cEOL>|<acEOLs> ] ) -> <nLinePos>
 */
HB_FUNC( MLPOS )
{
   HB_MLC_INFO MLC;
   HB_ISIZ nLine = hb_parnsdef( 3, 1 );
   HB_SIZE nOffset = 0;

   if( nLine >= 1 )
   {
      if( hb_mlInit( &MLC, 1 ) )
      {
         while( --nLine && hb_mlGetLine( &MLC ) )
            ;
         nOffset = MLC.nOffset;
         if( MLC.cdp )
            nOffset = hb_cdpTextLen( MLC.cdp, MLC.pszString, nOffset );
         if( MLC.nOffset < MLC.nLen )
            ++nOffset;
         hb_mlExit( &MLC );
      }
   }
   hb_retns( nOffset );
}

/* MLCToPos( <cString>, [ <nLineLength>=79 ],
 *           [ <nLine>=1 ], [ <nCol>=0 ],
 *           [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *           [ <cEOL>|<acEOLs> ] ) -> <nPos>
 */
HB_FUNC( MLCTOPOS )
{
   HB_SIZE nLine   = hb_parns( 3 );
   HB_SIZE nCol    = hb_parns( 4 );
   HB_SIZE nOffset = 0;

   if( nLine > 0 && HB_ISNUM( 4 ) )
   {
      HB_MLC_INFO MLC;
      if( hb_mlInit( &MLC, 2 ) )
      {
         if( MLC.nLineLength > 4 )
         {
            while( --nLine && hb_mlGetLine( &MLC ) )
               ;
            if( nCol && nLine == 0 )
            {
               MLC.nMaxCol = nCol;
               MLC.nLineLength = nCol;
               hb_mlGetLine( &MLC );
            }
            nOffset = MLC.nOffset;
            if( MLC.cdp )
               nOffset = hb_cdpTextLen( MLC.cdp, MLC.pszString, nOffset );
         }
         hb_mlExit( &MLC );
      }
   }
   hb_retns( nOffset + 1 );
}

/* MPosToLC( <cString>, [ <nLineLength>=79 ],
 *           [ <nPos>=1 ],
 *           [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *           [ <cEOL>|<acEOLs> ] ) -> <aLineCol>
 */
HB_FUNC( MPOSTOLC )
{
   HB_ISIZ nPos    = hb_parns( 3 );
   HB_SIZE nLine   = 0;
   HB_SIZE nCol    = 0;

   if( nPos > 0 )
   {
      HB_MLC_INFO MLC;
      if( hb_mlInit( &MLC, 1 ) )
      {
         if( MLC.cdp )
         {
            HB_SIZE nRest = nPos;
            nPos = hb_cdpTextPosEx( MLC.cdp, MLC.pszString, MLC.nLen, &nRest );
            nPos += nRest;
         }
         MLC.nMaxPos = nPos;
         if( MLC.nMaxPos <= MLC.nLen )
         {
            while( hb_mlGetLine( &MLC ) )
            {
               nCol = MLC.nCol;
               ++nLine;
            }
         }
         hb_mlExit( &MLC );
      }
   }
   hb_reta( 2 );
   hb_storvns( nLine, -1, 1 );
   hb_storvns( nCol, -1, 2 );
}
