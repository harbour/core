/*
 * Harbour Project source code:
 * hb_StrReplace()
 *
 * Copyright 2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* hb_StrReplace( <cString>, [ <cSource> | <acSource> | <hReplace> ], [ <cDest> | <acDest> ] )
 *    -> <cResult>
 */
HB_FUNC( HB_STRREPLACE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pSrc = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY |
                                ( HB_ISNIL( 3 ) ? HB_IT_HASH : 0 ) );

   if( pText && pSrc )
   {
      HB_SIZE nText = hb_itemGetCLen( pText ),
              nSrc = hb_itemSize( pSrc );

      if( nText > 0 && nSrc > 0 )
      {
         PHB_ITEM pDst = hb_param( 3, HB_IT_STRING | HB_IT_ARRAY );
         const char * pszDst = pDst && HB_IS_STRING( pDst ) ?
                               hb_itemGetCPtr( pDst ) : NULL;
         const char * pszSrc = HB_IS_STRING( pSrc ) ?
                               hb_itemGetCPtr( pSrc ) : NULL;
         const char * pszText = hb_itemGetCPtr( pText );
         const char * ptr;
         char * pszResult = NULL;
         HB_SIZE nDst, nSize, nPos, nAt, nSkip, nTmp;

         nDst = hb_itemSize( HB_IS_HASH( pSrc ) ? pSrc : pDst );
         nSize = nPos = nSkip = 0;
         while( nPos < nText )
         {
            if( pszSrc )
            {
               ptr = ( const char * )
                     memchr( pszSrc, ( HB_UCHAR ) pszText[ nPos ], nSrc );
               nAt = ptr ? ptr - pszSrc + 1 : 0;
               nSkip = 1;
            }
            else
            {
               for( nAt = 1; nAt <= nSrc; ++nAt )
               {
                  if( HB_IS_HASH( pSrc ) )
                  {
                     pDst = hb_hashGetKeyAt( pSrc, nAt );
                     nSkip = hb_itemGetCLen( pDst );
                     ptr = hb_itemGetCPtr( pDst );
                  }
                  else
                  {
                     nSkip = hb_arrayGetCLen( pSrc, nAt );
                     ptr = hb_arrayGetCPtr( pSrc, nAt );
                  }
                  if( nSkip > 0 && nSkip <= nText - nPos &&
                      memcmp( pszText + nPos, ptr , nSkip ) == 0 )
                     break;
               }
               if( nAt > nSrc )
               {
                  nAt = 0;
                  nSkip = 1;
               }
            }

            if( pszResult )
            {
               if( nAt != 0 )
               {
                  if( nAt <= nDst )
                  {
                     if( pszDst )
                        pszResult[ nSize++ ] = pszDst[ nAt - 1 ];
                     else
                     {
                        if( HB_IS_HASH( pSrc ) )
                        {
                           pDst = hb_hashGetValueAt( pSrc, nAt );
                           nTmp = hb_itemGetCLen( pDst );
                           ptr = hb_itemGetCPtr( pDst );
                        }
                        else
                        {
                           nTmp = hb_arrayGetCLen( pDst, nAt );
                           ptr = hb_arrayGetCPtr( pDst, nAt );
                        }
                        memcpy( &pszResult[ nSize ], ptr, nTmp );
                        nSize += nTmp;
                     }
                  }
               }
               else
                  pszResult[ nSize++ ] = pszText[ nPos ];
               nPos += nSkip;
            }
            else
            {
               if( nAt != 0 )
               {
                  if( nAt <= nDst )
                  {
                     if( pszDst )
                        nSize++;
                     else if( HB_IS_HASH( pSrc ) )
                        nSize += hb_itemGetCLen( hb_hashGetValueAt( pSrc, nAt ) );
                     else
                        nSize += hb_arrayGetCLen( pDst, nAt );
                  }
               }
               else
                  nSize++;
               nPos += nSkip;
               if( nPos == nText )
               {
                  pszResult = ( char * ) hb_xgrab( nSize + 1 );
                  nSize = nPos = 0;
               }
            }
         }
         hb_retclen_buffer( pszResult, nSize );
      }
      else
         hb_itemReturn( pText );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
