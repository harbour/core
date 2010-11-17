/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ATSKIPSTRINGS(), ATI() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
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

/* locates a substring in a string */

static HB_SIZE hb_AtSkipStrings( const char * szSub, HB_SIZE nSubLen, const char * szText, HB_SIZE nLen )
{
   char cLastChar = ' ';

   HB_TRACE(HB_TR_DEBUG, ("hb_AtSkipStrings(%s, %" HB_PFS "u, %s, %" HB_PFS "u)", szSub, nSubLen, szText, nLen));

   if( nSubLen > 0 && nLen >= nSubLen )
   {
      HB_SIZE nPos = 0;
      HB_SIZE nSubPos = 0;

      while( nPos < nLen && nSubPos < nSubLen )
      {
         if( szText[ nPos + 1 ] == '"' && ( szText[ nPos ] == 'e' || szText[ nPos ] == 'E' ) )
         {
            nPos++;

            while( ++nPos < nLen && ( szText[ nPos ] != '"' || szText[ nPos - 1 ] == '\\' ) )
            {
               /* Skip. */
            }

            nPos++;
            nSubPos = 0;
            continue;
         }

         if( szText[ nPos ] == '"' && szSub[ 0 ] != '"' )
         {
            while( ++nPos < nLen && szText[ nPos ] != '"' )
            {
               /* Skip. */
            }

            nPos++;
            nSubPos = 0;
            continue;
         }

         if( szText[ nPos ] == '\'' && szSub[ 0 ] != '\'' )
         {
            while( ++nPos < nLen && szText[ nPos ] != '\'' )
            {
               /* Skip. */
            }

            nPos++;
            nSubPos = 0;
            continue;
         }

         if( szText[ nPos ] == '[' && szSub[ 0 ] != '[' )
         {
            if( ! ( HB_ISALPHA( ( HB_BYTE ) cLastChar ) || HB_ISDIGIT( ( HB_BYTE ) cLastChar ) || strchr( "])}_.", cLastChar ) ) )
            {
               while( ++nPos < nLen && szText[ nPos ] != ']' )
               {
                  /* Skip. */
               }

               nPos++;
               nSubPos = 0;
               continue;
            }
         }

         if( szText[ nPos ] == szSub[ nSubPos ] )
         {
            nSubPos++;
            nPos++;
         }
         else if( nSubPos )
         {
            /* Go back to the first character after the first match,
               or else tests like "22345" $ "012223456789" will fail. */
            nPos -= ( nSubPos - 1 );
            nSubPos = 0;
         }
         else
         {
            cLastChar = szText[ nPos ];
            nPos++;
         }
      }

      return ( nSubPos < nSubLen ) ? 0 : ( nPos - nSubLen + 1 );
   }
   else
      return 0;
}

HB_FUNC( ATSKIPSTRINGS ) /* cFind, cWhere, nStart */
{
   PHB_ITEM pFind = hb_param( 1, HB_IT_STRING ), pWhere = hb_param( 2, HB_IT_STRING );

   if( pFind && pWhere )
   {
      HB_SIZE nStart = hb_parns( 3 );

      if( nStart > 0 )
         nStart--;

      if( nStart < hb_itemGetCLen( pWhere ) )
      {
         HB_SIZE nRet = hb_AtSkipStrings( hb_itemGetCPtr( pFind ), hb_itemGetCLen( pFind ),
                                          hb_itemGetCPtr( pWhere ) + nStart, hb_itemGetCLen( pWhere ) - nStart );

         if( nRet )
         {
            hb_retns( nRet + nStart );
            return;
         }
      }
   }

   hb_retns( 0 );
}

/* Case insensitive hb_strAt() function */
static HB_SIZE hb_strAtI( const char * szSub, HB_SIZE nSubLen, const char * szText, HB_SIZE nLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strAtI(%s, %" HB_PFS "u, %s, %" HB_PFS "u)", szSub, nSubLen, szText, nLen));

   if( nSubLen > 0 && nLen >= nSubLen )
   {
      HB_SIZE nPos = 0;
      HB_SIZE nSubPos = 0;

      while( nPos < nLen && nSubPos < nSubLen )
      {
         if( HB_TOLOWER( ( HB_BYTE ) szText[ nPos ] ) == HB_TOLOWER( ( HB_BYTE ) szSub[ nSubPos ] ) )
         {
            nSubPos++;
            nPos++;
         }
         else if( nSubPos )
         {
            /* Go back to the first character after the first match,
               or else tests like "22345" $ "012223456789" will fail. */
            nPos -= ( nSubPos - 1 );
            nSubPos = 0;
         }
         else
            nPos++;
      }
      return ( nSubPos < nSubLen ) ? 0 : ( nPos - nSubLen + 1 );
   }
   else
      return 0;
}

/* Case insensitive At() function */
HB_FUNC( ATI )
{
   PHB_ITEM pSub = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pText = hb_param( 2, HB_IT_STRING );

   if( pText && pSub )
   {
      PHB_ITEM pStart = hb_param( 3, HB_IT_NUMERIC );
      PHB_ITEM pEnd = hb_param( 4, HB_IT_NUMERIC );
      HB_ISIZ nLen = hb_itemGetCLen( pText );
      HB_ISIZ nStart = pStart ? hb_itemGetNS( pStart ) : 1;
      HB_ISIZ nEnd = pEnd ? hb_itemGetNS( pEnd ) : nLen;
      HB_SIZE nPos;

      if( nStart < 0 )
      {
         nStart += nLen;
         if( nStart < 0 )
            nStart = 0;
      }
      else if( nStart )
         nStart--;

      if( nEnd < 0 )
         nEnd += nLen + 1;
      if( nEnd > nLen )
         nEnd = nLen;

      /* Stop searching if starting past beyond end. */
      if( nStart >= nEnd )
         hb_retns( 0 );
      else
      {
         nPos = hb_strAtI( hb_itemGetCPtr( pSub ), hb_itemGetCLen( pSub ),
                           hb_itemGetCPtr( pText ) + nStart, nEnd - nStart );
         hb_retns( nPos ? nPos + nStart : 0 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1108, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
