/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ATSKIPSTRINGS(), ATI() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
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

/* locates a substring in a string */

HB_SIZE hb_AtSkipStrings( const char * szSub, HB_SIZE ulSubLen, const char * szText, HB_SIZE ulLen )
{
   char cLastChar = ' ';

   HB_TRACE(HB_TR_DEBUG, ("hb_AtSkipStrings(%s, %lu, %s, %lu)", szSub, ulSubLen, szText, ulLen));

   if( ulSubLen > 0 && ulLen >= ulSubLen )
   {
      HB_SIZE ulPos = 0;
      HB_SIZE ulSubPos = 0;

      while( ulPos < ulLen && ulSubPos < ulSubLen )
      {
         if( szText[ ulPos ] == '"' && szSub[0] != '"' )
         {
            while( ++ulPos < ulLen && szText[ ulPos ] != '"' )
            {
               /* Skip. */
            }

            ulPos++;
            ulSubPos = 0;
            continue;
         }

         if( szText[ ulPos ] == '\'' && szSub[0] != '\'' )
         {
            while( ++ulPos < ulLen && szText[ ulPos ] != '\'' )
            {
               /* Skip. */
            }

            ulPos++;
            ulSubPos = 0;
            continue;
         }

         if( szText[ ulPos ] == '[' && szSub[0] != '[' )
         {
            if( ! ( HB_ISALPHA( (BYTE) cLastChar ) || HB_ISDIGIT( (BYTE) cLastChar ) || strchr( "])}_.", cLastChar ) ) )
            {
               while( ++ulPos < ulLen && szText[ ulPos ] != ']' )
               {
                  /* Skip. */
               }

               ulPos++;
               ulSubPos = 0;
               continue;
            }
         }

         if( szText[ ulPos ] == szSub[ ulSubPos ] )
         {
            ulSubPos++;
            ulPos++;
         }
         else if( ulSubPos )
         {
            /* Go back to the first character after the first match,
               or else tests like "22345" $ "012223456789" will fail. */
            ulPos -= ( ulSubPos - 1 );
            ulSubPos = 0;
         }
         else
         {
            cLastChar = szText[ ulPos ];
            ulPos++;
         }
      }

      return ( ulSubPos < ulSubLen ) ? 0 : ( ulPos - ulSubLen + 1 );
   }
   else
   {
      return 0;
   }
}

HB_FUNC( ATSKIPSTRINGS ) /* cFind, cWhere, nStart */
{
   PHB_ITEM pFind = hb_param( 1, HB_IT_STRING ), pWhere = hb_param( 2, HB_IT_STRING );

   if( pFind && pWhere )
   {
      HB_SIZE ulStart = ( HB_SIZE ) hb_parnl(3);

      if( ulStart > 0 )
         ulStart--;

      if( ulStart < hb_itemGetCLen( pWhere ) )
      {
         HB_SIZE ulRet;

         ulRet = hb_AtSkipStrings( hb_itemGetCPtr( pFind ), hb_itemGetCLen( pFind ),
                                   hb_itemGetCPtr( pWhere ) + ulStart, hb_itemGetCLen( pWhere ) - ulStart );

         if( ulRet )
         {
            hb_retnl( ulRet + ulStart );
            return;
         }
      }
   }

   hb_retnl( 0 );
}

/* Case insensitive hb_strAt() function */
static HB_SIZE hb_strAtI( const char * szSub, HB_SIZE ulSubLen, const char * szText, HB_SIZE ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strAtI(%s, %lu, %s, %lu)", szSub, ulSubLen, szText, ulLen));

   if( ulSubLen > 0 && ulLen >= ulSubLen )
   {
      HB_SIZE ulPos = 0;
      HB_SIZE ulSubPos = 0;

      while( ulPos < ulLen && ulSubPos < ulSubLen )
      {
         if( HB_TOLOWER( ( BYTE ) szText[ ulPos ] ) == HB_TOLOWER( ( BYTE ) szSub[ ulSubPos ] ) )
         {
            ulSubPos++;
            ulPos++;
         }
         else if( ulSubPos )
         {
            /* Go back to the first character after the first match,
               or else tests like "22345" $ "012223456789" will fail. */
            ulPos -= ( ulSubPos - 1 );
            ulSubPos = 0;
         }
         else
            ulPos++;
      }
      return ( ulSubPos < ulSubLen ) ? 0 : ( ulPos - ulSubLen + 1 );
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
      LONG lLen = hb_itemGetCLen( pText );
      LONG lStart = pStart ? hb_itemGetNL( pStart ) : 1;
      LONG lEnd = pEnd ? hb_itemGetNL( pEnd ) : lLen;
      HB_SIZE ulPos;

      if( lStart < 0 )
      {
         lStart += lLen;
         if( lStart < 0 )
            lStart = 0;
      }
      else if( lStart )
         lStart--;

      if( lEnd < 0 )
         lEnd += lLen + 1;
      if( lEnd > lLen )
         lEnd = lLen;

      /* Stop searching if starting past beyond end. */
      if( lStart >= lEnd )
         hb_retnl( 0 );
      else
      {
         ulPos = hb_strAtI( hb_itemGetCPtr( pSub ), hb_itemGetCLen( pSub ),
                            hb_itemGetCPtr( pText ) + lStart, lEnd - lStart );
         hb_retnl( ulPos ? ulPos + lStart : 0 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1108, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
