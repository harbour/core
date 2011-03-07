/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * STRTRAN function
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* TOFIX: Check for string overflow, Clipper can crash if the resulting
          string is too large. Example:
          StrTran( "...", ".", Replicate( "A", 32000 ) ) [vszakats] */

/* replaces lots of characters in a string */
/* TOFIX: Will not work with a search string of > 64 KB on some platforms */
HB_FUNC( STRTRAN )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pSeek = hb_param( 2, HB_IT_STRING );

   if( pText && pSeek )
   {
      const char * szText = hb_itemGetCPtr( pText );
      HB_SIZE nText = hb_itemGetCLen( pText );
      HB_SIZE nSeek = hb_itemGetCLen( pSeek );

      if( nSeek && nSeek <= nText )
      {
         HB_SIZE nStart;

         nStart = hb_parnldef( 4, 1 );

         if( nStart == 0 )
         {
            /* Clipper seems to work this way */
            hb_retc_null();
         }
         else if( nStart > 0 )
         {
            PHB_ITEM pReplace = hb_param( 3, HB_IT_STRING );
            const char * szSeek = hb_itemGetCPtr( pSeek );
            const char * szReplace;
            HB_SIZE nReplace;
            HB_SIZE nCount;
            HB_BOOL bAll;

            if( pReplace )
            {
               szReplace = hb_itemGetCPtr( pReplace );
               nReplace = hb_itemGetCLen( pReplace );
            }
            else
            {
               szReplace = ""; /* shouldn't matter that we don't allocate */
               nReplace = 0;
            }

            if( HB_ISNUM( 5 ) )
            {
               nCount = hb_parns( 5 );
               bAll = HB_FALSE;
            }
            else
            {
               nCount = 0;
               bAll = HB_TRUE;
            }

            if( bAll || nCount > 0 )
            {
               HB_SIZE nFound = 0;
               HB_ISIZ nReplaced = 0;
               HB_SIZE n = 0;
               HB_SIZE nLength = nText;
               HB_SIZE nStop = nText - nSeek + 1;

               while( n < nStop )
               {
                  if( ( bAll || nReplaced < ( HB_ISIZ ) nCount ) &&
                      ! memcmp( szText + n, szSeek, nSeek ) )
                  {
                     nFound++;
                     if( nFound >= nStart )
                     {
                        nReplaced++;
                        nLength = nLength - nSeek + nReplace;
                        n += nSeek;
                     }
                     else
                        n++;
                  }
                  else
                     n++;
               }

               if( nFound )
               {
                  char * szResult = ( char * ) hb_xgrab( nLength + 1 );
                  char * szPtr = szResult;

                  nFound = 0;
                  n = 0;
                  while( n < nText )
                  {
                     if( nReplaced && ! memcmp( szText + n, szSeek, nSeek ) )
                     {
                        nFound++;
                        if( nFound >= nStart )
                        {
                           nReplaced--;
                           memcpy( szPtr, szReplace, nReplace );
                           szPtr += nReplace;
                           n += nSeek;
                        }
                        else
                        {
                           *szPtr = szText[ n ];
                           szPtr++;
                           n++;
                        }
                     }
                     else
                     {
                        *szPtr = szText[ n ];
                        szPtr++;
                        n++;
                     }
                  }
                  hb_retclen_buffer( szResult, nLength );
               }
               else
                  hb_itemReturn( pText );
            }
            else
               hb_itemReturn( pText );
         }
         else
            hb_itemReturn( pText );
      }
      else
         hb_itemReturn( pText );
   }
   else
   {
      /* NOTE: Undocumented but existing Clipper Run-time error [vszakats] */
#ifdef HB_CLP_STRICT
      hb_errRT_BASE_SubstR( EG_ARG, 1126, NULL, HB_ERR_FUNCNAME, 0 );
#else
      hb_errRT_BASE_SubstR( EG_ARG, 1126, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
   }
}
