/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * STRTRAN function
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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

   if( pText )
   {
      PHB_ITEM pSeek = hb_param( 2, HB_IT_STRING );

      if( pSeek )
      {
         char * szText = hb_itemGetCPtr( pText );
         ULONG ulText = hb_itemGetCLen( pText );
         ULONG ulSeek = hb_itemGetCLen( pSeek );

         if( ulSeek && ulSeek <= ulText )
         {
            char * szSeek = hb_itemGetCPtr( pSeek );
            char * szReplace;
            ULONG ulStart;

            ulStart = ( ISNUM( 4 ) ? hb_parnl( 4 ) : 1 );

            if( !ulStart )
            {
               /* Clipper seems to work this way */
               hb_retc( "" );
            }
            else if( ulStart > 0 )
            {
               PHB_ITEM pReplace = hb_param( 3, HB_IT_STRING );
               ULONG ulReplace;
               ULONG ulCount;
               BOOL bAll;

               if( pReplace )
               {
                  szReplace = hb_itemGetCPtr( pReplace );
                  ulReplace = hb_itemGetCLen( pReplace );
               }
               else
               {
                  szReplace = ""; /* shouldn't matter that we don't allocate */
                  ulReplace = 0;
               }

               if( ISNUM( 5 ) )
               {
                  ulCount = hb_parnl( 5 );
                  bAll = FALSE;
               }
               else
               {
                  ulCount = 0;
                  bAll = TRUE;
               }

               if( bAll || ulCount > 0 )
               {
                  ULONG ulFound = 0;
                  long lReplaced = 0;
                  ULONG i = 0;
                  ULONG ulLength = ulText;

                  while( i < ulText )
                  {
                     if( ( bAll || lReplaced < ( long ) ulCount ) && ! memcmp( szText + i, szSeek, ulSeek ) )
                     {
                        ulFound++;
                        if( ulFound >= ulStart )
                        {
                           lReplaced++;
                           ulLength = ulLength - ulSeek + ulReplace;
                           i += ulSeek;
                        }
                        else
                           i++;
                     }
                     else
                        i++;
                  }

                  if( ulFound )
                  {
                     char * szResult = ( char * ) hb_xgrab( ulLength + 1 );
                     char * szPtr = szResult;

                     ulFound = 0;
                     i = 0;
                     while( i < ulText )
                     {
                        if( lReplaced && ! memcmp( szText + i, szSeek, ulSeek ) )
                        {
                           ulFound++;
                           if( ulFound >= ulStart )
                           {
                              lReplaced--;
                              memcpy( szPtr, szReplace, ulReplace );
                              szPtr += ulReplace;
                              i += ulSeek;
                           }
                           else
                           {
                              *szPtr = szText[ i ];
                              szPtr++;
                              i++;
                           }
                        }
                        else
                        {
                           *szPtr = szText[ i ];
                           szPtr++;
                           i++;
                        }
                     }
                     hb_retclen( szResult, ulLength );
                     hb_xfree( szResult );
                  }
                  else
                     hb_retclen( szText, ulText );
               }
               else
                  hb_retclen( szText, ulText );
            }
            else
               hb_retclen( szText, ulText );
         }
         else
            hb_retclen( szText, ulText );
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1126, NULL, "STRTRAN" ); /* NOTE: Undocumented but existing Clipper Run-time error [vszakats] */

         if( pResult )
            hb_itemRelease( hb_itemReturn( pResult ) );
      }
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1126, NULL, "STRTRAN" ); /* NOTE: Undocumented but existing Clipper Run-time error [vszakats] */

      if( pResult )
         hb_itemRelease( hb_itemReturn( pResult ) );
   }
}

