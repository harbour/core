/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * AT(), RAT() functions
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

/* locates a substring in a string */
/* TEST: QOUT( "at( 'cde', 'abcdefgfedcba' ) = '" + at( 'cde', 'abcsefgfedcba' ) + "'" ) */

HARBOUR HB_AT( void )
{
   PHB_ITEM pSub = hb_param( 1, IT_STRING );
   PHB_ITEM pText = hb_param( 2, IT_STRING );

   if( pText && pSub )
   {
      hb_retnl( hb_strAt( hb_itemGetCPtr( pSub ), hb_itemGetCLen( pSub ),
                          hb_itemGetCPtr( pText ), hb_itemGetCLen( pText ) ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1108, NULL, "AT" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* locates a substring in a string starting at the end */
/* TEST: QOUT( "rat( 'cde', 'abcdefgfedcba' ) = '" + rat( 'cde', 'abcdefgfedcba' ) + "'" ) */
/* TOFIX: Will not work with a search string > 64 KB on some platforms */

HARBOUR HB_RAT( void )
{
   ULONG ulSubLen = hb_parclen( 1 );

   if( ulSubLen )
   {
      long lPos = hb_parclen( 2 ) - ulSubLen;

      if( lPos >= 0 )
      {
         char * szSub = hb_parc( 1 );
         char * szText = hb_parc( 2 );
         BOOL bFound = FALSE;

         while( lPos >= 0 && !bFound )
         {
            if( *( szText + lPos ) == *szSub )
               bFound = ( memcmp( szSub, szText + lPos, ulSubLen ) == 0 );
            lPos--;
         }

         hb_retnl( bFound ? lPos + 2 : 0 );
      }
      else
         hb_retni( 0 );
   }
   else
      /* This function never seems to raise an error */
      hb_retni( 0 );
}

