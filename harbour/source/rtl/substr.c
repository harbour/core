/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * LEFT() function
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

/* returns l characters from n characters into string */

HARBOUR HB_SUBSTR( void )
{
   PHB_ITEM pText = hb_param( 1, IT_STRING );

   if( pText && ISNUM( 2 ) )
   {
      long lPos = hb_parnl( 2 );

      if( lPos < 0 )
      {
         lPos += ( long ) hb_itemGetCLen( pText );
         if( lPos < 0 )
            lPos = 0;
      }
      else if( lPos )
      {
         lPos--;
      }

      if( lPos < ( long ) hb_itemGetCLen( pText ) )
      {
         long lLen;

         if( hb_pcount() >= 3 )
         {
            if( ISNUM( 3 ) )
            {
               lLen = hb_parnl( 3 );

               if( lLen > ( long ) hb_itemGetCLen( pText ) - lPos )
                  lLen = ( long ) hb_itemGetCLen( pText ) - lPos;
            }
            else
            {
               PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1110, NULL, "SUBSTR" );

               if( pResult )
               {
                  hb_itemReturn( pResult );
                  hb_itemRelease( pResult );
               }

               /* NOTE: Exit from inside [vszakats] */
               return;
            }
         }
         else
            lLen = ( long ) hb_itemGetCLen( pText ) - lPos;

         if( lLen > 0 )
            hb_retclen( hb_itemGetCPtr( pText ) + lPos, lLen );
         else
            hb_retc( "" );
      }
      else
         hb_retc( "" );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1110, NULL, "SUBSTR" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}
