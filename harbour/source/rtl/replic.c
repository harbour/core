/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * REPLICATE() function
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

/* returns n copies of given string */
/* TEST: QOUT( "replicate( 'abc', 5 ) = " + replicate( 'abc', 5 ) ) */
HB_FUNC( REPLICATE )
{
   if( ISCHAR( 1 ) && ISNUM( 2 ) )
   {
      long lTimes = hb_parnl( 2 );

      if( lTimes > 0 )
      {
         ULONG ulLen = hb_parclen( 1 );

         if( ( double ) ( ( double ) ulLen * ( double ) lTimes ) < ( double ) ULONG_MAX )
         {
            char * szText = hb_parc( 1 );
            char * szResult = ( char * ) hb_xgrab( ( ulLen * lTimes ) + 1 );
            char * szPtr = szResult;
            long i;

            for( i = 0; i < lTimes; i++ )
            {
               hb_xmemcpy( szPtr, szText, ulLen );
               szPtr += ulLen;
            }

            hb_retclen( szResult, ulLen * lTimes );
            hb_xfree( szResult );
         }
         else
         {
            PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_STROVERFLOW, 1234, NULL, "REPLICATE" );

            if( pResult )
            {
               hb_itemReturn( pResult );
               hb_itemRelease( pResult );
            }
         }
      }
      else
         hb_retc( "" );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1106, NULL, "REPLICATE" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

