/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RAT() function
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

HARBOUR HB_RAT( void )
{
   ULONG ulSubLen = hb_parclen( 1 );

   if( ulSubLen )
   {
      long lPos = hb_parclen( 2 ) - ulSubLen;

      if( lPos >= 0 )
      {
         char * pszSub = hb_parc( 1 );
         char * pszText = hb_parc( 2 );
         BOOL bFound = FALSE;

         while( lPos >= 0 && !bFound )
         {
            if( *( pszText + lPos ) == *pszSub )
               bFound = ( memcmp( pszSub, pszText + lPos, ulSubLen ) == 0 );
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

