/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_COLORINDEX() function
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

HARBOUR HB_HB_COLORINDEX( void )
{
   if( ISCHAR( 1 ) && ISNUM( 2 ) )
   {
      char * pszColor = hb_parc( 1 );
      ULONG ulColorPos;
      ULONG ulColorLen;
      USHORT uiColorIndex = ( USHORT ) hb_parni( 2 );

      /* Skip the given number of commas */

      for( ulColorPos = 0 ; pszColor[ ulColorPos ] != '\0' && uiColorIndex > 0 ; ulColorPos++ )
      {
         if( pszColor[ ulColorPos ] == ',' )
            uiColorIndex--;
      }

      /* if found, continue */

      if( uiColorIndex == 0 )
      {
         /* Skip the spaces after the comma */

         while( pszColor[ ulColorPos ] == ' ' ) ulColorPos++;

         /* Search for next comma or end of string */

         ulColorLen = 0;

         while( pszColor[ ulColorPos + ulColorLen ] != '\0' &&
                pszColor[ ulColorPos + ulColorLen ] != ',' ) ulColorLen++;

         /* Skip the trailing spaces */

         while( ulColorLen > 0 &&
                pszColor[ ulColorPos + ulColorLen - 1 ] == ' ' ) ulColorLen--;

         /* Return the string */

         hb_retclen( pszColor + ulColorPos, ulColorLen );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}

