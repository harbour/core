/*
 * $Id$
 */

/*
 * Harbour Compatibility Library for CA-Cl*pper source code:
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

#include "extend.h"

CLIPPER HB_COLORIN( void )
{
   if( ISCHAR( 1 ) && ISNUM( 2 ) )
   {
      char * pszColor = _parc( 1 );
      USHORT uiColorPos;
      USHORT uiColorLen;
      USHORT uiColorIndex = ( USHORT ) _parni( 2 );

      /* Skip the given number of commas */

      for( uiColorPos = 0 ; pszColor[ uiColorPos ] != '\0' && uiColorIndex > 0 ; uiColorPos++ )
      {
         if( pszColor[ uiColorPos ] == ',' )
            uiColorIndex--;
      }

      /* if found, continue */

      if( uiColorIndex == 0 )
      {
         /* Skip the spaces after the comma */

         while( pszColor[ uiColorPos ] == ' ' ) uiColorPos++;

         /* Search for next comma or end of string */

         uiColorLen = 0;

         while( pszColor[ uiColorPos + uiColorLen ] != '\0' &&
                pszColor[ uiColorPos + uiColorLen ] != ',' ) uiColorLen++;

         /* Skip the trailing spaces */

         while( uiColorLen > 0 &&
                pszColor[ uiColorPos + uiColorLen - 1 ] == ' ' ) uiColorLen--;

         /* Return the string */

         _retclen( pszColor + uiColorPos, uiColorLen );
      }
      else
         _retc( "" );
   }
   else
      _retc( "" );
}

