/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GETENV(), GETE() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    GETE()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"

/* NOTE: The second parameter is a Harbour extension. In CA-Cl*pper the 
         function will return an empty string if called with more than one
         parameter. [vszakats] */

HB_FUNC( GETENV )
{
   PHB_ITEM pName = hb_param( 1, IT_STRING );

   if( pName )
   {
      char * pszName = hb_itemGetC( pName );
      ULONG ulName = strlen( pszName );
      ULONG ulPos;

      /* strip the '=' or else it will clear the variable! */

      for( ulPos = 0; ulPos < ulName; ulPos++ )
      {
         if( pszName[ ulPos ] == '=' )
         {
            pszName[ ulPos ] = '\0';
            break;
         }
      }

      if( pszName[ 0 ] != '\0' )
      {
         char * szValue;

         /* NOTE: Convert the envvar name to uppercase. This is required for
                  DOS and OS/2 systems. [vszakats] */

         #if defined(HB_OS_DOS) || defined(HB_OS_OS2)
            hb_strupr( pszName );
         #endif

         szValue = getenv( pszName );

         hb_retc( szValue ? szValue : ( ( ISCHAR( 2 ) ? hb_parc( 2 ) : "" ) ) );
      }
      else
         hb_retc( "" );

      hb_itemFreeC( pszName );
   }
   else
      hb_retc( "" );
}

/* NOTE: Undocumented Clipper function. [vszakats] */

HB_FUNC( GETE )
{
   HB_FUNCNAME( GETENV )();
}

