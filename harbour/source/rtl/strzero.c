/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * STRZERO() function
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
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( STRZERO )
{
   if( hb_pcount() >= 1 && hb_pcount() <= 3 )
   {
      BOOL bValid;
      PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );
      PHB_ITEM pWidth  = NULL;
      PHB_ITEM pDec    = NULL;

      if( pNumber )
      {
         bValid = TRUE;

         if( hb_pcount() >= 2 )
         {
            pWidth = hb_param( 2, IT_NUMERIC );
            if( !pWidth )
               bValid = FALSE;
         }

         if( hb_pcount() >= 3 )
         {
            pDec = hb_param( 3, IT_NUMERIC );
            if( !pDec )
               bValid = FALSE;
         }
      }
      else
         bValid = FALSE;

      if( bValid )
      {
         char * szResult = hb_itemStr( pNumber, pWidth, pDec );

         if( szResult )
         {
            ULONG ulPos = 0;

            while( szResult[ ulPos ] != '\0' && szResult[ ulPos ] != '-' )
               ulPos++;

            if( szResult[ ulPos ] == '-' )
            {
               /* Negative sign found, put the negative sign to the first */
               /* position */

               szResult[ ulPos ] = ' ';

               ulPos = 0;
               while( szResult[ ulPos ] != '\0' && szResult[ ulPos ] == ' ' )
                  szResult[ ulPos++ ] = '0';

               szResult[ 0 ] = '-';
            }
            else
            {
               /* Negative sign not found */

               ulPos = 0;
               while( szResult[ ulPos ] != '\0' && szResult[ ulPos ] == ' ' )
                  szResult[ ulPos++ ] = '0';
            }

            hb_retc( szResult );
            hb_xfree( szResult );
         }
         else
            hb_retc( "" );
      }
      else
      {
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         /* NOTE: In CA-Cl*pper STRZERO() is written in Clipper, and will call
                  STR() to do the job, the error (if any) will also be thrown
                  by STR().  [vszakats] */
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1099, NULL, "STR" );
#else
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 9999, NULL, "STRZERO" );
#endif

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
}
