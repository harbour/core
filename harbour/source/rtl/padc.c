/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PADC() function
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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

/* centre-pads a date, number, or string with spaces or supplied character */
HB_FUNC( PADC )
{
   ULONG ulSize;
   char buffer[ 128 ];
   char * szText = hb_itemPadConv( hb_param( 1, HB_IT_ANY ), buffer, &ulSize );

   if( szText && ISNUM( 2 ) )
   {
      long lLen = hb_parnl( 2 );

      if( lLen > ( long ) ulSize )
      {
         char * szResult = ( char * ) hb_xgrab( lLen + 1 );
         char cPad;
         long w, lPos = ( lLen - ( long ) ulSize ) / 2;

         hb_xmemcpy( szResult + lPos, szText, ( long ) ulSize + 1 );

         cPad = ( ISCHAR( 3 ) ? *hb_parc( 3 ) : ' ' );

         for( w = 0; w < lPos; w++ )
            szResult[ w ] = cPad;

         for( w = ( long ) ulSize + lPos; w < lLen; w++ )
            szResult[ w ] = cPad;

         szResult[ lLen ] = '\0';

         hb_retclen( szResult, lLen );
         hb_xfree( szResult );
      }
      else
      {
         if( lLen < 0 )
            lLen = 0;

         hb_retclen( szText, lLen );
      }
   }
   else
      hb_retc( "" );
}

