/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * STUFF() function
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

/* replaces characters in a string */
HARBOUR HB_STUFF( void )
{
   if( ISCHAR( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) && ISCHAR( 4 ) )
   {
      char * szText = hb_parc( 1 );
      ULONG ulText = hb_parclen( 1 );
      ULONG ulPos = hb_parnl( 2 );
      ULONG ulDel = hb_parnl( 3 );
      ULONG ulInsert = hb_parclen( 4 );

      ULONG ulTotalLen;

      if( ulPos > 0 )
         ulPos--;

      if( ulPos > ulText )
         ulPos = ulText;

      if( ulDel > ulText - ulPos )
         ulDel = ulText - ulPos;

      if( ( ulTotalLen = ulText + ulInsert - ulDel ) > 0 )
      {
         char * szResult = ( char * ) hb_xgrab( ulTotalLen + 1 );

         hb_xmemcpy( szResult, szText, ulPos );
         hb_xmemcpy( szResult + ulPos, hb_parc( 4 ), ulInsert );
         hb_xmemcpy( szResult + ulPos + ulInsert, szText + ulPos + ulDel, ulText - ( ulPos + ulDel ) );

         szResult[ ulTotalLen ] = '\0';
         hb_retclen( szResult, ulTotalLen );
         hb_xfree( szResult );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}

