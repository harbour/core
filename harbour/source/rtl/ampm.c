/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * AMPM() compatibility function from the SAMPLES directory of Clipper.
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

HB_FUNC( AMPM )
{
   char * pszTime = hb_parc( 1 );
   ULONG  ulTimeLen = hb_parclen( 1 );
   char * pszResult = ( char * ) hb_xgrab( HB_MAX( ulTimeLen, 2 ) + 3 + 1 );
   USHORT uiHour = ( USHORT ) hb_strVal( pszTime );
   BOOL   bAM;

   memset( pszResult, '\0', 3 );
   memcpy( pszResult, pszTime, ulTimeLen );

   if( uiHour == 0 || uiHour == 24 )
   {
      if( ulTimeLen < 2 )
         ulTimeLen = 2;

      pszResult[ 0 ] = '1';
      pszResult[ 1 ] = '2';
      bAM = TRUE;
   }
   else if( uiHour > 12 )
   {
      if( ulTimeLen < 2 )
         ulTimeLen = 2;

      uiHour -= 12;
      pszResult[ 0 ] = ( char ) ( uiHour / 10 ) + '0';
      pszResult[ 1 ] = ( char ) ( uiHour % 10 ) + '0';

      if( pszResult[ 0 ] == '0' )
         pszResult[ 0 ] = ' ';

      bAM = FALSE;
   }
   else
      bAM = ( uiHour != 12 );

   strcpy( pszResult + ulTimeLen, bAM ? " am" : " pm" );

   hb_retclen( pszResult, ulTimeLen + 3 );
   hb_xfree( pszResult );
}

