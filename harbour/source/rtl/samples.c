/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility functions from the SAMPLES directory of Clipper.
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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
#include "itemapi.h"

/* NOTE: szTime must be 9 chars large */

static char * hb_SecToTimeStr( char * pszTime, ULONG ulTime )
{
   USHORT uiValue;

   uiValue = ( ulTime / 3600 ) % 24;
   pszTime[ 0 ] = ( char ) ( uiValue / 10 ) + '0';
   pszTime[ 1 ] = ( char ) ( uiValue % 10 ) + '0';
   pszTime[ 2 ] = ':';
   uiValue = ( ulTime / 60 ) % 60;
   pszTime[ 3 ] = ( char ) ( uiValue / 10 ) + '0';
   pszTime[ 4 ] = ( char ) ( uiValue % 10 ) + '0';
   pszTime[ 5 ] = ':';
   uiValue = ulTime % 60;
   pszTime[ 6 ] = ( char ) ( uiValue / 10 ) + '0';
   pszTime[ 7 ] = ( char ) ( uiValue % 10 ) + '0';
   pszTime[ 8 ] = '\0';

   return pszTime;
}

static ULONG hb_TimeStrToSec( char * pszTime )
{
   ULONG ulLen = strlen( pszTime );
   ULONG ulTime = 0;

   if( ulLen >= 0 )
      ulTime += ( ULONG ) hb_strVal( pszTime ) * 3600;

   if( ulLen >= 4 )
      ulTime += ( ULONG ) hb_strVal( pszTime + 3 ) * 60;

   if( ulLen >= 7 )
      ulTime += ( ULONG ) hb_strVal( pszTime + 6 );

   return ulTime;
}

HARBOUR HB_AMPM( void )
{
   char * pszTime = hb_parc( 1 );
   ULONG  ulTimeLen = hb_parclen( 1 );
   char * pszResult = ( char * ) hb_xgrab( MAX( ulTimeLen, 2 ) + 3 + 1 );
   USHORT uiHour = ( USHORT ) hb_strVal( pszTime );
   BOOL   bAM;

   memset( pszResult, 0, 3 );
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
         pszResult[ 0 ] =  ' ';

      bAM = FALSE;
   }
   else if( uiHour == 12 )
      bAM = FALSE;
   else
      bAM = TRUE;

   strcpy( pszResult + ulTimeLen, bAM ? " am" : " pm" );

   hb_retclen( pszResult, ulTimeLen + 3 );
   hb_xfree( pszResult );
}

HARBOUR HB_DAYS( void )
{
   hb_retnl( hb_parnl( 1 ) / 86400 );
}

HARBOUR HB_ELAPTIME( void )
{
   ULONG ulStart = hb_TimeStrToSec( hb_parc( 1 ) );
   ULONG ulEnd = hb_TimeStrToSec( hb_parc( 2 ) );
   char szTime[ 9 ];

   hb_retc( hb_SecToTimeStr( szTime, ( ulEnd < ulStart ? 86400 : 0 ) + ulEnd - ulStart ) );
}

HARBOUR HB_LENNUM( void )
{
   PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );
   ULONG ulLen = 0;

   if( pNumber )
   {
      char * pszString = hb_itemStr( pNumber, NULL, NULL );

      if( pszString )
      {
         ulLen = strlen( pszString );
         hb_strLTrim( pszString, &ulLen );
         hb_xfree( pszString );
      }
   }

   hb_retnl( ulLen );
}

HARBOUR HB_SECS( void )
{
   hb_retnl( hb_TimeStrToSec( hb_parc( 1 ) ) );
}

HARBOUR HB_TSTRING( void )
{
   char szTime[ 9 ];

   hb_retc( hb_SecToTimeStr( szTime, hb_parnl( 1 ) ) );
}
