/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility functions from the SAMPLES directory of Clipper.
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

/* NOTE: szTime must be 9 chars large. */

static char * hb_SecToTimeStr( char * pszTime, ULONG ulTime )
{
   USHORT uiValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_SecToTimeStr(%s, %lu)", pszTime, ulTime));

   uiValue = ( USHORT ) ( ( ulTime / 3600 ) % 24 );
   pszTime[ 0 ] = ( char ) ( uiValue / 10 ) + '0';
   pszTime[ 1 ] = ( char ) ( uiValue % 10 ) + '0';
   pszTime[ 2 ] = ':';
   uiValue = ( USHORT ) ( ( ulTime / 60 ) % 60 );
   pszTime[ 3 ] = ( char ) ( uiValue / 10 ) + '0';
   pszTime[ 4 ] = ( char ) ( uiValue % 10 ) + '0';
   pszTime[ 5 ] = ':';
   uiValue = ( USHORT ) ( ulTime % 60 );
   pszTime[ 6 ] = ( char ) ( uiValue / 10 ) + '0';
   pszTime[ 7 ] = ( char ) ( uiValue % 10 ) + '0';
   pszTime[ 8 ] = '\0';

   return pszTime;
}

static ULONG hb_TimeStrToSec( char * pszTime )
{
   ULONG ulLen;
   ULONG ulTime = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_TimeStrToSec(%s)", pszTime));

   ulLen = strlen( pszTime );

   if( ulLen >= 1 )
      ulTime += ( ULONG ) hb_strVal( pszTime, ulLen ) * 3600;

   if( ulLen >= 4 )
      ulTime += ( ULONG ) hb_strVal( pszTime + 3, ulLen - 3 ) * 60;

   if( ulLen >= 7 )
      ulTime += ( ULONG ) hb_strVal( pszTime + 6, ulLen - 6 );

   return ulTime;
}

HB_FUNC( DAYS )
{
   hb_retnl( hb_parnl( 1 ) / 86400 );
}

HB_FUNC( ELAPTIME )
{
   ULONG ulStart = hb_TimeStrToSec( hb_parc( 1 ) );
   ULONG ulEnd = hb_TimeStrToSec( hb_parc( 2 ) );
   char szTime[ 9 ];

   hb_retc( hb_SecToTimeStr( szTime, ( ulEnd < ulStart ? 86400 : 0 ) + ulEnd - ulStart ) );
}

HB_FUNC( SECS )
{
   hb_retnl( hb_TimeStrToSec( hb_parc( 1 ) ) );
}

HB_FUNC( TSTRING )
{
   char szTime[ 9 ];

   hb_retc( hb_SecToTimeStr( szTime, hb_parnl( 1 ) ) );
}
