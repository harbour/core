/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Number manipulation
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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

#include <math.h>

#include "hbapi.h"
#include "hbapiitm.h"

#define PI  ( 3.1415926535897932384626433 )

/* Ceiling( <nNumber> ) --> nInteger
   Return the smallest integer that is greater than or equal to <nNumber>
*/
HB_FUNC( CEILING )
{
   hb_retnl( ceil( hb_parnd( 1 ) ) );
}

/* DtoR( <nDegrees> ) --> nRadians
   Convert an angle size specified in radians to degrees
*/
HB_FUNC( DTOR )
{
   hb_retndlen( ( hb_parnd( 1 ) / 180 ) * PI, 10, 9 );
}

/* Floor( <nNumber> ) --> nInteger
   Return the largest integer that is less than or equal to <nNumber>
*/
HB_FUNC( FLOOR )
{
   hb_retnl( floor( hb_parnd( 1 ) ) );
}

/* NumAsLog10( <nNumber> ) --> nLog10 
   Convert a positive number to log base 10
*/
HB_FUNC( NUMASLOG10 )
{
   if( ISNUM( 1 ) )
   {
      hb_retnd( log10( hb_parnd(1) ) );
   }
}

/* NumGetDecimals( <nNumber> ) --> nDecimals
   Determine the number of decimal digits
*/
HB_FUNC( NUMGETDECIMALS )
{
   int iDec = 0;

   if( ISNUM( 1 ) )
   {
      hb_itemGetNLen( hb_param( 1, HB_IT_NUMERIC ), NULL, &iDec );
   }

   hb_retnl( iDec );
}

/* NumGetLen( <nNumber> ) --> nDigits
   Determine the number of whole number digits
*/
HB_FUNC( NUMGETLEN )
{
   ULONG ulLen = 0;

   if( ISNUM( 1 ) )
   {
      char * szBuffer = hb_itemStr( hb_param( 1, HB_IT_NUMERIC ), NULL, NULL );
      char * ptr = szBuffer;

      while( HB_ISSPACE( *ptr ) )
         ptr++;

      if( !strchr( ptr, '.' ) )
      {
         while( *ptr )
         {
            ptr++;
            ulLen++;
         }
      }
      else
      {
         while( *ptr != '.' )
         {
            ptr++;
            ulLen++;
         }
      }

      if( szBuffer )
         hb_xfree( szBuffer );
   }

   hb_retnl( ulLen );
}

/* RtoD( <nRadians> ) --> nDegrees
   Convert an angle size specified in radians to degrees
*/
HB_FUNC( RTOD )
{
   hb_retnd( 180 * ( hb_parnd( 1 ) / PI ) );
}

/* Sign( <nNumber> ) --> nSign
   Return the sign of a number as follows:
      0 - <nNumber> is zero
      1 - <nNumber> is positive
     -1 - <nNumber> is negative
*/
HB_FUNC( SIGN )
{
   if( ISNUM( 1 ) )
   {
      long lNumber = hb_parnl( 1 );

      hb_retni( lNumber == 0 ? 0 : ( lNumber > 0 ? 1 : -1 ) );
   }
}

/* NumAsCurrency( <nNumber>, <cSymbol>, <nSide> ) --> cCurrency
   Convert number to currency format, floating dollar symbol
*/
HB_FUNC( NUMASCURRENCY )
{
   char * szBuffer = hb_itemStr( hb_param( 1, HB_IT_NUMERIC ), NULL, NULL );
   long ulSymbolLen = hb_itemGetCLen( hb_param( 2, HB_IT_STRING ) );
   char * ptr = szBuffer;
   char * szCurrency;
   ULONG ulLen;

   ulLen = strlen( ptr );
   szCurrency = ( char * ) hb_xgrab( ulLen + ulSymbolLen ) ;

   if( hb_parni( 3 ) < 0 )
   {
      while( HB_ISSPACE( *ptr ) )
         ptr++;

      ulLen = strlen( ptr );

      memcpy( szCurrency, hb_parc( 2 ), ulSymbolLen );
      memcpy( szCurrency + ulSymbolLen, ptr, ulLen );
   }
   else
   {
      memcpy( szCurrency, ptr, ulLen );
      memcpy( szCurrency + ulLen, hb_parc( 2 ), ulSymbolLen );
   }

   if( szBuffer )
      hb_xfree( szBuffer );

   hb_retclen( szCurrency, ulLen + ulSymbolLen );
   hb_xfree( szCurrency );
}
