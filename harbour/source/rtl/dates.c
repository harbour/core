/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date API (C level)
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_dow()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_dtoc()
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_dateEncStr()
 *    hb_dateDecStr()
 *    hb_dateStrPut()
 *    hb_dateStrGet()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbdate.h"

long hb_dateEncode( long lDay, long lMonth, long lYear )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncode(%ld, %ld, %ld)", lDay, lMonth, lYear));

   /* Perform date validation */
   if( lYear >= 1 && lYear <= 2999 &&
       lMonth >= 1 && lMonth <= 12 &&
       lDay >= 1 )
   {
      /* Month, year, and lower day limits are simple,
         but upper day limit is dependent upon month and leap year */
      USHORT auiDayLimit[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

      if( ( ( lYear % 4 == 0 && lYear % 100 != 0 ) || lYear % 400 == 0 ) )
         auiDayLimit[ 1 ] = 29;

      if( lDay <= ( long ) auiDayLimit[ ( int ) lMonth - 1 ] )
      {
         long lFactor = ( lMonth < 3 ) ? -1 : 0;

         return ( 1461 * ( lFactor + 4800 + lYear ) / 4 ) +
                ( ( lMonth - 2 - ( lFactor * 12 ) ) * 367 ) / 12 -
                ( 3 * ( ( lFactor + 4900 + lYear ) / 100 ) / 4 ) +
                lDay - 32075;
      }
   }

   return 0;
}

void hb_dateDecode( long lJulian, long * plDay, long * plMonth, long * plYear )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecode(%ld, %p, %p, %p)", lJulian, plDay, plMonth, plYear));

   if( lJulian > 0 )
   {
      long U, V, W, X;

      lJulian += 68569;
      W = ( lJulian * 4 ) / 146097;
      lJulian -= ( ( 146097 * W ) + 3 ) / 4;
      X = 4000 * ( lJulian + 1 ) / 1461001;
      lJulian -= ( ( 1461 * X ) / 4 ) - 31;
      V = 80 * lJulian / 2447;
      U = V / 11;

      *plDay   = lJulian - ( 2447 * V / 80 );
      *plMonth = V + 2 - ( U * 12 );
      *plYear  = X + U + ( W - 49 ) * 100;
   }
   else
   {
      *plDay   =
      *plMonth =
      *plYear  = 0;
   }
}

void hb_dateStrPut( char * szDate, long lDay, long lMonth, long lYear )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrPut(%p, %ld, %ld, %ld)", szDate, lDay, lMonth, lYear));

   if( lDay && lMonth && lYear )
   {
      szDate[ 0 ] = ( lYear / 1000 ) + '0';
      szDate[ 1 ] = ( ( lYear % 1000 ) / 100 ) + '0';
      szDate[ 2 ] = ( ( lYear % 100 ) / 10 ) + '0';
      szDate[ 3 ] = ( lYear % 10 ) + '0';

      szDate[ 4 ] = ( lMonth / 10 ) + '0';
      szDate[ 5 ] = ( lMonth % 10 ) + '0';

      szDate[ 6 ] = ( lDay / 10 ) + '0';
      szDate[ 7 ] = ( lDay % 10 ) + '0';
   }
   else
      memset( szDate, ' ', 8 );
}

void hb_dateStrGet( const char * szDate, long * plDay, long * plMonth, long * plYear )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrGet(%s, %p, %p, %p)", szDate, plDay, plMonth, plYear));

   if( szDate && szDate[ 8 ] == '\0' )
   {
      /* Date string has correct length, so attempt to convert */
      *plDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
      *plMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
      *plYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
                 ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );
   }
   else
   {
      /* Date string missing or bad length, so force an empty date */
      *plDay   =
      *plMonth =
      *plYear  = 0;
   }
}

/* This function always closes the date with a zero byte, so it needs a
   9 character long buffer. */

char * hb_dateDecStr( char * szDate, long lJulian )
{
   long lDay, lMonth, lYear;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecStr(%p, %ld)", szDate, lJulian));

   hb_dateDecode( lJulian, &lDay, &lMonth, &lYear );
   hb_dateStrPut( szDate, lDay, lMonth, lYear );
   szDate[ 8 ] = '\0';

   return szDate;
}

long hb_dateEncStr( char * szDate )
{
   long lDay, lMonth, lYear;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncStr(%s)", szDate));

   hb_dateStrGet( szDate, &lDay, &lMonth, &lYear );

   return hb_dateEncode( lDay, lMonth, lYear );
}

/* NOTE: szFormattedDate must be an at least 11 chars wide buffer */

char * hb_dtoc( const char * szDate, char * szFormattedDate, const char * szDateFormat )
{
   /*
    * NOTE: szFormattedDate must point to a buffer of at least 11 bytes.
    *       szDateFormat must point to a buffer holding the date format to use.
    */
   int format_count, digit_count, size;

   HB_TRACE(HB_TR_DEBUG, ("hb_dtoc(%s, %p, %s)", szDate, szFormattedDate, szDateFormat));

   /*
    * Determine the maximum size of the formatted date string
    */
   size = strlen( szDateFormat );
   if( size > 10 ) size = 10;

   if( szDate && szFormattedDate && strlen( szDate ) == 8 ) /* A valid date is always 8 characters */
   {
      const char * szPtr;
      int digit;
      BOOL used_d, used_m, used_y;

      format_count = 0;
      used_d = used_m = used_y = FALSE;
      szPtr = szDateFormat;

      while( format_count < size )
      {
         digit = toupper( *szPtr );
         szPtr++;
         digit_count = 1;
         while( toupper( *szPtr ) == digit && format_count < size )
         {
            szPtr++;
            if( format_count + digit_count < size ) digit_count++;
         }
         switch( digit )
         {
            case 'D':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = '0';
                        digit_count--;
                     }
                  case 3:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = '0';
                        digit_count--;
                     }
                  case 2:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
                  default:
                     if( ! used_d && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 7 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size ) szFormattedDate[ format_count++ ] = digit;
               }
               used_d = TRUE;
               break;

            case 'M':
               switch ( digit_count )
               {
                  case 4:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = '0';
                        digit_count--;
                     }
                  case 3:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = '0';
                        digit_count--;
                     }
                  case 2:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
                  default:
                     if( ! used_m && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 5 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size ) szFormattedDate[ format_count++ ] = digit;
               }
               used_m = TRUE;
               break;

            case 'Y':
               switch( digit_count )
               {
                  case 4:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 0 ];
                        digit_count--;
                     }

                  case 3:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 1 ];
                        digit_count--;
                     }

                  case 2:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 2 ];
                        digit_count--;
                     }

                  default:
                     if( ! used_y && format_count < size )
                     {
                        szFormattedDate[ format_count++ ] = szDate[ 3 ];
                        digit_count--;
                     }
                     while( digit_count-- > 0 && format_count < size ) szFormattedDate[ format_count++ ] = digit;
               }
               used_y = TRUE;
               break;

            default:
               while( digit_count-- > 0 && format_count < size ) szFormattedDate[ format_count++ ] = digit;
         }
      }
   }
   else
   {
      /* Not a valid date string, so return a blank date with separators */
      format_count = size; /* size is either 8 or 10 */
      strncpy( szFormattedDate, szDateFormat, size );

      for( digit_count = 0; digit_count < size; digit_count++ )
      {
         switch( szFormattedDate[ digit_count ] )
         {
            case 'D':
            case 'd':
            case 'M':
            case 'm':
            case 'Y':
            case 'y':
               szFormattedDate[ digit_count ] = ' ';
         }
      }
   }

   szFormattedDate[ format_count ] = '\0';

   return szFormattedDate;
}

long hb_dow( long lDay, long lMonth, long lYear )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dow(%ld, %ld, %ld)", lDay, lMonth, lYear));

   if( lMonth < 3 )
   {
      lMonth += 13;
      lYear--;
   }
   else
      lMonth++;

   return ( lDay + 26 * lMonth / 10 + 
            lYear + lYear / 4 - lYear / 100 + lYear / 400 + 6 ) % 7 + 1;
}

