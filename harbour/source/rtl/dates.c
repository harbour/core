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
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_dateDOW()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_dateFormat()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_dateEncStr()
 *    hb_dateDecStr()
 *    hb_dateStrPut()
 *    hb_dateStrGet()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

#include <ctype.h>
#include <time.h>

#include "hbapi.h"
#include "hbdate.h"

#ifdef HB_C52_STRICT
   #define HB_DATE_YEAR_LIMIT    2999
#else
   #define HB_DATE_YEAR_LIMIT    9999
#endif

LONG HB_EXPORT hb_dateEncode( int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncode(%d, %d, %d)", iYear, iMonth, iDay));

   /* Perform date validation */
   if( iYear >= 1 && iYear <= HB_DATE_YEAR_LIMIT &&
       iMonth >= 1 && iMonth <= 12 &&
       iDay >= 1 )
   {
      /* Month, year, and lower day limits are simple,
         but upper day limit is dependent upon month and leap year */
      int auiDayLimit[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

      if( ( ( iYear % 4 == 0 && iYear % 100 != 0 ) || iYear % 400 == 0 ) )
         auiDayLimit[ 1 ] = 29;

      if( iDay <= auiDayLimit[ iMonth - 1 ] )
      {
         int iFactor = ( iMonth < 3 ) ? -1 : 0;

         return ( ( LONG )( iFactor + 4800 + iYear ) * 1461 / 4 ) +
                ( ( LONG )( iMonth - 2 - ( iFactor * 12 ) ) * 367 ) / 12 -
                ( ( LONG )( ( iFactor + 4900 + iYear ) / 100 ) * 3 / 4 ) +
                ( LONG ) iDay - 32075;
      }
   }

   return 0;
}

void HB_EXPORT hb_dateDecode( LONG lJulian, int *piYear, int *piMonth, int *piDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecode(%ld, %p, %p, %p)", lJulian, piYear, piMonth, piDay));

   if( lJulian > 0 )
   {
      LONG U, V, W, X;

      lJulian += 68569;
      W = ( lJulian * 4 ) / 146097;
      lJulian -= ( ( 146097 * W ) + 3 ) / 4;
      X = 4000 * ( lJulian + 1 ) / 1461001;
      lJulian -= ( ( 1461 * X ) / 4 ) - 31;
      V = 80 * lJulian / 2447;
      U = V / 11;

      *piYear  = (int) ( X + U + ( W - 49 ) * 100 );
      *piMonth = (int) ( V + 2 - ( U * 12 ) );
      *piDay   = (int) ( lJulian - ( 2447 * V / 80 ) );
   }
   else
   {
      *piYear  =
      *piMonth =
      *piDay   = 0;
   }
}

void HB_EXPORT hb_dateStrPut( char * szDate, int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrPut(%p, %d, %d, %d)", szDate, iYear, iMonth, iDay));

   if( iYear >= 0 && iMonth && iDay )
   {
      szDate[ 0 ] = ( ( iYear % 10000 ) / 1000 ) + '0';
      szDate[ 1 ] = ( ( iYear % 1000 ) / 100 ) + '0';
      szDate[ 2 ] = ( ( iYear % 100 ) / 10 ) + '0';
      szDate[ 3 ] = ( iYear % 10 ) + '0';

      szDate[ 4 ] = ( iMonth / 10 ) + '0';
      szDate[ 5 ] = ( iMonth % 10 ) + '0';

      szDate[ 6 ] = ( iDay / 10 ) + '0';
      szDate[ 7 ] = ( iDay % 10 ) + '0';
   }
   else if ( iYear || iMonth || iDay )
      memset( szDate, '0', 8 );
   else
      memset( szDate, ' ', 8 );
}

void HB_EXPORT hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrGet(%s, %p, %p, %p)", szDate, piYear, piMonth, piDay));

   if( szDate && szDate[ 8 ] == '\0' )
   {
      /* Date string has correct length, so attempt to convert */
      *piYear  = ( ( USHORT ) ( szDate[ 0 ] - '0' ) * 1000 ) +
                 ( ( USHORT ) ( szDate[ 1 ] - '0' ) * 100 ) +
                 ( ( USHORT ) ( szDate[ 2 ] - '0' ) * 10 ) +
                   ( USHORT ) ( szDate[ 3 ] - '0' );
      *piMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
      *piDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
   }
   else
   {
      /* Date string missing or bad length, so force an empty date */
      *piYear  =
      *piMonth =
      *piDay   = 0;
   }
}

/* This function always closes the date with a zero byte, so it needs a
   9 character long buffer. */

char HB_EXPORT * hb_dateDecStr( char * szDate, LONG lJulian )
{
   int iYear, iMonth, iDay;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecStr(%p, %ld)", szDate, lJulian));

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_dateStrPut( szDate, iYear, iMonth, iDay );
   szDate[ 8 ] = '\0';

   return szDate;
}

LONG HB_EXPORT hb_dateEncStr( char * szDate )
{
   int  iYear, iMonth, iDay;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncStr(%s)", szDate));

   hb_dateStrGet( szDate, &iYear, &iMonth, &iDay );

   return hb_dateEncode( iYear, iMonth, iDay );
}

char HB_EXPORT * hb_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat )
{
   /*
    * NOTE: szFormattedDate must point to a buffer of at least 11 bytes.
    *       szDateFormat must point to a buffer holding the date format to use.
    */
   int format_count, digit_count, size;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateFormat(%s, %p, %s)", szDate, szFormattedDate, szDateFormat));

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
/*                        szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
                        digit_count--;
                     }
                  case 3:
                     if( ! used_d && format_count < size )
                     {
/*                        szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 6 ];
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
/*                        szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
                        digit_count--;
                     }
                  case 3:
                     if( ! used_m && format_count < size )
                     {
/*                        szFormattedDate[ format_count++ ] = '0'; */
                        szFormattedDate[ format_count++ ] = szDate[ 4 ];
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

int HB_EXPORT hb_dateDOW( int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateDOW(%d, %d, %d)", iYear, iMonth, iDay));

   if( iMonth < 3 )
   {
      iMonth += 13;
      iYear--;
   }
   else
      iMonth++;

   return ( iDay + 26 * iMonth / 10 +
            iYear + iYear / 4 - iYear / 100 + iYear / 400 + 6 ) % 7 + 1;
}

void HB_EXPORT hb_dateToday( int * piYear, int * piMonth, int * piDay )
{
#if defined(HB_OS_WIN_32)
   {
      SYSTEMTIME st;
      GetLocalTime( &st );

      *piYear  = st.wYear;
      *piMonth = st.wMonth;
      *piDay   = st.wDay;
   }
#else
   {
      time_t t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );

      *piYear  = oTime->tm_year + 1900;
      *piMonth = oTime->tm_mon + 1;
      *piDay   = oTime->tm_mday;
   }
#endif
}

/* NOTE: The passed buffer must be at least 9 chars long */

void HB_EXPORT hb_dateTimeStr( char * pszTime )
{
#if defined(HB_OS_WIN_32)
   {
      SYSTEMTIME st;
      GetLocalTime( &st );
      sprintf( pszTime, "%02d:%02d:%02d", st.wHour, st.wMinute, st.wSecond );
   }
#else
   {
      time_t t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
      sprintf( pszTime, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min, oTime->tm_sec );
   }
#endif
}
