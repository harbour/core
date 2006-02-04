/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date conversion module
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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_dateEncStr()
 *    hb_dateDecStr()
 *    hb_dateStrPut()
 *    hb_dateStrGet()
 *
 */

#include "hbapi.h"
#include "hbdate.h"

#ifdef HB_C52_STRICT
   #define HB_DATE_YEAR_LIMIT    2999
#else
   #define HB_DATE_YEAR_LIMIT    9999
#endif

#define HB_STR_DATE_BASE      1721060     /* 0000/01/01 */

HB_EXPORT LONG hb_dateEncode( int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncode(%d, %d, %d)", iYear, iMonth, iDay));

   /* Perform date validation */
   if( iYear >= 0 && iYear <= HB_DATE_YEAR_LIMIT &&
       iMonth >= 1 && iMonth <= 12 &&
       iDay >= 1 )
   {
      /* Month, year, and lower day limits are simple,
         but upper day limit is dependent upon month and leap year */
      static int auiDayLimit[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

      if( iDay <= auiDayLimit[ iMonth - 1 ] ||
          ( iDay == 29 && iMonth == 2 &&
            ( iYear & 3 ) == 0 && ( iYear % 100 != 0 || iYear % 400 == 0 ) ) )
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

HB_EXPORT void hb_dateDecode( LONG lJulian, int *piYear, int *piMonth, int *piDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecode(%ld, %p, %p, %p)", lJulian, piYear, piMonth, piDay));

   if( lJulian >= HB_STR_DATE_BASE )
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

HB_EXPORT void hb_dateStrPut( char * szDate, int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrPut(%p, %d, %d, %d)", szDate, iYear, iMonth, iDay));

   if( iYear >= 0 && iMonth > 0 && iDay > 0 )
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
   else
   {
      memset( szDate, '0', 8 );
   }
}

HB_EXPORT void hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay )
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

HB_EXPORT int hb_dateJulianDOW( LONG lJulian )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateJulianDOW(%ld)", lJulian));

   if( lJulian >= HB_STR_DATE_BASE )
      return ( int ) ( ( lJulian + 1 ) % 7 ) + 1;
   else
      return 0;
}

HB_EXPORT int hb_dateDOW( int iYear, int iMonth, int iDay )
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

/* This function always closes the date with a zero byte, so it needs a
   9 character long buffer. */

HB_EXPORT char * hb_dateDecStr( char * szDate, LONG lJulian )
{
   int iYear, iMonth, iDay;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecStr(%p, %ld)", szDate, lJulian));

   if( lJulian == 0 )
   {
      memset( szDate, ' ', 8 );
   }
   else
   {
      hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
      hb_dateStrPut( szDate, iYear, iMonth, iDay );
   }
   szDate[ 8 ] = '\0';

   return szDate;
}

HB_EXPORT LONG hb_dateEncStr( const char * szDate )
{
   int  iYear, iMonth, iDay;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncStr(%s)", szDate));

   hb_dateStrGet( szDate, &iYear, &iMonth, &iDay );

   return hb_dateEncode( iYear, iMonth, iDay );
}
