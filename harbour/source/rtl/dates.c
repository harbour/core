/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date API
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
 *    hb_secondsToday()
 *    HB_SECONDS()
 *    hb_cmonth()
 *    HB_CMONTH()
 *    hb_cdow()
 *    HB_CDOW()
 *    HB_DAY()
 *    HB_MONTH()
 *    HB_YEAR()
 *    hb_dow()
 *    HB_DOW()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    HB_CTOD()
 *    HB_DATE()
 *    hb_dtoc()
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_dateEncStr()
 *    hb_dateDecStr()
 *    hb_dateStrPut()
 *    hb_dateStrGet()
 *    HB_STOD()
 *    HB_HB_STOD()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: The following #include "hbwinapi.h" must
         be ahead of any other #include statements! */
#include "hbwinapi.h"

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"

#include <ctype.h>
#include <time.h>
#if defined( OS_UNIX_COMPATIBLE )
   #include <sys/timeb.h>
#else
   #include <sys\timeb.h>
#endif
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__)
   #include <dos.h>
#endif

double hb_secondsToday( void )
{
#if defined(_MSC_VER)
   #define timeb _timeb
   #define ftime _ftime
#endif
   struct timeb tb;
   struct tm * oTime;

   HB_TRACE(HB_TR_DEBUG, ("hb_secondsToday()"));

   ftime( &tb );
   oTime = localtime( &tb.time );

   return ( oTime->tm_hour * 3600 ) +
          ( oTime->tm_min * 60 ) +
            oTime->tm_sec +
          ( ( double ) tb.millitm / 1000 );
}

char * hb_cmonth( int iMonth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cmonth(%d)", iMonth));

   return ( iMonth >= 1 && iMonth <= 12 ) ? hb_monthsname[ iMonth - 1 ] : "";
}

char * hb_cdow( int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdow(%d)", iDay));

   return ( iDay >= 1 && iDay <= 7 ) ? hb_daysname[ iDay - 1 ] : "";
}

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

HARBOUR HB_CTOD( void )
{
   if( ISCHAR( 1 ) )
   {
      char * szDate = hb_parc( 1 );
      int d_value = 0, m_value = 0, y_value = 0;
      char szDateFormat[ 9 ];

      if( szDate )
      {
         int d_pos = 0, m_pos = 0, y_pos = 0;
         int count, digit, size = strlen( hb_set.HB_SET_DATEFORMAT );

         for( count = 0; count < size; count++ )
         {
            switch( hb_set.HB_SET_DATEFORMAT[ count ] )
            {
               case 'D':
               case 'd':
                  if( d_pos == 0 )
                  {
                     if( m_pos == 0 && y_pos == 0 ) d_pos = 1;
                     else if( m_pos == 0 || y_pos == 0 ) d_pos = 2;
                     else d_pos = 3;
                  }
                  break;
               case 'M':
               case 'm':
                  if( m_pos == 0 )
                  {
                     if( d_pos == 0 && y_pos == 0 ) m_pos = 1;
                     else if( d_pos == 0 || y_pos == 0 ) m_pos = 2;
                     else m_pos = 3;
                  }
                  break;
               case 'Y':
               case 'y':
                  if( y_pos == 0 )
                  {
                     if( m_pos == 0 && d_pos == 0 ) y_pos = 1;
                     else if( m_pos == 0 || d_pos == 0 ) y_pos = 2;
                     else y_pos = 3;
                  }
            }
         }

         size = strlen( szDate );

         for( count = 0; count < size; count++ )
         {
            digit = szDate[ count ];
            if( isdigit( digit ) )
            {
               if( d_pos == 1 )
                  d_value = ( d_value * 10 ) + digit - '0';
               else if( m_pos == 1 )
                  m_value = ( m_value * 10 ) + digit - '0';
               else if( y_pos == 1 )
                  y_value = ( y_value * 10 ) + digit - '0';
            }
            else if( digit != ' ' )
            {
               d_pos--;
               m_pos--;
               y_pos--;
            }
         }

         if( y_value >= 0 && y_value < 100 )
         {
            count = hb_set.HB_SET_EPOCH % 100;
            digit = hb_set.HB_SET_EPOCH / 100;

            if( y_value >= count )
               y_value += ( digit * 100 );
            else
               y_value += ( ( digit * 100 ) + 100 );
         }
      }

      sprintf( szDateFormat, "%04i%02i%02i", y_value, m_value, d_value );

      hb_retds( szDateFormat );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1119, NULL, "CTOD" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
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

HARBOUR HB_DTOC( void )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];

      hb_retc( hb_dtoc( hb_pardsbuff( szDate, 1 ), szFormatted, hb_set.HB_SET_DATEFORMAT ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1118, NULL, "DTOC" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_DTOS( void )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];

      hb_retc( hb_pardsbuff( szDate, 1 ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1120, NULL, "DTOS" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible function */
/* NOTE: XBase++ checks for the parameter count at compile time */

HARBOUR HB_STOD( void )
{
   hb_retds( hb_parc( 1 ) );
}

#endif

/* NOTE: Harbour extension, exactly the same as STOD(). */

HARBOUR HB_HB_STOD( void )
{
   hb_retds( hb_parc( 1 ) );
}

HARBOUR HB_YEAR( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );

      hb_retnllen( lYear, 5 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1112, NULL, "YEAR" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_MONTH( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );

      hb_retnllen( lMonth, 3 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1113, NULL, "MONTH" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_DAY( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );

      hb_retnllen( lDay, 3 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1114, NULL, "DAY" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_TIME( void )
{
   char szResult[ 9 ];

   #if defined(_Windows) || defined(WINNT)
      SYSTEMTIME st;
      GetLocalTime( &st );
      sprintf( szResult, "%02d:%02d:%02d", st.wHour, st.wMinute, st.wSecond );
   #else
      time_t t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
      sprintf( szResult, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min, oTime->tm_sec );
   #endif

   hb_retclen( szResult, 8 );
}

HARBOUR HB_DATE( void )
{
   char szResult[ 9 ];

   #if defined(_Windows) || defined(WINNT)
      SYSTEMTIME st;
      GetLocalTime( &st );
      sprintf( szResult, "%04d%02d%02d", st.wYear, st.wMonth, st.wDay );
   #else
      time_t t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
      sprintf( szResult, "%04d%02d%02d", oTime->tm_year + 1900, oTime->tm_mon + 1, oTime->tm_mday );
   #endif

   hb_retds( szResult );
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

HARBOUR HB_DOW( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      if( pDate->item.asDate.value )
      {
         long lDay, lMonth, lYear;

         hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );

         hb_retnllen( hb_dow( lDay, lMonth, lYear ), 3 );
      }
      else
         hb_retnllen( 0, 3 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1115, NULL, "DOW" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_CMONTH( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      hb_retc( hb_cmonth( lMonth ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1116, NULL, "CMONTH" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_CDOW( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      if( pDate->item.asDate.value )
      {
         long lDay, lMonth, lYear;

         hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
         hb_retc( hb_cdow( hb_dow( lDay, lMonth, lYear ) ) );
      }
      else
         hb_retc( "" );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1117, NULL, "CDOW" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_SECONDS( void )
{
   hb_retnd( hb_secondsToday() );
}

