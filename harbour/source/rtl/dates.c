/*
 * $Id$
 */

/* Harbour Project source code
 * http://www.Harbour-Project.org/
 *
 * Copyright(C) 1999 by Antonio Linares.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * You can contact me at: alinares@fivetech.com
 *
 *  The following functions are Copyright 1999 David G. Holm  <dholm@jsd-llc.com>:
 *     HB_CTOD(), HB_DATE(), hb_dtoc().
 *  See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
 *
 *  The following functions are Copyright 1999 Jose Lalin <dezac@corevia.com>:
 *     hb_seconds(), HB_SECONDS(), hb_cmonth(), HB_CMONTH(),
 *     hb_cdow(), HB_CDOW(), HB_DAY(), HB_MONTH(), HB_YEAR(),
 *     hb_dow(), HB_DOW()
*/

#include "extend.h"
#include "errorapi.h"
#include "itemapi.h"
#include "set.h"
#include "dates.h"

#include <ctype.h>
#include <time.h>
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__)
   #include <dos.h>
#endif
#if defined(_MSC_VER)
   #include <sys\timeb.h>
#endif
#ifndef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
   #define HB_OPTIMIZE_DTOS
#endif

/* The other functions are pulled in automatically by initsymb.c */

double hb_secondsToday( void )
{
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__) /* || defined(_MSC_VER) */
   struct time t;
   gettime( &t );
   return ( ( t.ti_hour * 3600 ) + ( t.ti_min * 60 ) + t.ti_sec ) + ( double ) t.ti_hund / 100;
#elif defined(_MSC_VER)
   struct _timeb tb;
   struct tm *oTime;

   _ftime( &tb );
   oTime = localtime( &tb.time );
   return ( oTime->tm_hour * 3600 ) +
          ( oTime->tm_min * 60 ) +
            oTime->tm_sec +
          ( ( double ) tb.millitm / 1000 );
#else
   time_t t;
   struct tm *oTime;
   time( &t );
   oTime = localtime( &t );
   return ( oTime->tm_hour * 3600 ) + ( oTime->tm_min * 60 ) + oTime->tm_sec;
#endif
}

char * hb_cmonth( int month )
{
   if( month >= 1 && month <= 12 )
      return hb_monthsname[ month - 1 ];
   else
      return "";
}

char * hb_cdow( int day )
{
   if( day >= 1 && day <= 7 )
      return hb_daysname[ day - 1 ];
   else
      return "";
}

long hb_dateEncode( long lDay, long lMonth, long lYear )
{
   BOOL bValid = FALSE;
   long lFactor = ( lMonth < 3 ) ? -1 : 0;

   /* Perform date validation */
   if( lMonth >= 1 && lMonth <= 12 && lDay >= 1
      && lYear >= 1 && lYear <= 2999 )
   {
      /* Month, year, and lower day limits are simple,
         but upper day limit is dependent upon month and leap year */
      int aiDayLimit[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

      if( ( ( lYear % 4 == 0 && lYear % 100 != 0 ) || lYear % 400 == 0 ) )
         aiDayLimit[ 1 ] = 29;

      if( lDay <= ( long ) aiDayLimit[ ( int ) lMonth - 1 ] )
         bValid = TRUE;
   }

   if( bValid )
      return ( 1461 * ( lFactor + 4800 + lYear ) / 4 ) +
             ( ( lMonth - 2 - ( lFactor * 12 ) ) * 367 ) / 12 -
             ( 3 * ( ( lYear + 4900 + lFactor ) / 100 ) / 4 ) +
             lDay - 32075;
   else
      return 0;
}

void hb_dateDecode( long julian, long * plDay, long * plMonth, long * plYear )
{
   if( julian > 0 )
   {
      long U, V, W, X;

      julian += 68569;
      W = ( julian * 4 ) / 146097;
      julian -= ( ( 146097 * W ) + 3 ) / 4;
      X = 4000 * ( julian + 1 ) / 1461001;
      julian -= ( ( 1461 * X ) / 4 ) - 31;
      V = 80 * julian / 2447;
      U = V / 11;

      if( plDay )   *plDay   = julian - ( 2447 * V / 80 );
      if( plMonth ) *plMonth = V + 2 - ( U * 12 );
      if( plYear )  *plYear  = X + U + ( W - 49 ) * 100;
   }
   else
      *plDay   =
      *plMonth =
      *plYear  = 0;
}

void hb_dateStrPut( char * szDate, long lDay, long lMonth, long lYear )
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

void hb_dateStrGet( const char * szDate, long * plDay, long * plMonth, long * plYear )
{
   if( szDate && strlen( szDate ) == 8 )
   {
      /* Date string has correct length, so attempt to convert */
      *plDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
      *plMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
      *plYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
                 ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );
   }
   else
      /* Date string missing or bad length, so force an empty date */
      *plDay   =
      *plMonth =
      *plYear  = 0;
}

HARBOUR HB_CTOD( void )
{
   if( hb_pcount() == 1 )
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
               {
                  d_value = ( d_value * 10 ) + digit - '0';
               }
               else if( m_pos == 1 )
               {
                  m_value = ( m_value * 10 ) + digit - '0';
               }
               else if( y_pos == 1 )
               {
                  y_value = ( y_value * 10 ) + digit - '0';
               }
            }
            else if( digit != ' ' )
            {
               d_pos--;
               m_pos--;
               y_pos--;
            }
         }

         if( y_value > 0 && y_value < 100 )
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
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "CTOD" ); /* NOTE: Clipper catches this at compile time! */
}

char * hb_dtoc( const char * szDate, char * szFormattedDate, const char * szDateFormat )
{
   /*
    * NOTE: szFormattedDate must point to a buffer of at least 11 bytes.
    *       szDateFormat must point to a buffer holding the date format to use.
    */
   int format_count, digit_count, size;

   /*
    * Determine the maximum size of the formatted date string
    */
   size = strlen( szDateFormat );
   if( size > 10 ) size = 10;

   if( szDate && szFormattedDate && strlen( szDate ) == 8 ) /* A valid date is always 8 characters */
   {
      const char *szPtr;
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

   szFormattedDate[ format_count ] = '\0';

   return szFormattedDate;
}

HARBOUR HB_DTOC( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISDATE( 1 ) )
      {
         char * szDate = hb_pards( 1 );
         char szFormatted[ 11 ];

         hb_retc( hb_dtoc( szDate, szFormatted, hb_set.HB_SET_DATEFORMAT ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 1118, NULL, "DTOC" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "DTOC" ); /* NOTE: Clipper catches this at compile time! */
}

/* QUESTION: Should we drop error checkings to make it faster ? */
/*           This function may be called many times in a real world */
/*           application, for example in index creation. */
/*           Clipper does these checks, anyway. */
HARBOUR HB_DTOS( void )
{
#ifndef HB_OPTIMIZE_DTOS
   if( hb_pcount() == 1 )
   {
      if( ISDATE( 1 ) )
      {
#endif
         hb_retc( hb_pards( 1 ) );
#ifndef HB_OPTIMIZE_DTOS
      }
      else
         hb_errRT_BASE( EG_ARG, 1120, NULL, "DTOS" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "DTOS" ); /* NOTE: Clipper catches this at compile time! */
#endif
}

/* NOTE: XBase++ checks for the parameter count at compile time */

HARBOUR HB_STOD( void )
{
   hb_retds( hb_parc( 1 ) );
}

HARBOUR HB_DAY( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pDate = hb_param( 1, IT_DATE );

      if( pDate )
      {
         PHB_ITEM pReturn = hb_itemNew( NULL );
         long lDay, lMonth, lYear;

         hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );

         pReturn->type = IT_LONG;
         pReturn->item.asLong.value = lDay;
         pReturn->item.asLong.length = 3;
         hb_itemReturn( pReturn );
         hb_itemRelease( pReturn );
/*
         hb_retni( lDay );
 * It is dangerous to manipulate the stack return value directly!
         stack.Return.item.asInteger.length = 3;
 */
      }
      else
         hb_errRT_BASE( EG_ARG, 1114, NULL, "DAY" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "DAY" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_MONTH( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pDate = hb_param( 1, IT_DATE );

      if( pDate )
      {
         PHB_ITEM pReturn = hb_itemNew( NULL );
         long lDay, lMonth, lYear;

         hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );

         pReturn->type = IT_LONG;
         pReturn->item.asLong.value = lMonth;
         pReturn->item.asLong.length = 3;
         hb_itemReturn( pReturn );
         hb_itemRelease( pReturn );
/*
         hb_retni( lMonth );
 * It is dangerous to manipulate the stack return value directly!
         stack.Return.item.asInteger.length = 3;
 */
      }
      else
         hb_errRT_BASE( EG_ARG, 1113, NULL, "MONTH" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "MONTH" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_YEAR( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pDate = hb_param( 1, IT_DATE );

      if( pDate )
      {
         PHB_ITEM pReturn = hb_itemNew( NULL );
         long lDay, lMonth, lYear;

         hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );

         pReturn->type = IT_LONG;
         pReturn->item.asLong.value = lYear;
         pReturn->item.asLong.length = 5;
         hb_itemReturn( pReturn );
         hb_itemRelease( pReturn );
/*
         hb_retni( lYear );
 * It is dangerous to manipulate the stack return value directly!
         stack.Return.item.asInteger.length = 5;
 */
      }
      else
         hb_errRT_BASE( EG_ARG, 1112, NULL, "YEAR" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "YEAR" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_TIME( void )
{
   if( hb_pcount() == 0 )
   {
      time_t t;
      struct tm * oTime;
      char szTime[ 9 ];

      time( &t );
      oTime = localtime( &t );
      sprintf( szTime, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min, oTime->tm_sec );
      hb_retclen( szTime, 8 );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "TIME" );
}

HARBOUR HB_DATE( void )
{
   if( hb_pcount() == 0 )
   {
      time_t t;
      struct tm * oTime;
      char szTime[ 9 ];

      time( &t );
      oTime = localtime( &t );
      sprintf( szTime, "%04d%02d%02d", oTime->tm_year + 1900, oTime->tm_mon + 1, oTime->tm_mday );
      hb_retds( szTime );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "DATE" ); /* NOTE: Clipper catches this at compile time! */
}

long hb_dow( long d, long m, long y )
{
   if( m < 3 )
   {
      m += 13;
      y--;
   }
   else
      m++;

   return ( d + 26 * m / 10 + y + y / 4 - y / 100 + y / 400 + 6 ) % 7 + 1;
}

HARBOUR HB_DOW( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pDate = hb_param( 1, IT_DATE );

      if( pDate )
      {
         PHB_ITEM pReturn = hb_itemNew( NULL );

         pReturn->type = IT_LONG;

         if( pDate->item.asDate.value )
         {
            long lDay, lMonth, lYear;

            hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
            pReturn->item.asLong.value = hb_dow( lDay, lMonth, lYear );
            /* hb_retni( hb_dow( lDay, lMonth, lYear ) );
            */
         }
         else
            pReturn->item.asLong.value = 0;
            /* hb_retni( 0 );
            */

         pReturn->item.asLong.length = 3;
         hb_itemReturn( pReturn );
         hb_itemRelease( pReturn );
/*
         stack.Return.item.asInteger.length = 3;
 */
      }
      else
         hb_errRT_BASE( EG_ARG, 1115, NULL, "DOW" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "DOW" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_CMONTH( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pDate = hb_param( 1, IT_DATE );

      if( pDate )
      {
         long lDay, lMonth, lYear;

         hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
         hb_retc( hb_cmonth( lMonth ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 1116, NULL, "CMONTH" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "CMONTH" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_CDOW( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pDate = hb_param( 1, IT_DATE );

      if( pDate )
      {
         long lDay, lMonth, lYear;

         hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
         hb_retc( hb_cdow( hb_dow( lDay, lMonth, lYear ) ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 1117, NULL, "CDOW" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "CDOW" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_SECONDS( void )
{
   if( hb_pcount() == 0 )
      hb_retnd( hb_secondsToday() );
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "SECONDS" ); /* NOTE: Clipper catches this at compile time! */
}
