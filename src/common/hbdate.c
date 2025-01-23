/*
 * The Date conversion module
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 *   (hb_dateEncStr(), hb_dateDecStr(), hb_dateStrPut(), hb_dateStrGet())
 * Copyright 1999 Jose Lalin <dezac@corevia.com> (hb_dateDOW())
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *   (time/timestamp functions)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbapi.h"
#include "hbdate.h"
#if defined( HB_LONG_LONG_OFF )
#  include "hbmath.h"
#endif

#include <time.h>
#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
#  include <sys/time.h>
#elif defined( HB_OS_WIN )
#  include <windows.h>
#  include "hbwinuni.h"
#  if defined( HB_OS_WIN_CE )
#    include "hbwince.h"
#  endif
#else
#  include <sys/timeb.h>
#  if defined( _MSC_VER )
#     define timeb _timeb
#     define ftime _ftime
#  endif
#  ifndef TIME_ZONE_ID_INVALID
#     define TIME_ZONE_ID_INVALID ( DWORD ) 0xFFFFFFFF
#  endif
#endif


#ifdef HB_CLP_STRICT
   #define HB_DATE_YEAR_LIMIT      2999
#else
   #define HB_DATE_YEAR_LIMIT      9999
#endif

#define HB_STR_DATE_BASE           1721060 /* 0000-01-01 */
#define HB_SYS_DATE_BASE           2440588 /* 1970-01-01 */


void hb_timeStampGetLocal( int * piYear, int * piMonth, int * piDay,
                           int * piHour, int * piMinutes,
                           int * piSeconds, int * piMSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampGetLocal(%p,%p,%p,%p,%p,%p,%p)", ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec ) );

#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;

      GetLocalTime( &st );

      *piYear    = st.wYear;
      *piMonth   = st.wMonth;
      *piDay     = st.wDay;
      *piHour    = st.wHour;
      *piMinutes = st.wMinute;
      *piSeconds = st.wSecond;
      *piMSec    = st.wMilliseconds;
   }
#else
   {
      struct tm st;
      time_t seconds, millisecs;

#  if defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
      struct timeval tv;
      gettimeofday( &tv, NULL );
      seconds = tv.tv_sec;
      millisecs = tv.tv_usec / 1000;
#  else
      struct timeb tb;
      ftime( &tb );
      seconds = tb.time;
      millisecs = tb.millitm;
#  endif

#  if defined( HB_HAS_LOCALTIME_R )
      localtime_r( &seconds, &st );
#  else
      st = *localtime( &seconds );
#  endif

      *piYear    = st.tm_year + 1900;
      *piMonth   = st.tm_mon + 1;
      *piDay     = st.tm_mday;
      *piHour    = st.tm_hour;
      *piMinutes = st.tm_min;
      *piSeconds = st.tm_sec;
      *piMSec    = millisecs;
   }
#endif
}

/* return UTC Julian timestamp in milliseconds */
HB_MAXUINT hb_dateMilliSeconds( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateMilliSeconds()" ) );

#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;
      GetSystemTime( &st );
      return ( HB_MAXUINT ) hb_dateEncode( st.wYear, st.wMonth, st.wDay ) *
             HB_MILLISECS_PER_DAY +
             hb_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );
   }
#elif defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
   {
      struct timeval tv;
      gettimeofday( &tv, NULL );
      return ( ( HB_MAXUINT ) tv.tv_sec +
               ( HB_MAXUINT ) HB_SYS_DATE_BASE * HB_SECONDS_PER_DAY ) * 1000 +
             tv.tv_usec / 1000;
   }
#else
   {
      struct timeb tb;
      ftime( &tb );
      return ( ( HB_MAXUINT ) tb.time +
               ( HB_MAXUINT ) HB_SYS_DATE_BASE * HB_SECONDS_PER_DAY ) * 1000 +
             tb.millitm;
   }
#endif
}

/* return local timestamp */
void hb_timeStampGet( long * plJulian, long * plMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampGet(%p,%p)", ( void * ) plJulian, ( void * ) plMilliSec ) );

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
   *plJulian   = hb_dateEncode( iYear, iMonth, iDay );
   *plMilliSec = hb_timeEncode( iHour, iMinute, iSeconds, iMillisec );
}

double hb_dateSeconds( void )
{
   int iYear, iMonth, iDay, iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateSeconds()" ) );

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
   return ( double ) hb_timeEncode( iHour, iMinute, iSeconds, iMillisec ) / 1000;
}

long hb_dateEncode( int iYear, int iMonth, int iDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateEncode(%d, %d, %d)", iYear, iMonth, iDay ) );

   /* Perform date validation */
   if( iYear >= 0 && iYear <= HB_DATE_YEAR_LIMIT &&
       iMonth >= 1 && iMonth <= 12 &&
       iDay >= 1 )
   {
      /* Month, year, and lower day limits are simple,
         but upper day limit is dependent upon month and leap year */
      static const int auiDayLimit[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

      if( iDay <= auiDayLimit[ iMonth - 1 ] ||
          ( iDay == 29 && iMonth == 2 &&
            ( iYear & 3 ) == 0 && ( iYear % 100 != 0 || iYear % 400 == 0 ) ) )
      {
         int iFactor = ( iMonth < 3 ) ? -1 : 0;

         return ( ( long ) ( iFactor + 4800 + iYear ) * 1461 / 4 ) +
                ( ( long ) ( iMonth - 2 - ( iFactor * 12 ) ) * 367 ) / 12 -
                ( ( long ) ( ( iFactor + 4900 + iYear ) / 100 ) * 3 / 4 ) +
                ( long ) iDay - 32075;
      }
   }

   return 0;
}

void hb_dateDecode( long lJulian, int * piYear, int * piMonth, int * piDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateDecode(%ld, %p, %p, %p)", lJulian, ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay ) );

   if( lJulian >= HB_STR_DATE_BASE )
   {
      HB_LONGLONG U, V, W, X, J;

      J = lJulian;
      J += 68569;
      W = ( J * 4 ) / 146097;
      J -= ( ( 146097 * W ) + 3 ) / 4;
      X = 4000 * ( J + 1 ) / 1461001;
      J -= ( ( 1461 * X ) / 4 ) - 31;
      V = 80 * J / 2447;
      U = V / 11;

      *piYear  = ( int ) ( X + U + ( W - 49 ) * 100 );
      *piMonth = ( int ) ( V + 2 - ( U * 12 ) );
      *piDay   = ( int ) ( J - ( 2447 * V / 80 ) );
   }
   else
   {
      *piYear  =
      *piMonth =
      *piDay   = 0;
   }
}

void hb_dateStrPut( char * szDate, int iYear, int iMonth, int iDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateStrPut(%p, %d, %d, %d)", ( void * ) szDate, iYear, iMonth, iDay ) );

   if( iYear >= 0 && iMonth > 0 && iDay > 0 )
   {
      szDate[ 0 ] = ( char ) ( ( ( iYear / 1000 ) % 10 ) + '0' );
      szDate[ 1 ] = ( char ) ( ( ( iYear / 100 ) % 10 ) + '0' );
      szDate[ 2 ] = ( char ) ( ( ( iYear / 10 ) % 10 ) + '0' );
      szDate[ 3 ] = ( char ) ( ( iYear % 10 ) + '0' );

      szDate[ 4 ] = ( char ) ( ( ( iMonth / 10 ) % 10 ) + '0' );
      szDate[ 5 ] = ( char ) ( ( iMonth % 10 ) + '0' );

      szDate[ 6 ] = ( char ) ( ( ( iDay / 10 ) % 10 ) + '0' );
      szDate[ 7 ] = ( char ) ( ( iDay % 10 ) + '0' );
   }
   else
   {
      memset( szDate, '0', 8 );
   }
}

void hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateStrGet(%.8s, %p, %p, %p)", szDate, ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay ) );

#if defined( HB_CLP_STRICT ) || 1
   if( szDate )
#else
   if( szDate &&
       szDate[ 0 ] >= '0' && szDate[ 0 ] <= '9' &&
       szDate[ 1 ] >= '0' && szDate[ 1 ] <= '9' &&
       szDate[ 2 ] >= '0' && szDate[ 2 ] <= '9' &&
       szDate[ 3 ] >= '0' && szDate[ 3 ] <= '9' &&
       szDate[ 4 ] >= '0' && szDate[ 4 ] <= '9' &&
       szDate[ 5 ] >= '0' && szDate[ 5 ] <= '9' &&
       szDate[ 6 ] >= '0' && szDate[ 6 ] <= '9' &&
       szDate[ 7 ] >= '0' && szDate[ 7 ] <= '9' )
#endif
   {
      /* Date string has correct length, so attempt to convert */
      *piYear  = ( ( ( int ) ( szDate[ 0 ] - '0' )   * 10 +
                     ( int ) ( szDate[ 1 ] - '0' ) ) * 10 +
                     ( int ) ( szDate[ 2 ] - '0' ) ) * 10 +
                     ( int ) ( szDate[ 3 ] - '0' );
      *piMonth = ( szDate[ 4 ] - '0' ) * 10 + ( szDate[ 5 ] - '0' );
      *piDay   = ( szDate[ 6 ] - '0' ) * 10 + ( szDate[ 7 ] - '0' );
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

char * hb_dateDecStr( char * szDate, long lJulian )
{
   int iYear, iMonth, iDay;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateDecStr(%p, %ld)", ( void * ) szDate, lJulian ) );

   if( lJulian <= 0 )
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

long hb_dateEncStr( const char * szDate )
{
   int iYear, iMonth, iDay;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateEncStr(%.8s)", szDate ) );

   hb_dateStrGet( szDate, &iYear, &iMonth, &iDay );

   return hb_dateEncode( iYear, iMonth, iDay );
}

int hb_dateJulianDOW( long lJulian )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateJulianDOW(%ld)", lJulian ) );

   if( lJulian >= HB_STR_DATE_BASE )
      return ( int ) ( ( lJulian + 1 ) % 7 ) + 1;
   else
      return 0;
}

HB_BOOL hb_dateDecWeek( long lJulian, int * piYear, int * piWeek, int * piDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateDecWeek(%ld,%p,%p,%p)", lJulian, ( void * ) piYear, ( void * ) piWeek, ( void * ) piDay ) );

   if( lJulian >= HB_STR_DATE_BASE )
   {
      int iMonth, iDay;

      *piDay = ( int ) ( lJulian % 7 ) + 1;
      lJulian += 4 - *piDay;
      hb_dateDecode( lJulian, piYear, &iMonth, &iDay );
      *piWeek = ( lJulian - hb_dateEncode( *piYear, 1, 1 ) ) / 7 + 1;

      return HB_TRUE;
   }

   *piYear = *piWeek = *piDay = 0;

   return HB_FALSE;
}

long hb_dateEncWeek( int iYear, int iWeek, int iDay )
{
   long lDate = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateEncWeek(%d,%d,%d)", iYear, iWeek, iDay ) );

   if( iWeek > 0 && iWeek <= 53 && iDay > 0 && iDay <= 7 )
   {
      lDate = hb_dateEncode( iYear, 1, 1 );
      lDate += ( iWeek - 1 ) * 7 + iDay - ( lDate + 3 ) % 7 + 2;
   }

   return lDate;
}

int hb_dateDOW( int iYear, int iMonth, int iDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateDOW(%d, %d, %d)", iYear, iMonth, iDay ) );

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

void hb_dateToday( int * piYear, int * piMonth, int * piDay )
{
   int iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateToday(%p,%p,%p)", ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay ) );

   hb_timeStampGetLocal( piYear, piMonth, piDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
}

/* NOTE: The passed buffer must be at least 9 chars long */

void hb_dateTimeStr( char * pszTime )
{
   int iYear, iMonth, iDay, iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dateTimeStr(%p)", ( void * ) pszTime ) );

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
   hb_snprintf( pszTime, 9, "%02d:%02d:%02d", iHour, iMinute, iSeconds );
}

/* functions to operate on time and timestamp values */

long hb_timeEncode( int iHour, int iMinutes, int iSeconds, int iMSec )
{
   long lMilliSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeEncode(%d, %d, %d, %d)", iHour, iMinutes, iSeconds, iMSec ) );

   if( iHour >= 0 && iHour < 24 &&
       iMinutes >= 0 && iMinutes < 60 &&
       iSeconds >= 0 && iSeconds < 60 &&
       iMSec >= 0 && iMSec < 1000 ) /* <= intentionally for rounded milliseconds values */
   {
      lMilliSec = ( ( long ) ( iHour * 60 + iMinutes ) * 60 + iSeconds ) *
                  1000 + iMSec;
   }
   else
      lMilliSec = 0;

   return lMilliSec;
}

void hb_timeDecode( long lMilliSec, int * piHour, int * piMinutes,
                    int * piSeconds, int * piMSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeDecode(%ld, %p, %p, %p, %p)", lMilliSec, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec ) );

   if( lMilliSec <= 0 )
   {
      *piHour = *piMinutes = *piSeconds = *piMSec = 0;
   }
   else
   {
      *piMSec = lMilliSec % 1000;
      lMilliSec /= 1000;
      *piSeconds = lMilliSec % 60;
      lMilliSec /= 60;
      *piMinutes = lMilliSec % 60;
      lMilliSec /= 60;
      if( lMilliSec >= 24 )
         *piHour = *piMinutes = *piSeconds = *piMSec = 0;
      else
         *piHour = ( int ) lMilliSec;
   }
}

/* This function always closes the time with a zero byte, so it needs a
 * 13 character long buffer to store time in format "hh:mm:ss.fff"
 */
char * hb_timeStr( char * szTime, long lMilliSec )
{
   int iHour, iMinutes, iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStr(%p, %ld)", ( void * ) szTime, lMilliSec ) );

   hb_timeDecode( lMilliSec, &iHour, &iMinutes, &iSeconds, &iMSec );
   hb_snprintf( szTime, 13, "%02d:%02d:%02d.%03d",
                iHour, iMinutes, iSeconds, iMSec );

   return szTime;
}

HB_BOOL hb_timeStrGetUTC( const char * szTime,
                          int * piHour, int * piMinutes,
                          int * piSeconds, int * piMSec, int *piUTCOffset )
{
   int iHour, iMinutes, iSeconds, iMSec, iUTCOffset, iBlocks;
   HB_BOOL fValid;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStrGetUTC(%s, %p, %p, %p, %p, %p)", szTime, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec, ( void * ) piUTCOffset ) );

   iHour = iMinutes = iSeconds = iMSec = iUTCOffset = iBlocks = 0;
   fValid = HB_FALSE;

   if( szTime )
   {
      while( HB_ISSPACE( *szTime ) )
         ++szTime;

      if( HB_ISDIGIT( *szTime ) )
      {
         iHour = ( *szTime++ - '0' );
         if( HB_ISDIGIT( *szTime ) )
            iHour = iHour * 10 + ( *szTime++ - '0' );
         if( *szTime == ':' && HB_ISDIGIT( szTime[ 1 ] ) )
         {
            ++iBlocks;
            ++szTime;
            iMinutes = ( *szTime++ - '0' );
            if( HB_ISDIGIT( *szTime ) )
               iMinutes = iMinutes * 10 + ( *szTime++ - '0' );
            if( *szTime == ':' && HB_ISDIGIT( szTime[ 1 ] ) )
            {
               ++iBlocks;
               ++szTime;
               iSeconds = ( *szTime++ - '0' );
               if( HB_ISDIGIT( *szTime ) )
                  iSeconds = iSeconds * 10 + ( *szTime++ - '0' );
               if( *szTime == '.' && HB_ISDIGIT( szTime[ 1 ] ) )
               {
                  ++iBlocks;
                  ++szTime;
                  iMSec = ( *szTime++ - '0' ) * 100;
                  if( HB_ISDIGIT( *szTime ) )
                  {
                     iMSec += ( *szTime++ - '0' ) * 10;
                     if( HB_ISDIGIT( *szTime ) )
                        iMSec += ( *szTime++ - '0' );
                  }
                  if( HB_ISDIGIT( *szTime ) )
                     ++szTime;
               }
            }
         }
         if( iBlocks > 0 && ( szTime[ 0 ] == 'Z' || szTime[ 0 ] == 'z' ) )
            ++szTime;
         else
         {
            while( HB_ISSPACE( *szTime ) )
               ++szTime;
            if( ( szTime[ 0 ] == 'p' || szTime[ 0 ] == 'P' ) &&
                ( szTime[ 1 ] == 'm' || szTime[ 1 ] == 'M' ) )
            {
               ++iBlocks;
               szTime += 2;
               if( iHour == 0 )
                  iHour = 24;    /* wrong time */
               else if( iHour != 12 )
                  iHour += 12;
            }
            else if( ( szTime[ 0 ] == 'a' || szTime[ 0 ] == 'A' ) &&
                     ( szTime[ 1 ] == 'm' || szTime[ 1 ] == 'M' ) )
            {
               ++iBlocks;
               szTime += 2;
               if( iHour == 0 )
                  iHour = 24;    /* wrong time */
               else if( iHour == 12 )
                  iHour = 0;
            }
            else
            {
               if( HB_TOUPPER( szTime[ 0 ] ) == 'U' &&
                   HB_TOUPPER( szTime[ 1 ] ) == 'T' &&
                   HB_TOUPPER( szTime[ 2 ] ) == 'C' &&
                   ( szTime[ 3 ] == '+' || szTime[ 3 ] == '-' ) )
                  szTime += 3;
               if( ( szTime[ 0 ] == '+' || szTime[ 0 ] == '-' ) &&
                   HB_ISDIGIT( szTime[ 1 ] ) )
               {
                  HB_BOOL fMinus = ( szTime[ 0 ] == '-' );
                  iUTCOffset = szTime[ 1 ] - '0';
                  szTime += 2;
                  if( HB_ISDIGIT( *szTime ) )
                     iUTCOffset = iUTCOffset * 10 + ( *szTime++ - '0' );
                  iUTCOffset *= 60;
                  if( *szTime == ':' && HB_ISDIGIT( szTime[ 1 ] ) )
                     ++szTime;
                  if( szTime[ 0 ] >= '0' && szTime[ 0 ] <= '5' &&
                      HB_ISDIGIT( szTime[ 1 ] ) )
                  {
                     iUTCOffset += ( szTime[ 0 ] - '0' ) * 10 + ( szTime[ 1 ] - '0' );
                     szTime += 2;
                  }
                  iUTCOffset *= 60;
                  if( fMinus )
                     iUTCOffset = -iUTCOffset;
               }
            }
         }
         while( HB_ISSPACE( *szTime ) )
            ++szTime;
         if( *szTime == 0 && iBlocks > 0 &&
             iHour < 24 && iMinutes < 60 && iSeconds < 60 &&
             iUTCOffset >= -43200 && iUTCOffset <= 43200 )
            fValid = HB_TRUE;
         else
            iHour = iMinutes = iSeconds = iMSec = 0;
      }
   }

   if( piHour )
      *piHour = iHour;
   if( piMinutes )
      *piMinutes = iMinutes;
   if( piSeconds )
      *piSeconds = iSeconds;
   if( piMSec )
      *piMSec = iMSec;
   if( piUTCOffset )
      *piUTCOffset = iUTCOffset;

   return fValid;
}

HB_BOOL hb_timeStrGet( const char * szTime,
                       int * piHour, int * piMinutes,
                       int * piSeconds, int * piMSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStrGet(%s, %p, %p, %p, %p)", szTime, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec ) );

   return hb_timeStrGetUTC( szTime, piHour, piMinutes,
                            piSeconds, piMSec, NULL );
}

void hb_timeStrRawGet( const char * szTime,
                       int * piHour, int * piMinutes,
                       int * piSeconds, int * piMSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStrRawGet(%.10s, %p, %p, %p, %p)", szTime, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec ) );

   *piHour = *piMinutes = *piSeconds = *piMSec = 0;

   if( szTime )
   {
      int iLen = 0;

      while( iLen < 10 && HB_ISDIGIT( szTime[ iLen ] ) )
         ++iLen;

      if( iLen >= 2 && ( ( iLen & 1 ) == 0 || iLen == 7 || iLen == 9 ) )
      {
         *piHour = ( szTime[ 0 ] - '0' ) * 10 + ( szTime[ 1 ] - '0' );
         szTime += 2;
         iLen -= 2;
         if( iLen >= 2 )
         {
            *piMinutes = ( szTime[ 0 ] - '0' ) * 10 + ( szTime[ 1 ] - '0' );
            szTime += 2;
            iLen -= 2;
            if( iLen >= 2 )
            {
               *piSeconds = ( szTime[ 0 ] - '0' ) * 10 + ( szTime[ 1 ] - '0' );
               szTime += 2;
               iLen -= 2;
               switch( iLen )
               {
                  case 4:
                  case 3:
                     *piMSec = ( ( int ) ( szTime[ 0 ] - '0' )   * 10 +
                                 ( int ) ( szTime[ 1 ] - '0' ) ) * 10 +
                                 ( int ) ( szTime[ 2 ] - '0' );
                     break;
                  case 2:
                     *piMSec = ( ( int ) ( szTime[ 0 ] - '0' )   * 10 +
                                 ( int ) ( szTime[ 1 ] - '0' ) ) * 10;
                     break;
                  case 1:
                     *piMSec = ( int ) ( szTime[ 0 ] - '0' ) * 100;
                     break;
               }
            }
         }
      }
   }
}


/* This function always closes the time with a zero byte, so it needs a
 * 18 character long buffer to store time in format "YYYYMMDDhhmmssfff"
 * with trailing 0 byte.
 */
char * hb_timeStampStrRawPut( char * szDateTime, long lJulian, long lMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampStrRawPut(%p, %ld, %ld)", ( void * ) szDateTime, lJulian, lMilliSec ) );

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_dateStrPut( szDateTime, iYear, iMonth, iDay );
   hb_timeDecode( lMilliSec, &iHour, &iMinutes, &iSeconds, &iMSec );
   hb_snprintf( szDateTime + 8, 10, "%02d%02d%02d%03d",
                iHour, iMinutes, iSeconds, iMSec );

   return szDateTime;
}

void hb_timeStampStrRawGet( const char * szDateTime, long * plJulian, long * plMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec, iLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampStrRawGet(%s, %p, %p)", szDateTime, ( void * ) plJulian, ( void * ) plMilliSec ) );

   *plJulian = *plMilliSec = 0;

   iLen = 0;
   while( iLen < 10 && HB_ISDIGIT( szDateTime[ iLen ] ) )
      ++iLen;

   if( iLen == 8 || iLen >= 10 )
   {
      hb_dateStrGet( szDateTime, &iYear, &iMonth, &iDay );
      *plJulian = hb_dateEncode( iYear, iMonth, iDay );
      szDateTime += 8;
      iLen -= 8;
   }

   if( iLen >= 2 )
   {
      hb_timeStrRawGet( szDateTime, &iHour, &iMinutes, &iSeconds, &iMSec );
      *plMilliSec = hb_timeEncode( iHour, iMinutes, iSeconds, iMSec );
   }
}

/* This function always closes the time with a zero byte.
 * It needs a 24 character long buffer for full datetime representation
 * "YYYY-MM-DD hh:mm:ss.fff"
 */
char * hb_timeStampStr( char * szDateTime, long lJulian, long lMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampStr(%p, %ld, %ld)", ( void * ) szDateTime, lJulian, lMilliSec ) );

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_timeDecode( lMilliSec, &iHour, &iMinutes, &iSeconds, &iMSec );
   hb_snprintf( szDateTime, 24, "%04d-%02d-%02d %02d:%02d:%02d.%03d",
                iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec );
   szDateTime[ 23 ] = '\0';

   return szDateTime;
}

HB_BOOL hb_timeStampStrGetUTC( const char * szDateTime,
                               int * piYear, int * piMonth, int * piDay,
                               int * piHour, int * piMinutes, int * piSeconds,
                               int * piMSec, int * piUTCOffset )
{
   int iYear, iMonth, iDay;
   HB_BOOL fValid;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampStrGetUTC(%s, %p, %p, %p, %p, %p, %p, %p, %p)", szDateTime, ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec, ( void * ) piUTCOffset ) );

   iYear = iMonth = iDay = 0;
   fValid = HB_FALSE;

   if( szDateTime )
   {
      while( HB_ISSPACE( *szDateTime ) )
         ++szDateTime;
      if( HB_ISDIGIT( szDateTime[ 0 ] ) && HB_ISDIGIT( szDateTime[ 1 ] ) &&
          HB_ISDIGIT( szDateTime[ 2 ] ) && HB_ISDIGIT( szDateTime[ 3 ] ) &&
          ( szDateTime[ 4 ] == '-' || szDateTime[ 4 ] == '/' || szDateTime[ 4 ] == '.' ) )
      {
         iYear  = ( ( ( int ) ( szDateTime[ 0 ] - '0' )   * 10 +
                      ( int ) ( szDateTime[ 1 ] - '0' ) ) * 10 +
                      ( int ) ( szDateTime[ 2 ] - '0' ) ) * 10 +
                      ( int ) ( szDateTime[ 3 ] - '0' );
         /* ISO 8601 Calendar dates: YYYY-MM-DD */
         if( HB_ISDIGIT( szDateTime[ 5 ] ) && HB_ISDIGIT( szDateTime[ 6 ] ) &&
             szDateTime[ 7 ] == szDateTime[ 4 ] &&
             HB_ISDIGIT( szDateTime[ 8 ] ) && HB_ISDIGIT( szDateTime[ 9 ] ) &&
             ! HB_ISDIGIT( szDateTime[ 10 ] ) )
         {
            iMonth = ( szDateTime[ 5 ] - '0' ) * 10 + ( szDateTime[ 6 ] - '0' );
            iDay   = ( szDateTime[ 8 ] - '0' ) * 10 + ( szDateTime[ 9 ] - '0' );

            if( hb_dateEncode( iYear, iMonth, iDay ) != 0 ||
                ( iYear == 0 && iMonth == 0 && iDay == 0 ) )
            {
               szDateTime += 10;
               fValid = HB_TRUE;
            }
         }
         /* ISO 8601 Week dates: YYYY-Www-D */
         else if( ( szDateTime[ 5 ] == 'W' || szDateTime[ 5 ] == 'w' ) &&
                  HB_ISDIGIT( szDateTime[ 6 ] ) && HB_ISDIGIT( szDateTime[ 7 ] ) &&
                  szDateTime[ 8 ] == szDateTime[ 4 ] &&
                  HB_ISDIGIT( szDateTime[ 9 ] ) && ! HB_ISDIGIT( szDateTime[ 10 ] ) )
         {
            long lDate = hb_dateEncWeek( iYear,
                                         ( szDateTime[ 6 ] - '0' ) * 10 + ( szDateTime[ 7 ] - '0' ),
                                         szDateTime[ 9 ] - '0' );
            if( lDate )
            {
               hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
               szDateTime += 10;
               fValid = HB_TRUE;
            }
         }
         /* ISO 8601 Ordinal dates: YYYY-DDD */
         else if( szDateTime[ 4 ] == '-' && HB_ISDIGIT( szDateTime[ 5 ] ) &&
                  HB_ISDIGIT( szDateTime[ 6 ] ) && HB_ISDIGIT( szDateTime[ 7 ] ) &&
                  ! HB_ISDIGIT( szDateTime[ 8 ] ) )
         {
            iDay = ( ( int ) ( szDateTime[ 5 ] - '0' )   * 10 +
                     ( int ) ( szDateTime[ 6 ] - '0' ) ) * 10 +
                     ( int ) ( szDateTime[ 7 ] - '0' );
            if( iDay > 0 && ( iDay <= 365 || ( iDay == 366 &&
                iYear % 4 == 0 && ( iYear % 100 != 0 || iYear % 400 == 0 ) ) ) )
            {
               long lDate = hb_dateEncode( iYear, 1, 1 );
               if( lDate )
               {
                  hb_dateDecode( lDate + iDay - 1, &iYear, &iMonth, &iDay );
                  szDateTime += 8;
                  fValid = HB_TRUE;
               }
            }
         }

         if( fValid )
         {
            if( *szDateTime == 'T' || *szDateTime == 't' )
            {
               if( HB_ISDIGIT( szDateTime[ 1 ] ) )
                  ++szDateTime;
               fValid = HB_FALSE;
            }
            else
            {
               if( *szDateTime == ',' || *szDateTime == ';' )
                  ++szDateTime;
               while( HB_ISSPACE( *szDateTime ) )
                  ++szDateTime;
               if( *szDateTime == '\0' )
                  szDateTime = NULL;
            }
         }
         else
         {
            iYear = iMonth = iDay = 0;
            szDateTime = NULL;
         }
      }
   }

   if( piHour || piMinutes || piSeconds || piMSec || piUTCOffset ||
       ( ! fValid && szDateTime ) )
   {
      if( hb_timeStrGetUTC( szDateTime, piHour, piMinutes, piSeconds,
                            piMSec, piUTCOffset ) )
         fValid = HB_TRUE;
      else if( szDateTime )
         fValid = HB_FALSE;
   }
   else if( szDateTime )
      fValid = HB_FALSE;

   if( piYear )
      *piYear = iYear;
   if( piMonth )
      *piMonth = iMonth;
   if( piDay )
      *piDay = iDay;

   return fValid;
}

HB_BOOL hb_timeStampStrGet( const char * szDateTime,
                            int * piYear, int * piMonth, int * piDay,
                            int * piHour, int * piMinutes, int * piSeconds,
                            int * piMSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampStrGet(%s, %p, %p, %p, %p, %p, %p, %p)", szDateTime, ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec ) );

   return hb_timeStampStrGetUTC( szDateTime, piYear, piMonth, piDay,
                                 piHour, piMinutes, piSeconds,
                                 piMSec, NULL );
}

HB_BOOL hb_timeStampStrGetDT( const char * szDateTime,
                              long * plJulian, long * plMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec, iUTCOffset;
   HB_BOOL fValid;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampStrGetDT(%s, %p, %p)", szDateTime, ( void * ) plJulian, ( void * ) plMilliSec ) );

   fValid = hb_timeStampStrGetUTC( szDateTime, &iYear, &iMonth, &iDay,
                                   &iHour, &iMinutes, &iSeconds, &iMSec,
                                   &iUTCOffset );
   if( plJulian )
      *plJulian = hb_dateEncode( iYear, iMonth, iDay );
   if( plMilliSec )
      *plMilliSec = hb_timeEncode( iHour, iMinutes, iSeconds, iMSec );

   if( iUTCOffset != 0 && fValid )
   {
      *plMilliSec -= iUTCOffset * 1000;
      if( *plMilliSec < 0 )
      {
         *plMilliSec += HB_MILLISECS_PER_DAY;
         if( --( *plJulian ) < 0 )
            fValid = HB_FALSE;
      }
      else if( *plMilliSec >= HB_MILLISECS_PER_DAY )
      {
         *plMilliSec -= HB_MILLISECS_PER_DAY;
         ++( *plJulian );
      }
   }

   return fValid;
}

double hb_timeStampPackDT( long lJulian, long lMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampPackDT(%ld, %ld)", lJulian, lMilliSec ) );

   return ( double ) lJulian +
          ( double ) lMilliSec / HB_MILLISECS_PER_DAY;
}

void hb_timeStampUnpackDT( double dTimeStamp,
                           long * plJulian, long * plMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampUnpackDT(%f, %p, %p)", dTimeStamp, ( void * ) plJulian, ( void * ) plMilliSec ) );

   {
#if defined( HB_LONG_LONG_OFF )
      double dJulian, dTime;

      dTime = modf( dTimeStamp + 0.5 / HB_MILLISECS_PER_DAY, &dJulian );
      if( plJulian )
         *plJulian = ( long ) dJulian;
      if( plMilliSec )
         *plMilliSec = ( long ) ( dTime * HB_MILLISECS_PER_DAY );
#else
      HB_LONGLONG llMilliSec = ( HB_LONGLONG ) ( dTimeStamp * HB_MILLISECS_PER_DAY + 0.5 );
      if( plJulian )
         *plJulian = ( long ) ( llMilliSec / HB_MILLISECS_PER_DAY );
      if( plMilliSec )
         *plMilliSec = ( long ) ( llMilliSec % HB_MILLISECS_PER_DAY );
#endif
   }
}

double hb_timeStampPack( int iYear, int iMonth, int iDay,
                         int iHour, int iMinutes, int iSeconds, int iMSec )
{
   double dTimeStamp = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampPack(%d, %d, %d, %d, %d, %d, %d)", iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec ) );

   if( iHour >= 0 && iHour < 24 &&
       iMinutes >= 0 && iMinutes < 60 &&
       iSeconds >= 0 && iSeconds < 60 &&
       iMSec >= 0 && iMSec < 1000 )
   {
      long lJulian = hb_dateEncode( iYear, iMonth, iDay );

      if( lJulian != 0 || ( iYear == 0 && iMonth == 0 && iDay == 0 ) )
      {
         dTimeStamp = ( double ) lJulian +
                      ( double ) ( ( ( long ) ( iHour * 60 + iMinutes ) * 60 +
                                     iSeconds ) * 1000 + iMSec ) /
                      HB_MILLISECS_PER_DAY;
      }
   }
   return dTimeStamp;
}

void hb_timeStampUnpack( double dTimeStamp,
                         int * piYear, int * piMonth, int * piDay,
                         int * piHour, int * piMinutes, int * piSeconds,
                         int * piMSec )
{
   long lJulian, lMilliSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampUnpack(%f, %p, %p, %p, %p, %p, %p, %p)", dTimeStamp, ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay, ( void * ) piHour, ( void * ) piMinutes, ( void * ) piSeconds, ( void * ) piMSec ) );

   hb_timeStampUnpackDT( dTimeStamp, &lJulian, &lMilliSec );
   hb_dateDecode( lJulian, piYear, piMonth, piDay );
   hb_timeDecode( lMilliSec, piHour, piMinutes, piSeconds, piMSec );
}

double hb_timeStampPackD( int iYear, int iMonth, int iDay,
                          int iHour, int iMinutes, double dSeconds )
{
   double dTimeStamp = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampPackD(%d, %d, %d, %d, %d, %f)", iYear, iMonth, iDay, iHour, iMinutes, dSeconds ) );

   if( iHour >= 0 && iHour < 24 &&
       iMinutes >= 0 && iMinutes < 60 &&
       dSeconds >= 0 && dSeconds < 60 )
   {
      long lJulian = hb_dateEncode( iYear, iMonth, iDay );

      if( lJulian != 0 || ( iYear == 0 && iMonth == 0 && iDay == 0 ) )
      {
         dTimeStamp = ( double ) lJulian +
                      ( double ) ( ( ( iHour * 60 + iMinutes ) * 60 ) +
                                   dSeconds ) / HB_SECONDS_PER_DAY;
      }
   }
   return dTimeStamp;
}

void hb_timeStampUnpackD( double dTimeStamp,
                          int * piYear, int * piMonth, int * piDay,
                          int * piHour, int * piMinutes, double * pdSeconds )
{
   long lJulian, lMilliSec;
   int iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampUnpackD(%f, %p, %p, %p, %p, %p, %p)", dTimeStamp, ( void * ) piYear, ( void * ) piMonth, ( void * ) piDay, ( void * ) piHour, ( void * ) piMinutes, ( void * ) pdSeconds ) );

   hb_timeStampUnpackDT( dTimeStamp, &lJulian, &lMilliSec );
   hb_dateDecode( lJulian, piYear, piMonth, piDay );
   hb_timeDecode( lMilliSec, piHour, piMinutes, &iSeconds, &iMSec );

   if( pdSeconds )
      *pdSeconds = ( double ) iSeconds + ( double ) iMSec / 1000;
}

long hb_timeUTCOffset( void ) /* in seconds */
{
#if defined( HB_OS_WIN )
   {
      TIME_ZONE_INFORMATION tzInfo;
      DWORD retval;

      memset( &tzInfo, 0, sizeof( tzInfo ) );
      retval = GetTimeZoneInformation( &tzInfo );

      /* disabled because users reported that in some
       * countries/windows versions GetTimeZoneInformation()
       * returns TIME_ZONE_ID_INVALID but sets correct
       * tzInfo.StandardBias field.
       */
#if 0
      if( retval == TIME_ZONE_ID_INVALID )
         return 0;
#endif

      return -( tzInfo.Bias +
            ( retval == TIME_ZONE_ID_DAYLIGHT ? tzInfo.DaylightBias :
                      /*TIME_ZONE_ID_STANDARD*/ tzInfo.StandardBias ) ) * 60;
   }
#else
   {
      struct tm timeinfo;
      time_t current, utc, local;

      time( &current );

#if defined( HB_HAS_LOCALTIME_R )
      utc = mktime( gmtime_r( &current, &timeinfo ) );
      local = mktime( localtime_r( &current, &timeinfo ) );
#else
      timeinfo = *gmtime( &current );
      utc = mktime( &timeinfo );
      timeinfo = *localtime( &current );
      local = mktime( &timeinfo );
#endif
      return ( long ) difftime( local, utc ) + ( timeinfo.tm_isdst > 0 ? 3600 : 0 );
   }
#endif
}

long hb_timeStampUTCOffset( int iYear, int iMonth, int iDay,
                            int iHour, int iMinutes, int iSeconds )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeStampUTCOffset(%d, %d, %d, %d, %d, %d)", iYear, iMonth, iDay, iHour, iMinutes, iSeconds ) );

#if defined( HB_OS_WIN )
   {
      typedef BOOL ( WINAPI * P_TZSPECIFICLOCALTIMETOSYSTEMTIME )( LPTIME_ZONE_INFORMATION, LPSYSTEMTIME, LPSYSTEMTIME );

      static HB_BOOL s_fInit = HB_TRUE;
      static P_TZSPECIFICLOCALTIMETOSYSTEMTIME s_pTzSpecificLocalTimeToSystemTime = NULL;

      if( s_fInit )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "kernel32" ) );
         if( hModule )
            s_pTzSpecificLocalTimeToSystemTime = ( P_TZSPECIFICLOCALTIMETOSYSTEMTIME )
               HB_WINAPI_GETPROCADDRESS( hModule, "TzSpecificLocalTimeToSystemTime" );
         s_fInit = HB_FALSE;
      }

      if( s_pTzSpecificLocalTimeToSystemTime )
      {
         SYSTEMTIME lt, st;

         lt.wYear         = ( WORD ) iYear;
         lt.wMonth        = ( WORD ) iMonth;
         lt.wDay          = ( WORD ) iDay;
         lt.wHour         = ( WORD ) iHour;
         lt.wMinute       = ( WORD ) iMinutes;
         lt.wSecond       = ( WORD ) iSeconds;
         lt.wMilliseconds = 0;
         lt.wDayOfWeek    = 0;

         if( s_pTzSpecificLocalTimeToSystemTime( NULL, &lt, &st ) )
         {
            double dOffset = ( hb_timeStampPack( lt.wYear, lt.wMonth, lt.wDay,
                                                 lt.wHour, lt.wMinute, lt.wSecond,
                                                 lt.wMilliseconds ) -
                               hb_timeStampPack( st.wYear, st.wMonth, st.wDay,
                                                 st.wHour, st.wMinute, st.wSecond,
                                                 st.wMilliseconds ) ) * HB_SECONDS_PER_DAY;
            return ( long ) ( dOffset + ( dOffset < 0 ? -0.5 : 0.5 ) );
         }
      }

      return hb_timeUTCOffset();
   }
#else
   {
      struct tm timeinfo;
      time_t utc, local;

      timeinfo.tm_sec   = iSeconds;       /* seconds */
      timeinfo.tm_min   = iMinutes;       /* minutes */
      timeinfo.tm_hour  = iHour;          /* hours */
      timeinfo.tm_mday  = iDay;           /* day of the month */
      timeinfo.tm_mon   = iMonth - 1;     /* month */
      timeinfo.tm_year  = iYear - 1900;   /* year */
      timeinfo.tm_isdst = -1;             /* daylight saving time */

      local = mktime( &timeinfo );

      if( local != ( time_t ) -1 )
      {
         int isdst = ( timeinfo.tm_isdst > 0 ? 3600 : 0 );
#if defined( HB_HAS_LOCALTIME_R )
         utc = mktime( gmtime_r( &local, &timeinfo ) );
#else
         timeinfo = *gmtime( &local );
         utc = mktime( &timeinfo );
#endif
         return ( long ) difftime( local, utc ) + isdst;
      }
      return 0;
   }
#endif
}

double hb_timeLocalToUTC( double dTimeStamp )
{
   int iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeLocalToUTC(%f)", dTimeStamp ) );

   hb_timeStampUnpack( dTimeStamp,
                       &iYear, &iMonth, &iDay,
                       &iHour, &iMinutes, &iSeconds, &iMSec );

   return dTimeStamp - ( double )
          hb_timeStampUTCOffset( iYear, iMonth, iDay,
                                 iHour, iMinutes, iSeconds ) / HB_SECONDS_PER_DAY;
}

HB_MAXUINT hb_timerGet( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timerGet()" ) );

#if defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 199309L && defined( CLOCK_REALTIME )
   {
      static int s_iClkId = -1;
      struct timespec ts;

      if( s_iClkId < 0 )
      {
         int i, piClkId[] = {
#  if defined( CLOCK_MONOTONIC )
            CLOCK_MONOTONIC,
#  endif
#  if defined( CLOCK_MONOTONIC_COARSE )
            CLOCK_MONOTONIC_COARSE,
#  endif
#  if defined( CLOCK_REALTIME )
            CLOCK_REALTIME,
#  endif
#  if defined( CLOCK_REALTIME_COARSE )
            CLOCK_REALTIME_COARSE,
#  endif
            0 };

         for( i = 0; i < ( int ) HB_SIZEOFARRAY( piClkId ); ++i )
         {
            s_iClkId = piClkId[ i ];
            if( s_iClkId == 0 || clock_getres( s_iClkId, &ts ) == 0 )
               break;
         }
      }
      if( s_iClkId != 0 && clock_gettime( s_iClkId, &ts ) == 0 )
         return ( HB_MAXUINT ) ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
   }
#endif
#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
   {
      struct timeval tv;
      gettimeofday( &tv, NULL );
      return ( HB_MAXUINT ) tv.tv_sec * 1000 + tv.tv_usec / 1000;
   }
#elif defined( HB_OS_WIN )
   {
      static DWORD s_dwCounter = 0, s_dwLast = 0;
      DWORD dwTime = timeGetTime();

      if( dwTime < s_dwLast )
         ++s_dwCounter;
      s_dwLast = dwTime;
      return ( ( HB_MAXUINT ) s_dwCounter << 32 ) + dwTime;
   }
#else
   {
      struct timeb tb;
      ftime( &tb );
      return ( HB_MAXUINT ) tb.time * 1000 + tb.millitm;
   }
#endif
}

HB_MAXUINT hb_timerInit( HB_MAXINT nTimeOut )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timerInit(%" PFHL "d)", nTimeOut ) );

   return nTimeOut > 0 ? hb_timerGet() : 0;
}

HB_MAXINT hb_timerTest( HB_MAXINT nTimeOut, HB_MAXUINT * pnTimer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timerTest(%" PFHL "d, %p)", nTimeOut, ( void * ) pnTimer ) );

   if( nTimeOut > 0 )
   {
      HB_MAXUINT nTime = hb_timerGet();

      if( nTime > *pnTimer )
      {
         nTimeOut -= nTime - *pnTimer;
         if( nTimeOut < 0 )
            nTimeOut = 0;
      }
      *pnTimer = nTime;
   }
   return nTimeOut;
}

#if defined( HB_OS_VXWORKS )

/* NOTE: This function is declared, but not present in
         libs in VxWorks 6.8. So here we emulate its
         base functionality. [vszakats] */

int gettimeofday( struct timeval * tv, void * tz )
{
   int ret;
   struct timespec tp;

   HB_SYMBOL_UNUSED( tz );

   if( ( ret = clock_gettime( CLOCK_REALTIME, &tp ) ) == 0 )
   {
      tv->tv_sec  = tp.tv_sec;
      tv->tv_usec = ( tp.tv_nsec + 500 ) / 1000;
   }
   else
   {
      tv->tv_sec  = 0;
      tv->tv_usec = 0;
   }

   return ret;
}

#endif
