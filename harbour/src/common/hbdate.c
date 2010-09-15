/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date conversion module
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
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
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
 *    hb_dateEncStr()
 *    hb_dateDecStr()
 *    hb_dateStrPut()
 *    hb_dateStrGet()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_dateDOW()
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    time/timestamp functions
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbdate.h"
#if defined( HB_LONG_LONG_OFF )
#  include "hbmath.h"
#endif

#include <time.h>
#if defined( HB_OS_UNIX )
#  include <sys/time.h>
#elif defined( HB_OS_WIN )
#  include <windows.h>
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
   #define HB_DATE_YEAR_LIMIT    2999
#else
   #define HB_DATE_YEAR_LIMIT    9999
#endif

#define HB_STR_DATE_BASE      1721060     /* 0000/01/01 */
#define HB_SYS_DATE_BASE      2440588     /* 1970/01/01 */


void hb_timeStampGetLocal( int * piYear, int * piMonth, int * piDay,
                           int * piHour, int * piMinutes,
                           int * piSeconds, int * piMSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampGetLocal(%p,%p,%p,%p,%p,%p,%p)", piYear, piMonth, piDay, piHour, piMinutes, piSeconds, piMSec));

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

#  if defined( HB_OS_UNIX )
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

/* return UTC julian timestamp in milliseconds */
HB_MAXUINT hb_dateMilliSeconds( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateMilliSeconds()"));

#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;
      GetSystemTime( &st );
      return ( HB_MAXUINT ) hb_dateEncode( st.wYear, st.wMonth, st.wDay ) *
             HB_MILLISECS_PER_DAY +
             hb_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );
   }
#elif defined( HB_OS_UNIX )
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
             tb.time;
   }
#endif
}

/* return local timestamp */
void hb_timeStampGet( long * plJulian, long * plMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampGet(%p,%p)", plJulian, plMilliSec));

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
   *plJulian   = hb_dateEncode( iYear, iMonth, iDay );
   *plMilliSec = hb_timeEncode( iHour, iMinute, iSeconds, iMillisec );
}

double hb_dateSeconds( void )
{
   int iYear, iMonth, iDay, iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateSeconds()"));

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
   return ( double ) hb_timeEncode( iHour, iMinute, iSeconds, iMillisec ) / 1000;
}

long hb_dateEncode( int iYear, int iMonth, int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncode(%d, %d, %d)", iYear, iMonth, iDay));

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

         return ( ( long )( iFactor + 4800 + iYear ) * 1461 / 4 ) +
                ( ( long )( iMonth - 2 - ( iFactor * 12 ) ) * 367 ) / 12 -
                ( ( long )( ( iFactor + 4900 + iYear ) / 100 ) * 3 / 4 ) +
                ( long ) iDay - 32075;
      }
   }

   return 0;
}

void hb_dateDecode( long lJulian, int *piYear, int *piMonth, int *piDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecode(%ld, %p, %p, %p)", lJulian, piYear, piMonth, piDay));

   if( lJulian >= HB_STR_DATE_BASE )
   {
      long U, V, W, X;

      lJulian += 68569;
      W = ( lJulian * 4 ) / 146097;
      lJulian -= ( ( 146097 * W ) + 3 ) / 4;
      X = 4000 * ( lJulian + 1 ) / 1461001;
      lJulian -= ( ( 1461 * X ) / 4 ) - 31;
      V = 80 * lJulian / 2447;
      U = V / 11;

      *piYear  = ( int ) ( X + U + ( W - 49 ) * 100 );
      *piMonth = ( int ) ( V + 2 - ( U * 12 ) );
      *piDay   = ( int ) ( lJulian - ( 2447 * V / 80 ) );
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
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrPut(%p, %d, %d, %d)", szDate, iYear, iMonth, iDay));

   if( iYear >= 0 && iMonth > 0 && iDay > 0 )
   {
      szDate[ 0 ] = ( char ) ( ( ( iYear / 1000 ) % 10 ) + '0' );
      szDate[ 1 ] = ( char ) ( ( ( iYear / 100 ) % 10 ) + '0' );
      szDate[ 2 ] = ( char ) ( ( ( iYear / 10 ) % 10 ) + '0' );
      szDate[ 3 ] = ( char ) ( ( iYear % 10 ) + '0' );

      szDate[ 4 ] = ( char ) ( ( iMonth / 10 ) + '0' );
      szDate[ 5 ] = ( char ) ( ( iMonth % 10 ) + '0' );

      szDate[ 6 ] = ( char ) ( ( iDay / 10 ) + '0' );
      szDate[ 7 ] = ( char ) ( ( iDay % 10 ) + '0' );
   }
   else
   {
      memset( szDate, '0', 8 );
   }
}

void hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateStrGet(%.8s, %p, %p, %p)", szDate, piYear, piMonth, piDay));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_dateDecStr(%p, %ld)", szDate, lJulian));

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
   int  iYear, iMonth, iDay;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateEncStr(%.8s)", szDate));

   hb_dateStrGet( szDate, &iYear, &iMonth, &iDay );

   return hb_dateEncode( iYear, iMonth, iDay );
}

int hb_dateJulianDOW( long lJulian )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateJulianDOW(%ld)", lJulian));

   if( lJulian >= HB_STR_DATE_BASE )
      return ( int ) ( ( lJulian + 1 ) % 7 ) + 1;
   else
      return 0;
}

int hb_dateDOW( int iYear, int iMonth, int iDay )
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

void hb_dateToday( int * piYear, int * piMonth, int * piDay )
{
   int iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateToday(%p,%p,%p)", piYear, piMonth, piDay));

   hb_timeStampGetLocal( piYear, piMonth, piDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
}

/* NOTE: The passed buffer must be at least 9 chars long */

void hb_dateTimeStr( char * pszTime )
{
   int iYear, iMonth, iDay, iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateTimeStr(%p)", pszTime));

   hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                         &iHour, &iMinute, &iSeconds, &iMillisec );
   hb_snprintf( pszTime, 9, "%02d:%02d:%02d", iHour, iMinute, iSeconds );
}

/* functions to operate on time and timestamp values */

long hb_timeEncode( int iHour, int iMinutes, int iSeconds, int iMSec )
{
   long lMilliSec;

   HB_TRACE(HB_TR_DEBUG, ("hb_timeEncode(%d, %d, %d, %d)", iHour, iMinutes, iSeconds, iMSec));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_timeDecode(%ld, %p, %p, %p, %p)", lMilliSec, piHour, piMinutes, piSeconds, piMSec));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStr(%p, %ld)", szTime, lMilliSec));

   hb_timeDecode( lMilliSec, &iHour, &iMinutes, &iSeconds, &iMSec );
   hb_snprintf( szTime, 13, "%02d:%02d:%02d.%03d",
                iHour, iMinutes, iSeconds, iMSec );

   return szTime;
}

HB_BOOL hb_timeStrGet( const char * szTime,
                       int * piHour, int * piMinutes,
                       int * piSeconds, int * piMSec )
{
   int iHour, iMinutes, iSeconds, iMSec, iBlocks;
   HB_BOOL fValid;

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStrGet(%s, %p, %p, %p, %p)", szTime, piHour, piMinutes, piSeconds, piMSec));

   iHour = iMinutes = iSeconds = iMSec = iBlocks = 0;
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
         while( HB_ISSPACE( *szTime ) )
            ++szTime;
         if( *szTime == 0 && iBlocks > 0 &&
             iHour < 24 && iMinutes < 60 && iSeconds < 60 )
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

   return fValid;
}

void hb_timeStrRawGet( const char * szTime,
                       int * piHour, int * piMinutes,
                       int * piSeconds, int * piMSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_timeStrRawGet(%.10s, %p, %p, %p, %p)", szTime, piHour, piMinutes, piSeconds, piMSec));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampStrRawPut(%p, %ld, %ld)", szDateTime, lJulian, lMilliSec));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampStrRawGet(%s, %p, %p)", szDateTime, plJulian, plMilliSec));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampStr(%p, %ld, %ld)", szDateTime, lJulian, lMilliSec));

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_timeDecode( lMilliSec, &iHour, &iMinutes, &iSeconds, &iMSec );
   hb_snprintf( szDateTime, 24, "%04d-%02d-%02d %02d:%02d:%02d.%03d",
                iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec );
   szDateTime[ 23 ] = '\0';

   return szDateTime;
}

HB_BOOL hb_timeStampStrGet( const char * szDateTime,
                            int * piYear, int * piMonth, int * piDay,
                            int * piHour, int * piMinutes, int * piSeconds,
                            int * piMSec )
{
   int iYear, iMonth, iDay;
   HB_BOOL fValid;

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampStrGet(%s, %p, %p, %p, %p, %p, %p, %p)", szDateTime, piYear, piMonth, piDay, piHour, piMinutes, piSeconds, piMSec));

   iYear = iMonth = iDay = 0;
   fValid = HB_FALSE;

   if( szDateTime )
   {
      while( HB_ISSPACE( *szDateTime ) )
         ++szDateTime;
      if( HB_ISDIGIT( szDateTime[ 0 ] ) && HB_ISDIGIT( szDateTime[ 1 ] ) &&
          HB_ISDIGIT( szDateTime[ 2 ] ) && HB_ISDIGIT( szDateTime[ 3 ] ) &&
          ( szDateTime[ 4 ] == '-' || szDateTime[ 4 ] == '/' || szDateTime[ 4 ] == '.' ) &&
          HB_ISDIGIT( szDateTime[ 5 ] ) && HB_ISDIGIT( szDateTime[ 6 ] ) &&
          szDateTime[ 7 ] == szDateTime[ 4 ] &&
          HB_ISDIGIT( szDateTime[ 9 ] ) && HB_ISDIGIT( szDateTime[ 9 ] ) &&
          !HB_ISDIGIT( szDateTime[ 10 ] ) )
      {
         iYear  = ( ( ( int ) ( szDateTime[ 0 ] - '0' )   * 10 +
                      ( int ) ( szDateTime[ 1 ] - '0' ) ) * 10 +
                      ( int ) ( szDateTime[ 2 ] - '0' ) ) * 10 +
                      ( int ) ( szDateTime[ 3 ] - '0' );
         iMonth = ( szDateTime[ 5 ] - '0' ) * 10 + ( szDateTime[ 6 ] - '0' );
         iDay   = ( szDateTime[ 8 ] - '0' ) * 10 + ( szDateTime[ 9 ] - '0' );
         if( hb_dateEncode( iYear, iMonth, iDay ) != 0 ||
             ( iYear == 0 && iMonth == 0 && iDay == 0 ) )
         {
            szDateTime += 10;
            if( *szDateTime == 'T' || *szDateTime == 't' )
            {
               if( HB_ISDIGIT( szDateTime[ 1 ] ) )
                  ++szDateTime;
            }
            else
            {
               if( *szDateTime == ',' )
                  ++szDateTime;
               while( HB_ISSPACE( *szDateTime ) )
                  ++szDateTime;
               if( *szDateTime == '\0' )
                  szDateTime = NULL;
               fValid = HB_TRUE;
            }
         }
         else
         {
            iYear = iMonth = iDay = 0;
            szDateTime = NULL;
         }
      }
   }

   if( piHour || piMinutes || piSeconds || piMSec )
   {
      if( !hb_timeStrGet( szDateTime, piHour, piMinutes, piSeconds, piMSec ) )
      {
         if( szDateTime )
            fValid = HB_FALSE;
      }
      else
         fValid = HB_TRUE;
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

HB_BOOL hb_timeStampStrGetDT( const char * szDateTime,
                              long * plJulian, long * plMilliSec )
{
   int iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec;
   HB_BOOL fValid;

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampStrGetDT(%s, %p, %p)", szDateTime, plJulian, plMilliSec));

   fValid = hb_timeStampStrGet( szDateTime, &iYear, &iMonth, &iDay,
                                &iHour, &iMinutes, &iSeconds, &iMSec );
   if( plJulian )
      *plJulian = hb_dateEncode( iYear, iMonth, iDay );
   if( plMilliSec )
      *plMilliSec = hb_timeEncode( iHour, iMinutes, iSeconds, iMSec );

   return fValid;
}

double hb_timeStampPackDT( long lJulian, long lMilliSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampPackDT(%ld, %ld)", lJulian, lMilliSec));

   return ( double ) lJulian +
          ( double ) lMilliSec / HB_MILLISECS_PER_DAY;
}

void hb_timeStampUnpackDT( double dTimeStamp,
                           long * plJulian, long * plMilliSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampUnpackDT(%f, %p, %p)", dTimeStamp, plJulian, plMilliSec));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampPack(%d, %d, %d, %d, %d, %d, %d)", iYear, iMonth, iDay, iHour, iMinutes, iSeconds, iMSec));

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
                                 HB_SECONDS_PER_DAY;
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

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampUnpack(%f, %p, %p, %p, %p, %p, %p, %p)", dTimeStamp, piYear, piMonth, piDay, piHour, piMinutes, piSeconds, piMSec));

   hb_timeStampUnpackDT( dTimeStamp, &lJulian, &lMilliSec );
   hb_dateDecode( lJulian, piYear, piMonth, piDay );
   hb_timeDecode( lMilliSec, piHour, piMinutes, piSeconds, piMSec );
}

double hb_timeStampPackD( int iYear, int iMonth, int iDay,
                          int iHour, int iMinutes, double dSeconds )
{
   double dTimeStamp = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampPackD(%d, %d, %d, %d, %d, %f)", iYear, iMonth, iDay, iHour, iMinutes, dSeconds));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampUnpackD(%f, %p, %p, %p, %p, %p, %p)", dTimeStamp, piYear, piMonth, piDay, piHour, piMinutes, pdSeconds));

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
      utc = mktime( gmtime( &current ) );
      timeinfo = *localtime( &current );
      local = mktime( &timeinfo );
#endif
      return ( long ) difftime( local, utc ) + ( timeinfo.tm_isdst > 0 ? 3600 : 0 );
   }
#endif
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
