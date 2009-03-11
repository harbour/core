/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SECONDS() function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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

#define HB_OS_WIN_USED

#define INCL_DOS
#define INCL_DOSPROFILE

#include "hbapi.h"
#include "hbdate.h"

#include <time.h>

#if ( defined( HB_OS_BSD ) || defined( HB_OS_LINUX ) ) && !defined( __WATCOMC__ )
   #include <sys/time.h>
#elif !( defined( HB_OS_WIN_CE ) && defined( _MSC_VER ) )
   #include <sys/timeb.h>
#endif
#if defined( HB_OS_UNIX_COMPATIBLE )
   #include <sys/times.h>
   #include <unistd.h>
#endif
#if defined( HB_OS_WIN )
   #include <windows.h>
#elif defined(_MSC_VER)
   #define timeb _timeb
   #define ftime _ftime
#endif
#if defined( HB_OS_OS2 )
   #define BUFSIZE   16 * 1024
   #include <unistd.h>
   #if defined( __WATCOMC__ )
      #include <process.h>
   #endif
#endif

void hb_dateTimeStamp( LONG * plJulian, LONG * plMilliSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateTimeStamp(%p,%p)", plJulian, plMilliSec));

#if defined(HB_OS_WIN)
   {
      SYSTEMTIME st;

      GetLocalTime( &st );

      *plJulian = hb_dateEncode( st.wYear, st.wMonth, st.wDay );
      *plMilliSec = ( ( st.wHour * 60 + st.wMinute ) * 60 + st.wSecond ) * 1000 +
                    st.wMilliseconds;
   }
#else
   {
      struct tm st;
      time_t seconds, millisecs;

#  if defined( HB_OS_UNIX ) && !defined( __WATCOMC__ )
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

#if ( defined( _POSIX_C_SOURCE ) || defined( _XOPEN_SOURCE ) || \
      defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) ) && \
    ! defined( HB_OS_DARWIN_5 )
      localtime_r( &seconds, &st );
#else
      st = *localtime( &seconds );
#endif
      *plJulian = hb_dateEncode( st.tm_year + 1900, st.tm_mon + 1, st.tm_mday );
      *plMilliSec = ( ( st.tm_hour * 60 + st.tm_min ) * 60 + st.tm_sec ) * 1000 +
                    millisecs;
   }
#endif
}

HB_ULONG hb_dateMilliSeconds( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateMilliSeconds()"));

#if defined(HB_OS_WIN)
   {
      SYSTEMTIME st;
      GetLocalTime( &st );
      return ( HB_ULONG ) hb_dateEncode( st.wYear, st.wMonth, st.wDay ) * 86400000L +
             ( ( st.wHour * 60 + st.wMinute ) * 60 + st.wSecond ) * 1000 +
             st.wMilliseconds;
   }
#elif defined( HB_OS_UNIX ) && !defined( __WATCOMC__ )
   {
      struct timeval tv;
      gettimeofday( &tv, NULL );
      return ( HB_ULONG ) tv.tv_sec * 1000 + tv.tv_usec / 1000;
   }
#else
   {
      struct timeb tb;
      ftime( &tb );
      return ( HB_ULONG ) tb.time * 1000 + tb.millitm;
   }
#endif
}

double hb_dateSeconds( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateSeconds()"));
#if defined(HB_OS_WIN)
   {
      SYSTEMTIME SystemTime;
      GetLocalTime( &SystemTime );
      return ( SystemTime.wHour * 3600 ) +
             ( SystemTime.wMinute * 60 ) +
               SystemTime.wSecond +
             ( ( double ) SystemTime.wMilliseconds / 1000.0 );
   }
#else
   {
      struct tm oTime;
      time_t seconds;
      double msecs;

#  if defined( HB_OS_UNIX ) && !defined( __WATCOMC__ )
      struct timeval tv;
      gettimeofday( &tv, NULL );
      seconds = tv.tv_sec;
      msecs = ( double ) tv.tv_usec / 1000000.0;
#  else
      struct timeb tb;
      ftime( &tb );
      seconds = tb.time;
      msecs = ( double ) tb.millitm / 1000.0;
#  endif

#if ( defined( _POSIX_C_SOURCE ) || defined( _XOPEN_SOURCE ) || \
      defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) ) && \
    ! defined( HB_OS_DARWIN_5 )
      localtime_r( &seconds, &oTime );
#else
      oTime = *localtime( &seconds );
#endif
      return ( oTime.tm_hour * 3600 ) +
             ( oTime.tm_min * 60 ) +
               oTime.tm_sec + msecs;
   }
#endif
}

HB_FUNC( SECONDS )
{
   hb_retnd( hb_dateSeconds() );
}


HB_FUNC( HB_MILLISECONDS )
{
   hb_retnint( ( HB_LONG ) hb_dateMilliSeconds() );
}


HB_FUNC( HB_CLOCKS2SECS )
{
#ifdef CLOCKS_PER_SEC
   hb_retnd( ( double ) hb_parnint( 1 ) / CLOCKS_PER_SEC );
#else
   hb_retnd( ( double ) hb_parnint( 1 ) / 1000 );
#endif
}

/*
   secondsCPU(n) -> nTime
   FlagShip/CLIP compatible function, which reports how many CPU and/or
   system seconds have elapsed since the beginning of the program execution.
    n == 1  utime  -> user CPU time of the current process
    n == 2  stime  -> system CPU time behalf of the current process
    n == 3  u + s  -> sum of utime + stime (default)
    n == 11 cutime -> sum of the user CPU time of the current + child process
    n == 12 cstime -> sum of the system CPU time of the current + child process
    n == 13 cu+cs  -> sum of cutime + cstime
*/
double hb_secondsCPU( int n )
{
   double d = 0.0;
#if defined( HB_OS_WIN ) && !defined( HB_OS_UNIX_COMPATIBLE )
   FILETIME Create, Exit, Kernel, User;
#endif

#if defined( HB_OS_OS2 )
   static ULONG s_timer_interval = 0;

   QSGREC ** pBuf;
#endif

   if( ( n < 1 || n > 3 ) && ( n < 11 || n > 13 ) )
      n = 3;

#if defined( HB_OS_UNIX_COMPATIBLE )
   {
      struct tms tm;

      times(&tm);

      if( n > 10 )
      {
         n -= 10;
         if( n & 1 )
            d += tm.tms_cutime;
         if( n & 2 )
            d += tm.tms_cstime;
      }
      if( n & 1 )
         d += tm.tms_utime;
      if( n & 2 )
         d += tm.tms_stime;

      /* In POSIX-1996 the CLK_TCK symbol is mentioned as obsolescent */
      /* d /= CLK_TCK; */
      d /= (double) sysconf(_SC_CLK_TCK);
   }
#else
   if( n > 10 )
      n -= 10;
#if defined( HB_OS_WIN )
   if( hb_iswinnt() &&
       GetProcessTimes( GetCurrentProcess(), &Create, &Exit, &Kernel, &User ) )
   {
      if( n & 1 )
      {
         d += ( double ) ( ( ( HB_LONG ) User.dwHighDateTime << 32 ) +
                             ( HB_LONG ) User.dwLowDateTime );
      }
      if( n & 2 )
      {
         d += ( double ) ( ( ( HB_LONG ) Kernel.dwHighDateTime << 32 ) +
                             ( HB_LONG ) Kernel.dwLowDateTime );
      }
      d /= 10000000.0;
   }
   else
#elif defined( HB_OS_OS2 )

   if( s_timer_interval == 0 )
      DosQuerySysInfo( QSV_TIMER_INTERVAL, QSV_TIMER_INTERVAL, ( PVOID ) &s_timer_interval, sizeof( ULONG ) );

   pBuf = ( QSGREC ** ) hb_xalloc( BUFSIZE );

   if( pBuf )
   {
#if defined( __GNUC__ )
      APIRET rc = DosQuerySysState( QS_PROCESS, 0L, _getpid(), 0L, pBuf, BUFSIZE );
#else
      APIRET rc = DosQuerySysState( QS_PROCESS, 0L, getpid(), 0L, pBuf, BUFSIZE );
#endif

      if( rc == NO_ERROR )
      {
         QSGREC * pGrec = * pBuf;
         QSPREC * pPrec = ( QSPREC * ) ( ( ULONG ) pGrec + sizeof( QSGREC ) );
         QSTREC * pTrec = pPrec->pThrdRec;

         int i;

         for( i = 0; i < pPrec->cTCB; i++, pTrec++ )
         {
            if( n & 1 )
               d += pTrec->usertime;

            if( n & 2 )
               d += pTrec->systime;
         }

         d = d * 10.0 / s_timer_interval;
      }

      hb_xfree( pBuf );
   }
   else

#endif
   {
      /* TODO: this code is only for DOS and other platforms which cannot
               calculate process time */

      if( n & 1 )
         d = hb_dateSeconds();
   }
#endif
   return d;
}
