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

#include "hbapi.h"

#include <time.h>
#if defined( HB_OS_BSD)
   #include <sys/time.h>
#else
   #include <sys/timeb.h>
#endif
#if defined( OS_UNIX_COMPATIBLE )
   #include <sys/times.h>
   #include <unistd.h>
#endif

double hb_dateSeconds( void )
{
#if defined(_MSC_VER)
   #define timeb _timeb
   #define ftime _ftime
#endif
#if defined(HB_OS_BSD)
   struct timeval tv;
   struct timezone tz;
#else
   struct timeb tb;
#endif
   time_t seconds;
   time_t fraction;
   struct tm * oTime;

   HB_TRACE(HB_TR_DEBUG, ("hb_dateSeconds()"));

#if defined(HB_OS_BSD)
   gettimeofday( &tv, &tz );
   seconds = tv.tv_sec;
   fraction = tv.tv_usec / 1000U;
#else
   ftime( &tb );
   seconds = tb.time;
   fraction = tb.millitm;
#endif

   oTime = localtime( &seconds );

   return ( oTime->tm_hour * 3600 ) +
          ( oTime->tm_min * 60 ) +
            oTime->tm_sec +
          ( ( double ) fraction / 1000.0 );
}

#ifdef HB_EXTENSION

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
double hb_secondsCPU(int n)
{
   double d = 0;

#if defined( OS_UNIX_COMPATIBLE )
   struct tms tm;

   times(&tm);

   if ((n < 1 || n > 3) && (n < 11 || n > 13))
      n = 3;
   
   if (n > 10)
   {
      n -= 10;
      if (n & 1)
         d += tm.tms_cutime;
      if (n & 2)
         d += tm.tms_cstime;
   }
   if (n & 1)
      d += tm.tms_utime;
   if (n & 2)
      d += tm.tms_stime;

   d /= (double) sysconf(_SC_CLK_TCK);
#else
   /* TODO: this code is only for DOS and other platforms which cannot
            calculate process time */

   if ((n < 1 || n > 3) && (n < 11 || n > 13))
      n = 3;
   else if (n > 10)
      n -= 10;
   if (n & 1)
      d = hb_dateSeconds(  );
#endif
   return d;
}

#endif

HB_FUNC( SECONDS )
{
   hb_retnd( hb_dateSeconds() );
}

#ifdef HB_EXTENSION

HB_FUNC( SECONDSCPU )
{
   hb_retnd( hb_secondsCPU( hb_parni( 1 ) ) );
}

HB_FUNC( HB_CLOCKS2SECS )
{
   hb_retnd((double) hb_parnl( 1 ) / CLOCKS_PER_SEC );
}

#endif
