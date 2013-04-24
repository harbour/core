/*
 * Harbour Project source code:
 * hb_SecondsCPU()
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
#include "hbdate.h"

#if defined( HB_OS_UNIX )
   #if ! defined( HB_OS_VXWORKS )
      #include <sys/times.h>
   #endif
   #include <unistd.h>
#endif
#if defined( HB_OS_OS2 )
   #define INCL_DOS
   #define INCL_DOSPROFILE
   #define INCL_ERRORS
   #include <os2.h>
   #define BUFSIZE  16 * 1024
   #include <unistd.h>
   #if defined( __WATCOMC__ )
      #include <process.h>
   #endif
#elif defined( HB_OS_WIN )
   #include <windows.h>
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif
#endif

/*
   SecondsCPU(n) -> nTime
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

#if defined( HB_OS_WIN ) && ! defined( HB_OS_UNIX )
   FILETIME Create, Exit, Kernel, User;
#endif

#if defined( HB_OS_OS2 )
   static ULONG s_timer_interval = 0;

   QSGREC ** pBuf;
#endif

   if( ( n < 1 || n > 3 ) && ( n < 11 || n > 13 ) )
      n = 3;

#if defined( HB_OS_UNIX ) && ! defined( HB_OS_VXWORKS )
   {
      struct tms tm;

      times( &tm );

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
      d /= ( double ) sysconf( _SC_CLK_TCK );
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
         d += ( double ) ( ( ( HB_MAXINT ) User.dwHighDateTime << 32 ) +
                             ( HB_MAXINT ) User.dwLowDateTime );
      }
      if( n & 2 )
      {
         d += ( double ) ( ( ( HB_MAXINT ) Kernel.dwHighDateTime << 32 ) +
                             ( HB_MAXINT ) Kernel.dwLowDateTime );
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
         QSGREC * pGrec = *pBuf;
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
