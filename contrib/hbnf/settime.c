/* Rewritten in 2012 by Viktor Szakats (vszakats.net/harbour) and kept in the
   public domain.
   This is an original work by Glenn Scott and is placed in the public domain.

      Rev 1.3   15 Aug 1991 23:06:08   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.2   14 Jun 1991 19:53:00   GLENN
   Minor edit to file header

      Rev 1.1   12 Jun 1991 02:34:58   GLENN
   Documentation mods: change documented return value form "n" to "l" in
   accordance with the new return value from ft_int86().

      Rev 1.0   01 Apr 1991 01:02:16   GLENN
   Nanforum Toolkit
 */

/* stime() exists only in SVr4, SVID, X/OPEN and Linux */
#ifndef _SVID_SOURCE
#define _SVID_SOURCE
#endif

#include "hbapi.h"
#include "hbdate.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#elif defined( HB_OS_DOS )
   #include <dos.h>
#endif
#include <time.h>

HB_FUNC( FT_SETTIME )
{
   HB_BOOL fResult;
   int iHour = 0, iMinute = 0, iSeconds = 0, iMillisec = 0;

   if( HB_ISCHAR( 1 ) )
   {
      const char * pszTime = hb_parc( 1 );
      HB_SIZE      nLen    = strlen( pszTime );

      if( nLen >= 1 )
         iHour = ( int ) hb_strVal( pszTime, nLen );
      if( nLen >= 4 )
         iMinute = ( int ) hb_strVal( pszTime + 3, nLen - 3 );
      if( nLen >= 7 )
         iSeconds = ( int ) hb_strVal( pszTime + 6, nLen - 6 );
   }
   else
   {
      int iYear, iMonth, iDay;
      hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                            &iHour, &iMinute, &iSeconds, &iMillisec );
   }

#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;
      GetLocalTime( &st );
      st.wHour         = ( WORD ) iHour;
      st.wMinute       = ( WORD ) iMinute;
      st.wSecond       = ( WORD ) iSeconds;
      st.wMilliseconds = ( WORD ) iMillisec * 10;
      fResult = SetLocalTime( &st );
   }
#elif defined( HB_OS_LINUX ) && ! defined( HB_OS_ANDROID ) && ! defined( __WATCOMC__ )
   {
      /* stime() exists only in SVr4, SVID, X/OPEN and Linux */
      HB_ULONG lNewTime;
      time_t   tm;

      lNewTime = iHour * 3600 + iMinute * 60 + iSeconds;
      tm       = time( NULL );
      tm      += lNewTime - ( tm % 86400 );
      fResult  = stime( &tm ) == 0;
   }
#elif defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.h.ah = 45;
      regs.h.ch = iHour;
      regs.h.cl = iMinute;
      regs.h.dh = iSeconds;
      HB_DOS_INT86( 0x21, &regs, &regs );
      fResult = regs.h.al == 0;
   }
#else
   fResult = HB_FALSE;
#endif

   hb_retl( fResult );
}
