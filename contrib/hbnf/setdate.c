/* Rewritten in 2012 by Viktor Szakats (vszakats.net/harbour) and kept in the
   public domain.
   This is an original work by Glenn Scott and is placed in the public domain.

      Rev 1.3   15 Aug 1991 23:04:36   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.2   14 Jun 1991 19:52:58   GLENN
   Minor edit to file header

      Rev 1.1   12 Jun 1991 02:32:28   GLENN
   Documentation mod and change documented return value from "n" to "l"
   reflecting Ted's update of ft_int86().

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

HB_FUNC( FT_SETDATE )
{
   HB_BOOL fResult;
   int     iYear, iMonth, iDay;
   long    lDate;

   if( HB_ISDATE( 1 ) )
      hb_dateDecode( lDate = hb_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }

#if defined( HB_OS_WIN )
   {
      SYSTEMTIME st;
      GetLocalTime( &st );
      st.wYear      = ( WORD ) iYear;
      st.wMonth     = ( WORD ) iMonth;
      st.wDay       = ( WORD ) iDay;
      st.wDayOfWeek = ( WORD ) hb_dateJulianDOW( lDate );
      fResult       = SetLocalTime( &st );
   }
#elif defined( HB_OS_LINUX ) && ! defined( HB_OS_ANDROID ) && ! defined( __WATCOMC__ )
   {
      /* stime() exists only in SVr4, SVID, X/OPEN and Linux */
      long   lNewDate;
      time_t tm;

      lNewDate = lDate - hb_dateEncode( 1970, 1, 1 );
      tm       = time( NULL );
      tm       = lNewDate * 86400 + ( tm % 86400 );
      fResult  = stime( &tm ) == 0;
   }
#elif defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.h.ah        = 43;
      regs.HB_XREGS.cx = iYear;
      regs.h.dh        = iMonth;
      regs.h.dl        = iDay;
      HB_DOS_INT86( 0x21, &regs, &regs );
      fResult = regs.h.al == 0;
   }
#else
   fResult = HB_FALSE;
#endif

   hb_retl( fResult );
}
