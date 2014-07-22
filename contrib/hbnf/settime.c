/*
 * Rewritten in 2012 by Viktor Szakats (vszakats.net/harbour) and kept in the
 * public domain.
 * This is an original work by Glenn Scott and is placed in the public domain.
 *
 * Modification history:
 *
 *    Rev 1.3   15 Aug 1991 23:06:08   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:00   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:34:58   GLENN
 * Documentation mods: change documented return value form "n" to "l" in
 * accordance with the new return value from ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:16   GLENN
 * Nanforum Toolkit
 *
 */

#include "hbapi.h"
#include "hbdate.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

HB_FUNC( FT_SETTIME )
{
#if defined( HB_OS_DOS )
   int        iHour, iMinute, iSeconds;
   union REGS regs;

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
      int iYear, iMonth, iDay, iMillisec;
      hb_timeStampGetLocal( &iYear, &iMonth, &iDay,
                            &iHour, &iMinute, &iSeconds, &iMillisec );
   }

   regs.h.ah = 45;
   regs.h.ch = iHour;
   regs.h.cl = iMinute;
   regs.h.dh = iSeconds;
   HB_DOS_INT86( 0x21, &regs, &regs );

   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}
