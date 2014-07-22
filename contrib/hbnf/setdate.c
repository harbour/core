/*
 * Rewritten in 2012 by Viktor Szakats (vszakats.net/harbour) and kept in the
 * public domain.
 * This is an original work by Glenn Scott and is placed in the public domain.
 *
 * Modification history:
 *
 *    Rev 1.3   15 Aug 1991 23:04:36   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:52:58   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:32:28   GLENN
 * Documentation mod and change documented return value from "n" to "l"
 * reflecting Ted's update of ft_int86().
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

HB_FUNC( FT_SETDATE )
{
#if defined( HB_OS_DOS )
   int        iYear, iMonth, iDay;
   union REGS regs;

   if( HB_ISDATE( 1 ) )
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      hb_dateToday( &iYear, &iMonth, &iDay );

   regs.h.ah        = 43;
   regs.HB_XREGS.cx = iYear;
   regs.h.dh        = iMonth;
   regs.h.dl        = iDay;
   HB_DOS_INT86( 0x21, &regs, &regs );

   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}
