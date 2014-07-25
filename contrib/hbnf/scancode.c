/* Rewritten in 2014 by Viktor Szakats (vszakats.net/harbour) and kept in the
   public domain.
   This is an original work by Glenn Scott/John Kaster and is placed in the
   public domain.

      Rev 1.3   15 Aug 1991 23:04:32   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.2   14 Jun 1991 19:52:52   GLENN
   Minor edit to file header

      Rev 1.1   12 Jun 1991 02:30:32   GLENN
   Documentation mod and check for ft_int86() compatibility

      Rev 1.0   01 Apr 1991 01:02:12   GLENN
   Nanforum Toolkit
 */

#include "hbapi.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

HB_FUNC( FT_SCANCODE )
{
   HB_BYTE ret[ 2 ];

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0;
      HB_DOS_INT86( 0x16, &regs, &regs );
      ret[ 0 ] = regs.h.al;
      ret[ 1 ] = regs.h.ah;
   }
#else
   ret[ 0 ] = ret[ 1 ] = 0;
#endif

   hb_retclen( ( char * ) ret, 2 );
}
