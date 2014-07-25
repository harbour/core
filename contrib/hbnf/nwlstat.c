/* Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
   See COPYING.txt for licensing terms.

   This is an original work by Glenn Scott and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:06:04   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   12 Jun 1991 02:19:46   GLENN
   Documentation correction and check for compatibility with new return
   value for ft_int86().

      Rev 1.0   01 Apr 1991 01:01:54   GLENN
   Nanforum Toolkit
 */

#include "hbapi.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

HB_FUNC( FT_NWLSTAT )
{
   int iConnect;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0xDC;
      HB_DOS_INT86( 0x2F, &regs, &regs );
      iConnect = regs.h.al;

      if( iConnect < 0 )
         iConnect += 256
   }
#else
   iConnect = 0;
#endif

   hb_retni( iConnect );
}

HB_FUNC( FT_REBOOT )
{
#if defined( HB_OS_DOS )
   int iTODO;
#endif
}
