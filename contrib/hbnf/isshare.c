/* Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
   See COPYING.txt for licensing terms. */

#include "hbapi.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

HB_FUNC( FT_ISSHARE )
{
   int iShare;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0x1000;
      regs.HB_XREGS.cx = 0;
      HB_DOS_INT86( 0x2F, &regs, &regs );
      iShare = regs.h.al;
   }
#else
   iShare = 0;
#endif

   hb_retni( iShare );
}
