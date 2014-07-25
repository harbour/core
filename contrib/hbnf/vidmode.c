/* Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
   See COPYING.txt for licensing terms.

   This is an original work by Glenn Scott and is placed in the public domain.

      Rev 1.3   15 Aug 1991 23:06:12   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.2   14 Jun 1991 19:53:14   GLENN
   Minor edit to file header

      Rev 1.1   14 Jun 1991 18:00:42   GLENN
   Documentation change (minor), and checked for compatibility with new
   ft_int86().

      Rev 1.0   01 Apr 1991 01:02:30   GLENN
   Nanforum Toolkit
 */

#include "hbapi.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

HB_FUNC( FT_SETMODE )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.h.ah = 0;
      regs.h.al = ( HB_U8 ) hb_parni( 1 );
      HB_DOS_INT86( 0x10, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_GETMODE )
{
   int iMode;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.h.ah = 0x0F;
      HB_DOS_INT86( 0x10, &regs, &regs );
      iMode = regs.h.al;
   }
#else
   iMode = 0;
#endif

   hb_retni( iMode );
}
