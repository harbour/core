/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * NETNAME() function
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#define HB_OS_WIN_32_USED

#include "hbapi.h"

/* TODO: Implement NETNAME() for other platforms */

/* NOTE: Clipper will only return a maximum of 15 bytes from this function.
         And it will be padded with spaces. Harbour does the same in the
         DOS platform.
         [vszakats] */

HB_FUNC( NETNAME )
{
#if defined(HB_OS_DOS)
   {
      char szValue[ 16 ];
      union REGS regs;

      regs.HB_XREGS.ax = 0x5E00;

      #if defined(__DJGPP__)
      {
         /* TODO: Add support for protected mode */
         szValue[ 0 ] = '\0';
      }
      #else
      {
         struct SREGS sregs;

         regs.HB_XREGS.dx = FP_OFF( szValue );
         sregs.ds = FP_SEG( szValue );

         HB_DOS_INT86X( 0x21, &regs, &regs, &sregs );
      }
      #endif

      hb_retc( regs.h.ch == 0 ? "" : szValue );
   }
#elif defined(HB_OS_WIN_32)
   {
      DWORD ulLen = MAX_COMPUTERNAME_LENGTH + 1;
      char * pszValue = ( char * ) hb_xgrab( ulLen );

      pszValue[ 0 ] = '\0';

      GetComputerName( pszValue, &ulLen );

      hb_retc( pszValue );
      hb_xfree( pszValue );
   }
#else
   hb_retc( "" );
#endif
}

