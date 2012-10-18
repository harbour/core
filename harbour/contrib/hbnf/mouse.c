/*
 * $Id$
 */

/*
 * Harbour Project source code
 * Mouse support functions for Nanfor Library
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
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
 * along with this software; see the file COPYING.  If not, write to
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

#if defined( HB_OS_DOS )
#  include <dos.h>
#endif

HB_FUNC( _MSET_SENSITIVE ) /* nHoriz, nVert, nDouble */
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0x1A;
      regs.HB_XREGS.bx = hb_parni( 1 );
      regs.HB_XREGS.cx = hb_parni( 2 );
      regs.HB_XREGS.dx = hb_parni( 3 );
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_MGETSENS )
{
   int iHoriz;
   int iVert;
   int iDouble;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0x1B;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iHoriz  = regs.HB_XREGS.bx;
      iVert   = regs.HB_XREGS.cx;
      iDouble = regs.HB_XREGS.dx;
   }
#else
   {
      iHoriz  = 0;
      iVert   = 0;
      iDouble = 0;
   }
#endif

   hb_storni( iHoriz, 1 );
   hb_storni( iVert, 2 );
   hb_storni( iDouble, 3 );
}

HB_FUNC( FT_MCONOFF )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0x1A;
      regs.HB_XREGS.cx = hb_parni( 2 ) * 8; /* nLeft */
      regs.HB_XREGS.dx = hb_parni( 1 ) * 8; /* nTop */
      regs.HB_XREGS.si = hb_parni( 4 ) * 8; /* nRight */
      regs.HB_XREGS.di = hb_parni( 3 ) * 8; /* nBottom */
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_MBUTPRS )
{
   int inX;
   int inY;
   int inButton;
   int iStatus;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 5;
      regs.HB_XREGS.bx = hb_parni( 1 );
      HB_DOS_INT86( 0x33, &regs, &regs );
      inX      = regs.HB_XREGS.dx;
      inY      = regs.HB_XREGS.cx;
      inButton = regs.HB_XREGS.bx;
      iStatus  = regs.HB_XREGS.ax;
   }
#else
   {
      inX      = 0;
      inY      = 0;
      inButton = 0;
      iStatus  = 0;
   }
#endif

   hb_storni( inButton, 2 );
   hb_storni( inX, 3 );
   hb_storni( inY, 4 );

   hb_retni( iStatus );
}

HB_FUNC( FT_MBUTREL )
{
   int inX;
   int inY;
   int inButton;
   int iStatus;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 6;
      regs.HB_XREGS.bx = hb_parni( 1 );
      HB_DOS_INT86( 0x33, &regs, &regs );
      inX      = regs.HB_XREGS.dx;
      inY      = regs.HB_XREGS.cx;
      inButton = regs.HB_XREGS.bx;
      iStatus  = regs.HB_XREGS.ax;
   }
#else
   {
      inX      = 0;
      inY      = 0;
      inButton = 0;
      iStatus  = 0;
   }
#endif

   hb_storni( inButton, 2 );
   hb_storni( inX, 3 );
   hb_storni( inY, 4 );

   hb_retni( iStatus );
}

HB_FUNC( FT_MDEFCRS )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 10;
      regs.HB_XREGS.bx = hb_parni( 1 ); /* nCurType */
      regs.HB_XREGS.cx = hb_parni( 2 ); /* nScrMask */
      regs.HB_XREGS.dx = hb_parni( 3 ); /* nCurMask */
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}
