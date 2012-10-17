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

HB_FUNC( FT_MGETPAGE )
{
   int iPage;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0x1E;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iPage = regs.HB_XREGS.bx;
   }
#else
   {
      iPage = 0;
   }
#endif

   hb_retni( iPage );
}

HB_FUNC( FT_MSETPAGE )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0x1D;
      regs.HB_XREGS.bx = hb_parni( 1 );
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_MVERSION )
{
   int iMinor;
   int iType;
   int iIRQ;
   int iMajor;

#if defined( HB_OS_DOS )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 0x24;
      HB_DOS_INT86( 0x33, &regs, &regs );

      iMinor = regs.h.bl;
      iType  = regs.h.ch;
      iIRQ   = regs.h.cl;
      iMajor = regs.h.bh;
   }
#else
   {
      iMinor = 0;
      iType  = 0;
      iIRQ   = 0;
      iMajor = 0;
   }
#endif

   hb_storni( iMinor, 1 );
   hb_storni( iType, 2 );
   hb_storni( iIRQ, 3 );

   hb_retni( iMajor );
}

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

HB_FUNC( FT_MMICKEYS )
{
   int iX;
   int iY;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0x0B;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iX = regs.HB_XREGS.cx;
      iY = regs.HB_XREGS.dx;
   }
#else
   {
      iX = 0;
      iY = 0;
   }
#endif

   hb_storni( 1, iX );
   hb_storni( 2, iY );
}

HB_FUNC( _M_RESET )
{
   HB_BOOL fMouse;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0;
      HB_DOS_INT86( 0x33, &regs, &regs );
      fMouse = regs.HB_XREGS.ax != 0;
   }
#else
   {
      fMouse = HB_FALSE;
   }
#endif

   hb_retl( fMouse );
}

HB_FUNC( _MSE_SHOWCURS )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 1;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _MSE_MHIDECRS )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 2;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_MGETPOS )
{
   int iX;
   int iY;
   int iButton;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iX      = regs.HB_XREGS.dx;
      iY      = regs.HB_XREGS.cx;
      iButton = regs.HB_XREGS.bx;
   }
#else
   {
      iX      = 0;
      iY      = 0;
      iButton = 0;
   }
#endif

   hb_storni( iX, 1 );
   hb_storni( iY, 2 );

   hb_retni( iButton );
}

HB_FUNC( FT_MGETX )
{
   int iRow;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iRow = regs.HB_XREGS.dx / 8;
   }
#else
   {
      iRow = 0;
   }
#endif

   hb_retni( iRow );
}

HB_FUNC( FT_MGETY )
{
   int iCol;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iCol = regs.HB_XREGS.cx / 8;
   }
#else
   {
      iCol = 0;
   }
#endif

   hb_retni( iCol );
}

HB_FUNC( FT_MSETPOS )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 4;
      regs.HB_XREGS.dx = hb_parni( 1 ); /* x */
      regs.HB_XREGS.cx = hb_parni( 2 ); /* y */
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_MSETCOORD )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 4;
      regs.HB_XREGS.dx = hb_parni( 1 ) * 8; /* x */
      regs.HB_XREGS.cx = hb_parni( 2 ) * 8; /* y */
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_MXLIMIT )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 7;
      regs.HB_XREGS.cx = hb_parni( 1 ); /* nXMin */
      regs.HB_XREGS.dx = hb_parni( 2 ); /* nXMax */
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_MYLIMIT )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 8;
      regs.HB_XREGS.cx = hb_parni( 1 ); /* nYMin */
      regs.HB_XREGS.dx = hb_parni( 2 ); /* nYMax */
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

HB_FUNC( FT_MGETCOORD )
{
   int inX;
   int inY;
   int inButton;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
      inX      = regs.HB_XREGS.dx / 8;
      inY      = regs.HB_XREGS.cx / 8;
      inButton = regs.HB_XREGS.bx;
   }
#else
   {
      inX      = 0;
      inY      = 0;
      inButton = 0;
   }
#endif

   hb_storni( inX, 1 );
   hb_storni( inY, 2 );

   hb_retni( inButton );
}
