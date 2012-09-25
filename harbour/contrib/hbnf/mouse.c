/*
 * $Id$
 */

/*
 * Harbour Project source code
 *
 * mouse.c Support functions for Nanfor Library
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
#  include "dos.h"
#endif
#include "hbapiitm.h"
#include "hbapigt.h"

HB_FUNC( _MGET_PAGE )
{
   int iPage;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1E;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iPage             = regs.HB_XREGS.bx;
   }
#else
   {
      iPage = 0;
   }
#endif
   hb_retni( iPage );
}

HB_FUNC( _MSET_PAGE )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1D;
      regs.HB_XREGS.bx  = hb_parni( 1 );
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _MGET_MVERSION )
{
   int   iMinor;
   int   iType;
   int   iIRQ;
   int   iMajor;

#if defined( HB_OS_DOS )
   {
      union REGS regs;

      regs.HB_XREGS.ax  = 0x24;
      HB_DOS_INT86( 0x33, &regs, &regs );

      iMinor            = regs.h.bl;
      iType             = regs.h.ch;
      iIRQ              = regs.h.cl;
      iMajor            = regs.h.bh;
   }
#else
   {
      iMinor   = 0;
      iType    = 0;
      iIRQ     = 0;
      iMajor   = 0;
   }
#endif

   {
      PHB_ITEM pArray = hb_itemArrayNew( 4 );

      hb_arraySetNI( pArray, 1, iMinor );
      hb_arraySetNI( pArray, 2, iType );
      hb_arraySetNI( pArray, 3, iIRQ );
      hb_arraySetNI( pArray, 4, iMajor );

      hb_itemReturnRelease( pArray );
   }
}

HB_FUNC( _MGET_HORISPEED )
{
   int iSpeed;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1B;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iSpeed            = regs.HB_XREGS.bx;
   }
#else
   {
      iSpeed = 0;
   }
#endif
   hb_retni( iSpeed );
}

HB_FUNC( _MGET_VERSPEED )
{
   int iSpeed;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1B;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iSpeed            = regs.HB_XREGS.cx;
   }
#else
   {
      iSpeed = 0;
   }
#endif
   hb_retni( iSpeed );
}

HB_FUNC( _MGET_DOUBLESPEED )
{
   int iSpeed;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1B;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iSpeed            = regs.HB_XREGS.dx;
   }
#else
   {
      iSpeed = 0;
   }
#endif
   hb_retni( iSpeed );
}

HB_FUNC( _MSET_SENSITIVE ) /* nHoriz,nVert,nDouble) */
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1A;
      regs.HB_XREGS.bx  = hb_parni( 1 );
      regs.HB_XREGS.cx  = hb_parni( 2 );
      regs.HB_XREGS.dx  = hb_parni( 3 );
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _MSE_CONOFF ) /* nTop*8,nLeft*8,nBotton*8,nRight*8) */
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1A;
      regs.HB_XREGS.cx  = hb_parni( 2 );
      regs.HB_XREGS.dx  = hb_parni( 1 );
      regs.HB_XREGS.si  = hb_parni( 4 );
      regs.HB_XREGS.di  = hb_parni( 3 );
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _MGET_MICS )
{
   int   iHori;
   int   iVert;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x0B;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iHori             = regs.HB_XREGS.cx;
      iVert             = regs.HB_XREGS.dx;
   }
#else
   {
      iHori = 0;
      iVert = 0;
   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew( 2 );

      hb_arraySetNI( pArray, 1, iHori );
      hb_arraySetNI( pArray, 2, iVert );

      hb_itemReturnRelease( pArray );
   }
}

HB_FUNC( _M_RESET )
{
   int iMouse;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iMouse            = regs.HB_XREGS.ax;
   }
#else
   {
      iMouse = 0;
   }
#endif
   {
      hb_retl( iMouse );
   }
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

HB_FUNC( _MSE_GETPOS )
{
   int   iHori;
   int   iVert;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iHori             = regs.HB_XREGS.cx;
      iVert             = regs.HB_XREGS.dx;
   }
#else
   {
      iHori = 0;
      iVert = 0;
   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew( 2 );

      hb_arraySetNI( pArray, 1, iHori );
      hb_arraySetNI( pArray, 2, iVert );

      hb_itemReturnRelease( pArray );
   }
}

HB_FUNC( _M_GETX )
{
   int iRow;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iRow              = regs.HB_XREGS.dx;
   }
#else
   {
      iRow = 0;
   }
#endif
   hb_retni( iRow );
}

HB_FUNC( _M_GETY )
{
   int iCol;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
      iCol              = regs.HB_XREGS.cx;
   }
#else
   {
      iCol = 0;
   }
#endif
   hb_retni( iCol );
}

HB_FUNC( _M_MSETPOS )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 4;
      regs.HB_XREGS.cx  = hb_parni( 1 );
      regs.HB_XREGS.dx  = hb_parni( 2 );
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _M_MSETCOORD )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 4;
      regs.HB_XREGS.cx  = hb_parni( 1 );
      regs.HB_XREGS.dx  = hb_parni( 2 );
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _M_MXLIMIT )
{
#if defined( HB_OS_DOS )
   {
      union REGS  regs;
      int         iMaxRow  = hb_parni( 2 );
      int         iMinRow  = hb_parni( 1 );

      regs.HB_XREGS.ax  = 7;
      regs.HB_XREGS.cx  = iMinRow;
      regs.HB_XREGS.dx  = iMaxRow;

      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _M_MYLIMIT )
{
#if defined( HB_OS_DOS )
   {
      union REGS  regs;
      int         iMaxCol  = hb_parni( 2 );
      int         iMinCol  = hb_parni( 1 );
      regs.HB_XREGS.ax  = 8;

      regs.HB_XREGS.cx  = iMinCol;
      regs.HB_XREGS.dx  = iMaxCol;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _M_MBUTPRS )
{
   int   inX;
   int   inY;
   int   inButton;
   int   lStatus;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 6;
      regs.HB_XREGS.bx  = hb_parni( 1 );
      HB_DOS_INT86( 0x33, &regs, &regs );

      inY               = regs.HB_XREGS.cx;
      inX               = regs.HB_XREGS.dx;
      inButton          = regs.HB_XREGS.bx;
      lStatus           = regs.HB_XREGS.ax;
   }
#else
   {
      inY      = 0;
      inX      = 0;
      inButton = 0;
      lStatus  = 0;
   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew( 4 );

      hb_arraySetNI( pArray, 1, inButton ); /* NOTE: I've changed 1 to 3 */
      hb_arraySetNI( pArray, 2, inX );
      hb_arraySetNI( pArray, 3, inY );
      hb_arraySetNI( pArray, 4, lStatus ); /* NOTE: I've changed 1 to 3 */

      hb_itemReturnRelease( pArray );
   }
}

HB_FUNC( _M_MBUTREL )
{
#if defined( HB_OS_DOS )
   union REGS regs;
   regs.HB_XREGS.ax  = 0x0A;
   regs.HB_XREGS.bx  = hb_parni( 1 );

   HB_DOS_INT86( 0x33, &regs, &regs );

   hb_reta( 4 );
   hb_storvni( regs.HB_XREGS.bx, -1, 1 );
   hb_storvni( regs.HB_XREGS.cx, -1, 2 );
   hb_storvni( regs.HB_XREGS.dx, -1, 3 );
   hb_storvni( regs.HB_XREGS.ax, -1, 4 );
#else
   hb_reta( 4 );
   hb_storvni( 0, -1, 1 );
   hb_storvni( 0, -1, 2 );
   hb_storvni( 0, -1, 3 );
   hb_storvni( 0, -1, 4 );
#endif
}

HB_FUNC( _M_MDEFCRS )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x0A;
      regs.HB_XREGS.bx  = hb_parni( 1 );
      regs.HB_XREGS.cx  = hb_parni( 2 );
      regs.HB_XREGS.dx  = hb_parni( 3 );

      HB_DOS_INT86( 0x33, &regs, &regs );
   }
#endif
}

HB_FUNC( _M_MGETCOORD )
{
   int   inX;
   int   inY;
   int   inButton;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      inButton          = regs.HB_XREGS.bx;
      inY               = regs.HB_XREGS.cx;
      inX               = regs.HB_XREGS.dx;
   }
#else
   {
      inX      = 0;
      inY      = 0;
      inButton = 0;
   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew( 3 );

      hb_arraySetNI( pArray, 1, inX );
      hb_arraySetNI( pArray, 2, inY );
      hb_arraySetNI( pArray, 3, inButton );

      hb_itemReturnRelease( pArray );
   }
}
